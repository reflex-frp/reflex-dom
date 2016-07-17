{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Internal where

import Prelude hiding (concat, mapM, mapM_, sequence, sequence_)

import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Class
import Reflex.Dom.Internal.Foreign
import Reflex.Dom.PerformEvent.Base
import Reflex.Dom.PostBuild.Class
import Reflex.Host.Class
import Reflex.Spider (Global, Spider, SpiderHost, runSpiderHost)

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Reader hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Control.Monad.Ref
import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum (..))
import Data.IORef
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding
import GHCJS.DOM hiding (runWebGUI)
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import qualified GHCJS.DOM.Types as DOM

{-# INLINABLE mainWidget #-}
mainWidget :: (forall x. Widget x ()) -> IO ()
mainWidget w = runWebGUI $ \webView -> withWebViewSingleton webView $ \webViewSing -> do
  Just doc <- fmap DOM.castToHTMLDocument <$> webViewGetDomDocument webView
  Just body <- getBody doc
  attachWidget body webViewSing w

--TODO: The x's should be unified here
{-# INLINABLE mainWidgetWithHead #-}
mainWidgetWithHead :: (forall x. Widget x ()) -> (forall x. Widget x ()) -> IO ()
mainWidgetWithHead h b = runWebGUI $ \webView -> withWebViewSingleton webView $ \webViewSing -> do
  Just doc <- fmap DOM.castToHTMLDocument <$> webViewGetDomDocument webView
  Just headElement <- fmap DOM.castToHTMLElement <$> getHead doc
  attachWidget headElement webViewSing h
  Just body <- getBody doc
  attachWidget body webViewSing b

{-# INLINABLE mainWidgetWithCss #-}
mainWidgetWithCss :: ByteString -> (forall x. Widget x ()) -> IO ()
mainWidgetWithCss css w = runWebGUI $ \webView -> withWebViewSingleton webView $ \webViewSing -> do
  Just doc <- fmap DOM.castToHTMLDocument <$> webViewGetDomDocument webView
  Just headElement <- fmap DOM.castToHTMLElement <$> getHead doc
  setInnerHTML headElement . Just $ "<style>" <> T.unpack (decodeUtf8 css) <> "</style>" --TODO: Fix this
  Just body <- getBody doc
  attachWidget body webViewSing w

{-# INLINABLE mainWidgetFragment #-}
mainWidgetFragment :: (forall x. Widget x ()) -> IO ()
mainWidgetFragment w = runWebGUI $ \webView -> withWebViewSingleton webView $ \webViewSing -> do
  Just doc <- fmap DOM.castToHTMLDocument <$> webViewGetDomDocument webView
  Just body <- getBody doc
  attachWidgetFragment body webViewSing w

type Widget x = PostBuildT Spider (ImmediateDomBuilderT Spider (WithWebView x (PerformEventT Spider (SpiderHost Global)))) --TODO: Make this more abstract --TODO: Put the WithWebView underneath PerformEventT - I think this would perform better

{-# INLINABLE attachWidget #-}
attachWidget :: DOM.IsElement e => e -> WebViewSingleton x -> Widget x a -> IO a
attachWidget rootElement wv w = fst <$> attachWidget' rootElement wv w

{-# INLINABLE attachWidget' #-}
attachWidget' :: DOM.IsElement e => e -> WebViewSingleton x -> Widget x a -> IO (a, FireCommand Spider (SpiderHost Global))
attachWidget' rootElement wv w = do
  Just doc <- getOwnerDocument rootElement
  events <- newChan
  setInnerHTML rootElement $ Just (""::String)
  (result, fc@(FireCommand fire)) <- runSpiderHost $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    let builderEnv = ImmediateDomBuilderEnv
          { _immediateDomBuilderEnv_document = doc
          , _immediateDomBuilderEnv_parent = toNode rootElement
          , _immediateDomBuilderEnv_events = events
          }
    results@(_, FireCommand fire) <- hostPerformEventT $ runWithWebView (runImmediateDomBuilderT (runPostBuildT w postBuild) builderEnv) wv
    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
    return results
  void $ forkIO $ forever $ do
    ers <- readChan events
    _ <- postGUISync $ runSpiderHost $ do
      mes <- liftIO $ forM ers $ \(TriggerRef er :=> TriggerInvocation a _) -> do
        me <- readIORef er
        return $ fmap (\e -> e :=> Identity a) me
      _ <- fire (catMaybes mes) $ return ()
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
    return ()
  return (result, fc)

{-# INLINABLE attachWidgetFragment #-}
attachWidgetFragment :: DOM.IsElement e => e -> WebViewSingleton x -> Widget x a -> IO a
attachWidgetFragment rootElement wv w = fst <$> attachWidgetFragment' rootElement wv w

{-# INLINABLE attachWidgetFragment' #-}
attachWidgetFragment' :: DOM.IsElement e => e -> WebViewSingleton x -> Widget x a -> IO (a, FireCommand Spider (SpiderHost Global))
attachWidgetFragment' rootElement wv w = do
  Just doc <- getOwnerDocument rootElement
  Just df <- createDocumentFragment doc
  events <- newChan
  (result, fc@(FireCommand fire)) <- runSpiderHost $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    let builderEnv = ImmediateDomBuilderEnv
          { _immediateDomBuilderEnv_document = doc
          , _immediateDomBuilderEnv_parent = toNode df
          , _immediateDomBuilderEnv_events = events
          }
    results@(_, FireCommand fire) <- hostPerformEventT $ runWithWebView (runImmediateDomBuilderT (runPostBuildT w postBuild) builderEnv) wv
    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
    return results
  void $ forkIO $ forever $ do
    ers <- readChan events
    _ <- postGUISync $ runSpiderHost $ do
      mes <- liftIO $ forM ers $ \(TriggerRef er :=> TriggerInvocation a _) -> do
        me <- readIORef er
        return $ fmap (\e -> e :=> Identity a) me
      _ <- fire (catMaybes mes) $ return ()
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
    return ()
  setInnerHTML rootElement $ Just (""::String)
  _ <- appendChild rootElement $ Just df
  return (result, fc)

data AppInput t = AppInput
  { _appInput_window :: Window t
  }

data AppOutput t = AppOutput --TODO: Add quit event
  { _appOutput_windowConfig :: WindowConfig t
  }

runApp' :: (t ~ Spider) => (forall x. AppInput t -> Widget x (AppOutput t)) -> IO ()
runApp' app = runWebGUI $ \webView -> withWebViewSingleton webView $ \webViewSing -> do
  Just doc <- fmap DOM.castToHTMLDocument <$> webViewGetDomDocument webView
  Just body <- getBody doc
  Just win <- getDefaultView doc
  rec o <- attachWidget body webViewSing $ do
        w <- lift $ wrapWindow win $ _appOutput_windowConfig o
        app $ AppInput
          { _appInput_window = w
          }
  return ()
