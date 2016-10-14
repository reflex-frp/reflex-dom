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
{-# LANGUAGE PatternSynonyms #-}
module Reflex.Dom.Internal where

import Prelude hiding (concat, mapM, mapM_, sequence, sequence_)

import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Class
import Reflex.Dom.Internal.Foreign
import Reflex.PerformEvent.Base
import Reflex.PostBuild.Class
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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import qualified GHCJS.DOM.Types as DOM
import GHCJS.DOM.Types (MonadJSM(..), runJSM, askJSM, JSM)

{-# INLINABLE mainWidget #-}
mainWidget :: (forall x. Widget x ()) -> IO ()
mainWidget w = runWebGUI $ \(webView, ctx) -> withWebViewSingleton (webView, ctx) $ \webViewSing -> (`runJSM` ctx) $ do
  Just doc <- currentDocument
  Just body <- getBody doc
  attachWidget body webViewSing w

--TODO: The x's should be unified here
{-# INLINABLE mainWidgetWithHead #-}
mainWidgetWithHead :: (forall x. Widget x ()) -> (forall x. Widget x ()) -> IO ()
mainWidgetWithHead h b = runWebGUI $ \(webView, ctx) -> withWebViewSingleton (webView, ctx) $ \webViewSing -> (`runJSM` ctx) $ do
  Just doc <- currentDocument
  Just headElement <- getHead doc
  attachWidget headElement webViewSing h
  Just body <- getBody doc
  attachWidget body webViewSing b

{-# INLINABLE mainWidgetWithCss #-}
mainWidgetWithCss :: ByteString -> (forall x. Widget x ()) -> IO ()
mainWidgetWithCss css w = runWebGUI $ \(webView, ctx) -> withWebViewSingleton (webView, ctx) $ \webViewSing -> (`runJSM` ctx) $ do
  Just doc <- currentDocument
  Just headElement <- getHead doc
  setInnerHTML headElement . Just $ "<style>" <> T.unpack (decodeUtf8 css) <> "</style>" --TODO: Fix this
  Just body <- getBody doc
  attachWidget body webViewSing w

type Widget x = PostBuildT Spider (ImmediateDomBuilderT Spider (WithWebView x (PerformEventT Spider (SpiderHost Global)))) --TODO: Make this more abstract --TODO: Put the WithWebView underneath PerformEventT - I think this would perform better

#ifndef __GHCJS__
instance MonadJSM m => MonadJSM (PostBuildT t m) where
    liftJSM' = PostBuildT . liftJSM'
#endif

{-# INLINABLE attachWidget #-}
attachWidget :: DOM.IsElement e => e -> WebViewSingleton x -> Widget x a -> JSM a
attachWidget rootElement wv w = fst <$> attachWidget' rootElement wv w

{-# INLINABLE attachWidget' #-}
attachWidget' :: DOM.IsElement e => e -> WebViewSingleton x -> Widget x a -> JSM (a, FireCommand Spider (SpiderHost Global))
attachWidget' rootElement wv w = do
  ctx <- askJSM
  Just doc <- getOwnerDocument rootElement
  Just df <- createDocumentFragment doc
  events <- liftIO $ newChan
  (result, fc@(FireCommand fire)) <- liftIO $ runSpiderHost $ do
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
  liftIO $ void $ forkIO $ forever $ do
    ers <- readChan events
    _ <- runSpiderHost $ do
      mes <- liftIO $ forM ers $ \(TriggerRef er :=> TriggerInvocation a _) -> do
        me <- readIORef er
        return $ fmap (\e -> e :=> Identity a) me
      _ <- fire (catMaybes mes) $ return ()
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> (`runJSM` ctx) cb
    return ()
  setInnerHTML rootElement $ Just (""::String)
  _ <- appendChild rootElement $ Just df
  return (result, fc)

-- | Run a reflex-dom application inside of an existing DOM element with the given ID
mainWidgetInElementById :: Text -> (forall x. Widget x ()) -> IO ()
mainWidgetInElementById eid w = runWebGUI $ \(webView, ctx) -> withWebViewSingleton (webView, ctx) $ \webViewSing -> (`runJSM` ctx) $ do
  Just doc <- currentDocument
  Just root <- getElementById doc eid
  attachWidget root webViewSing w

data AppInput t = AppInput
  { _appInput_window :: Window t
  }

data AppOutput t = AppOutput --TODO: Add quit event
  { _appOutput_windowConfig :: WindowConfig t
  }

runApp' :: (t ~ Spider) => (forall x. AppInput t -> Widget x (AppOutput t)) -> IO ()
runApp' app = runWebGUI $ \(webView, ctx) -> withWebViewSingleton (webView, ctx) $ \webViewSing -> (`runJSM` ctx) $ do
  Just doc <- currentDocument
  Just body <- getBody doc
  Just win <- getDefaultView doc
  rec o <- attachWidget body webViewSing $ do
        w <- lift $ wrapWindow win $ _appOutput_windowConfig o
        app $ AppInput
          { _appInput_window = w
          }
  return ()
