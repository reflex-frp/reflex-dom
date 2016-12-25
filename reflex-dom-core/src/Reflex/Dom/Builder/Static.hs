{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Builder.Static where

import Blaze.ByteString.Builder.Html.Utf8
import Control.Lens hiding (element)
import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.State.Strict
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Constant
import Data.Functor.Misc
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import Data.Text.Encoding
import Data.Tuple
import GHC.Generics
import Reflex.Class
import Reflex.Dom.Builder.Class
import Reflex.Dom.Widget.Basic (applyMap)
import Reflex.Dynamic
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Base
import Reflex.Spider
import Reflex.TriggerEvent.Class

data StaticDomBuilderEnv = StaticDomBuilderEnv { _staticDomBuilderEnv_shouldEscape :: Bool }

newtype StaticDomBuilderT t m a = StaticDomBuilderT
    { unStaticDomBuilderT :: ReaderT StaticDomBuilderEnv (StateT [Behavior t Builder] m) a -- Accumulated Html will be in reversed order
    }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance PrimMonad m => PrimMonad (StaticDomBuilderT x m) where
  type PrimState (StaticDomBuilderT x m) = PrimState m
  primitive = lift . primitive

instance MonadTrans (StaticDomBuilderT t) where
  lift = StaticDomBuilderT . lift . lift

runStaticDomBuilderT :: (Monad m, Reflex t) => StaticDomBuilderT t m a -> StaticDomBuilderEnv -> m (a, Behavior t Builder)
runStaticDomBuilderT (StaticDomBuilderT a) e = do
  (result, a') <- runStateT (runReaderT a e) []
  return (result, mconcat $ reverse a')

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (StaticDomBuilderT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance PerformEvent t m => PerformEvent t (StaticDomBuilderT t m) where
  type Performable (StaticDomBuilderT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance MonadSample t m => MonadSample t (StaticDomBuilderT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (StaticDomBuilderT t m) where
  {-# INLINABLE hold #-}
  hold v0 v' = lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = lift $ holdIncremental v0 v'

instance (Monad m, Ref m ~ Ref IO, Reflex t) => TriggerEvent t (StaticDomBuilderT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = return (never, const $ return ())
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = return (never, \_ _ -> return ())
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete _ = return never

instance MonadRef m => MonadRef (StaticDomBuilderT t m) where
  type Ref (StaticDomBuilderT t m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (StaticDomBuilderT t m) where
  atomicModifyRef r = lift . atomicModifyRef r

type SupportsStaticDomBuilder t m = (Reflex t, MonadIO m, MonadHold t m, MonadFix m, PerformEvent t m, MonadReflexCreateTrigger t m, MonadRef m, Ref m ~ Ref IO, MonadAdjust t m)

data StaticDomSpace

-- | Static documents never produce any events, so this type has no inhabitants
data StaticDomEvent (a :: k)

-- | Static documents don't process events, so all handlers are equivalent
data StaticDomHandler (a :: k) (b :: k) = StaticDomHandler

data StaticEventSpec (er :: EventTag -> *) = StaticEventSpec deriving (Generic)

instance Default (StaticEventSpec er)

instance DomSpace StaticDomSpace where
  type EventSpec StaticDomSpace = StaticEventSpec
  type RawTextNode StaticDomSpace = ()
  type RawElement StaticDomSpace = ()
  type RawFile StaticDomSpace = ()
  type RawInputElement StaticDomSpace = ()
  type RawTextAreaElement StaticDomSpace = ()
  type RawSelectElement StaticDomSpace = ()
  addEventSpecFlags _ _ _ _ = StaticEventSpec

instance (Reflex t, MonadAdjust t m, MonadHold t m) => MonadAdjust t (StaticDomBuilderT t m) where
  runWithReplace a0 a' = do
    e <- StaticDomBuilderT ask
    (result0, result') <- lift $ runWithReplace (runStaticDomBuilderT a0 e) (flip runStaticDomBuilderT e <$> a')
    o <- hold (snd result0) $ snd <$> result'
    StaticDomBuilderT $ modify $ (:) $ join o
    return (fst result0, fst <$> result')
  sequenceDMapWithAdjust (dm0 :: DMap k (StaticDomBuilderT t m)) dm' = do
    e <- StaticDomBuilderT ask
    let loweredDm0 = mapKeyValuePairsMonotonic (\(k :=> v) -> WrapArg k :=> fmap swap (runStaticDomBuilderT v e)) dm0
        loweredDm' = ffor dm' $ \(PatchDMap p) -> PatchDMap $
          mapKeyValuePairsMonotonic (\(k :=> ComposeMaybe mv) -> WrapArg k :=> ComposeMaybe (fmap (fmap swap . flip runStaticDomBuilderT e) mv)) p
    (children0, children') <- lift $ sequenceDMapWithAdjust loweredDm0 loweredDm'
    let result0 = mapKeyValuePairsMonotonic (\(WrapArg k :=> Identity (_, v)) -> k :=> Identity v) children0
        result' = ffor children' $ \(PatchDMap p) -> PatchDMap $
          mapKeyValuePairsMonotonic (\(WrapArg k :=> mv) -> k :=> fmap snd mv) p
        outputs0 :: DMap k (Constant (Behavior t Builder))
        outputs0 = mapKeyValuePairsMonotonic (\(WrapArg k :=> Identity (o, _)) -> k :=> Constant o) children0
        outputs' :: Event t (PatchDMap k (Constant (Behavior t Builder)))
        outputs' = ffor children' $ \(PatchDMap p) -> PatchDMap $
          mapKeyValuePairsMonotonic (\(WrapArg k :=> ComposeMaybe mv) -> k :=> ComposeMaybe (fmap (Constant . fst . runIdentity) mv)) p
    outputs <- holdIncremental outputs0 outputs'
    StaticDomBuilderT $ modify $ (:) $ pull $ do
      os <- sample $ currentIncremental outputs
      fmap mconcat $ forM (DMap.toList os) $ \(_ :=> Constant o) -> do
        sample o
    return (result0, result')

instance SupportsStaticDomBuilder t m => DomBuilder t (StaticDomBuilderT t m) where
  type DomBuilderSpace (StaticDomBuilderT t m) = StaticDomSpace
  {-# INLINABLE textNode #-}
  textNode (TextNodeConfig initialContents setContents) = StaticDomBuilderT $ do
    --TODO: Do not escape quotation marks; see https://stackoverflow.com/questions/25612166/what-characters-must-be-escaped-in-html-5
    shouldEscape <- asks _staticDomBuilderEnv_shouldEscape
    let escape = if shouldEscape then fromHtmlEscapedText else byteString . encodeUtf8
    modify . (:) <=< hold (escape initialContents) $ fmap escape setContents
    return $ TextNode ()
  {-# INLINABLE element #-}
  element elementTag cfg child = do
    -- https://www.w3.org/TR/html-markup/syntax.html#syntax-elements
    let voidElements = Set.fromList ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]
    let noEscapeElements = Set.fromList ["style", "script"]
    let toAttr (AttributeName _mns k) v = byteString (encodeUtf8 k) <> byteString "=\"" <> fromHtmlEscapedText v <> byteString "\""
    es <- newFanEventWithTrigger $ \_ _ -> return (return ())
    StaticDomBuilderT $ do
      let shouldEscape = elementTag `Set.notMember` noEscapeElements
      (result, innerHtml) <- lift $ lift $ runStaticDomBuilderT child $ StaticDomBuilderEnv shouldEscape
      attrs0 <- foldDyn applyMap (cfg ^. initialAttributes) (cfg ^. modifyAttributes)
      let attrs1 = ffor (current attrs0) $ mconcat . fmap (\(k, v) -> " " <> toAttr k v) . Map.toList
      let tagBS = encodeUtf8 elementTag
      if Set.member elementTag voidElements
        then modify $ (:) $ mconcat [constant ("<" <> byteString tagBS), attrs1, constant (byteString " />")]
        else do
          let open = mconcat [constant ("<" <> byteString tagBS), attrs1, constant (byteString ">")]
          let close = constant $ byteString $ "</" <> tagBS <> ">"
          modify $ (:) $ mconcat [open, innerHtml, close]
      return (Element es (), result)
  {-# INLINABLE inputElement #-}
  inputElement cfg = do
    (e, _result) <- element "input" (cfg ^. inputElementConfig_elementConfig) $ return ()
    let v0 = constDyn $ cfg ^. inputElementConfig_initialValue
    let c0 = constDyn $ cfg ^. inputElementConfig_initialChecked
    let hasFocus = constDyn False -- TODO should this be coming from initialAtttributes
    return $ InputElement
      { _inputElement_value = v0
      , _inputElement_checked = c0
      , _inputElement_checkedChange = never
      , _inputElement_input = never
      , _inputElement_hasFocus = hasFocus
      , _inputElement_element = e
      , _inputElement_raw = ()
      , _inputElement_files = constDyn mempty
      }
  {-# INLINABLE textAreaElement #-}
  textAreaElement cfg = do
    --TODO: Support setValue event
    (e, _domElement) <- element "textarea" (cfg ^. textAreaElementConfig_elementConfig) $ return ()
    let v0 = constDyn $ cfg ^. textAreaElementConfig_initialValue
    let hasFocus = constDyn False -- TODO should this be coming from initialAtttributes
    return $ TextAreaElement
      { _textAreaElement_value = v0
      , _textAreaElement_input = never
      , _textAreaElement_hasFocus = hasFocus
      , _textAreaElement_element = e
      , _textAreaElement_raw = ()
      }
  selectElement cfg child = do
    (e, result) <- element "select" (_selectElementConfig_elementConfig cfg) child
    v <- holdDyn (cfg ^. selectElementConfig_initialValue) (cfg ^. selectElementConfig_setValue)
    let wrapped = SelectElement
          { _selectElement_value = v
          , _selectElement_change = never
          , _selectElement_hasFocus = constDyn False --TODO: How do we make sure this is correct?
          , _selectElement_element = e
          , _selectElement_raw = ()
          }
    return (wrapped, result)
  placeRawElement () = return ()
  wrapRawElement () _ = return $ Element (EventSelector $ const never) ()

--TODO: Make this more abstract --TODO: Put the WithWebView underneath PerformEventT - I think this would perform better
type StaticWidget x = PostBuildT Spider (StaticDomBuilderT Spider (PerformEventT Spider (SpiderHost Global)))

{-# INLINE renderStatic #-}
renderStatic :: StaticWidget x a -> IO (a, ByteString)
renderStatic w = do
  runSpiderHost $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    let env0 = StaticDomBuilderEnv True
    ((res, bs), FireCommand fire) <- hostPerformEventT $ runStaticDomBuilderT (runPostBuildT w postBuild) env0
    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
    bs' <- sample bs
    return (res, LBS.toStrict $ toLazyByteString bs')
