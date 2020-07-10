{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Builder.Static where

import Data.IORef (IORef)
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
import Data.Functor.Compose
import Data.Functor.Constant
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Map.Misc (applyMap)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Tuple
import GHC.Generics
import Reflex.Adjustable.Class
import Reflex.Class
import Reflex.Dom.Main (DomHost, DomTimeline, runDomHost)
import Reflex.Dom.Builder.Class
import Reflex.Dynamic
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Base
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

data StaticDomBuilderEnv t = StaticDomBuilderEnv
  { _staticDomBuilderEnv_shouldEscape :: Bool
  , _staticDomBuilderEnv_selectValue :: Maybe (Behavior t Text)
    -- ^ When the parent element is a "select" whose value has been set, this value tells us the current value.
    -- We use this to add a "selected" attribute to the appropriate "option" child element.
    -- This is not yet a perfect simulation of what the browser does, but it is much closer than doing nothing.
    -- TODO: Handle edge cases, e.g. setting to a value for which there is no option, then adding that option dynamically afterwards.
  , _staticDomBuilderEnv_nextRunWithReplaceKey :: IORef Int
  }

newtype StaticDomBuilderT t m a = StaticDomBuilderT
    { unStaticDomBuilderT :: ReaderT (StaticDomBuilderEnv t) (StateT [Behavior t Builder] m) a -- Accumulated Html will be in reversed order
    }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance PrimMonad m => PrimMonad (StaticDomBuilderT x m) where
  type PrimState (StaticDomBuilderT x m) = PrimState m
  primitive = lift . primitive

instance MonadTrans (StaticDomBuilderT t) where
  lift = StaticDomBuilderT . lift . lift

runStaticDomBuilderT :: (Monad m, Reflex t) => StaticDomBuilderT t m a -> StaticDomBuilderEnv t -> m (a, Behavior t Builder)
runStaticDomBuilderT (StaticDomBuilderT a) e = do
  (result, a') <- runStateT (runReaderT a e) []
  return (result, mconcat $ reverse a')

instance PostBuild t m => PostBuild t (StaticDomBuilderT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

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
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = lift . headE

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

type SupportsStaticDomBuilder t m = (Reflex t, MonadIO m, MonadHold t m, MonadFix m, PerformEvent t m, MonadReflexCreateTrigger t m, MonadRef m, Ref m ~ Ref IO, Adjustable t m)

data StaticDomSpace

-- | Static documents never produce any events, so this type has no inhabitants
data StaticDomEvent (a :: k)

-- | Static documents don't process events, so all handlers are equivalent
data StaticDomHandler (a :: k) (b :: k) = StaticDomHandler

data StaticEventSpec (er :: EventTag -> *) = StaticEventSpec deriving (Generic)

instance Default (StaticEventSpec er)

instance DomSpace StaticDomSpace where
  type EventSpec StaticDomSpace = StaticEventSpec
  type RawDocument StaticDomSpace = ()
  type RawTextNode StaticDomSpace = ()
  type RawCommentNode StaticDomSpace = ()
  type RawElement StaticDomSpace = ()
  type RawInputElement StaticDomSpace = ()
  type RawTextAreaElement StaticDomSpace = ()
  type RawSelectElement StaticDomSpace = ()
  addEventSpecFlags _ _ _ _ = StaticEventSpec

instance (SupportsStaticDomBuilder t m, Monad m) => HasDocument (StaticDomBuilderT t m) where
  askDocument = pure ()

instance (Reflex t, Adjustable t m, MonadHold t m, SupportsStaticDomBuilder t m) => Adjustable t (StaticDomBuilderT t m) where
  runWithReplace a0 a' = do
    e <- StaticDomBuilderT ask
    key <- replaceStart e
    (result0, result') <- lift $ runWithReplace (runStaticDomBuilderT a0 e) (flip runStaticDomBuilderT e <$> a')
    o <- hold (snd result0) $ fmapCheap snd result'
    StaticDomBuilderT $ modify $ (:) $ join o
    replaceEnd key
    return (fst result0, fmapCheap fst result')
  traverseIntMapWithKeyWithAdjust = hoistIntMapWithKeyWithAdjust traverseIntMapWithKeyWithAdjust
  traverseDMapWithKeyWithAdjust = hoistDMapWithKeyWithAdjust traverseDMapWithKeyWithAdjust mapPatchDMap
  traverseDMapWithKeyWithAdjustWithMove = hoistDMapWithKeyWithAdjust traverseDMapWithKeyWithAdjustWithMove mapPatchDMapWithMove

replaceStart :: (DomBuilder t m, MonadIO m) => StaticDomBuilderEnv t -> m Text
replaceStart env = do
  str <- show <$> liftIO (atomicModifyRef (_staticDomBuilderEnv_nextRunWithReplaceKey env) $ \k -> (succ k, k))
  let key = "-" <> T.pack str
  _ <- commentNode $ def { _commentNodeConfig_initialContents = "replace-start" <> key }
  pure key

replaceEnd :: DomBuilder t m => Text -> m ()
replaceEnd key = void $ commentNode $ def { _commentNodeConfig_initialContents = "replace-end" <> key }

hoistIntMapWithKeyWithAdjust :: forall t m p a b.
  ( Adjustable t m
  , MonadHold t m
  , Patch (p a)
  , Functor p
  , Patch (p (Behavior t Builder))
  , PatchTarget (p (Behavior t Builder)) ~ IntMap (Behavior t Builder)
  , Ref m ~ IORef, MonadIO m, MonadFix m, PerformEvent t m, MonadReflexCreateTrigger t m, MonadRef m -- TODO remove
  )
  => (forall x. (IntMap.Key -> a -> m x)
      -> IntMap a
      -> Event t (p a)
      -> m (IntMap x, Event t (p x))
     ) -- ^ The base monad's traversal
  -> (IntMap.Key -> a -> StaticDomBuilderT t m b)
  -> IntMap a
  -> Event t (p a)
  -> StaticDomBuilderT t m (IntMap b, Event t (p b))
hoistIntMapWithKeyWithAdjust base f im0 im' = do
  e <- StaticDomBuilderT ask
  (children0, children') <- lift $ base (\k v -> runStaticDomBuilderT (f k v) e) im0 im'
  let result0 = IntMap.map fst children0
      result' = (fmap . fmap) fst children'
      outputs0 :: IntMap (Behavior t Builder)
      outputs0 = IntMap.map snd children0
      outputs' :: Event t (p (Behavior t Builder))
      outputs' = (fmap . fmap) snd children'
  outputs <- holdIncremental outputs0 outputs'
  StaticDomBuilderT $ modify $ (:) $ pull $ do
    os <- sample $ currentIncremental outputs
    fmap mconcat $ forM (IntMap.toList os) $ \(_, o) -> do
      sample o
  return (result0, result')

hoistDMapWithKeyWithAdjust :: forall (k :: * -> *) v v' t m p.
  ( Adjustable t m
  , MonadHold t m
  , PatchTarget (p k (Constant (Behavior t Builder))) ~ DMap k (Constant (Behavior t Builder))
  , Patch (p k (Constant (Behavior t Builder)))
  , Ref m ~ IORef, MonadIO m, MonadFix m, PerformEvent t m, MonadReflexCreateTrigger t m, MonadRef m -- TODO remove
  )
  => (forall vv vv'.
         (forall a. k a -> vv a -> m (vv' a))
      -> DMap k vv
      -> Event t (p k vv)
      -> m (DMap k vv', Event t (p k vv'))
     ) -- ^ The base monad's traversal
  -> (forall vv vv'. (forall a. vv a -> vv' a) -> p k vv -> p k vv') -- ^ A way of mapping over the patch type
  -> (forall a. k a -> v a -> StaticDomBuilderT t m (v' a))
  -> DMap k v
  -> Event t (p k v)
  -> StaticDomBuilderT t m (DMap k v', Event t (p k v'))
hoistDMapWithKeyWithAdjust base mapPatch f dm0 dm' = do
  e <- StaticDomBuilderT ask
  (children0, children') <- lift $ base (\k v -> fmap (Compose . swap) (runStaticDomBuilderT (f k v) e)) dm0 dm'
  let result0 = DMap.map (snd . getCompose) children0
      result' = ffor children' $ mapPatch $ snd . getCompose
      outputs0 :: DMap k (Constant (Behavior t Builder))
      outputs0 = DMap.map (Constant . fst . getCompose) children0
      outputs' :: Event t (p k (Constant (Behavior t Builder)))
      outputs' = ffor children' $ mapPatch $ Constant . fst . getCompose
  outputs <- holdIncremental outputs0 outputs'
  StaticDomBuilderT $ modify $ (:) $ pull $ do
    os <- sample $ currentIncremental outputs
    fmap mconcat $ forM (DMap.toList os) $ \(_ :=> Constant o) -> do
      sample o
  return (result0, result')

instance SupportsStaticDomBuilder t m => NotReady t (StaticDomBuilderT t m) where
  notReadyUntil _ = pure ()
  notReady = pure ()

-- TODO: the uses of illegal lenses in this instance causes it to be somewhat less efficient than it can be. replacing them with explicit cases to get the underlying Maybe Event and working with those is ideal.
instance SupportsStaticDomBuilder t m => DomBuilder t (StaticDomBuilderT t m) where
  type DomBuilderSpace (StaticDomBuilderT t m) = StaticDomSpace
  {-# INLINABLE textNode #-}
  textNode (TextNodeConfig initialContents mSetContents) = StaticDomBuilderT $ do
    --TODO: Do not escape quotation marks; see https://stackoverflow.com/questions/25612166/what-characters-must-be-escaped-in-html-5
    shouldEscape <- asks _staticDomBuilderEnv_shouldEscape
    let escape = if shouldEscape then fromHtmlEscapedText else byteString . encodeUtf8
    modify . (:) =<< case mSetContents of
      Nothing -> return (pure (escape initialContents))
      Just setContents -> hold (escape initialContents) $ fmapCheap escape setContents --Only because it doesn't get optimized when profiling is on
    return $ TextNode ()
  {-# INLINABLE commentNode #-}
  commentNode (CommentNodeConfig initialContents mSetContents) = StaticDomBuilderT $ do
    --TODO: Do not escape quotation marks; see https://stackoverflow.com/questions/25612166/what-characters-must-be-escaped-in-html-5
    shouldEscape <- asks _staticDomBuilderEnv_shouldEscape
    let escape = if shouldEscape then fromHtmlEscapedText else byteString . encodeUtf8
    modify . (:) =<< (\c -> "<!--" <> c <> "-->") <$> case mSetContents of
      Nothing -> return (pure (escape initialContents))
      Just setContents -> hold (escape initialContents) $ fmapCheap escape setContents --Only because it doesn't get optimized when profiling is on
    return $ CommentNode ()
  {-# INLINABLE element #-}
  element elementTag cfg child = do
    -- https://www.w3.org/TR/html-markup/syntax.html#syntax-elements
    let voidElements = Set.fromList ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]
    let noEscapeElements = Set.fromList ["style", "script"]
    let toAttr (AttributeName _mns k) v = byteString (encodeUtf8 k) <> byteString "=\"" <> fromHtmlEscapedText v <> byteString "\""
    es <- newFanEventWithTrigger $ \_ _ -> return (return ())
    StaticDomBuilderT $ do
      let shouldEscape = elementTag `Set.notMember` noEscapeElements
      nextRunWithReplaceKey <- asks _staticDomBuilderEnv_nextRunWithReplaceKey
      (result, innerHtml) <- lift $ lift $ runStaticDomBuilderT child $ StaticDomBuilderEnv shouldEscape Nothing nextRunWithReplaceKey
      attrs0 <- foldDyn applyMap (cfg ^. initialAttributes) (cfg ^. modifyAttributes)
      selectValue <- asks _staticDomBuilderEnv_selectValue
      let addSelectedAttr attrs sel = case Map.lookup "value" attrs of
            Just v | v == sel -> attrs <> Map.singleton "selected" ""
            _ -> Map.delete "selected" attrs
      let attrs1 = case (elementTag, selectValue) of
            ("option", Just sv) -> pull $ addSelectedAttr <$> sample (current attrs0) <*> sample sv
            _ -> current attrs0
      let attrs2 = ffor attrs1 $ mconcat . fmap (\(k, v) -> " " <> toAttr k v) . Map.toList
      let tagBS = encodeUtf8 elementTag
      if Set.member elementTag voidElements
        then modify $ (:) $ mconcat [constant ("<" <> byteString tagBS), attrs2, constant (byteString " />")]
        else do
          let open = mconcat [constant ("<" <> byteString tagBS), attrs2, constant (byteString ">")]
          let close = constant $ byteString $ "</" <> tagBS <> ">"
          modify $ (:) $ mconcat [open, innerHtml, close]
      let e = Element
            { _element_events = es
            , _element_raw = ()
            }
      return (e, result)
  {-# INLINABLE inputElement #-}
  inputElement cfg = do
    -- Tweak the config to update the "value" and "checked" attributes appropriately.
    -- TODO: warn upon overwriting values.
    let setInitialValue = Map.insert "value" (_inputElementConfig_initialValue cfg)
        setUpdatedValue updatedAttrs = case _inputElementConfig_setValue cfg of
          Nothing -> updatedAttrs
          Just e -> (Map.singleton "value" . Just <$> e) <> updatedAttrs
        setInitialChecked = case _inputElementConfig_initialChecked cfg of
          True -> Map.insert "checked" "checked"
          False -> id
        setUpdatedChecked updatedAttrs = case _inputElementConfig_setChecked cfg of
          Nothing -> updatedAttrs
          Just e -> (Map.singleton "checked" (Just "checked") <$ e) <> updatedAttrs
        adjustedConfig = _inputElementConfig_elementConfig cfg
          & elementConfig_initialAttributes %~ setInitialValue . setInitialChecked
          & elementConfig_modifyAttributes %~ setUpdatedValue . setUpdatedChecked
    (e, _result) <- element "input" adjustedConfig $ return ()
    v <- case _inputElementConfig_setValue cfg of
      Nothing -> pure $ constDyn (cfg ^. inputElementConfig_initialValue)
      Just ev -> holdDyn (cfg ^. inputElementConfig_initialValue) ev
    c <- case _inputElementConfig_setChecked cfg of
      Nothing -> pure $ constDyn $ _inputElementConfig_initialChecked cfg
      Just ev -> holdDyn (_inputElementConfig_initialChecked cfg) ev
    let hasFocus = constDyn False -- TODO should this be coming from initialAtttributes
    return $ InputElement
      { _inputElement_value = v
      , _inputElement_checked = c
      , _inputElement_checkedChange = never
      , _inputElement_input = never
      , _inputElement_hasFocus = hasFocus
      , _inputElement_element = e
      , _inputElement_raw = ()
      , _inputElement_files = constDyn mempty
      }
  {-# INLINABLE textAreaElement #-}
  textAreaElement cfg = do
    (e, _domElement) <- element "textarea" (_textAreaElementConfig_elementConfig cfg) $ do
      -- Set the initial value
      void $ textNode $ def
        & textNodeConfig_initialContents .~ _textAreaElementConfig_initialValue cfg
        & textNodeConfig_setContents .~ fromMaybe never (_textAreaElementConfig_setValue cfg)
    v <- case _textAreaElementConfig_setValue cfg of
      Nothing -> pure $ constDyn (cfg ^. textAreaElementConfig_initialValue)
      Just ev -> holdDyn (cfg ^. textAreaElementConfig_initialValue) ev
    let hasFocus = constDyn False -- TODO should this be coming from initialAtttributes
    return $ TextAreaElement
      { _textAreaElement_value = v
      , _textAreaElement_input = never
      , _textAreaElement_hasFocus = hasFocus
      , _textAreaElement_element = e
      , _textAreaElement_raw = ()
      }
  selectElement cfg child = do
    v <- holdDyn (cfg ^. selectElementConfig_initialValue) (cfg ^. selectElementConfig_setValue)
    (e, result) <- element "select" (_selectElementConfig_elementConfig cfg) $ do
      (a, innerHtml) <- StaticDomBuilderT $ do
        nextRunWithReplaceKey <- asks _staticDomBuilderEnv_nextRunWithReplaceKey
        lift $ lift $ runStaticDomBuilderT child $ StaticDomBuilderEnv False (Just $ current v) nextRunWithReplaceKey
      StaticDomBuilderT $ lift $ modify $ (:) innerHtml
      return a
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
type StaticWidget x = PostBuildT DomTimeline (StaticDomBuilderT DomTimeline (PerformEventT DomTimeline DomHost))

{-# INLINE renderStatic #-}
renderStatic :: StaticWidget x a -> IO (a, ByteString)
renderStatic w = do
  runDomHost $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    nextRunWithReplaceKey <- newRef 0
    let env0 = StaticDomBuilderEnv True Nothing nextRunWithReplaceKey
    ((res, bs), FireCommand fire) <- hostPerformEventT $ runStaticDomBuilderT (runPostBuildT w postBuild) env0
    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
    bs' <- sample bs
    return (res, LBS.toStrict $ toLazyByteString bs')
