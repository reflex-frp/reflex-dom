{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TemplateHaskell, PolyKinds, TypeOperators, DeriveFunctor, LambdaCase, CPP, ForeignFunctionInterface, DeriveDataTypeable, ConstraintKinds #-}
module Reflex.Dom.Class where

import Prelude hiding (mapM, mapM_, sequence, concat)

import Reflex
import Reflex.Host.Class

import Control.Monad.Identity hiding (mapM, mapM_, forM, forM_, sequence)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
import Control.Monad.Ref
import Control.Monad.Reader hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Monad.State hiding (mapM, mapM_, forM, forM_, sequence)
import Data.Dependent.Sum (DSum (..))
import GHCJS.DOM.Types hiding (Event)
import GHCJS.DOM (WebView)
import Control.Monad.Exception

-- | Alias for Data.Map.singleton
(=:) :: k -> a -> Map k a
(=:) = Map.singleton

keycodeEnter :: Int
keycodeEnter = 13

keycodeEscape :: Int
keycodeEscape = 27

class ( Reflex t, MonadHold t m, MonadIO m, MonadAsyncException m, Functor m, MonadReflexCreateTrigger t m
      , HasDocument m, HasWebView m, HasWebView (WidgetHost m), HasWebView (GuiAction m)
      , MonadIO (WidgetHost m), MonadAsyncException (WidgetHost m), MonadIO (GuiAction m), MonadAsyncException (GuiAction m), Functor (WidgetHost m), MonadSample t (WidgetHost m)
      , HasPostGui t (GuiAction m) (WidgetHost m), HasPostGui t (GuiAction m) m, HasPostGui t (GuiAction m) (GuiAction m)
      , MonadRef m, MonadRef (WidgetHost m)
      , Ref m ~ Ref IO, Ref (WidgetHost m) ~ Ref IO --TODO: Eliminate this reliance on IO
      , MonadFix m
      ) => MonadWidget t m | m -> t where
  type WidgetHost m :: * -> *
  type GuiAction m :: * -> *
  askParent :: m Node
  subWidget :: Node -> m a -> m a
  subWidgetWithVoidActions :: Node -> m a -> m (a, Event t (WidgetHost m ()))
  liftWidgetHost :: WidgetHost m a -> m a --TODO: Is this a good idea?
  schedulePostBuild :: WidgetHost m () -> m ()
  addVoidAction :: Event t (WidgetHost m ()) -> m ()
  getRunWidget :: IsNode n => m (n -> m a -> WidgetHost m (a, WidgetHost m (), Event t (WidgetHost m ())))

class Monad m => HasDocument m where
  askDocument :: m HTMLDocument

instance HasDocument m => HasDocument (ReaderT r m) where
  askDocument = lift askDocument

instance HasDocument m => HasDocument (StateT r m) where
  askDocument = lift askDocument

class Monad m => HasWebView m where
  askWebView :: m WebView

instance HasWebView m => HasWebView (ReaderT r m) where
  askWebView = lift askWebView

instance HasWebView m => HasWebView (StateT r m) where
  askWebView = lift askWebView

newtype Restore m = Restore { restore :: forall a. m a -> IO a }

class Monad m => MonadIORestore m where
  askRestore :: m (Restore m)

instance MonadIORestore m => MonadIORestore (ReaderT r m) where
  askRestore = do
    r <- ask
    parentRestore <- lift askRestore
    return $ Restore $ \a -> restore parentRestore $ runReaderT a r

class (MonadRef h, Ref h ~ Ref m, MonadRef m) => HasPostGui t h m | m -> t h where
  askPostGui :: m (h () -> IO ())
  askRunWithActions :: m ([DSum (EventTrigger t)] -> h ())

runFrameWithTriggerRef :: (HasPostGui t h m, MonadRef m, MonadIO m) => Ref m (Maybe (EventTrigger t a)) -> a -> m ()
runFrameWithTriggerRef r a = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  liftIO . postGui $ mapM_ (\t -> runWithActions [t :=> a]) =<< readRef r  

instance HasPostGui t h m => HasPostGui t h (ReaderT r m) where
  askPostGui = lift askPostGui
  askRunWithActions = lift askRunWithActions

instance MonadWidget t m => MonadWidget t (ReaderT r m) where
  type WidgetHost (ReaderT r m) = WidgetHost m
  type GuiAction (ReaderT r m) = GuiAction m
  askParent = lift askParent
  subWidget n w = do
    r <- ask
    lift $ subWidget n $ runReaderT w r
  subWidgetWithVoidActions n w = do
    r <- ask
    lift $ subWidgetWithVoidActions n $ runReaderT w r
  liftWidgetHost = lift . liftWidgetHost
  schedulePostBuild = lift . schedulePostBuild
  addVoidAction = lift . addVoidAction
  getRunWidget = do
    r <- ask
    runWidget <- lift getRunWidget
    return $ \rootElement w -> do
      (a, postBuild, voidActions) <- runWidget rootElement $ runReaderT w r
      return (a, postBuild, voidActions)

performEvent_ :: MonadWidget t m => Event t (WidgetHost m ()) -> m ()
performEvent_ = addVoidAction

performEvent :: (MonadWidget t m, Ref m ~ Ref IO) => Event t (WidgetHost m a) -> m (Event t a)
performEvent e = do
  (eResult, reResultTrigger) <- newEventWithTriggerRef
  addVoidAction $ ffor e $ \o -> do
    result <- o
    runFrameWithTriggerRef reResultTrigger result
  return eResult

performEventAsync :: forall t m a. MonadWidget t m => Event t ((a -> IO ()) -> WidgetHost m ()) -> m (Event t a)
performEventAsync e = do
  (eResult, reResultTrigger) <- newEventWithTriggerRef
  addVoidAction $ ffor e $ \o -> do
    postGui <- askPostGui
    runWithActions <- askRunWithActions
    o $ \a -> postGui $ mapM_ (\t -> runWithActions [t :=> a]) =<< readRef reResultTrigger
  return eResult

getPostBuild :: MonadWidget t m => m (Event t ())
getPostBuild = do
  (e, trigger) <- newEventWithTriggerRef
  schedulePostBuild $ runFrameWithTriggerRef trigger ()
  return e

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
{-

class HasRunFrame t m | m -> t where
  askRunFrame :: m (DMap (EventTrigger t) -> IO ())

type MonadWidget t h m = (MonadWidget' t h m, MonadWidget' t h (WidgetEventM m))

--TODO: Remove Spider hardcoding
class (t ~ Spider, Reflex t, MonadHold t m, ReflexHost t, HasRunFrame t h, MonadReflexHost t h, MonadIO h, MonadRef h, Ref h ~ IORef, MonadFix h, HasDocument h, MonadSample t (WidgetEventM m), WidgetEventM m ~ WidgetEventM (WidgetEventM m), MonadFix m) => MonadWidget' t h m | m -> t h where

  type WidgetEventM m :: * -> *
  -- | Warning: dAttributes should not contain "id" attributes
  elDynAttr' :: String -> Dynamic t (Map String String) -> m a -> m (El t, a)
  performEvent :: Event t (h a) -> m (Event t a)
  performEvent_ :: Event t (h ()) -> m ()
  getEInit :: m (Event t ())
  putEChildren :: Event t [Node] -> m ()
  dyn :: Dynamic t (WidgetEventM m a) -> m (Event t a) --TODO: Should probably return Dynamic t a
  listWithKey :: (Ord k, Show k) => Dynamic t (Map k v) -> (k -> Dynamic t v -> WidgetEventM m v') -> m (Dynamic t (Map k v'))
  text :: String -> m ()
  dynText :: Dynamic t String -> m ()

{-# INLINABLE performEventAsync #-}
performEventAsync :: forall t h m a. (MonadWidget t h m) => Event t ((a -> IO ()) -> h ()) -> m (Event t a)
performEventAsync eAction = do
  let setup mAction = do
        (eResult, reResultTrigger) <- newEventWithTriggerRef
        runFrame <- askRunFrame
        let runAction a = a $ \x -> readRef reResultTrigger >>= mapM_ (\eResultTrigger -> runFrame $ DMap.singleton eResultTrigger x)
        maybe (return ()) runAction mAction
        return (eResult, runAction)
  eSetup <- performEvent . fmap (setup . (^?here)) . align eAction =<< getEInit
  dRunAction <- holdDyn Nothing $ fmap (Just . snd) eSetup
  performEvent_ $ attachDynWith (\mRunAction a -> maybe (return ()) ($ a) mRunAction) dRunAction eAction -- Note: this will never fire while eInit is firing, but that situation should be handled in setup; this relies on all the results from the first performEvent being fired simultaneously; otherwise, an eAction could potentially sneak in between setup being sent to performEvent and the result coming back
  switchPromptly never (fmap fst eSetup)

data TextInput t
  = TextInput { _textInput_value :: Dynamic t String
              , _textInput_keypress :: Event t Int
              , _textInput_keydown :: Event t Int
              , _textInput_keyup :: Event t Int
              , _textInput_hasFocus :: Dynamic t Bool
              , _textInput_element :: Dynamic t (Maybe HTMLInputElement)
              }

textInputGetEnter :: Reflex t => TextInput t -> Event t ()
textInputGetEnter i = fmapMaybe (\n -> if n == keycodeEnter then Just () else Nothing) $ _textInput_keypress i

setElementAttributes :: IsElement self => self -> Map String String -> IO ()
setElementAttributes e attrs = do
  oldAttrs <- maybe (return Set.empty) namedNodeMapGetNames =<< elementGetAttributes e
  forM_ (Set.toList $ oldAttrs `Set.difference` Map.keysSet attrs) $ elementRemoveAttribute e
  iforM_ attrs $ elementSetAttribute e

wrapDomEvent :: (HasRunFrame t h, MonadIO m, MonadReflexHost t h, MonadIO h) => e -> (e -> m () -> IO (IO ())) -> m a -> h (Event t a)
wrapDomEvent self elementOnevent getValue = do
  runFrame <- askRunFrame
  e <- newEventWithTrigger $ \et -> do
    unsubscribe <- {-# SCC "a" #-} liftIO $ {-# SCC "b" #-} elementOnevent self $ {-# SCC "c" #-} do
      v <- {-# SCC "d" #-} getValue
      liftIO $ runFrame $ DMap.singleton et v
    return $ liftIO $ do
      {-# SCC "e" #-} unsubscribe
  return $! {-# SCC "f" #-} e

{-# INLINABLE input #-}
input' :: MonadWidget t h m => String -> Event t String -> Dynamic t (Map String String) -> m (TextInput t)
input' inputType eSetValue dAttrs = do
  dEffectiveAttrs <- mapDyn (Map.insert "type" inputType) dAttrs
  let mkSelf (initialValue, initialAttrs) = do
        doc <- askDocument
        Just e <- liftIO $ liftM (fmap castToHTMLInputElement) $ documentCreateElement doc "input"
        liftIO $ htmlInputElementSetValue e initialValue
        iforM_ initialAttrs $ \attr value -> liftIO $ elementSetAttribute e attr value
        eChange <- wrapDomEvent e elementOninput $ liftIO $ htmlInputElementGetValue e
        runFrame <- askRunFrame
        eChangeFocus <- newEventWithTrigger $ \eChangeFocusTrigger -> do
          unsubscribeOnblur <- liftIO $ elementOnblur e $ liftIO $ do
            runFrame $ DMap.singleton eChangeFocusTrigger False
          unsubscribeOnfocus <- liftIO $ elementOnfocus e $ liftIO $ do
            runFrame $ DMap.singleton eChangeFocusTrigger True
          return $ liftIO $ unsubscribeOnblur >> unsubscribeOnfocus
        eKeypress <- wrapDomEvent e elementOnkeypress $ liftIO . uiEventGetKeyCode =<< event
        eKeydown <- wrapDomEvent e elementOnkeydown $ liftIO . uiEventGetKeyCode =<< event
        eKeyup <- wrapDomEvent e elementOnkeyup $ liftIO . uiEventGetKeyCode =<< event
        return (e, eChange, eKeypress, eKeydown, eKeyup, eChangeFocus)
  dInitialValue <- holdDyn "" eSetValue
  dInitial <- combineDyn (,) dInitialValue dEffectiveAttrs
  eCreated <- performEvent . fmap mkSelf . tagDyn dInitial =<< getEInit
  dMyElement <- holdDyn Nothing $ fmap (Just . (^. _1)) eCreated
  performEvent_ . updated =<< combineDyn (maybe (const $ return ()) $ \e attrs -> liftIO $ setElementAttributes e attrs) dMyElement dEffectiveAttrs
  putEChildren $ fmap ((:[]) . toNode . (^. _1)) eCreated
  performEvent_ $ fmapMaybe (fmap $ \(e,v) -> liftIO $ htmlInputElementSetValue e v) $ attachWith (\e v -> case e of
    Nothing -> Nothing
    Just e' -> Just (e',v)) (current dMyElement) eSetValue
  eChange <- liftM switch $ hold never $ fmap (^. _2) eCreated
  dFocus <- holdDyn False . switch =<< hold never (fmap (^. _6) eCreated) --TODO: Check that 'False' is always the correct starting value - i.e.: an element will always receive an 'onfocus' event, even if it gets focus immediately
  dValue <- holdDyn "" $ leftmost [eSetValue, eChange]
  eKeypress <- liftM switch $ hold never $ fmap (^. _3) eCreated
  eKeydown <- liftM switch $ hold never $ fmap (^. _4) eCreated
  eKeyup <- liftM switch $ hold never $ fmap (^. _5) eCreated
  return $ TextInput dValue eKeypress eKeydown eKeyup dFocus dMyElement

input :: MonadWidget t h m => String -> Map String String -> m (TextInput t)
input t as = input' t never (constDyn as)

data TextArea t
  = TextArea { _textArea_value :: Dynamic t String
             , _textArea_element :: Dynamic t (Maybe HTMLTextAreaElement)
             }

textArea' :: MonadWidget t h m => Event t String -> Dynamic t (Map String String) -> m (TextArea t)
textArea' eSetValue dAttrs = do
  let mkSelf (initialValue, initialAttrs) = do
        doc <- askDocument
        Just e <- liftIO $ liftM (fmap castToHTMLTextAreaElement) $ documentCreateElement doc "textarea"
        liftIO $ htmlTextAreaElementSetValue e initialValue
        iforM_ initialAttrs $ \attr value -> liftIO $ elementSetAttribute e attr value
        eChange <- wrapDomEvent e elementOninput $ liftIO $ htmlTextAreaElementGetValue e
        return (e, eChange)
  dInitialValue <- holdDyn "" eSetValue
  dInitial <- combineDyn (,) dInitialValue dAttrs
  eCreated <- performEvent . fmap mkSelf . tagDyn dInitial =<< getEInit
  dMyElement <- holdDyn Nothing $ fmap (Just . fst) eCreated
  --performEvent . updated =<< combineDyn (maybe (const $ return ()) $ \e attrs -> liftIO $ setElementAttributes e attrs) dMyElement dAttrs
  putEChildren $ fmap ((:[]) . toNode . fst) eCreated
  performEvent_ $ fmapMaybe (fmap $ \(e,v) -> liftIO $ htmlTextAreaElementSetValue e v) $ attachWith (\e v -> case e of
    Nothing -> Nothing
    Just e' -> Just (e',v)) (current dMyElement) eSetValue
  eChange <- liftM switch $ hold never $ fmap snd eCreated
  dValue <- holdDyn "" $ leftmost [eSetValue, eChange]
  return $ TextArea dValue dMyElement


data Checkbox t
  = Checkbox { _checkbox_checked :: Dynamic t Bool
             }

checkbox :: MonadWidget t h m => Bool -> Map String String -> m (Checkbox t)
checkbox checked attrs = do
  let mkSelf = do
        doc <- askDocument
        Just e <- liftIO $ liftM (fmap castToHTMLInputElement) $ documentCreateElement doc "input"
        liftIO $ elementSetAttribute e "type" "checkbox"
        iforM_ attrs $ \attr value -> liftIO $ elementSetAttribute e attr value
        if checked == True then liftIO $ elementSetAttribute e "checked" "true" else return ()
        eChange <- wrapDomEvent e elementOnclick $ liftIO $ htmlInputElementGetChecked e
        return ([toNode e], eChange)
  eCreated <- performEvent . fmap (const mkSelf) =<< getEInit
  putEChildren $ fmap (^. _1) eCreated
  eChange <- liftM switch $ hold never $ fmap (^. _2) eCreated
  dValue <- holdDyn checked eChange
  return $ Checkbox dValue

checkboxView :: MonadWidget t h m => Dynamic t (Map String String) -> Dynamic t Bool -> m (Event t ())
checkboxView dAttrs dValue = do
  dEffectiveAttrs <- mapDyn (Map.insert "type" "checkbox") dAttrs
  let mkSelf (initialValue, initialAttrs) = do
        doc <- askDocument
        Just e <- liftIO $ liftM (fmap castToHTMLInputElement) $ documentCreateElement doc "input"
        iforM_ initialAttrs $ \attr value -> liftIO $ elementSetAttribute e attr value
        liftIO $ htmlInputElementSetChecked e initialValue
        eClicked <- wrapDomEvent e elementOnclick preventDefault
        return (e, eClicked)
  dInitial <- combineDyn (,) dValue dEffectiveAttrs
  eCreated <- performEvent . fmap mkSelf . tagDyn dInitial =<< getEInit
  putEChildren $ fmap ((:[]) . toNode . (^. _1)) eCreated
  dElement <- holdDyn Nothing $ fmap (Just . (^. _1)) eCreated
  performEvent_ . updated =<< combineDyn (\me a -> forM_ me $ \e -> liftIO $ setElementAttributes e a) dElement dEffectiveAttrs
  performEvent_ . updated =<< combineDyn (\v me -> maybe (return ()) (\e -> liftIO $ htmlInputElementSetChecked e v) me) dValue dElement
  liftM switch $ hold never $ fmap (^. _2) eCreated

{-# INLINABLE textInput #-}
textInput :: MonadWidget t h m => m (TextInput t)
textInput = input "text" $ Map.singleton "class" "form-control"

data Dropdown t k
  = Dropdown { _dropdown_value :: Dynamic t k
             }

setInnerText :: (HasDocument m, IsHTMLElement self, MonadIO m) => self -> String -> m (Maybe Node)
setInnerText e s = do
  liftIO $ htmlElementSetInnerHTML e ""
  doc <- askDocument
  t <- liftIO $ documentCreateTextNode doc s
  liftIO $ nodeAppendChild e t

--TODO: Retain set value when allowed values changes
{-# INLINABLE dropdown #-}
dropdown :: forall t h m k. (MonadWidget t h m, Show k, Read k) => Dynamic t (Map k String) -> m (Dropdown t (Maybe k))
dropdown dOptions = do
  let triggerChange (e, reChangeTrigger) runFrame = do
        v <- htmlSelectElementGetValue e
        readRef reChangeTrigger >>= mapM_ (\eChangeTrigger -> runFrame $ DMap.singleton eChangeTrigger $ readMay v)
  let mkSelf = do
        doc <- askDocument
        Just e <- liftIO $ liftM (fmap castToHTMLSelectElement) $ documentCreateElement doc "select"
        liftIO $ elementSetAttribute e "class" "form-control"
        runFrame <- askRunFrame
        reChangeTrigger <- newRef Nothing
        eChange <- newEventWithTrigger $ \eChangeTrigger -> do
          writeRef reChangeTrigger $ Just eChangeTrigger
          unsubscribe <- liftIO $ elementOnchange e $ liftIO $ triggerChange (e, reChangeTrigger) runFrame
          return $ do
            writeRef reChangeTrigger Nothing
            liftIO unsubscribe
        return ((e, reChangeTrigger), eChange)
  eCreated <- performEvent . fmap (const mkSelf) =<< getEInit
  dState <- holdDyn Nothing $ fmap (Just . (^. _1)) eCreated
  putEChildren $ fmap ((:[]) . toNode . fst . (^. _1)) eCreated
  let updateOptions :: Map k String -> Maybe (HTMLSelectElement, Ref h (Maybe (EventTrigger t (Maybe k)))) -> h ()
      updateOptions options = maybe (return ()) $ \myState@(selectElement, _) -> do
        doc <- askDocument
        liftIO $ htmlElementSetInnerHTML selectElement ""
        iforM_ options $ \k optionText -> do
          Just optionElement <- liftIO $ liftM (fmap castToHTMLOptionElement) $ documentCreateElement doc "option"
          liftIO $ elementSetAttribute optionElement "value" $ show k
          _ <- setInnerText optionElement optionText
          liftIO $ nodeAppendChild selectElement $ Just optionElement
        runFrame <- askRunFrame
        liftIO $ triggerChange myState runFrame
  performEvent_ . updated =<< combineDyn updateOptions dOptions dState
  eChange <- liftM switch $ hold never $ fmap (^. _2) eCreated
  dValue <- holdDyn Nothing eChange
  return $ Dropdown dValue

data Link t
  = Link { _link_clicked :: Event t ()
         }

linkClassWithExtraSetup :: MonadWidget t h m => (HTMLElement -> h ()) -> String -> String -> m (Link t)
linkClassWithExtraSetup extraSetup s c = do
  let mkSelf = do
        doc <- askDocument
        Just e <- liftIO $ liftM (fmap castToHTMLElement) $ documentCreateElement doc "a"
        _ <- setInnerText e s
        liftIO $ elementSetAttribute e "class" c
        eClicked <- wrapDomEvent e elementOnclick $ return ()
        extraSetup e
        return ([toNode e], eClicked)
  eCreated <- performEvent . fmap (const mkSelf) =<< getEInit
  putEChildren $ fmap (^. _1) eCreated
  eClicked <- liftM switch $ hold never $ fmap (^. _2) eCreated
  return $ Link eClicked

{-# INLINABLE linkClass #-}
linkClass :: MonadWidget t h m => String -> String -> m (Link t)
linkClass = linkClassWithExtraSetup $ const $ return ()

{-# INLINABLE link #-}
link :: MonadWidget t h m => String -> m (Link t)
link s = linkClass s ""

{-# INLINABLE button #-}
button :: MonadWidget t h m => String -> m (Link t)
button s = linkClass s "btn btn-primary"

{-# INLINABLE elAttr #-}
elAttr :: forall t h m a. MonadWidget t h m => String -> Map String String -> m a -> m a
elAttr elementTag attrs child = liftM snd $ elAttr' elementTag attrs child

{-# INLINABLE el' #-}
el' :: forall t h m a. MonadWidget t h m => String -> m a -> m (El t, a)
el' elementTag child = elAttr' elementTag Map.empty child

{-# INLINABLE elAttr' #-}
elAttr' :: forall t h m a. MonadWidget t h m => String -> Map String String -> m a -> m (El t, a)
elAttr' elementTag attrs child = do
  dAttributes <- holdDyn attrs never
  elDynAttr' elementTag dAttributes child

{-# INLINABLE elDynAttr #-}
elDynAttr :: forall t h m a. MonadWidget t h m => String -> Dynamic t (Map String String) -> m a -> m a
elDynAttr elementTag dAttributes child = liftM snd $ elDynAttr' elementTag dAttributes child

{-# INLINABLE el #-}
el :: forall t h m a. MonadWidget t h m => String -> m a -> m a
el elementTag child = elAttr elementTag Map.empty child

newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

workflowView :: forall t h m a. MonadWidget t h m => Workflow t (WidgetEventM m) a -> m (Event t a)
workflowView w0 = do
  rec eResult <- dyn =<< mapDyn unWorkflow =<< holdDyn w0 eReplace
      eReplace <- liftM switch $ hold never $ fmap snd eResult
      eResult `seq` eReplace `seq` return ()
  return $ fmap fst eResult

insertNext :: (Ord k, Enum k) => v -> Map k v -> Map k v
insertNext v m =
  let k = case Map.maxViewWithKey m of
        Nothing -> toEnum 0
        Just ((k0, _), _) -> succ k0
  in Map.insert k v m

{-# INLINABLE oldList #-}
oldList :: (MonadWidget t h m, MonadWidget t h (WidgetEventM m), Ord k, Show k) => Dynamic t (Map k (WidgetEventM m v')) -> m (Dynamic t (Map k ()))
oldList dm = el "ul" $ list dm (\dv -> el "li" (dyn dv) >> return ())

{-# INLINABLE list #-}
list :: forall t m h k v v'. (MonadWidget t h m, Ord k, Show k) => Dynamic t (Map k v) -> (Dynamic t v -> WidgetEventM m v') -> m (Dynamic t (Map k v'))
list dm mkChild = listWithKey dm (\_ dv -> mkChild dv)

instance MonadWidget' t h m => MonadWidget' t h (ReaderT r m) where
  type WidgetEventM (ReaderT r m) = ReaderT r (WidgetEventM m)
  elDynAttr' tag dAttrs = mapReaderT (elDynAttr' tag dAttrs)
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_
  getEInit = lift getEInit
  putEChildren = lift . putEChildren
  dyn d = do
    r <- ask
    lift $ dyn =<< mapDyn (flip runReaderT r) d
  listWithKey d w = do
    r <- ask
    lift $ listWithKey d $ \k dv -> runReaderT (w k dv) r
  text = lift . text
  dynText = lift . dynText

-}
