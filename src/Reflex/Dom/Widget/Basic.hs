{-# LANGUAGE ScopedTypeVariables, LambdaCase, ConstraintKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, RecursiveDo #-}

module Reflex.Dom.Widget.Basic where

import Reflex.Dom.Class

import Reflex
import Reflex.Host.Class
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Dependent.Sum (DSum (..))
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Ref
import GHCJS.DOM.Node
import GHCJS.DOM.UIEvent
import GHCJS.DOM.EventM (event, Signal)
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Types hiding (Widget (..), unWidget, Event)
import GHCJS.DOM.NamedNodeMap
import Control.Lens
import Data.Monoid
import Data.These
import Data.Align

import GHCJS.Types

import Data.Maybe

type AttributeMap = Map String String

data El t
  = El { _el_element :: HTMLElement
       , _el_clicked :: Event t ()
       , _el_keypress :: Event t Int
       }

class Attributes m a where
  addAttributes :: IsElement e => a -> e -> m ()

instance MonadIO m => Attributes m AttributeMap where
  addAttributes curAttrs e = liftIO $ imapM_ (elementSetAttribute e) curAttrs

instance MonadWidget t m => Attributes m (Dynamic t AttributeMap) where
  addAttributes attrs e = do
    schedulePostBuild $ do
      curAttrs <- sample $ current attrs
      liftIO $ imapM_ (elementSetAttribute e) curAttrs
    addVoidAction $ flip fmap (updated attrs) $ \newAttrs -> liftIO $ do
      oldAttrs <- maybe (return Set.empty) namedNodeMapGetNames =<< elementGetAttributes e
      forM_ (Set.toList $ oldAttrs `Set.difference` Map.keysSet newAttrs) $ elementRemoveAttribute e
      imapM_ (elementSetAttribute e) newAttrs --TODO: avoid re-setting unchanged attributes; possibly do the compare using Align in haskell

buildEmptyElement :: (MonadWidget t m, Attributes m attrs) => String -> attrs -> m HTMLElement
buildEmptyElement elementTag attrs = do
  doc <- askDocument
  p <- askParent
  Just e <- liftIO $ documentCreateElement doc elementTag
  addAttributes attrs e
  _ <- liftIO $ nodeAppendChild p $ Just e
  return $ castToHTMLElement e

-- We need to decide what type of attrs we've got statically, because it will often be a recursively defined value, in which case inspecting it will lead to a cycle
buildElement :: (MonadWidget t m, Attributes m attrs) => String -> attrs -> m a -> m (HTMLElement, a)
buildElement elementTag attrs child = do
  e <- buildEmptyElement elementTag attrs
  result <- subWidget (toNode e) child
  return (e, result)

namedNodeMapGetNames :: IsNamedNodeMap self => self -> IO (Set String)
namedNodeMapGetNames self = do
  l <- namedNodeMapGetLength self
  let locations = if l == 0 then [] else [0..l-1] -- Can't use 0..l-1 if l is 0 because l is unsigned and will wrap around
  liftM Set.fromList $ forM locations $ \i -> do
    Just n <- namedNodeMapItem self i
    nodeGetNodeName n

text :: MonadWidget t m => String -> m ()
text = void . text'

--TODO: Wrap the result
text' :: MonadWidget t m => String -> m Text
text' s = do
  doc <- askDocument
  p <- askParent
  Just n <- liftIO $ documentCreateTextNode doc s
  _ <- liftIO $ nodeAppendChild p $ Just n
  return n

dynText :: MonadWidget t m => Dynamic t String -> m ()
dynText s = do
  n <- text' ""
  schedulePostBuild $ do
    curS <- sample $ current s
    liftIO $ nodeSetNodeValue n curS
  addVoidAction $ fmap (liftIO . nodeSetNodeValue n) $ updated s

--TODO: Should this be renamed to 'widgetView' for consistency with 'widgetHold'?
dyn :: MonadWidget t m => Dynamic t (m a) -> m (Event t a)
dyn child = do
  startPlaceholder <- text' ""
  endPlaceholder <- text' ""
  (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
  let e = fmap snd newChildBuilt --TODO: Get rid of this hack
  childVoidAction <- hold never e
  performEvent_ $ fmap (const $ return ()) e --TODO: Get rid of this hack
  addVoidAction $ switch childVoidAction
  doc <- askDocument
  runWidget <- getRunWidget
  let build c = do
        Just df <- liftIO $ documentCreateDocumentFragment doc
        result <- runWidget df c
        runFrameWithTriggerRef newChildBuiltTriggerRef result
        Just p <- liftIO $ nodeGetParentNode endPlaceholder
        liftIO $ nodeInsertBefore p (Just df) (Just endPlaceholder)
        return ()
  schedulePostBuild $ do
    c <- sample $ current child
    build c
  addVoidAction $ ffor (updated child) $ \newChild -> do
    liftIO $ deleteBetweenExclusive startPlaceholder endPlaceholder
    build newChild
  return $ fmap fst newChildBuilt

widgetHold :: MonadWidget t m => m a -> Event t (m a) -> m (Dynamic t a)
widgetHold child0 newChild = do
  startPlaceholder <- text' ""
  result0 <- child0
  endPlaceholder <- text' ""
  (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
  childVoidAction <- hold never $ fmap snd newChildBuilt
  addVoidAction $ switch childVoidAction
  doc <- askDocument
  runWidget <- getRunWidget
  let build c = do
        Just df <- liftIO $ documentCreateDocumentFragment doc
        result <- runWidget df c
        runFrameWithTriggerRef newChildBuiltTriggerRef result
        Just p <- liftIO $ nodeGetParentNode endPlaceholder
        liftIO $ nodeInsertBefore p (Just df) (Just endPlaceholder)
        return ()
  addVoidAction $ ffor newChild $ \c -> do
    liftIO $ deleteBetweenExclusive startPlaceholder endPlaceholder
    build c
  holdDyn result0 $ fmap fst newChildBuilt

--TODO: Something better than Dynamic t (Map k v) - we want something where the Events carry diffs, not the whole value
listWithKey :: (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
listWithKey vals mkChild = do
  doc <- askDocument
  startPlaceholder <- text' ""
  endPlaceholder <- text' ""
  (newChildren, newChildrenTriggerRef) <- newEventWithTriggerRef
  performEvent_ $ fmap (const $ return ()) newChildren --TODO: Get rid of this hack
  children <- hold Map.empty $ traceEventWith (\x -> "newChildren: " <> show (Map.size x)) newChildren
  addVoidAction $ switch $ fmap (mergeWith (>>) . map snd . Map.elems) children
  runWidget <- getRunWidget
  let buildChild df k v = runWidget df $ do
        childStart <- text' ""
        result <- mkChild k =<< holdDyn v (fmapMaybe (Map.lookup k) (updated vals))
        childEnd <- text' ""
        return (result, (childStart, childEnd))
  schedulePostBuild $ do
    Just df <- liftIO $ documentCreateDocumentFragment doc
    curVals <- sample $ current vals
    initialState <- iforM curVals $ buildChild df
    runFrameWithTriggerRef newChildrenTriggerRef initialState --TODO: Do all these in a single runFrame
    Just p <- liftIO $ nodeGetParentNode endPlaceholder
    liftIO $ nodeInsertBefore p (Just df) (Just endPlaceholder)
    return ()
  addVoidAction $ flip fmap (updated vals) $ \newVals -> do
    curState <- sample children
    --TODO: Should we remove the parent from the DOM first to avoid reflows?
    newState <- liftM (Map.mapMaybe id) $ iforM (align curState newVals) $ \k -> \case
      This ((_, (start, end)), _) -> do
        liftIO $ putStrLn "Deleting item"
        liftIO $ deleteBetweenInclusive start end
        return Nothing
      That v -> do
        liftIO $ putStrLn "Creating item"
        Just df <- liftIO $ documentCreateDocumentFragment doc
        s <- buildChild df k v
        let placeholder = case Map.lookupGT k curState of
              Nothing -> endPlaceholder
              Just (_, ((_, (start, _)), _)) -> start
        Just p <- liftIO $ nodeGetParentNode placeholder
        liftIO $ nodeInsertBefore p (Just df) (Just placeholder)
        return $ Just s
      These state _ -> do
        liftIO $ putStrLn "Leaving item unchanged"
        return $ Just state
    liftIO $ putStrLn "Triggering newChildren"
    liftIO $ putStrLn . ("newChildrenTriggerRef is Just? " <>) . show . isJust =<< readRef newChildrenTriggerRef
    runFrameWithTriggerRef newChildrenTriggerRef newState
    liftIO $ putStrLn "Triggering newChildren done"
  holdDyn Map.empty $ fmap (fmap (fst . fst)) newChildren

selectViewListWithKey_ :: forall t m k v a. (MonadWidget t m, Ord k) => Dynamic t k -> Dynamic t (Map k v) -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a)) -> m (Event t k)
selectViewListWithKey_ selection vals mkChild = do
  let selectionDemux = demux selection -- For good performance, this value must be shared across all children
  selectChild <- listWithKey vals $ \k v -> do
    selected <- getDemuxed selectionDemux k
    selectSelf <- mkChild k v selected
    return $ fmap (const k) selectSelf
  liftM switchPromptlyDyn $ mapDyn (leftmost . Map.elems) selectChild

--------------------------------------------------------------------------------
-- Basic DOM manipulation helpers
--------------------------------------------------------------------------------

instance Eq (JSRef a) where
  (==) = eqRef

-- | s and e must both be children of the same node and s must precede e
deleteBetweenExclusive s e = do
  Just currentParent <- nodeGetParentNode e -- May be different than it was at initial construction, e.g., because the parent may have dumped us in from a DocumentFragment
  let go = do
        Just x <- nodeGetPreviousSibling e -- This can't be Nothing because we should hit 's' first
--        done <- nodeIsEqualNode s $ Just x
        when (unNode (toNode s) /= unNode (toNode x)) $ do
          nodeRemoveChild currentParent $ Just x
          go
  go

-- | s and e must both be children of the same node and s must precede e
deleteBetweenInclusive s e = do
  Just currentParent <- nodeGetParentNode e -- May be different than it was at initial construction, e.g., because the parent may have dumped us in from a DocumentFragment
  let go = do
        Just x <- nodeGetPreviousSibling e -- This can't be Nothing because we should hit 's' first
        nodeRemoveChild currentParent $ Just x
        done <- nodeIsEqualNode s $ Just x
        when (not done) go
  go
  nodeRemoveChild currentParent $ Just e

--------------------------------------------------------------------------------
-- Adapters
--------------------------------------------------------------------------------

--TODO: Get rid of extra version of this function
wrapDomEvent element elementOnevent getValue = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  e <- newEventWithTrigger $ \et -> do
        unsubscribe <- {-# SCC "a" #-} liftIO $ {-# SCC "b" #-} elementOnevent element $ {-# SCC "c" #-} do
          v <- {-# SCC "d" #-} getValue
          liftIO $ postGui $ runWithActions [et :=> v]
        return $ liftIO $ do
          {-# SCC "e" #-} unsubscribe
  return $! {-# SCC "f" #-} e

wrapElement :: (Functor (Event t), MonadIO m, MonadSample t m, MonadReflexCreateTrigger t m, Reflex t, HasPostGui t h m) => HTMLElement -> m (El t)
wrapElement e = do
  clicked <- wrapDomEvent e elementOnclick (return ())
  keypress <- wrapDomEvent e elementOnkeypress $ liftIO . uiEventGetKeyCode =<< event
  return $ El e clicked keypress  

elDynAttr' elementTag attrs child = do
  (e, result) <- buildElement elementTag attrs child
  e' <- wrapElement e
  return (e', result)

{-# INLINABLE elAttr #-}
elAttr :: forall t m a. MonadWidget t m => String -> Map String String -> m a -> m a
elAttr elementTag attrs child = do
  (_, result) <- buildElement elementTag attrs child
  return result

{-# INLINABLE el' #-}
--el' :: forall t m a. MonadWidget t m => String -> m a -> m (El t, a)
el' tag child = elAttr' tag (Map.empty :: AttributeMap) child

{-# INLINABLE elAttr' #-}
--elAttr' :: forall t m a. MonadWidget t m => String -> Map String String -> m a -> m (El t, a)
elAttr' elementTag attrs child = do
  (e, result) <- buildElement elementTag attrs child
  e' <- wrapElement e
  return (e', result)

{-# INLINABLE elDynAttr #-}
elDynAttr :: forall t m a. MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m a
elDynAttr elementTag attrs child = do
  (_, result) <- buildElement elementTag attrs child
  return result

{-# INLINABLE el #-}
el :: forall t m a. MonadWidget t m => String -> m a -> m a
el tag child = elAttr tag Map.empty child

--------------------------------------------------------------------------------
-- Copied and pasted from Reflex.Widget.Class
--------------------------------------------------------------------------------

list dm mkChild = listWithKey dm (\_ dv -> mkChild dv)

{-

--TODO: Update dynamically
{-# INLINABLE dynHtml #-}
dynHtml :: MonadWidget t m => Dynamic t String -> m ()
dynHtml ds = do
  let mkSelf h = do
        doc <- askDocument
        Just e <- liftIO $ liftM (fmap castToHTMLElement) $ documentCreateElement doc "div"
        liftIO $ htmlElementSetInnerHTML e h
        return e
  eCreated <- performEvent . fmap mkSelf . tagDyn ds =<< getEInit
  putEChildren $ fmap ((:[]) . toNode) eCreated

-}

data Link t
  = Link { _link_clicked :: Event t ()
         }

linkClass :: MonadWidget t m => String -> String -> m (Link t)
linkClass s c = do
  (l,_) <- elAttr' "a" ("class" =: c) $ text s
  return $ Link $ _el_clicked l

link :: MonadWidget t m => String -> m (Link t)
link s = linkClass s ""

newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

workflowView :: forall t m a. MonadWidget t m => Workflow t m a -> m (Event t a)
workflowView w0 = do
  rec eResult <- dyn =<< mapDyn unWorkflow =<< holdDyn w0 eReplace
      eReplace <- liftM switch $ hold never $ fmap snd eResult
      eResult `seq` eReplace `seq` return ()
  return $ fmap fst eResult

divClass :: forall t m a. MonadWidget t m => String -> m a -> m a
divClass = elAttr "div" . Map.singleton "class"

dtdd :: forall t m a. MonadWidget t m => String -> m a -> m a
dtdd h w = do
  el "dt" $ text h
  el "dd" $ w

blank :: forall t m. MonadWidget t m => m ()
blank = return ()

tableDynAttr :: forall t m r k v. (MonadWidget t m, Show k, Ord k) => String -> [(String, k -> Dynamic t r -> m v)] -> Dynamic t (Map k r) -> (k -> m (Dynamic t (Map String String))) -> m (Dynamic t (Map k (El t, [v])))
tableDynAttr klass cols dRows rowAttrs = elAttr "div" (Map.singleton "style" "zoom: 1; overflow: auto; background: white;") $ do
    elAttr "table" (Map.singleton "class" klass) $ do
      el "thead" $ el "tr" $ do
        mapM (\(h, _) -> el "th" $ text h) cols
      el "tbody" $ do
        listWithKey dRows (\k r -> do
          dAttrs <- rowAttrs k
          elDynAttr' "tr" dAttrs $ mapM (\x -> el "td" $ snd x k r) cols)

--TODO preselect a tab on open
tabDisplay :: forall t m k. (MonadFix m, MonadWidget t m, Show k, Ord k) => String -> String -> Map k (String, m ()) -> m ()
tabDisplay ulClass activeClass tabItems = do
  rec dCurrentTab <- holdDyn Nothing (updated dTabClicks)
      dTabClicks :: Dynamic t (Maybe k) <- elAttr "ul" (Map.singleton "class" ulClass) $ do
        tabClicksList :: [Event t k] <- (liftM Map.elems) $ imapM (\k (s,_) -> headerBarLink s k =<< mapDyn (== (Just k)) dCurrentTab) tabItems
        let eTabClicks :: Event t k = leftmost tabClicksList
        holdDyn Nothing $ fmap Just eTabClicks :: m (Dynamic t (Maybe k))
  divClass "" $ do
    let dTabs :: Dynamic t (Map k (String, m ())) = constDyn tabItems
    listWithKey dTabs (\k dTab -> do
      dAttrs <- mapDyn (\sel -> do
        let t1 = listToMaybe $ Map.keys tabItems
        if sel == Just k || (sel == Nothing && t1 == Just k) then Map.empty else Map.singleton "style" "display:none;") dCurrentTab 
      elDynAttr "div" dAttrs $ dyn =<< mapDyn snd dTab)
    return ()
  where
    headerBarLink :: (MonadWidget t m, Ord k) => String -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k dBool = do
      dAttributes <- mapDyn (\b -> if b then Map.singleton "class" activeClass else Map.empty) dBool
      elDynAttr "li" dAttributes $ do
        a <- link x
        return $ fmap (const k) (_link_clicked a)


