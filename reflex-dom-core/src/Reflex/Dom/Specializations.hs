{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Dom.Specializations () where

import Control.Concurrent.Chan (Chan)
import Data.Dependent.Sum (DSum)
import Data.Dependent.Map (DMap)
import Data.Some (Some)
import Data.Text (Text)
import Reflex.Spider
import Reflex.Class
import Foreign.JavaScript.TH
import Reflex.PerformEvent.Base
import Reflex.PostBuild.Base
import Reflex.Requester.Base
import Reflex.TriggerEvent.Base
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import qualified Reflex.Dom.Builder.Hydration as H
import Data.Functor.Identity
import Data.Functor.Compose
import Data.IORef
import GHCJS.DOM.Types (JSM)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Types as DOM

{-# SPECIALIZE makeElement :: forall er a. Text -> ElementConfig er (SpiderTimeline Global) GhcjsDomSpace -> ImmediateDomBuilderT (SpiderTimeline Global) (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global))) a -> ImmediateDomBuilderT (SpiderTimeline Global) (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global))) ((Element er GhcjsDomSpace (SpiderTimeline Global), a), DOM.Element) #-}

{-# SPECIALIZE wrap :: forall er. RawElement GhcjsDomSpace -> RawElementConfig er (SpiderTimeline Global) GhcjsDomSpace -> ImmediateDomBuilderT (SpiderTimeline Global) (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global))) (Element er GhcjsDomSpace (SpiderTimeline Global)) #-}

{-# SPECIALIZE mapIntMapWithAdjustImpl :: forall v v'.
     (   (IntMap.Key -> (Event (SpiderTimeline Global) (), v) -> (ImmediateDomBuilderT Spider (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global)))) v')
      -> IntMap (Event (SpiderTimeline Global) (), v)
      -> Event (SpiderTimeline Global) (PatchIntMap (Event (SpiderTimeline Global) (), v))
      -> (ImmediateDomBuilderT Spider (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global)))) (IntMap v', Event (SpiderTimeline Global) (PatchIntMap v'))
     )
  -> (IntMap.Key -> v -> PostBuildT (SpiderTimeline Global) (ImmediateDomBuilderT Spider (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global)))) v')
  -> IntMap v
  -> Event (SpiderTimeline Global) (PatchIntMap v)
  -> PostBuildT (SpiderTimeline Global) (ImmediateDomBuilderT Spider (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global)))) (IntMap v', Event (SpiderTimeline Global) (PatchIntMap v'))
  #-}

{-# SPECIALIZE append :: DOM.Node -> ImmediateDomBuilderT (SpiderTimeline Global) (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global))) () #-}

{-# SPECIALIZE drawChildUpdate :: forall k v' a. ImmediateDomBuilderEnv (SpiderTimeline Global) -> (IORef (ChildReadyState k) -> JSM ()) -> ImmediateDomBuilderT (SpiderTimeline Global) (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global))) (v' a) -> RequesterT (SpiderTimeline Global) JSM Identity (TriggerEventT (SpiderTimeline Global) (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global)))) (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v' a) #-}

{-# SPECIALIZE traverseIntMapWithKeyWithAdjust' :: forall v v'. (IntMap.Key -> v -> ImmediateDomBuilderT (SpiderTimeline Global) (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global))) v') -> IntMap v -> Event (SpiderTimeline Global) (PatchIntMap v) -> ImmediateDomBuilderT (SpiderTimeline Global) (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global))) (IntMap v', Event (SpiderTimeline Global) (PatchIntMap v')) #-}

{-# SPECIALIZE hoistTraverseIntMapWithKeyWithAdjust :: forall v v'.
     (   (IntMap.Key -> v -> RequesterT (SpiderTimeline Global) JSM Identity (TriggerEventT (SpiderTimeline Global) (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global)))) (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v'))
      -> IntMap v
      -> Event (SpiderTimeline Global) (PatchIntMap v)
      -> RequesterT (SpiderTimeline Global) JSM Identity (TriggerEventT (SpiderTimeline Global) (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global)))) (IntMap (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v'), Event (SpiderTimeline Global) (PatchIntMap (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v')))
     )
  -> (PatchIntMap (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v') -> IntMap (IORef ChildReadyStateInt) -> IO (IntMap (IORef ChildReadyStateInt)))
  -> (IORef (IntMap DOM.Text) -> IORef DOM.Text -> PatchIntMap (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v') -> JSM ())
  -> (IntMap.Key -> v -> ImmediateDomBuilderT (SpiderTimeline Global) (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global))) v')
  -> IntMap v
  -> Event (SpiderTimeline Global) (PatchIntMap v)
  -> ImmediateDomBuilderT (SpiderTimeline Global) (WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global))) (IntMap v', Event (SpiderTimeline Global) (PatchIntMap v'))
  #-}

-- Hydration stuff

type HydrationM = WithJSContextSingleton () (PerformEventT Spider (SpiderHost Global))

{-# SPECIALIZE H.makeElement
  :: Text
  -> ElementConfig er Spider H.HydrationDomSpace
  -> H.HydrationDomBuilderT Spider HydrationM a
  -> H.HydrationDomBuilderT Spider HydrationM
    ((Element er H.HydrationDomSpace Spider, a), Either DOM.Element (IORef DOM.Element))
  #-}

{-# SPECIALIZE H.makeNodeInternal
  :: H.HydrationDomBuilderT Spider HydrationM DOM.Element
  -> H.HydrationDomBuilderT Spider HydrationM DOM.Element
  #-}
{-# SPECIALIZE H.makeNodeInternal
  :: H.HydrationDomBuilderT Spider HydrationM DOM.Text
  -> H.HydrationDomBuilderT Spider HydrationM DOM.Text
  #-}

{-# SPECIALIZE H.makeNodeInternal
  :: H.HydrationDomBuilderT Spider HydrationM DOM.Comment
  -> H.HydrationDomBuilderT Spider HydrationM DOM.Comment
  #-}

{-# SPECIALIZE H.drawChildUpdate
  :: H.HydrationDomBuilderEnv Spider HydrationM
  -> (IORef (H.ChildReadyState Int) -> JSM ())
  -> H.HydrationDomBuilderT Spider HydrationM (Identity a)
  -> H.DomRenderHookT Spider HydrationM (Compose (H.TraverseChild Spider HydrationM Int) Identity a)
  #-}

{-# SPECIALIZE H.drawChildUpdate
  :: H.HydrationDomBuilderEnv Spider HydrationM
  -> (IORef (H.ChildReadyState (Some k)) -> JSM ())
  -> H.HydrationDomBuilderT Spider HydrationM (f a)
  -> H.DomRenderHookT Spider HydrationM (Compose (H.TraverseChild Spider HydrationM (Some k)) f a)
  #-}

{-# SPECIALIZE H.drawChildUpdateInt
  :: H.HydrationDomBuilderEnv Spider HydrationM
  -> (IORef (H.ChildReadyState k) -> JSM ())
  -> H.HydrationDomBuilderT Spider HydrationM v
  -> H.DomRenderHookT Spider HydrationM (H.TraverseChild Spider HydrationM k v)
  #-}

{-# SPECIALIZE H.append
  :: DOM.Node
  -> H.HydrationDomBuilderT Spider HydrationM ()
  #-}

{-# SPECIALIZE H.wrap
  :: Chan [DSum (EventTriggerRef Spider) TriggerInvocation]
  -> DOM.Element
  -> RawElementConfig er Spider H.HydrationDomSpace
  -> H.HydrationDomBuilderT Spider HydrationM (DMap EventName (EventFilterTriggerRef Spider er))
  #-}

{-# SPECIALIZE H.triggerBody
  :: DOM.JSContextRef
  -> ElementConfig er Spider H.HydrationDomSpace
  -> Chan [DSum (EventTriggerRef Spider) TriggerInvocation]
  -> DMap EventName (EventFilterTriggerRef Spider er)
  -> DOM.Element
  -> H.WrapArg er EventName x
  -> H.EventTrigger Spider x
  -> IO (IO ())
  #-}

{-# SPECIALIZE H.traverseIntMapWithKeyWithAdjust'
  :: (IntMap.Key -> v -> H.HydrationDomBuilderT Spider HydrationM v')
  -> IntMap v
  -> Event Spider (PatchIntMap v)
  -> H.HydrationDomBuilderT Spider HydrationM (IntMap v', Event Spider (PatchIntMap v'))
  #-}

{-# SPECIALIZE H.hoistTraverseIntMapWithKeyWithAdjust
  :: ((IntMap.Key -> v -> H.DomRenderHookT Spider HydrationM (H.TraverseChild Spider HydrationM Int v'))
    -> IntMap v
    -> Event Spider (PatchIntMap v)
    -> H.DomRenderHookT Spider HydrationM (IntMap (H.TraverseChild Spider HydrationM Int v'), Event Spider (PatchIntMap (H.TraverseChild Spider HydrationM Int v'))))
  -> (PatchIntMap (H.TraverseChild Spider HydrationM Int v')
    -> IntMap (IORef (H.ChildReadyState Int))
    -> IO (IntMap (IORef (H.ChildReadyState Int))))
  -> (IORef (IntMap DOM.Text)
    -> DOM.Text
    -> PatchIntMap (H.TraverseChild Spider HydrationM Int v')
    -> JSM ())
  -> (IntMap.Key -> v -> H.HydrationDomBuilderT Spider HydrationM v')
  -> IntMap v
  -> Event Spider (PatchIntMap v)
  -> H.HydrationDomBuilderT Spider HydrationM (IntMap v', Event Spider (PatchIntMap v'))
  #-}

{-# SPECIALIZE H.runHydrationDomBuilderT
  :: H.HydrationDomBuilderT Spider HydrationM a
  -> H.HydrationDomBuilderEnv Spider HydrationM
  -> Chan [DSum (EventTriggerRef Spider) TriggerInvocation]
  -> HydrationM a
  #-}

{-# SPECIALIZE H.runDomRenderHookT
  :: H.DomRenderHookT Spider HydrationM a
  -> Chan [DSum (EventTriggerRef Spider) TriggerInvocation]
  -> HydrationM a
  #-}
