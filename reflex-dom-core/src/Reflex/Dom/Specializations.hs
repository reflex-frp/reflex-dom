{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Dom.Specializations where

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
import Data.Functor.Identity
import Data.Functor.Compose
import Data.IORef
import GHCJS.DOM.Types (JSM)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.FastMutableIntMap

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
