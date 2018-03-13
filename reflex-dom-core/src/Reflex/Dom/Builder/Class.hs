{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#if !MIN_VERSION_base(4,9,0)
{-# LANGUAGE ImpredicativeTypes #-}
#endif
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Builder.Class
       ( module Reflex.Dom.Builder.Class
       , module Reflex.Dom.Builder.Class.Events
       , module Reflex.NotReady.Class
       ) where

import Reflex.Class as Reflex
import Reflex.Dom.Builder.Class.Events
#ifdef USE_TEMPLATE_HASKELL
import Reflex.Dom.Builder.Class.TH
#endif
import Reflex.DynamicWriter
import Reflex.EventWriter
import Reflex.NotReady.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Base
import Reflex.Query.Base
import Reflex.Query.Class
import Reflex.Requester.Base

import qualified Control.Category
import Control.Lens hiding (element)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Data.Default
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import Data.Semigroup
import Data.String
import Data.Text (Text)
import Data.Type.Coercion
import GHCJS.DOM.Types (JSM)
import GHCJS.DOM.Debug (DomHasCallStack)

class Default (EventSpec d EventResult) => DomSpace d where
  type EventSpec d :: (EventTag -> *) -> *
  type RawTextNode d :: *
  type RawElement d :: *
  type RawFile d :: *
  type RawInputElement d :: *
  type RawTextAreaElement d :: *
  type RawSelectElement d :: *
  addEventSpecFlags :: proxy d -> EventName en -> (Maybe (er en) -> EventFlags) -> EventSpec d er -> EventSpec d er

-- | @'DomBuilder' t m@ indicates that @m@ is a 'Monad' capable of building
-- dynamic DOM in the 'Reflex' timeline @t@
class (Monad m, Reflex t, DomSpace (DomBuilderSpace m), NotReady t m, Adjustable t m) => DomBuilder t m | m -> t where
  type DomBuilderSpace m :: *
  textNode :: DomHasCallStack => TextNodeConfig t -> m (TextNode (DomBuilderSpace m) t)
  default textNode :: ( MonadTrans f
                      , m ~ f m'
                      , DomBuilderSpace m' ~ DomBuilderSpace m
                      , DomBuilder t m'
                      , DomHasCallStack
                      )
                   => TextNodeConfig t -> m (TextNode (DomBuilderSpace m) t)
  textNode = lift . textNode
  {-# INLINABLE textNode #-}
  element :: DomHasCallStack => Text -> ElementConfig er t (DomBuilderSpace m) -> m a -> m (Element er (DomBuilderSpace m) t, a)
  default element :: ( MonadTransControl f
                     , StT f a ~ a
                     , m ~ f m'
                     , DomBuilderSpace m' ~ DomBuilderSpace m
                     , DomBuilder t m'
                     , DomHasCallStack
                     )
                  => Text -> ElementConfig er t (DomBuilderSpace m) -> m a -> m (Element er (DomBuilderSpace m) t, a)
  element t cfg child = liftWith $ \run -> element t cfg $ run child
  {-# INLINABLE element #-}
  inputElement :: DomHasCallStack => InputElementConfig er t (DomBuilderSpace m) -> m (InputElement er (DomBuilderSpace m) t)
  default inputElement :: ( MonadTransControl f
                          , m ~ f m'
                          , DomBuilderSpace m' ~ DomBuilderSpace m
                          , DomBuilder t m'
                          , DomHasCallStack
                          )
                       => InputElementConfig er t (DomBuilderSpace m) -> m (InputElement er (DomBuilderSpace m) t)
  inputElement = lift . inputElement
  {-# INLINABLE inputElement #-}
  textAreaElement :: DomHasCallStack => TextAreaElementConfig er t (DomBuilderSpace m) -> m (TextAreaElement er (DomBuilderSpace m) t)
  default textAreaElement :: ( MonadTransControl f
                             , m ~ f m'
                             , DomBuilderSpace m' ~ DomBuilderSpace m
                             , DomBuilder t m'
                             , DomHasCallStack
                             )
                          => TextAreaElementConfig er t (DomBuilderSpace m) -> m (TextAreaElement er (DomBuilderSpace m) t)
  textAreaElement = lift . textAreaElement
  {-# INLINABLE textAreaElement #-}
  selectElement :: DomHasCallStack => SelectElementConfig er t (DomBuilderSpace m) -> m a -> m (SelectElement er (DomBuilderSpace m) t, a)
  default selectElement :: ( MonadTransControl f
                           , StT f a ~ a
                           , m ~ f m'
                           , DomBuilderSpace m' ~ DomBuilderSpace m
                           , DomBuilder t m'
                           , DomHasCallStack
                           )
                        => SelectElementConfig er t (DomBuilderSpace m) -> m a -> m (SelectElement er (DomBuilderSpace m) t, a)
  selectElement cfg child = do
    liftWith $ \run -> selectElement cfg $ run child
  {-# INLINABLE selectElement #-}
  placeRawElement :: DomHasCallStack => RawElement (DomBuilderSpace m) -> m ()
  default placeRawElement :: ( MonadTrans f
                             , m ~ f m'
                             , DomBuilderSpace m' ~ DomBuilderSpace m
                             , DomBuilder t m'
                             , DomHasCallStack
                             )
                          => RawElement (DomBuilderSpace m) -> m ()
  placeRawElement = lift . placeRawElement
  {-# INLINABLE placeRawElement #-}
  wrapRawElement :: DomHasCallStack => RawElement (DomBuilderSpace m) -> RawElementConfig er t (DomBuilderSpace m) -> m (Element er (DomBuilderSpace m) t)
  default wrapRawElement :: ( MonadTrans f
                            , m ~ f m'
                            , DomBuilderSpace m' ~ DomBuilderSpace m
                            , DomBuilder t m'
                            , DomHasCallStack
                            )
                         => RawElement (DomBuilderSpace m) -> RawElementConfig er t (DomBuilderSpace m) -> m (Element er (DomBuilderSpace m) t)
  wrapRawElement e cfg = lift $ wrapRawElement e $ cfg
    { _rawElementConfig_eventSpec = _rawElementConfig_eventSpec cfg
    }
  {-# INLINABLE wrapRawElement #-}

class DomBuilder t m => MountableDomBuilder t m where
  type DomFragment m :: *
  buildDomFragment :: DomHasCallStack => m a -> m (DomFragment m, a)
  mountDomFragment :: DomHasCallStack => DomFragment m -> Event t (DomFragment m) -> m ()

type Namespace = Text

data TextNodeConfig t
   = TextNodeConfig { _textNodeConfig_initialContents :: {-# UNPACK #-} !Text
                    , _textNodeConfig_setContents :: !(Maybe (Event t Text))
                    }

#ifndef USE_TEMPLATE_HASKELL
textNodeConfig_initialContents :: Lens' (TextNodeConfig t) Text
textNodeConfig_initialContents f (TextNodeConfig a b) = (\a' -> TextNodeConfig a' b) <$> f a
{-# INLINE textNodeConfig_initialContents #-}
#endif

instance (Reflex t) => Default (TextNodeConfig t) where
  {-# INLINABLE def #-}
  def = TextNodeConfig
    { _textNodeConfig_initialContents = mempty
    , _textNodeConfig_setContents = Nothing
    }

newtype TextNode d t = TextNode
  { _textNode_raw :: RawTextNode d
  }

data AttributeName = AttributeName !(Maybe Namespace) !Text deriving (Show, Read, Eq, Ord)

mapKeysToAttributeName :: Map Text v -> Map AttributeName v
mapKeysToAttributeName = Map.mapKeysMonotonic (AttributeName Nothing)

-- | By default, AttributeNames are unnamespaced
instance IsString AttributeName where
  fromString = AttributeName Nothing . fromString

data Propagation
   = Propagation_Continue
   | Propagation_Stop
   | Propagation_StopImmediate
   deriving (Show, Read, Eq, Ord)

instance Semigroup Propagation where
  {-# INLINABLE (<>) #-}
  (<>) = max

instance Monoid Propagation where
  {-# INLINABLE mempty #-}
  mempty = Propagation_Continue
  {-# INLINABLE mappend #-}
  mappend = (<>)

data EventFlags = EventFlags --TODO: Monoid; ways of building each flag
  { _eventFlags_propagation :: Propagation
  , _eventFlags_preventDefault :: Bool
  }

instance Semigroup EventFlags where
  {-# INLINABLE (<>) #-}
  EventFlags p pd <> EventFlags p' pd' = EventFlags (p <> p') (pd || pd')

instance Monoid EventFlags where
  {-# INLINABLE mempty #-}
  mempty = EventFlags Propagation_Continue False
  {-# INLINABLE mappend #-}
  mappend = (<>)

preventDefault :: EventFlags
preventDefault = mempty { _eventFlags_preventDefault = True }

stopPropagation :: EventFlags
stopPropagation = mempty { _eventFlags_propagation = Propagation_Stop }

data ElementConfig er t s
   = ElementConfig { _elementConfig_namespace :: Maybe Namespace
                   , _elementConfig_initialAttributes :: Map AttributeName Text
                   , _elementConfig_modifyAttributes :: Maybe (Event t (Map AttributeName (Maybe Text)))
                   , _elementConfig_eventSpec :: EventSpec s er
                   }

#ifndef USE_TEMPLATE_HASKELL
elementConfig_namespace :: Lens' (ElementConfig er t s) (Maybe Namespace)
elementConfig_namespace f (ElementConfig a b c d) = (\a' -> ElementConfig a' b c d) <$> f a
{-# INLINE elementConfig_namespace #-}
elementConfig_initialAttributes :: Lens' (ElementConfig er t s) (Map AttributeName Text)
elementConfig_initialAttributes f (ElementConfig a b c d) = (\b' -> ElementConfig a b' c d) <$> f b
{-# INLINE elementConfig_initialAttributes #-}
elementConfig_eventSpec :: Lens
    (ElementConfig er1 t s1)
    (ElementConfig er2 t s2)
    (EventSpec s1 er1)
    (EventSpec s2 er2)
elementConfig_eventSpec f (ElementConfig a b c d) = (\d' -> ElementConfig a b c d') <$> f d
{-# INLINE elementConfig_eventSpec #-}
#endif

data Element er d t
   = Element { _element_events :: EventSelector t (WrapArg er EventName) --TODO: EventSelector should have two arguments
             , _element_raw :: RawElement d
             }

data InputElementConfig er t s
   = InputElementConfig { _inputElementConfig_initialValue :: Text
                        , _inputElementConfig_setValue :: Maybe (Event t Text)
                        , _inputElementConfig_initialChecked :: Bool
                        , _inputElementConfig_setChecked :: Maybe (Event t Bool)
                        , _inputElementConfig_elementConfig :: ElementConfig er t s
                        }

#ifndef USE_TEMPLATE_HASKELL
inputElementConfig_initialValue :: Lens' (InputElementConfig er t m) Text
inputElementConfig_initialValue f (InputElementConfig a b c d e) = (\a' -> InputElementConfig a' b c d e) <$> f a
{-# INLINE inputElementConfig_initialValue #-}
inputElementConfig_initialChecked :: Lens' (InputElementConfig er t m) Bool
inputElementConfig_initialChecked f (InputElementConfig a b c d e) = (\c' -> InputElementConfig a b c' d e) <$> f c
{-# INLINE inputElementConfig_initialChecked #-}
inputElementConfig_elementConfig :: Lens
    (InputElementConfig er1 t m1)
    (InputElementConfig er2 t m2)
    (ElementConfig er1 t m1)
    (ElementConfig er2 t m2)
inputElementConfig_elementConfig f (InputElementConfig a b c d e) = (\e' -> InputElementConfig a b c d e') <$> f e
{-# INLINE inputElementConfig_elementConfig #-}
#endif

instance (Reflex t, er ~ EventResult, DomSpace s) => Default (InputElementConfig er t s) where
  {-# INLINABLE def #-}
  def = InputElementConfig
    { _inputElementConfig_initialValue = ""
    , _inputElementConfig_setValue = Nothing
    , _inputElementConfig_initialChecked = False
    , _inputElementConfig_setChecked = Nothing
    , _inputElementConfig_elementConfig = def
    }

data InputElement er d t
   = InputElement { _inputElement_value :: Dynamic t Text
                  , _inputElement_checked :: Dynamic t Bool
                  , _inputElement_checkedChange :: Event t Bool
                  , _inputElement_input :: Event t Text
                  , _inputElement_hasFocus :: Dynamic t Bool
                  , _inputElement_element :: Element er d t
                  , _inputElement_raw :: RawInputElement d
                  , _inputElement_files :: Dynamic t [RawFile d]
                  }

data TextAreaElementConfig er t m
   = TextAreaElementConfig { _textAreaElementConfig_initialValue :: Text
                           , _textAreaElementConfig_setValue :: Maybe (Event t Text)
                           , _textAreaElementConfig_elementConfig :: ElementConfig er t m
                           }

#ifndef USE_TEMPLATE_HASKELL
textAreaElementConfig_initialValue :: Lens' (TextAreaElementConfig er t m) Text
textAreaElementConfig_initialValue f (TextAreaElementConfig a b c) = (\a' -> TextAreaElementConfig a' b c) <$> f a
{-# INLINE textAreaElementConfig_initialValue #-}
textAreaElementConfig_elementConfig :: Lens
    (TextAreaElementConfig er1 t m1)
    (TextAreaElementConfig er2 t m2)
    (ElementConfig er1 t m1)
    (ElementConfig er2 t m2)
textAreaElementConfig_elementConfig f (TextAreaElementConfig a b c) = (\c' -> TextAreaElementConfig a b c') <$> f c
{-# INLINE textAreaElementConfig_elementConfig #-}
#endif

instance (Reflex t, er ~ EventResult, DomSpace s) => Default (TextAreaElementConfig er t s) where
  {-# INLINABLE def #-}
  def = TextAreaElementConfig
    { _textAreaElementConfig_initialValue = ""
    , _textAreaElementConfig_setValue = Nothing
    , _textAreaElementConfig_elementConfig = def
    }

data TextAreaElement er d t
   = TextAreaElement { _textAreaElement_value :: Dynamic t Text
                     , _textAreaElement_input :: Event t Text
                     , _textAreaElement_hasFocus :: Dynamic t Bool
                     , _textAreaElement_element :: Element er d t
                     , _textAreaElement_raw :: RawTextAreaElement d
                     }

extractRawElementConfig :: ElementConfig er t m -> RawElementConfig er t m
extractRawElementConfig cfg = RawElementConfig
  { _rawElementConfig_modifyAttributes = _elementConfig_modifyAttributes cfg
  , _rawElementConfig_eventSpec = _elementConfig_eventSpec cfg
  }

data RawElementConfig er t s = RawElementConfig
  { _rawElementConfig_modifyAttributes :: Maybe (Event t (Map AttributeName (Maybe Text)))
  , _rawElementConfig_eventSpec :: EventSpec s er
  }

#ifndef USE_TEMPLATE_HASKELL
rawElementConfig_eventSpec :: Lens
    (RawElementConfig er1 t s1)
    (RawElementConfig er2 t s2)
    (EventSpec s1 er1)
    (EventSpec s2 er2)
rawElementConfig_eventSpec f (RawElementConfig a b) = (\b' -> RawElementConfig a b') <$> f b
{-# INLINE rawElementConfig_eventSpec #-}
#endif

instance (Reflex t, er ~ EventResult, DomSpace s) => Default (RawElementConfig er t s) where
  def = RawElementConfig
    { _rawElementConfig_modifyAttributes = Nothing
    , _rawElementConfig_eventSpec = def
    }

data SelectElementConfig er t m = SelectElementConfig
  { _selectElementConfig_initialValue :: Text
  , _selectElementConfig_setValue :: Maybe (Event t Text)
  , _selectElementConfig_elementConfig :: ElementConfig er t m
  }

#ifndef USE_TEMPLATE_HASKELL
selectElementConfig_initialValue :: Lens' (SelectElementConfig er t m) Text
selectElementConfig_initialValue f (SelectElementConfig a b c) = (\a' -> SelectElementConfig a' b c) <$> f a
{-# INLINE selectElementConfig_initialValue #-}
selectElementConfig_elementConfig :: Lens
    (SelectElementConfig er1 t m1)
    (SelectElementConfig er2 t m2)
    (ElementConfig er1 t m1)
    (ElementConfig er2 t m2)
selectElementConfig_elementConfig f (SelectElementConfig a b c) = (\c' -> SelectElementConfig a b c') <$> f c
{-# INLINE selectElementConfig_elementConfig #-}
#endif

instance (Reflex t, er ~ EventResult, DomSpace s) => Default (SelectElementConfig er t s) where
  def = SelectElementConfig
    { _selectElementConfig_initialValue = ""
    , _selectElementConfig_setValue = Nothing
    , _selectElementConfig_elementConfig = def
    }

data SelectElement er d t = SelectElement
  { _selectElement_element :: Element er d t
  , _selectElement_value :: Dynamic t Text
  , _selectElement_change :: Event t Text -- ^ Fires when the value is changed by the user, but not when it is set by setValue
  , _selectElement_hasFocus :: Dynamic t Bool
  , _selectElement_raw :: RawSelectElement d
  }

#ifdef USE_TEMPLATE_HASKELL
concat <$> mapM (uncurry makeLensesWithoutField)
  [ (["_textNodeConfig_setContents"], ''TextNodeConfig)
  , ([ "_inputElementConfig_setValue"
     , "_inputElementConfig_setChecked" ], ''InputElementConfig)
  , (["_rawElementConfig_modifyAttributes"], ''RawElementConfig)
  , (["_elementConfig_modifyAttributes"], ''ElementConfig)
  , (["_textAreaElementConfig_setValue"], ''TextAreaElementConfig)
  , (["_selectElementConfig_setValue"], ''SelectElementConfig)
  ]
#endif

-- | This lens is technically illegal. The implementation of 'TextNodeConfig' uses a 'Maybe' under the hood for efficiency reasons. However, always interacting with 'TextNodeConfig' via lenses will always behave correctly, and if you pattern match on it, you should always treat 'Nothing' as 'never'.
textNodeConfig_setContents :: Reflex t => Lens (TextNodeConfig t) (TextNodeConfig t) (Event t Text) (Event t Text)
textNodeConfig_setContents =
  let getter = fromMaybe never . _textNodeConfig_setContents
      setter t e = t { _textNodeConfig_setContents = Just e }
  in lens getter setter

-- | This lens is technically illegal. The implementation of 'InputElementConfig' uses a 'Maybe' under the hood for efficiency reasons. However, always interacting with 'InputElementConfig' via lenses will always behave correctly, and if you pattern match on it, you should always treat 'Nothing' as 'never'.
inputElementConfig_setValue :: Reflex t => Lens (InputElementConfig er t m) (InputElementConfig er t m) (Event t Text) (Event t Text)
inputElementConfig_setValue =
  let getter = fromMaybe never . _inputElementConfig_setValue
      setter t e = t { _inputElementConfig_setValue = Just e }
  in lens getter setter

-- | This lens is technically illegal. The implementation of 'InputElementConfig' uses a 'Maybe' under the hood for efficiency reasons. However, always interacting with 'InputElementConfig' via lenses will always behave correctly, and if you pattern match on it, you should always treat 'Nothing' as 'never'.
inputElementConfig_setChecked :: Reflex t => Lens (InputElementConfig er t m) (InputElementConfig er t m) (Event t Bool) (Event t Bool)
inputElementConfig_setChecked =
  let getter = fromMaybe never . _inputElementConfig_setChecked
      setter t e = t { _inputElementConfig_setChecked = Just e }
  in lens getter setter

-- | This lens is technically illegal. The implementation of 'RawElementConfig' uses a 'Maybe' under the hood for efficiency reasons. However, always interacting with 'RawElementConfig' via lenses will always behave correctly, and if you pattern match on it, you should always treat 'Nothing' as 'never'.
rawElementConfig_modifyAttributes :: Reflex t => Lens (RawElementConfig er t m) (RawElementConfig er t m) (Event t (Map AttributeName (Maybe Text))) (Event t (Map AttributeName (Maybe Text)))
rawElementConfig_modifyAttributes =
  let getter = fromMaybe never . _rawElementConfig_modifyAttributes
      setter t e = t { _rawElementConfig_modifyAttributes = Just e }
  in lens getter setter

-- | This lens is technically illegal. The implementation of 'RawElementConfig' uses a 'Maybe' under the hood for efficiency reasons. However, always interacting with 'RawElementConfig' via lenses will always behave correctly, and if you pattern match on it, you should always treat 'Nothing' as 'never'.
elementConfig_modifyAttributes :: Reflex t => Lens (ElementConfig er t m) (ElementConfig er t m) (Event t (Map AttributeName (Maybe Text))) (Event t (Map AttributeName (Maybe Text)))
elementConfig_modifyAttributes =
  let getter = fromMaybe never . _elementConfig_modifyAttributes
      setter t e = t { _elementConfig_modifyAttributes = Just e }
  in lens getter setter

-- | This lens is technically illegal. The implementation of 'TextAreaElementConfig' uses a 'Maybe' under the hood for efficiency reasons. However, always interacting with 'TextAreaElementConfig' via lenses will always behave correctly, and if you pattern match on it, you should always treat 'Nothing' as 'never'.
textAreaElementConfig_setValue :: Reflex t => Lens (TextAreaElementConfig er t m) (TextAreaElementConfig er t m) (Event t Text) (Event t Text)
textAreaElementConfig_setValue =
  let getter = fromMaybe never . _textAreaElementConfig_setValue
      setter t e = t { _textAreaElementConfig_setValue = Just e }
  in lens getter setter

-- | This lens is technically illegal. The implementation of 'SelectElementConfig' uses a 'Maybe' under the hood for efficiency reasons. However, always interacting with 'SelectElementConfig' via lenses will always behave correctly, and if you pattern match on it, you should always treat 'Nothing' as 'never'.
selectElementConfig_setValue :: Reflex t => Lens (SelectElementConfig er t m) (SelectElementConfig er t m) (Event t Text) (Event t Text)
selectElementConfig_setValue =
  let getter = fromMaybe never . _selectElementConfig_setValue
      setter t e = t { _selectElementConfig_setValue = Just e }
  in lens getter setter

class InitialAttributes a where
  initialAttributes :: Lens' a (Map AttributeName Text)

instance InitialAttributes (ElementConfig er t m) where
  {-# INLINABLE initialAttributes #-}
  initialAttributes = elementConfig_initialAttributes

instance InitialAttributes (InputElementConfig er t m) where
  {-# INLINABLE initialAttributes #-}
  initialAttributes = inputElementConfig_elementConfig . elementConfig_initialAttributes

instance InitialAttributes (TextAreaElementConfig er t m) where
  {-# INLINABLE initialAttributes #-}
  initialAttributes = textAreaElementConfig_elementConfig . elementConfig_initialAttributes

instance InitialAttributes (SelectElementConfig er t m) where
  {-# INLINABLE initialAttributes #-}
  initialAttributes = selectElementConfig_elementConfig . elementConfig_initialAttributes

class ModifyAttributes t a | a -> t where
  modifyAttributes :: Reflex t => Lens' a (Event t (Map AttributeName (Maybe Text)))

instance ModifyAttributes t (ElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = elementConfig_modifyAttributes

instance ModifyAttributes t (InputElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = inputElementConfig_elementConfig . elementConfig_modifyAttributes

instance ModifyAttributes t (TextAreaElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = textAreaElementConfig_elementConfig . elementConfig_modifyAttributes

instance ModifyAttributes t (SelectElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = selectElementConfig_elementConfig . elementConfig_modifyAttributes

instance ModifyAttributes t (RawElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = rawElementConfig_modifyAttributes

class HasNamespace a where
  namespace :: Lens' a (Maybe Namespace)

instance HasNamespace (ElementConfig er t m) where
  {-# INLINABLE namespace #-}
  namespace = elementConfig_namespace

instance (Reflex t, er ~ EventResult, DomSpace s) => Default (ElementConfig er t s) where
  {-# INLINABLE def #-}
  def = ElementConfig
    { _elementConfig_namespace = Nothing
    , _elementConfig_initialAttributes = mempty
    , _elementConfig_modifyAttributes = Nothing
    , _elementConfig_eventSpec = def
    }

instance (DomBuilder t m, PerformEvent t m, MonadFix m, MonadHold t m) => DomBuilder t (PostBuildT t m) where
  type DomBuilderSpace (PostBuildT t m) = DomBuilderSpace m
  wrapRawElement e = lift . wrapRawElement e

instance (MountableDomBuilder t m, PerformEvent t m, MonadFix m, MonadHold t m) => MountableDomBuilder t (PostBuildT t m) where
  type DomFragment (PostBuildT t m) = DomFragment m
  buildDomFragment = liftThrough buildDomFragment
  mountDomFragment f0 f' = lift $ mountDomFragment f0 f'

instance (DomBuilder t m, Monoid w, MonadHold t m, MonadFix m) => DomBuilder t (DynamicWriterT t w m) where
  type DomBuilderSpace (DynamicWriterT t w m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (DynamicWriterT child) = DynamicWriterT $ do
    s <- get
    (el, (a, newS)) <- lift $ element elementTag cfg $ runStateT child s
    put newS
    return (el, a)
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (DynamicWriterT child) = DynamicWriterT $ do
    s <- get
    (el, (a, newS)) <- lift $ selectElement cfg $ runStateT child s
    put newS
    return (el, a)
  placeRawElement = lift . placeRawElement
  wrapRawElement e = lift . wrapRawElement e

instance (DomBuilder t m, MonadHold t m, MonadFix m) => DomBuilder t (RequesterT t request response m) where
  type DomBuilderSpace (RequesterT t request response m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (RequesterT child) = RequesterT $ do
    r <- ask
    old <- get
    (el, (a, new)) <- lift $ lift $ element elementTag cfg $ runReaderT (runStateT child old) r
    put new
    return (el, a)
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (RequesterT child) = RequesterT $ do
    r <- ask
    old <- get
    (el, (a, new)) <- lift $ lift $ selectElement cfg $ runReaderT (runStateT child old) r
    put new
    return (el, a)
  placeRawElement = lift . placeRawElement
  wrapRawElement e = lift . wrapRawElement e

instance (DomBuilder t m, MonadHold t m, MonadFix m, Semigroup w) => DomBuilder t (EventWriterT t w m) where
  type DomBuilderSpace (EventWriterT t w m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (EventWriterT child) = EventWriterT $ do
    old <- get
    (el, (a, new)) <- lift $ element elementTag cfg $ runStateT child old
    put new
    return (el, a)
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (EventWriterT child) = EventWriterT $ do
    old <- get
    (el, (a, new)) <- lift $ selectElement cfg $ runStateT child old
    put new
    return (el, a)
  placeRawElement = lift . placeRawElement
  wrapRawElement e = lift . wrapRawElement e

instance (DomBuilder t m, MonadFix m, MonadHold t m, Group q, Query q, Additive q) => DomBuilder t (QueryT t q m) where
  type DomBuilderSpace (QueryT t q m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (QueryT child) = QueryT $ do
    s <- get
    (e, (a, newS)) <- lift $ element elementTag cfg $ runStateT child s
    put newS
    return (e, a)
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (QueryT child) = QueryT $ do
    s <- get
    (e, (a, newS)) <- lift $ selectElement cfg $ runStateT child s
    put newS
    return (e, a)
  placeRawElement = lift . placeRawElement
  wrapRawElement e = lift . wrapRawElement e

-- * Convenience functions

class HasDomEvent t target eventName where
  type DomEventType target eventName :: *
  domEvent :: EventName eventName -> target -> Event t (DomEventType target eventName)

instance Reflex t => HasDomEvent t (Element EventResult d t) en where
  type DomEventType (Element EventResult d t) en = EventResultType en
  {-# INLINABLE domEvent #-}
  domEvent en e = coerceEvent $ Reflex.select (_element_events e) (WrapArg en)

instance Reflex t => HasDomEvent t (InputElement EventResult d t) en where
  type DomEventType (InputElement EventResult d t) en = EventResultType en
  {-# INLINABLE domEvent #-}
  domEvent en = domEvent en . _inputElement_element

instance Reflex t => HasDomEvent t (TextAreaElement EventResult d t) en where
  type DomEventType (TextAreaElement EventResult d t) en = EventResultType en
  {-# INLINABLE domEvent #-}
  domEvent en = domEvent en . _textAreaElement_element

instance DomBuilder t m => DomBuilder t (ReaderT r m) where
  type DomBuilderSpace (ReaderT r m) = DomBuilderSpace m

type LiftDomBuilder t f m =
  ( Reflex t
  , MonadTransControlStateless f
  , Monad m
  , DomBuilder t m
  , DomBuilderSpace (f m) ~ DomBuilderSpace m
  )

class MonadTransControl t => MonadTransControlStateless t where
  stTCoercion :: proxy t -> Coercion (StT t a) a
  default stTCoercion :: (a ~ StT t a) => proxy t -> Coercion (StT t a) a
  stTCoercion _ = Control.Category.id

toStT :: MonadTransControlStateless t => proxy t -> a -> StT t a
toStT = coerceWith . sym . stTCoercion

fromStT :: MonadTransControlStateless t => proxy t -> StT t a -> a
fromStT = coerceWith . stTCoercion

instance MonadTransControlStateless (ReaderT r)

type RunStateless t = forall n b. Monad n => t n b -> n b

liftWithStateless :: forall m t a. (Monad m, MonadTransControlStateless t) => (RunStateless t -> m a) -> t m a
liftWithStateless a = liftWith $ \run -> a $ fmap (fromStT (Proxy :: Proxy t)) . run

liftTextNode :: (MonadTrans f, DomBuilder t m) => TextNodeConfig t -> f m (TextNode (DomBuilderSpace m) t)
liftTextNode = lift . textNode

liftElement :: LiftDomBuilder t f m => Text -> ElementConfig er t (DomBuilderSpace m) -> f m a -> f m (Element er (DomBuilderSpace m) t, a)
liftElement elementTag cfg child = liftWithStateless $ \run -> element elementTag cfg $ run child

class (Reflex t, Monad m) => DomRenderHook t m | m -> t where
  withRenderHook :: (forall x. JSM x -> JSM x) -> m a -> m a
  requestDomAction :: Event t (JSM a) -> m (Event t a)
  requestDomAction_ :: Event t (JSM a) -> m ()

instance DomRenderHook t m => DomRenderHook t (ReaderT e m) where
  withRenderHook hook (ReaderT a) = ReaderT $ \e -> withRenderHook hook $ a e
  requestDomAction = lift . requestDomAction
  requestDomAction_ = lift . requestDomAction_

instance DomRenderHook t m => DomRenderHook t (StateT e m) where
  withRenderHook hook (StateT a) = StateT $ \s -> withRenderHook hook $ a s
  requestDomAction = lift . requestDomAction
  requestDomAction_ = lift . requestDomAction_

deriving instance DomRenderHook t m => DomRenderHook t (EventWriterT t w m)
deriving instance DomRenderHook t m => DomRenderHook t (RequesterT t req rsp m)
deriving instance DomRenderHook t m => DomRenderHook t (PostBuildT t m)
deriving instance DomRenderHook t m => DomRenderHook t (QueryT t q m)

{-# DEPRECATED liftElementConfig "Use 'id' instead; this function is no longer necessary" #-}
liftElementConfig :: ElementConfig er t s -> ElementConfig er t s
liftElementConfig = id
