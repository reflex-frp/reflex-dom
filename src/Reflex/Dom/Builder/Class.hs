{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Builder.Class
       ( module Reflex.Dom.Builder.Class
       , module Reflex.Dom.Builder.Class.Events
       ) where

import Reflex.Class as Reflex
import Reflex.Dom.Builder.Class.Events
import Reflex.DynamicWriter
import Reflex.EventWriter
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Base
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
import Data.Proxy
import Data.Semigroup
import Data.String
import Data.Text (Text)
import Data.Type.Coercion
import GHC.Exts (Constraint)

class Default (EventSpec d EventResult) => DomSpace d where
  type EventSpec d :: (EventTag -> *) -> *
  type RawTextNode d :: *
  type RawElement d :: *
  type RawFile d :: *
  type RawInputElement d :: *
  type RawTextAreaElement d :: *
  type RawSelectElement d :: *
  addEventSpecFlags :: proxy d -> EventName en -> (Maybe (er en) -> EventFlags) -> EventSpec d er -> EventSpec d er

{-# INLINABLE liftElementConfig #-}
liftElementConfig :: (DomBuilderSpace (f m) ~ DomBuilderSpace m) => ElementConfig er t (f m) -> ElementConfig er t m
liftElementConfig cfg = cfg
  { _elementConfig_eventSpec = _elementConfig_eventSpec cfg
  }

-- | @'DomBuilder' t m@ indicates that @m@ is a 'Monad' capable of building
-- dynamic DOM in the 'Reflex' timeline @t@
class (Monad m, Reflex t, DomSpace (DomBuilderSpace m), MonadAdjust t m) => DomBuilder t m | m -> t where
  type DomBuilderSpace m :: *
  textNode :: TextNodeConfig t -> m (TextNode (DomBuilderSpace m) t)
  default textNode :: ( MonadTrans f
                      , m ~ f m'
                      , DomBuilderSpace m' ~ DomBuilderSpace m
                      , DomBuilder t m'
                      )
                   => TextNodeConfig t -> m (TextNode (DomBuilderSpace m) t)
  textNode = lift . textNode
  {-# INLINABLE textNode #-}
  element :: Text -> ElementConfig er t m -> m a -> m (Element er (DomBuilderSpace m) t, a)
  default element :: ( MonadTransControl f
                     , StT f a ~ a
                     , m ~ f m'
                     , DomBuilderSpace m' ~ DomBuilderSpace m
                     , DomBuilder t m'
                     )
                  => Text -> ElementConfig er t m -> m a -> m (Element er (DomBuilderSpace m) t, a)
  element t cfg child = liftWith $ \run -> element t (liftElementConfig cfg) $ run child
  {-# INLINABLE element #-}
  {-
  -- | Create a placeholder in the DOM, with the ability to insert new DOM before it
  -- The provided DOM will be executed after the current frame, so it will not be affected by any occurrences that are concurrent with the occurrence that created it
  placeholder :: PlaceholderConfig above t m -> m (Placeholder above t)
-}
  inputElement :: InputElementConfig er t m -> m (InputElement er (DomBuilderSpace m) t)
  default inputElement :: ( MonadTransControl f
                          , m ~ f m'
                          , DomBuilderSpace m' ~ DomBuilderSpace m
                          , DomBuilder t m'
                          )
                       => InputElementConfig er t m -> m (InputElement er (DomBuilderSpace m) t)
  inputElement cfg = lift $ inputElement $ cfg
    { _inputElementConfig_elementConfig = liftElementConfig $ _inputElementConfig_elementConfig cfg
    }
  {-# INLINABLE inputElement #-}
  textAreaElement :: TextAreaElementConfig er t m -> m (TextAreaElement er (DomBuilderSpace m) t)
  default textAreaElement :: ( MonadTransControl f
                             , m ~ f m'
                             , DomBuilderSpace m' ~ DomBuilderSpace m
                             , DomBuilder t m'
                             )
                          => TextAreaElementConfig er t m -> m (TextAreaElement er (DomBuilderSpace m) t)
  textAreaElement cfg = lift $ textAreaElement $ cfg
    { _textAreaElementConfig_elementConfig = liftElementConfig $ _textAreaElementConfig_elementConfig cfg
    }
  {-# INLINABLE textAreaElement #-}
  selectElement :: SelectElementConfig er t m -> m a -> m (SelectElement er (DomBuilderSpace m) t, a)
  default selectElement :: ( MonadTransControl f
                           , StT f a ~ a
                           , m ~ f m'
                           , DomBuilderSpace m' ~ DomBuilderSpace m
                           , DomBuilder t m'
                           )
                        => SelectElementConfig er t m -> m a -> m (SelectElement er (DomBuilderSpace m) t, a)
  selectElement cfg child = do
    let cfg' = cfg
          { _selectElementConfig_elementConfig = liftElementConfig $ _selectElementConfig_elementConfig cfg
          }
    liftWith $ \run -> selectElement cfg' $ run child
  {-# INLINABLE selectElement #-}
  placeRawElement :: RawElement (DomBuilderSpace m) -> m ()
  default placeRawElement :: ( MonadTrans f
                             , m ~ f m'
                             , DomBuilderSpace m' ~ DomBuilderSpace m
                             , DomBuilder t m'
                             )
                          => RawElement (DomBuilderSpace m) -> m ()
  placeRawElement = lift . placeRawElement
  {-# INLINABLE placeRawElement #-}
  wrapRawElement :: RawElement (DomBuilderSpace m) -> RawElementConfig er t m -> m (Element er (DomBuilderSpace m) t)
  default wrapRawElement :: ( MonadTrans f
                            , m ~ f m'
                            , DomBuilderSpace m' ~ DomBuilderSpace m
                            , DomBuilder t m'
                            )
                         => RawElement (DomBuilderSpace m) -> RawElementConfig er t m -> m (Element er (DomBuilderSpace m) t)
  wrapRawElement e cfg = lift $ wrapRawElement e $ cfg
    { _rawElementConfig_eventSpec = _rawElementConfig_eventSpec cfg
    }
  {-# INLINABLE wrapRawElement #-}

type Namespace = Text

data TextNodeConfig t
   = TextNodeConfig { _textNodeConfig_initialContents :: Text
                    , _textNodeConfig_setContents :: Event t Text
                    }

instance (Reflex t) => Default (TextNodeConfig t) where
  {-# INLINABLE def #-}
  def = TextNodeConfig
    { _textNodeConfig_initialContents = mempty
    , _textNodeConfig_setContents = never
    }

data TextNode d t = TextNode
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

data ElementConfig er t m
   = ElementConfig { _elementConfig_namespace :: Maybe Namespace
                   , _elementConfig_initialAttributes :: Map AttributeName Text
                   , _elementConfig_modifyAttributes :: Event t (Map AttributeName (Maybe Text))
                   , _elementConfig_eventSpec :: EventSpec (DomBuilderSpace m) er
                   }

data Element er d t
   = Element { _element_events :: EventSelector t (WrapArg er EventName) --TODO: EventSelector should have two arguments
             , _element_raw :: RawElement d
             }

data PlaceholderConfig above t m
   = PlaceholderConfig { _placeholderConfig_insertAbove :: Event t (m above)
                       , _placeholderConfig_deleteSelf :: Event t ()
                       }

instance Reflex t => Default (PlaceholderConfig above t m) where
  {-# INLINABLE def #-}
  def = PlaceholderConfig
    { _placeholderConfig_insertAbove = never
    , _placeholderConfig_deleteSelf = never
    }

data Placeholder above t
   = Placeholder { _placeholder_insertedAbove :: Event t above
                 , _placeholder_deletedSelf :: Event t ()
                 }

data InputElementConfig er t m
   = InputElementConfig { _inputElementConfig_initialValue :: Text
                        , _inputElementConfig_setValue :: Event t Text
                        , _inputElementConfig_initialChecked :: Bool
                        , _inputElementConfig_setChecked :: Event t Bool
                        , _inputElementConfig_elementConfig :: ElementConfig er t m
                        }

instance (Reflex t, er ~ EventResult, DomBuilder t m) => Default (InputElementConfig er t m) where
  {-# INLINABLE def #-}
  def = InputElementConfig
    { _inputElementConfig_initialValue = ""
    , _inputElementConfig_setValue = never
    , _inputElementConfig_initialChecked = False
    , _inputElementConfig_setChecked = never
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
                           , _textAreaElementConfig_setValue :: Event t Text
                           , _textAreaElementConfig_elementConfig :: ElementConfig er t m
                           }

instance (Reflex t, er ~ EventResult, DomBuilder t m) => Default (TextAreaElementConfig er t m) where
  {-# INLINABLE def #-}
  def = TextAreaElementConfig
    { _textAreaElementConfig_initialValue = ""
    , _textAreaElementConfig_setValue = never
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

data RawElementConfig er t m = RawElementConfig
  { _rawElementConfig_modifyAttributes :: Event t (Map AttributeName (Maybe Text))
  , _rawElementConfig_eventSpec :: EventSpec (DomBuilderSpace m) er
  }

instance (Reflex t, DomSpace (DomBuilderSpace m)) => Default (RawElementConfig EventResult t m) where
  def = RawElementConfig
    { _rawElementConfig_modifyAttributes = never
    , _rawElementConfig_eventSpec = def
    }

data SelectElementConfig er t m = SelectElementConfig
  { _selectElementConfig_initialValue :: Text
  , _selectElementConfig_setValue :: Event t Text
  , _selectElementConfig_elementConfig :: ElementConfig er t m
  }

instance (Reflex t, er ~ EventResult, DomBuilder t m) => Default (SelectElementConfig er t m) where
  def = SelectElementConfig
    { _selectElementConfig_initialValue = ""
    , _selectElementConfig_setValue = never
    , _selectElementConfig_elementConfig = def
    }

data SelectElement er d t = SelectElement
  { _selectElement_element :: Element er d t
  , _selectElement_value :: Dynamic t Text
  , _selectElement_change :: Event t Text -- ^ Fires when the value is changed by the user, but not when it is set by setValue
  , _selectElement_hasFocus :: Dynamic t Bool
  , _selectElement_raw :: RawSelectElement d
  }

concat <$> mapM makeLenses
  [ ''TextNodeConfig
  , ''ElementConfig
  , ''PlaceholderConfig
  , ''InputElementConfig
  , ''TextAreaElementConfig
  , ''SelectElementConfig
  , ''RawElementConfig
  ]

class CanDeleteSelf t a | a -> t where
  deleteSelf :: Lens' a (Event t ())

instance CanDeleteSelf t (PlaceholderConfig above t m) where
  {-# INLINABLE deleteSelf #-}
  deleteSelf = placeholderConfig_deleteSelf

class InsertAbove t m above above' a a' | a -> t m above, a' -> t m above', a above' -> a', a' above -> a where
  insertAbove :: Lens a a' (Event t (m above)) (Event t (m above'))

instance InsertAbove t m above above' (PlaceholderConfig above t m) (PlaceholderConfig above' t m) where
  insertAbove = placeholderConfig_insertAbove

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

class ModifyAttributes t a | a -> t where
  modifyAttributes :: Lens' a (Event t (Map AttributeName (Maybe Text)))

instance ModifyAttributes t (ElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = elementConfig_modifyAttributes

instance ModifyAttributes t (InputElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = inputElementConfig_elementConfig . elementConfig_modifyAttributes

instance ModifyAttributes t (TextAreaElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = textAreaElementConfig_elementConfig . elementConfig_modifyAttributes

instance ModifyAttributes t (RawElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = rawElementConfig_modifyAttributes

class HasNamespace a where
  namespace :: Lens' a (Maybe Namespace)

instance HasNamespace (ElementConfig er t m) where
  {-# INLINABLE namespace #-}
  namespace = elementConfig_namespace

instance (Reflex t, er ~ EventResult, DomBuilder t m) => Default (ElementConfig er t m) where
  {-# INLINABLE def #-}
  def = ElementConfig
    { _elementConfig_namespace = Nothing
    , _elementConfig_initialAttributes = mempty
    , _elementConfig_modifyAttributes = never
    , _elementConfig_eventSpec = def
    }

instance (DomBuilder t m, PerformEvent t m, MonadFix m, MonadHold t m) => DomBuilder t (PostBuildT t m) where
  type DomBuilderSpace (PostBuildT t m) = DomBuilderSpace m
  wrapRawElement e cfg = liftWith $ \run -> wrapRawElement e $ fmap1 run cfg

instance (DomBuilder t m, Monoid w, MonadHold t m, MonadFix m) => DomBuilder t (DynamicWriterT t w m) where
  type DomBuilderSpace (DynamicWriterT t w m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (DynamicWriterT child) = DynamicWriterT $ do
    s <- get
    let cfg' = liftElementConfig cfg
    (el, (a, newS)) <- lift $ element elementTag cfg' $ runStateT child s
    put newS
    return (el, a)
  inputElement cfg = lift $ inputElement $ cfg & inputElementConfig_elementConfig %~ liftElementConfig
  textAreaElement cfg = lift $ textAreaElement $ cfg & textAreaElementConfig_elementConfig %~ liftElementConfig
  selectElement cfg (DynamicWriterT child) = DynamicWriterT $ do
    s <- get
    let cfg' = cfg & selectElementConfig_elementConfig %~ liftElementConfig
    (el, (a, newS)) <- lift $ selectElement cfg' $ runStateT child s
    put newS
    return (el, a)
  placeRawElement = lift . placeRawElement
  wrapRawElement e cfg = lift $ wrapRawElement e $ cfg
    { _rawElementConfig_eventSpec = _rawElementConfig_eventSpec cfg
    }

instance (DomBuilder t m, MonadHold t m, MonadFix m) => DomBuilder t (RequesterT t request response m) where
  type DomBuilderSpace (RequesterT t request response m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (RequesterT child) = RequesterT $ do
    r <- ask
    let cfg' = liftElementConfig cfg
    (el, (a, e)) <- lift $ lift $ element elementTag cfg' $ runReaderT (runEventWriterT child) r
    tellEvent e
    return (el, a)
  inputElement cfg = lift $ inputElement $ cfg & inputElementConfig_elementConfig %~ liftElementConfig
  textAreaElement cfg = lift $ textAreaElement $ cfg & textAreaElementConfig_elementConfig %~ liftElementConfig
  selectElement cfg (RequesterT child) = RequesterT $ do
    r <- ask
    let cfg' = cfg & selectElementConfig_elementConfig %~ liftElementConfig
    (el, (a, e)) <- lift $ lift $ selectElement cfg' $ runReaderT (runEventWriterT child) r
    tellEvent e
    return (el, a)
  placeRawElement = lift . placeRawElement
  wrapRawElement e cfg = lift $ wrapRawElement e $ cfg
    { _rawElementConfig_eventSpec = _rawElementConfig_eventSpec cfg
    }

instance (DomBuilder t m, MonadHold t m, MonadFix m, Semigroup w, Monoid w) => DomBuilder t (EventWriterT t w m) where
  type DomBuilderSpace (EventWriterT t w m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg child = do
    let cfg' = liftElementConfig cfg
    (el, (a, e)) <- lift $ element elementTag cfg' $ runEventWriterT child
    tellEvent e
    return (el, a)
  inputElement cfg = lift $ inputElement $ cfg & inputElementConfig_elementConfig %~ liftElementConfig
  textAreaElement cfg = lift $ textAreaElement $ cfg & textAreaElementConfig_elementConfig %~ liftElementConfig
  selectElement cfg child = do
    let cfg' = cfg & selectElementConfig_elementConfig %~ liftElementConfig
    (el, (a, e)) <- lift $ selectElement cfg' $ runEventWriterT child
    tellEvent e
    return (el, a)
  placeRawElement = lift . placeRawElement
  wrapRawElement e cfg = lift $ wrapRawElement e $ cfg
    { _rawElementConfig_eventSpec = _rawElementConfig_eventSpec cfg
    }

-- * Convenience functions

--TODO: Move/replace
class Functor1 (f :: (k -> *) -> *) where
  type Functor1Constraint f (a :: k -> *) (b :: k -> *) :: Constraint
  type Functor1Constraint f a b = ()
  fmap1 :: Functor1Constraint f a b => (forall x. a x -> b x) -> f a -> f b

instance Functor1 (ElementConfig er t) where
  type Functor1Constraint (ElementConfig er t) a b = DomBuilderSpace a ~ DomBuilderSpace b
  {-# INLINABLE fmap1 #-}
  fmap1 _ cfg = cfg
    { _elementConfig_eventSpec = _elementConfig_eventSpec cfg
    }

instance Reflex t => Functor1 (PlaceholderConfig above t) where
  {-# INLINABLE fmap1 #-}
  fmap1 f cfg = cfg
    { _placeholderConfig_insertAbove = f <$> _placeholderConfig_insertAbove cfg
    }

instance Functor1 (InputElementConfig er t) where
  type Functor1Constraint (InputElementConfig er t) a b = Functor1Constraint (ElementConfig er t) a b
  fmap1 f cfg = cfg & inputElementConfig_elementConfig %~ fmap1 f

instance Functor1 (TextAreaElementConfig er t) where
  type Functor1Constraint (TextAreaElementConfig er t) a b = Functor1Constraint (ElementConfig er t) a b
  fmap1 f cfg = cfg & textAreaElementConfig_elementConfig %~ fmap1 f

instance Functor1 (SelectElementConfig er t) where
  type Functor1Constraint (SelectElementConfig er t) a b = Functor1Constraint (ElementConfig er t) a b
  fmap1 f cfg = cfg & selectElementConfig_elementConfig %~ fmap1 f

instance Functor1 (RawElementConfig er t) where
  type Functor1Constraint (RawElementConfig er t) a b = DomBuilderSpace a ~ DomBuilderSpace b
  {-# INLINABLE fmap1 #-}
  fmap1 _ cfg = cfg
    { _rawElementConfig_eventSpec = _rawElementConfig_eventSpec cfg
    }

class HasDomEvent t target eventName where
  type DomEventType target eventName :: *
  domEvent :: EventName eventName -> target -> Event t (DomEventType target eventName)

instance Reflex t => HasDomEvent t (Element EventResult d t) en where
  type DomEventType (Element EventResult d t) en = EventResultType en
  {-# INLINABLE domEvent #-}
  domEvent en e = unEventResult <$> Reflex.select (_element_events e) (WrapArg en)

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
  default stTCoercion :: proxy t -> Coercion a a
  stTCoercion _ = Control.Category.id

toStT :: MonadTransControlStateless t => proxy t -> a -> StT t a
toStT = coerceWith . sym . stTCoercion

fromStT :: MonadTransControlStateless t => proxy t -> StT t a -> a
fromStT = coerceWith . stTCoercion

instance MonadTransControlStateless (ReaderT r)

type RunStateless t = forall n b. Monad n => t n b -> n b

liftWithStateless :: forall m t a. (Monad m, MonadTransControlStateless t) => (RunStateless t -> m a) -> t m a
liftWithStateless a = liftWith $ \run -> a $ \x -> fromStT (Proxy :: Proxy t) <$> run x

liftTextNode :: (MonadTrans f, DomBuilder t m) => TextNodeConfig t -> f m (TextNode (DomBuilderSpace m) t)
liftTextNode = lift . textNode

liftElement :: LiftDomBuilder t f m => Text -> ElementConfig er t (f m) -> f m a -> f m (Element er (DomBuilderSpace m) t, a)
liftElement elementTag cfg child = liftWithStateless $ \run -> element elementTag (fmap1 run cfg) $ run child

{-
liftPlaceholder :: LiftDomBuilder t f m => PlaceholderConfig above t (f m) -> f m (Placeholder above t)
liftPlaceholder cfg = liftWithStateless $ \run -> placeholder $ fmap1 run cfg
-}

liftInputElement :: LiftDomBuilder t f m => InputElementConfig er t (f m) -> f m (InputElement er (DomBuilderSpace m) t)
liftInputElement cfg = liftWithStateless $ \run -> inputElement $ fmap1 run cfg

liftTextAreaElement :: LiftDomBuilder t f m => TextAreaElementConfig er t (f m) -> f m (TextAreaElement er (DomBuilderSpace m) t)
liftTextAreaElement cfg = liftWithStateless $ \run -> textAreaElement $ fmap1 run cfg

liftWrapRawElement :: LiftDomBuilder t f m => RawElement (DomBuilderSpace m) -> RawElementConfig er t (f m) -> f m (Element er (DomBuilderSpace m) t)
liftWrapRawElement e es = liftWithStateless $ \run -> wrapRawElement e $ fmap1 run es
