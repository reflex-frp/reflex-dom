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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Builder.Class
       ( module Reflex.Dom.Builder.Class
       , module Reflex.Dom.Builder.Class.Events
       , module Reflex.Dom.Deletable.Class
       ) where

import Reflex
import Reflex.Dom.Builder.Class.Events
import Reflex.Dom.Deletable.Class

import qualified Control.Category
import Control.Lens hiding (element)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Default
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Functor.Misc
import Data.Map (Map)
import Data.Proxy
import Data.Semigroup
import Data.Text (Text)
import Data.Type.Coercion
import GHC.Exts (Constraint)

data Pair1 (f :: k -> *) (g :: k -> *) (a :: k) = Pair1 (f a) (g a)

data Maybe1 f a = Nothing1 | Just1 (f a)

class DomSpace d where
  type DomHandler d :: * -> * -> *
  type DomHandler1 d :: (EventTag -> *) -> (EventTag -> *) -> * --TODO: Why can't this be k -> * ?
  type RawEvent d :: EventTag -> *
  type RawTextNode d :: *
  type RawElement d :: *
  defaultEventHandler :: proxy d -> DomHandler1 d (Pair1 EventName (RawEvent d)) (Maybe1 EventResult)

-- | @'DomBuilder' t m@ indicates that @m@ is a 'Monad' capable of building dynamic DOM in the 'Reflex' timeline @t@
class (Monad m, Reflex t, Deletable t m, DomSpace (DomBuilderSpace m)) => DomBuilder t m | m -> t where
  type DomBuilderSpace m :: *
  textNode :: TextNodeConfig t -> m (TextNode (DomBuilderSpace m) t)
  element :: Text -> ElementConfig er t m -> m a -> m (Element er (DomBuilderSpace m) t, a)
  -- | Create a placeholder in the DOM, with the ability to insert new DOM before it
  -- The provided DOM will be executed after the current frame, so it will not be affected by any occurrences that are concurrent with the occurrence that created it
  placeholder :: PlaceholderConfig above t m -> m (Placeholder above t)
  inputElement :: InputElementConfig er t m -> m (InputElement er (DomBuilderSpace m) t)
  textAreaElement :: TextAreaElementConfig er t m -> m (TextAreaElement er (DomBuilderSpace m) t)

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

type AttributeName = (Maybe Namespace, Text)

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

newtype EventFilter d er en = EventFilter (DomHandler d (RawEvent d en) (EventFlags, DomHandler d () (Maybe (er en))))

data ElementConfig er t m
   = ElementConfig { _elementConfig_namespace :: Maybe Namespace
                   , _elementConfig_initialAttributes :: Map AttributeName Text
                   , _elementConfig_modifyAttributes :: Event t (Map AttributeName (Maybe Text))
                   , _elementConfig_eventFilters :: DMap EventName (EventFilter (DomBuilderSpace m) er)
                   , _elementConfig_eventHandler :: DomHandler1 (DomBuilderSpace m) (Pair1 EventName (RawEvent (DomBuilderSpace m))) (Maybe1 er)
                   }

instance (Reflex t, er ~ EventResult, DomBuilder t m) => Default (ElementConfig er t m) where
  {-# INLINABLE def #-}
  def = ElementConfig
    { _elementConfig_namespace = Nothing
    , _elementConfig_initialAttributes = mempty
    , _elementConfig_modifyAttributes = never
    , _elementConfig_eventFilters = DMap.empty
    , _elementConfig_eventHandler = defaultEventHandler (Proxy :: Proxy (DomBuilderSpace m))
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
                  , _inputElement_input :: Event t Text
                  , _inputElement_hasFocus :: Dynamic t Bool
                  , _inputElement_element :: Element er d t
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
                     }

makeLenses ''TextNodeConfig
makeLenses ''ElementConfig
makeLenses ''PlaceholderConfig
makeLenses ''InputElementConfig
makeLenses ''TextAreaElementConfig

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

class InitialAttributes a => ModifyAttributes t a | a -> t where
  modifyAttributes :: Lens' a (Event t (Map AttributeName (Maybe Text)))

instance ModifyAttributes t (ElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = elementConfig_modifyAttributes

class HasNamespace a where
  namespace :: Lens' a (Maybe Namespace)

instance HasNamespace (ElementConfig er t m) where
  {-# INLINABLE namespace #-}
  namespace = elementConfig_namespace

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
    { _elementConfig_eventFilters = _elementConfig_eventFilters cfg
    , _elementConfig_eventHandler = _elementConfig_eventHandler cfg
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
  {-# INLINABLE textNode #-}
  textNode = liftTextNode
  element = liftElement
  placeholder = liftPlaceholder
  inputElement = liftInputElement
  textAreaElement = liftTextAreaElement

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

liftPlaceholder :: LiftDomBuilder t f m => PlaceholderConfig above t (f m) -> f m (Placeholder above t)
liftPlaceholder cfg = liftWithStateless $ \run -> placeholder $ fmap1 run cfg

liftInputElement :: LiftDomBuilder t f m => InputElementConfig er t (f m) -> f m (InputElement er (DomBuilderSpace m) t)
liftInputElement cfg = liftWithStateless $ \run -> inputElement $ fmap1 run cfg

liftTextAreaElement :: LiftDomBuilder t f m => TextAreaElementConfig er t (f m) -> f m (TextAreaElement er (DomBuilderSpace m) t)
liftTextAreaElement cfg = liftWithStateless $ \run -> textAreaElement $ fmap1 run cfg
