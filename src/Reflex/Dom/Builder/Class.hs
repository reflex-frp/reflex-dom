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
       , module Reflex.Deletable.Class
       ) where

import Reflex.Class as Reflex
import Reflex.Dom.Builder.Class.Events
import Reflex.Deletable.Class
import Reflex.DynamicWriter
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class

import qualified Control.Category
import Control.Lens hiding (element)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Data.Default
import Data.Foldable
import Data.Functor.Misc
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Semigroup
import Data.Text (Text)
import Data.Traversable
import Data.Type.Coercion
import GHC.Exts (Constraint)

class Default (EventSpec d EventResult) => DomSpace d where
  type EventSpec d :: (EventTag -> *) -> *
  type RawTextNode d :: *
  type RawElement d :: *
  type RawInputElement d :: *
  type RawTextAreaElement d :: *
  addEventSpecFlags :: proxy d -> EventName en -> (Maybe (er en) -> EventFlags) -> EventSpec d er -> EventSpec d er

-- | @'DomBuildek' t m@ indicates that @m@ is a 'Monad' capable of building dynamic DOM in the 'Reflex' timeline @t@
class (Monad m, Reflex t, Deletable t m, DomSpace (DomBuilderSpace m)) => DomBuilder t m | m -> t where
  type DomBuilderSpace m :: *
  textNode :: TextNodeConfig t -> m (TextNode (DomBuilderSpace m) t)
  element :: Text -> ElementConfig er t m -> m a -> m (Element er (DomBuilderSpace m) t, a)
  -- | Create a placeholder in the DOM, with the ability to insert new DOM before it
  -- The provided DOM will be executed after the current frame, so it will not be affected by any occurrences that are concurrent with the occurrence that created it
  placeholder :: PlaceholderConfig above t m -> m (Placeholder above t)
  inputElement :: InputElementConfig er t m -> m (InputElement er (DomBuilderSpace m) t)
  textAreaElement :: TextAreaElementConfig er t m -> m (TextAreaElement er (DomBuilderSpace m) t)
  placeRawElement :: RawElement (DomBuilderSpace m) -> m ()
  wrapRawElement :: RawElement (DomBuilderSpace m) -> RawElementConfig er t m -> m (Element er (DomBuilderSpace m) t)

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

makeLenses ''TextNodeConfig
makeLenses ''ElementConfig
makeLenses ''PlaceholderConfig
makeLenses ''InputElementConfig
makeLenses ''TextAreaElementConfig
makeLenses ''RawElementConfig

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

{-# INLINABLE liftPostBuildTElementConfig #-}
liftPostBuildTElementConfig :: ElementConfig er t (PostBuildT t m) -> ElementConfig er t m
liftPostBuildTElementConfig cfg = cfg
  { _elementConfig_eventSpec = _elementConfig_eventSpec cfg
  }

{-# INLINABLE liftDynamicWriterTElementConfig #-}
liftDynamicWriterTElementConfig :: ElementConfig er t (DynamicWriterT t w m) -> ElementConfig er t m
liftDynamicWriterTElementConfig cfg = cfg
  { _elementConfig_eventSpec = _elementConfig_eventSpec cfg
  }

instance (DomBuilder t m, PerformEvent t m, MonadFix m, MonadHold t m) => DomBuilder t (PostBuildT t m) where
  type DomBuilderSpace (PostBuildT t m) = DomBuilderSpace m
  {-# INLINABLE textNode #-}
  textNode = lift . textNode
  {-# INLINABLE element #-}
  element t cfg child = liftWith $ \run -> element t (liftPostBuildTElementConfig cfg) $ run child
  {-# INLINABLE placeholder #-}
  placeholder cfg = lift $ do
    rec childPostBuild <- deletable (_placeholder_deletedSelf p) $ performEvent $ return () <$ _placeholder_insertedAbove p
        p <- placeholder $ cfg
          { _placeholderConfig_insertAbove = ffor (_placeholderConfig_insertAbove cfg) $ \a -> runPostBuildT a =<< headE childPostBuild
          }
    return p
  {-# INLINABLE inputElement #-}
  inputElement cfg = lift $ inputElement $ cfg & inputElementConfig_elementConfig %~ liftPostBuildTElementConfig
  {-# INLINABLE textAreaElement #-}
  textAreaElement cfg = lift $ textAreaElement $ cfg & textAreaElementConfig_elementConfig %~ liftPostBuildTElementConfig
  placeRawElement = lift . placeRawElement
  wrapRawElement e cfg = liftWith $ \run -> wrapRawElement e $ fmap1 run cfg

instance (DomBuilder t m, Monoid w, MonadHold t m, MonadFix m) => DomBuilder t (DynamicWriterT t w m) where
  type DomBuilderSpace (DynamicWriterT t w m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (DynamicWriterT child) = DynamicWriterT $ do
    s <- get
    let cfg' = liftDynamicWriterTElementConfig cfg
    (el, (a, newS)) <- lift $ element elementTag cfg' $ runStateT child s
    put newS
    return (el, a)
  placeholder cfg = do
    let cfg' = cfg
          { _placeholderConfig_insertAbove = runDynamicWriterTInternal <$> _placeholderConfig_insertAbove cfg
          }
    let manageChildren :: Event t (NonEmpty (Replaceable t (Dynamic t w))) -- ^ Add nodes on the right; these are in reverse order
                       -> Event t () -- ^ No more nodes will be added after this event fires
                       -> m (Replaceable t (Dynamic t w))
        manageChildren newChildren additionsCeased = do
          rec nextId <- hold (0 :: Int) newNextId -- We assume this will never wrap around
              let numberedNewChildren :: Event t (Int, PatchMap (Map Int (Replaceable t (Dynamic t w))))
                  numberedNewChildren = flip pushAlways newChildren $ \rcs -> do
                    let cs = reverse $ toList rcs
                    myFirstId <- sample nextId
                    let (myNextId, numbered) = mapAccumL (\n v -> (succ n, (n, Just v))) myFirstId cs
                    return (myNextId, PatchMap $ Map.fromList numbered)
                  newNextId = fst <$> numberedNewChildren
          mconcatIncrementalReplaceableDynMap Map.empty (snd <$> numberedNewChildren) additionsCeased
    rec children <- lift $ manageChildren childOutputs $ cfg ^. deleteSelf
        p <- DynamicWriterT $ do
          modify (children:)
          lift $ placeholder cfg'
        let result = fst <$> _placeholder_insertedAbove p
            childOutputs = fmapMaybe (nonEmpty . snd) $ _placeholder_insertedAbove p
    return $ p
      { _placeholder_insertedAbove = result
      }
  inputElement cfg = lift $ inputElement $ cfg & inputElementConfig_elementConfig %~ liftDynamicWriterTElementConfig
  textAreaElement cfg = lift $ textAreaElement $ cfg & textAreaElementConfig_elementConfig %~ liftDynamicWriterTElementConfig
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
  {-# INLINABLE textNode #-}
  textNode = liftTextNode
  element = liftElement
  placeholder = liftPlaceholder
  inputElement = liftInputElement
  textAreaElement = liftTextAreaElement
  placeRawElement = lift . placeRawElement
  wrapRawElement = liftWrapRawElement

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

liftWrapRawElement :: LiftDomBuilder t f m => RawElement (DomBuilderSpace m) -> RawElementConfig er t (f m) -> f m (Element er (DomBuilderSpace m) t)
liftWrapRawElement e es = liftWithStateless $ \run -> wrapRawElement e $ fmap1 run es
