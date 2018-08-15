{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Reflex.Dom.Contrib.CssClass where

------------------------------------------------------------------------------
import           Data.Default
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Monoid
import           Data.Set     (Set)
import qualified Data.Set     as S
import           Data.String
import           Data.Text    (Text)
import qualified Data.Text    as T
import           Reflex.Dom.Core
------------------------------------------------------------------------------

class CssClassRep a where
    cssClass :: a -> CssClass

instance CssClassRep Text where
    cssClass = singleClass

------------------------------------------------------------------------------
-- | Passthrough instance for Either
instance (CssClassRep a, CssClassRep b) => CssClassRep (Either a b) where
  cssClass (Left a)  = cssClass a
  cssClass (Right b) = cssClass b

------------------------------------------------------------------------------
-- | Passthrough instance for Maybe
instance CssClassRep a => CssClassRep (Maybe a) where
  cssClass Nothing  = mempty
  cssClass (Just a) = cssClass a

-- | This representaion of the CSS classes is especially useful when you want
-- to allow the user to pass in arbitrary clases to a widget, but the widget
-- still needs to add its own classes.  Problems can arise if there are
-- overlaps.
newtype CssClass = CssClass { unCssClass :: Set Text }
  deriving (Eq, Ord, Monoid, Show)

instance Default CssClass where
    def = mempty

renderClass :: CssClass -> Text
renderClass (CssClass s) = T.intercalate " " $ S.toList s

singleClass :: Text -> CssClass
singleClass c = CssClass $ S.singleton c

manyClasses :: [Text] -> CssClass
manyClasses cs = CssClass $ S.fromList cs

addClass :: CssClassRep a => a -> CssClass -> CssClass
addClass a c = cssClass a <> c

removeClass :: CssClassRep a => a -> CssClass -> CssClass
removeClass c (CssClass s) = CssClass $ S.difference s (unCssClass $ cssClass c)

classAttr :: CssClass -> Map Text Text
classAttr c = if S.null (unCssClass c)
                then mempty
                else M.singleton "class" (renderClass c)

elKlass :: MonadWidget t m => Text -> CssClass -> m a -> m a
elKlass e k = elAttr e (classAttr k)

elKlass'
    :: MonadWidget t m
    => Text
    -> CssClass
    -> m a
    -> m (Element EventResult GhcjsDomSpace t, a)
elKlass' e k = elAttr' e (classAttr k)

elDynKlass :: MonadWidget t m => Text -> Dynamic t CssClass -> m a -> m a
elDynKlass e k = elDynAttr e (classAttr <$> k)

elDynKlass'
    :: MonadWidget t m
    => Text
    -> Dynamic t CssClass
    -> m a
    -> m (El t, a)
elDynKlass' e k = elDynAttr' e (classAttr <$> k)

------------------------------------------------------------------------------
-- | The correct way to add a CssClass to an existing Map of attributes,
-- taking into account the possibility that the map already has some classes.
addToClassAttr
    :: CssClass
    -> Map Text Text
    -> Map Text Text
addToClassAttr cls = M.alter (Just . renderClass . f) "class"
  where
    f Nothing  = cls
    f (Just c) = cls <> manyClasses (T.words c)

instance IsString CssClass where
    fromString = CssClass . S.fromList . T.words . T.pack
