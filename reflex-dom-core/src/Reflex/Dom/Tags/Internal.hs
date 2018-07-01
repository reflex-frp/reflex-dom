{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Reflex.Dom.Tags.Internal 
  ( module Reflex.Dom.Tags.Internal
  , module Reflex.Dom.Core
  ) where


import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (fromString)

import Reflex.Dom.Core hiding (tag, (=:))


-- | Class newtype around Text.
--
--   This is needed to guide type inference for `IsString` (OverloadedStrings)
--   extension.
newtype Cls = Cls { unCls :: Text }

-- | Little convenience alias to make type signatures shorter.
type AttrMap = Map Text Text

-- | Redefined more specialized variant of =:
--
--   This is needed to guide the type inference with string literals are used
--   with `OverloadedStrings` extension enabled.
(=:) :: Text -> Text -> AttrMap
(=:) = Map.singleton




-- | Dispatcher class to call the right Reflex.Dom promitive based on the argument's tag.
class DomBuilder t m => ReflexTag arg t m a where
  tag :: Text -> arg -> m a -> m a
  tag name arg child = snd <$> tag' name arg child

  tag' :: Text -> arg  -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)


instance (DomBuilder t m) => ReflexTag () t m a where
  tag' n () = el' n

instance (DomBuilder t m) => ReflexTag Cls t m a where
  tag' n (Cls cls) = elClass' n cls

-- | Useful if you don't have OverloadedStrings activated.
instance (DomBuilder t m) => ReflexTag String t m a where
  tag' n s = elClass' n (fromString s)

instance DomBuilder t m => ReflexTag AttrMap t m a where
  tag' = elAttr'

instance (PostBuild t m, DomBuilder t m) => ReflexTag (Dynamic t Cls) t m a where
  tag' n a = elDynClass' n (unCls <$> a)


instance (PostBuild t m, DomBuilder t m) => ReflexTag (Dynamic t AttrMap) t m a where
  tag' = elDynAttr'
