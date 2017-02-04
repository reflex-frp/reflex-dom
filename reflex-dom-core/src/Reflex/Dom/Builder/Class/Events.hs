{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE TypeFamilies #-}
module Reflex.Dom.Builder.Class.Events where

#ifdef USE_TEMPLATE_HASKELL
import Data.GADT.Compare.TH
#else
import Data.GADT.Compare
       (GOrdering(..), (:~:)(..), GEq(..), GCompare(..))
import Control.Monad (ap)
#endif

data EventTag
   = AbortTag
   | BlurTag
   | ChangeTag
   | ClickTag
   | ContextmenuTag
   | DblclickTag
   | DragTag
   | DragendTag
   | DragenterTag
   | DragleaveTag
   | DragoverTag
   | DragstartTag
   | DropTag
   | ErrorTag
   | FocusTag
   | InputTag
   | InvalidTag
   | KeydownTag
   | KeypressTag
   | KeyupTag
   | LoadTag
   | MousedownTag
   | MouseenterTag
   | MouseleaveTag
   | MousemoveTag
   | MouseoutTag
   | MouseoverTag
   | MouseupTag
   | MousewheelTag
   | ScrollTag
   | SelectTag
   | SubmitTag
   | WheelTag
   | BeforecutTag
   | CutTag
   | BeforecopyTag
   | CopyTag
   | BeforepasteTag
   | PasteTag
   | ResetTag
   | SearchTag
   | SelectstartTag
   | TouchstartTag
   | TouchmoveTag
   | TouchendTag
   | TouchcancelTag

data EventName :: EventTag -> * where
  Abort :: EventName 'AbortTag
  Blur :: EventName 'BlurTag
  Change :: EventName 'ChangeTag
  Click :: EventName 'ClickTag
  Contextmenu :: EventName 'ContextmenuTag
  Dblclick :: EventName 'DblclickTag
  Drag :: EventName 'DragTag
  Dragend :: EventName 'DragendTag
  Dragenter :: EventName 'DragenterTag
  Dragleave :: EventName 'DragleaveTag
  Dragover :: EventName 'DragoverTag
  Dragstart :: EventName 'DragstartTag
  Drop :: EventName 'DropTag
  Error :: EventName 'ErrorTag
  Focus :: EventName 'FocusTag
  Input :: EventName 'InputTag
  Invalid :: EventName 'InvalidTag
  Keydown :: EventName 'KeydownTag
  Keypress :: EventName 'KeypressTag
  Keyup :: EventName 'KeyupTag
  Load :: EventName 'LoadTag
  Mousedown :: EventName 'MousedownTag
  Mouseenter :: EventName 'MouseenterTag
  Mouseleave :: EventName 'MouseleaveTag
  Mousemove :: EventName 'MousemoveTag
  Mouseout :: EventName 'MouseoutTag
  Mouseover :: EventName 'MouseoverTag
  Mouseup :: EventName 'MouseupTag
  Mousewheel :: EventName 'MousewheelTag
  Scroll :: EventName 'ScrollTag
  Select :: EventName 'SelectTag
  Submit :: EventName 'SubmitTag
  Wheel :: EventName 'WheelTag
  Beforecut :: EventName 'BeforecutTag
  Cut :: EventName 'CutTag
  Beforecopy :: EventName 'BeforecopyTag
  Copy :: EventName 'CopyTag
  Beforepaste :: EventName 'BeforepasteTag
  Paste :: EventName 'PasteTag
  Reset :: EventName 'ResetTag
  Search :: EventName 'SearchTag
  Selectstart :: EventName 'SelectstartTag
  Touchstart :: EventName 'TouchstartTag
  Touchmove :: EventName 'TouchmoveTag
  Touchend :: EventName 'TouchendTag
  Touchcancel :: EventName 'TouchcancelTag

newtype EventResult en = EventResult { unEventResult :: EventResultType en }

type family EventResultType (en :: EventTag) :: * where
  EventResultType 'ClickTag = ()
  EventResultType 'DblclickTag = (Int, Int)
  EventResultType 'KeypressTag = Int
  EventResultType 'KeydownTag = Int
  EventResultType 'KeyupTag = Int
  EventResultType 'ScrollTag = Int
  EventResultType 'MousemoveTag = (Int, Int)
  EventResultType 'MousedownTag = (Int, Int)
  EventResultType 'MouseupTag = (Int, Int)
  EventResultType 'MouseenterTag = ()
  EventResultType 'MouseleaveTag = ()
  EventResultType 'FocusTag = ()
  EventResultType 'BlurTag = ()
  EventResultType 'ChangeTag = ()
  EventResultType 'DragTag = ()
  EventResultType 'DragendTag = ()
  EventResultType 'DragenterTag = ()
  EventResultType 'DragleaveTag = ()
  EventResultType 'DragoverTag = ()
  EventResultType 'DragstartTag = ()
  EventResultType 'DropTag = ()
  EventResultType 'AbortTag = ()
  EventResultType 'ContextmenuTag = ()
  EventResultType 'ErrorTag = ()
  EventResultType 'InputTag = ()
  EventResultType 'InvalidTag = ()
  EventResultType 'LoadTag = ()
  EventResultType 'MouseoutTag = ()
  EventResultType 'MouseoverTag = ()
  EventResultType 'MousewheelTag = ()
  EventResultType 'SelectTag = ()
  EventResultType 'SubmitTag = ()
  EventResultType 'BeforecutTag = ()
  EventResultType 'CutTag = ()
  EventResultType 'BeforecopyTag = ()
  EventResultType 'CopyTag = ()
  EventResultType 'BeforepasteTag = ()
  EventResultType 'PasteTag = ()
  EventResultType 'ResetTag = ()
  EventResultType 'SearchTag = ()
  EventResultType 'SelectstartTag = ()
  EventResultType 'TouchstartTag = TouchEventResult
  EventResultType 'TouchmoveTag = TouchEventResult
  EventResultType 'TouchendTag = TouchEventResult
  EventResultType 'TouchcancelTag = TouchEventResult
  EventResultType 'WheelTag = ()

data TouchEventResult = TouchEventResult
  { _touchEventResult_altKey :: Bool
  , _touchEventResult_changedTouches :: [TouchResult]
  , _touchEventResult_ctrlKey :: Bool
  , _touchEventResult_metaKey :: Bool
  , _touchEventResult_shiftKey :: Bool
  , _touchEventResult_targetTouches :: [TouchResult]
  , _touchEventResult_touches :: [TouchResult]
  }
  deriving (Show, Read, Eq, Ord)

data TouchResult = TouchResult
  { _touchResult_identifier :: Word
  , _touchResult_screenX :: Int
  , _touchResult_screenY :: Int
  , _touchResult_clientX :: Int
  , _touchResult_clientY :: Int
  , _touchResult_pageX :: Int
  , _touchResult_pageY :: Int
  }
  deriving (Show, Read, Eq, Ord)

#ifdef USE_TEMPLATE_HASKELL
deriveGEq ''EventName
deriveGCompare ''EventName
#else
instance GEq EventName
    where geq Abort Abort             = return Refl
          geq Blur Blur               = return Refl
          geq Change Change           = return Refl
          geq Click Click             = return Refl
          geq Contextmenu Contextmenu = return Refl
          geq Dblclick Dblclick       = return Refl
          geq Drag Drag               = return Refl
          geq Dragend Dragend         = return Refl
          geq Dragenter Dragenter     = return Refl
          geq Dragleave Dragleave     = return Refl
          geq Dragover Dragover       = return Refl
          geq Dragstart Dragstart     = return Refl
          geq Drop Drop               = return Refl
          geq Error Error             = return Refl
          geq Focus Focus             = return Refl
          geq Input Input             = return Refl
          geq Invalid Invalid         = return Refl
          geq Keydown Keydown         = return Refl
          geq Keypress Keypress       = return Refl
          geq Keyup Keyup             = return Refl
          geq Load Load               = return Refl
          geq Mousedown Mousedown     = return Refl
          geq Mouseenter Mouseenter   = return Refl
          geq Mouseleave Mouseleave   = return Refl
          geq Mousemove Mousemove     = return Refl
          geq Mouseout Mouseout       = return Refl
          geq Mouseover Mouseover     = return Refl
          geq Mouseup Mouseup         = return Refl
          geq Mousewheel Mousewheel   = return Refl
          geq Scroll Scroll           = return Refl
          geq Select Select           = return Refl
          geq Submit Submit           = return Refl
          geq Wheel Wheel             = return Refl
          geq Beforecut Beforecut     = return Refl
          geq Cut Cut                 = return Refl
          geq Beforecopy Beforecopy   = return Refl
          geq Copy Copy               = return Refl
          geq Beforepaste Beforepaste = return Refl
          geq Paste Paste             = return Refl
          geq Reset Reset             = return Refl
          geq Search Search           = return Refl
          geq Selectstart Selectstart = return Refl
          geq Touchstart Touchstart   = return Refl
          geq Touchmove Touchmove     = return Refl
          geq Touchend Touchend       = return Refl
          geq Touchcancel Touchcancel = return Refl
          geq _ _ = Nothing

newtype GComparing a b t = GComparing (Either (GOrdering a b) t)

instance Functor (GComparing a b) where fmap f (GComparing x) = GComparing (either Left (Right . f) x)
instance Monad (GComparing a b) where
    return = GComparing . Right
    GComparing (Left  x) >>= f = GComparing (Left x)
    GComparing (Right x) >>= f = f x
instance Applicative (GComparing a b) where
    pure = return
    (<*>) = ap

runGComparing (GComparing x) = either id id x

instance GCompare EventName
    where gcompare Abort Abort             = runGComparing $ return GEQ
          gcompare Abort _                 = GLT
          gcompare _ Abort                 = GGT
          gcompare Blur Blur               = runGComparing $ return GEQ
          gcompare Blur _                  = GLT
          gcompare _ Blur                  = GGT
          gcompare Change Change           = runGComparing $ return GEQ
          gcompare Change _                = GLT
          gcompare _ Change                = GGT
          gcompare Click Click             = runGComparing $ return GEQ
          gcompare Click _                 = GLT
          gcompare _ Click                 = GGT
          gcompare Contextmenu Contextmenu = runGComparing $ return GEQ
          gcompare Contextmenu _           = GLT
          gcompare _ Contextmenu           = GGT
          gcompare Dblclick Dblclick       = runGComparing $ return GEQ
          gcompare Dblclick _              = GLT
          gcompare _ Dblclick              = GGT
          gcompare Drag Drag               = runGComparing $ return GEQ
          gcompare Drag _                  = GLT
          gcompare _ Drag                  = GGT
          gcompare Dragend Dragend         = runGComparing $ return GEQ
          gcompare Dragend _               = GLT
          gcompare _ Dragend               = GGT
          gcompare Dragenter Dragenter     = runGComparing $ return GEQ
          gcompare Dragenter _             = GLT
          gcompare _ Dragenter             = GGT
          gcompare Dragleave Dragleave     = runGComparing $ return GEQ
          gcompare Dragleave _             = GLT
          gcompare _ Dragleave             = GGT
          gcompare Dragover Dragover       = runGComparing $ return GEQ
          gcompare Dragover _              = GLT
          gcompare _ Dragover              = GGT
          gcompare Dragstart Dragstart     = runGComparing $ return GEQ
          gcompare Dragstart _             = GLT
          gcompare _ Dragstart             = GGT
          gcompare Drop Drop               = runGComparing $ return GEQ
          gcompare Drop _                  = GLT
          gcompare _ Drop                  = GGT
          gcompare Error Error             = runGComparing $ return GEQ
          gcompare Error _                 = GLT
          gcompare _ Error                 = GGT
          gcompare Focus Focus             = runGComparing $ return GEQ
          gcompare Focus _                 = GLT
          gcompare _ Focus                 = GGT
          gcompare Input Input             = runGComparing $ return GEQ
          gcompare Input _                 = GLT
          gcompare _ Input                 = GGT
          gcompare Invalid Invalid         = runGComparing $ return GEQ
          gcompare Invalid _               = GLT
          gcompare _ Invalid               = GGT
          gcompare Keydown Keydown         = runGComparing $ return GEQ
          gcompare Keydown _               = GLT
          gcompare _ Keydown               = GGT
          gcompare Keypress Keypress       = runGComparing $ return GEQ
          gcompare Keypress _              = GLT
          gcompare _ Keypress              = GGT
          gcompare Keyup Keyup             = runGComparing $ return GEQ
          gcompare Keyup _                 = GLT
          gcompare _ Keyup                 = GGT
          gcompare Load Load               = runGComparing $ return GEQ
          gcompare Load _                  = GLT
          gcompare _ Load                  = GGT
          gcompare Mousedown Mousedown     = runGComparing $ return GEQ
          gcompare Mousedown _             = GLT
          gcompare _ Mousedown             = GGT
          gcompare Mouseenter Mouseenter   = runGComparing $ return GEQ
          gcompare Mouseenter _            = GLT
          gcompare _ Mouseenter            = GGT
          gcompare Mouseleave Mouseleave   = runGComparing $ return GEQ
          gcompare Mouseleave _            = GLT
          gcompare _ Mouseleave            = GGT
          gcompare Mousemove Mousemove     = runGComparing $ return GEQ
          gcompare Mousemove _             = GLT
          gcompare _ Mousemove             = GGT
          gcompare Mouseout Mouseout       = runGComparing $ return GEQ
          gcompare Mouseout _              = GLT
          gcompare _ Mouseout              = GGT
          gcompare Mouseover Mouseover     = runGComparing $ return GEQ
          gcompare Mouseover _             = GLT
          gcompare _ Mouseover             = GGT
          gcompare Mouseup Mouseup         = runGComparing $ return GEQ
          gcompare Mouseup _               = GLT
          gcompare _ Mouseup               = GGT
          gcompare Mousewheel Mousewheel   = runGComparing $ return GEQ
          gcompare Mousewheel _            = GLT
          gcompare _ Mousewheel            = GGT
          gcompare Scroll Scroll           = runGComparing $ return GEQ
          gcompare Scroll _                = GLT
          gcompare _ Scroll                = GGT
          gcompare Select Select           = runGComparing $ return GEQ
          gcompare Select _                = GLT
          gcompare _ Select                = GGT
          gcompare Submit Submit           = runGComparing $ return GEQ
          gcompare Submit _                = GLT
          gcompare _ Submit                = GGT
          gcompare Wheel Wheel             = runGComparing $ return GEQ
          gcompare Wheel _                 = GLT
          gcompare _ Wheel                 = GGT
          gcompare Beforecut Beforecut     = runGComparing $ return GEQ
          gcompare Beforecut _             = GLT
          gcompare _ Beforecut             = GGT
          gcompare Cut Cut                 = runGComparing $ return GEQ
          gcompare Cut _                   = GLT
          gcompare _ Cut                   = GGT
          gcompare Beforecopy Beforecopy   = runGComparing $ return GEQ
          gcompare Beforecopy _            = GLT
          gcompare _ Beforecopy            = GGT
          gcompare Copy Copy               = runGComparing $ return GEQ
          gcompare Copy _                  = GLT
          gcompare _ Copy                  = GGT
          gcompare Beforepaste Beforepaste = runGComparing $ return GEQ
          gcompare Beforepaste _           = GLT
          gcompare _ Beforepaste           = GGT
          gcompare Paste Paste             = runGComparing $ return GEQ
          gcompare Paste _                 = GLT
          gcompare _ Paste                 = GGT
          gcompare Reset Reset             = runGComparing $ return GEQ
          gcompare Reset _                 = GLT
          gcompare _ Reset                 = GGT
          gcompare Search Search           = runGComparing $ return GEQ
          gcompare Search _                = GLT
          gcompare _ Search                = GGT
          gcompare Selectstart Selectstart = runGComparing $ return GEQ
          gcompare Selectstart _           = GLT
          gcompare _ Selectstart           = GGT
          gcompare Touchstart Touchstart   = runGComparing $ return GEQ
          gcompare Touchstart _            = GLT
          gcompare _ Touchstart            = GGT
          gcompare Touchmove Touchmove     = runGComparing $ return GEQ
          gcompare Touchmove _             = GLT
          gcompare _ Touchmove             = GGT
          gcompare Touchend Touchend       = runGComparing $ return GEQ
          gcompare Touchend _              = GLT
          gcompare _ Touchend              = GGT
          gcompare Touchcancel Touchcancel = runGComparing $ return GEQ
          gcompare Touchcancel _           = GLT
          gcompare _ Touchcancel           = GGT
#endif
