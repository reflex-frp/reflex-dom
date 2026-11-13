{-# LANGUAGE CPP #-}

module Reflex.Dom.Builder.Class.Events where

#ifdef USE_TEMPLATE_HASKELL
import Data.GADT.Compare.TH
#else
import Data.Type.Equality ((:~:)(..))
import Data.GADT.Compare
       (GOrdering(..), GEq(..), GCompare(..))
#endif
import Data.Kind (Type)
import Data.Text (Text)

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

data EventName :: EventTag -> Type where
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

newtype EventResult en = EventResult {unEventResult :: EventResultType en}

type family EventResultType (en :: EventTag) :: Type where
  EventResultType 'ClickTag = ()
  EventResultType 'DblclickTag = (Int, Int)
  EventResultType 'KeypressTag = Word
  EventResultType 'KeydownTag = Word
  EventResultType 'KeyupTag = Word
  EventResultType 'ScrollTag = Double
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
  EventResultType 'PasteTag = Maybe Text
  EventResultType 'ResetTag = ()
  EventResultType 'SearchTag = ()
  EventResultType 'SelectstartTag = ()
  EventResultType 'TouchstartTag = TouchEventResult
  EventResultType 'TouchmoveTag = TouchEventResult
  EventResultType 'TouchendTag = TouchEventResult
  EventResultType 'TouchcancelTag = TouchEventResult
  EventResultType 'WheelTag = WheelEventResult

data DeltaMode = DeltaPixel | DeltaLine | DeltaPage
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

data WheelEventResult = WheelEventResult
  { _wheelEventResult_deltaX :: Double
  , _wheelEventResult_deltaY :: Double
  , _wheelEventResult_deltaZ :: Double
  , _wheelEventResult_deltaMode :: DeltaMode
  }
  deriving stock (Eq, Ord, Read, Show)

data TouchEventResult = TouchEventResult
  { _touchEventResult_altKey :: Bool
  , _touchEventResult_changedTouches :: [TouchResult]
  , _touchEventResult_ctrlKey :: Bool
  , _touchEventResult_metaKey :: Bool
  , _touchEventResult_shiftKey :: Bool
  , _touchEventResult_targetTouches :: [TouchResult]
  , _touchEventResult_touches :: [TouchResult]
  }
  deriving stock (Eq, Ord, Read, Show)

data TouchResult = TouchResult
  { _touchResult_identifier :: Word
  , _touchResult_screenX :: Int
  , _touchResult_screenY :: Int
  , _touchResult_clientX :: Int
  , _touchResult_clientY :: Int
  , _touchResult_pageX :: Int
  , _touchResult_pageY :: Int
  }
  deriving stock (Eq, Ord, Read, Show)

#ifdef USE_TEMPLATE_HASKELL
deriveGEq ''EventName
deriveGCompare ''EventName
#else
instance GEq EventName
    where geq Abort Abort             = pure Refl
          geq Blur Blur               = pure Refl
          geq Change Change           = pure Refl
          geq Click Click             = pure Refl
          geq Contextmenu Contextmenu = pure Refl
          geq Dblclick Dblclick       = pure Refl
          geq Drag Drag               = pure Refl
          geq Dragend Dragend         = pure Refl
          geq Dragenter Dragenter     = pure Refl
          geq Dragleave Dragleave     = pure Refl
          geq Dragover Dragover       = pure Refl
          geq Dragstart Dragstart     = pure Refl
          geq Drop Drop               = pure Refl
          geq Error Error             = pure Refl
          geq Focus Focus             = pure Refl
          geq Input Input             = pure Refl
          geq Invalid Invalid         = pure Refl
          geq Keydown Keydown         = pure Refl
          geq Keypress Keypress       = pure Refl
          geq Keyup Keyup             = pure Refl
          geq Load Load               = pure Refl
          geq Mousedown Mousedown     = pure Refl
          geq Mouseenter Mouseenter   = pure Refl
          geq Mouseleave Mouseleave   = pure Refl
          geq Mousemove Mousemove     = pure Refl
          geq Mouseout Mouseout       = pure Refl
          geq Mouseover Mouseover     = pure Refl
          geq Mouseup Mouseup         = pure Refl
          geq Mousewheel Mousewheel   = pure Refl
          geq Scroll Scroll           = pure Refl
          geq Select Select           = pure Refl
          geq Submit Submit           = pure Refl
          geq Wheel Wheel             = pure Refl
          geq Beforecut Beforecut     = pure Refl
          geq Cut Cut                 = pure Refl
          geq Beforecopy Beforecopy   = pure Refl
          geq Copy Copy               = pure Refl
          geq Beforepaste Beforepaste = pure Refl
          geq Paste Paste             = pure Refl
          geq Reset Reset             = pure Refl
          geq Search Search           = pure Refl
          geq Selectstart Selectstart = pure Refl
          geq Touchstart Touchstart   = pure Refl
          geq Touchmove Touchmove     = pure Refl
          geq Touchend Touchend       = pure Refl
          geq Touchcancel Touchcancel = pure Refl
          geq _ _ = Nothing

instance GCompare EventName
    where gcompare Abort Abort             = GEQ
          gcompare Abort _                 = GLT
          gcompare _ Abort                 = GGT
          gcompare Blur Blur               = GEQ
          gcompare Blur _                  = GLT
          gcompare _ Blur                  = GGT
          gcompare Change Change           = GEQ
          gcompare Change _                = GLT
          gcompare _ Change                = GGT
          gcompare Click Click             = GEQ
          gcompare Click _                 = GLT
          gcompare _ Click                 = GGT
          gcompare Contextmenu Contextmenu = GEQ
          gcompare Contextmenu _           = GLT
          gcompare _ Contextmenu           = GGT
          gcompare Dblclick Dblclick       = GEQ
          gcompare Dblclick _              = GLT
          gcompare _ Dblclick              = GGT
          gcompare Drag Drag               = GEQ
          gcompare Drag _                  = GLT
          gcompare _ Drag                  = GGT
          gcompare Dragend Dragend         = GEQ
          gcompare Dragend _               = GLT
          gcompare _ Dragend               = GGT
          gcompare Dragenter Dragenter     = GEQ
          gcompare Dragenter _             = GLT
          gcompare _ Dragenter             = GGT
          gcompare Dragleave Dragleave     = GEQ
          gcompare Dragleave _             = GLT
          gcompare _ Dragleave             = GGT
          gcompare Dragover Dragover       = GEQ
          gcompare Dragover _              = GLT
          gcompare _ Dragover              = GGT
          gcompare Dragstart Dragstart     = GEQ
          gcompare Dragstart _             = GLT
          gcompare _ Dragstart             = GGT
          gcompare Drop Drop               = GEQ
          gcompare Drop _                  = GLT
          gcompare _ Drop                  = GGT
          gcompare Error Error             = GEQ
          gcompare Error _                 = GLT
          gcompare _ Error                 = GGT
          gcompare Focus Focus             = GEQ
          gcompare Focus _                 = GLT
          gcompare _ Focus                 = GGT
          gcompare Input Input             = GEQ
          gcompare Input _                 = GLT
          gcompare _ Input                 = GGT
          gcompare Invalid Invalid         = GEQ
          gcompare Invalid _               = GLT
          gcompare _ Invalid               = GGT
          gcompare Keydown Keydown         = GEQ
          gcompare Keydown _               = GLT
          gcompare _ Keydown               = GGT
          gcompare Keypress Keypress       = GEQ
          gcompare Keypress _              = GLT
          gcompare _ Keypress              = GGT
          gcompare Keyup Keyup             = GEQ
          gcompare Keyup _                 = GLT
          gcompare _ Keyup                 = GGT
          gcompare Load Load               = GEQ
          gcompare Load _                  = GLT
          gcompare _ Load                  = GGT
          gcompare Mousedown Mousedown     = GEQ
          gcompare Mousedown _             = GLT
          gcompare _ Mousedown             = GGT
          gcompare Mouseenter Mouseenter   = GEQ
          gcompare Mouseenter _            = GLT
          gcompare _ Mouseenter            = GGT
          gcompare Mouseleave Mouseleave   = GEQ
          gcompare Mouseleave _            = GLT
          gcompare _ Mouseleave            = GGT
          gcompare Mousemove Mousemove     = GEQ
          gcompare Mousemove _             = GLT
          gcompare _ Mousemove             = GGT
          gcompare Mouseout Mouseout       = GEQ
          gcompare Mouseout _              = GLT
          gcompare _ Mouseout              = GGT
          gcompare Mouseover Mouseover     = GEQ
          gcompare Mouseover _             = GLT
          gcompare _ Mouseover             = GGT
          gcompare Mouseup Mouseup         = GEQ
          gcompare Mouseup _               = GLT
          gcompare _ Mouseup               = GGT
          gcompare Mousewheel Mousewheel   = GEQ
          gcompare Mousewheel _            = GLT
          gcompare _ Mousewheel            = GGT
          gcompare Scroll Scroll           = GEQ
          gcompare Scroll _                = GLT
          gcompare _ Scroll                = GGT
          gcompare Select Select           = GEQ
          gcompare Select _                = GLT
          gcompare _ Select                = GGT
          gcompare Submit Submit           = GEQ
          gcompare Submit _                = GLT
          gcompare _ Submit                = GGT
          gcompare Wheel Wheel             = GEQ
          gcompare Wheel _                 = GLT
          gcompare _ Wheel                 = GGT
          gcompare Beforecut Beforecut     = GEQ
          gcompare Beforecut _             = GLT
          gcompare _ Beforecut             = GGT
          gcompare Cut Cut                 = GEQ
          gcompare Cut _                   = GLT
          gcompare _ Cut                   = GGT
          gcompare Beforecopy Beforecopy   = GEQ
          gcompare Beforecopy _            = GLT
          gcompare _ Beforecopy            = GGT
          gcompare Copy Copy               = GEQ
          gcompare Copy _                  = GLT
          gcompare _ Copy                  = GGT
          gcompare Beforepaste Beforepaste = GEQ
          gcompare Beforepaste _           = GLT
          gcompare _ Beforepaste           = GGT
          gcompare Paste Paste             = GEQ
          gcompare Paste _                 = GLT
          gcompare _ Paste                 = GGT
          gcompare Reset Reset             = GEQ
          gcompare Reset _                 = GLT
          gcompare _ Reset                 = GGT
          gcompare Search Search           = GEQ
          gcompare Search _                = GLT
          gcompare _ Search                = GGT
          gcompare Selectstart Selectstart = GEQ
          gcompare Selectstart _           = GLT
          gcompare _ Selectstart           = GGT
          gcompare Touchstart Touchstart   = GEQ
          gcompare Touchstart _            = GLT
          gcompare _ Touchstart            = GGT
          gcompare Touchmove Touchmove     = GEQ
          gcompare Touchmove _             = GLT
          gcompare _ Touchmove             = GGT
          gcompare Touchend Touchend       = GEQ
          gcompare Touchend _              = GLT
          gcompare _ Touchend              = GGT
          gcompare Touchcancel Touchcancel = GEQ
#endif
