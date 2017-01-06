{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Dom.Builder.Class.Events where

import Data.GADT.Compare.TH

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
  EventResultType 'TouchstartTag = ()
  EventResultType 'TouchmoveTag = [(Int, Int)]
  EventResultType 'TouchendTag = ()
  EventResultType 'TouchcancelTag = ()
  EventResultType 'WheelTag = ()

deriveGEq ''EventName
deriveGCompare ''EventName
