# Reflex-Dom Quick(ish) Reference

## Typeclasses

Function signatures have been simplified. The 't' type parameter and many typeclass constraints are removed, and annotations are added regarding the monad of the result type (See the Reflex Quickstart for more discussion).  Annotations:

```haskell
[ ]   -- Pure function
[W]   -- Function runs in any monad supporting MonadWidget
[I]   -- Function runs in IO
```

Functions from Reflex that run in MonadSample or MonadHold context ([S] or [H]) will also run in [W].

## Creating widgets (DOM elements hooked into the FRP system)

### Basic widgets

These functions generally take an element tag (such as "div", "h1", etc) and a child widget, and produce a widget of the given type containing the child.  Some of these functions return data of type `El`, which is a handle to the created DOM element along with a set of potential Events that might occur on that element.

Widgets may return any type (this is 'a' in many of the functions below).  Often this will be an Event carrying user interactions with the widget.

```haskell
-- Simplest form.  Create a widget of given type containing the given child.
-- Return whatever the child returns.
[W]   el         :: Text ->                                  m a -> m a

-- This version returns the 'El' as well.
[W]   el'        :: Text ->                                  m a -> m (El, a)

-- These two additionally apply attributes to the element, such as ("class" =: "blah"). (=:) is 'Map.singleton'
[W]   elAttr     :: Text ->            Map Text Text ->      m a -> m a
[W]   elAttr'    :: Text ->            Map Text Text ->      m a -> m (El, a)

-- As above, but now the attribute map is Dynamic
[W]   elDynAttr  :: Text ->   Dynamic (Map Text Text) ->     m a -> m a
[W]   elDynAttr' :: Text ->   Dynamic (Map Text Text) ->     m a -> m (El, a)

-- As above, but with an optional XML namespace for the tag.  Note that this does *not* set the 'xmlns' attribute.  See https://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-DocCrElNS
[W]   elDynAttrNS' :: Maybe Text -> Text -> Dynamic (Map Text Text) -> m a -> m (El, a)

-- Shortcut for elAttr when you only want to set the "class" attribute.
[W]   elClass    :: Text ->                       Text ->    m a -> m a

-- Even shorter-cut for above when element type is "div".  Create a div of given class.
[W]   divClass   ::                               Text ->    m a -> m a

-- Create a widget of given type with arbitrary, Dynamic HTML inside.
[W]   elDynHtml'     :: Text ->                        Dynamic Text ->   m El
[W]   elDynHtmlAttr' :: Text ->   Map Text Text ->     Dynamic Text ->   m El

-- Create a static text element
[W]   text    ::              Text ->   m ()

-- Create a dynamic text element
[W]   dynText ::      Dynamic Text ->   m ()
[W]   display :: Show a => Dynamic a -> m ()

-- Create a "button" element with given label, return onClick Event
[W]   button :: Text -> m (Event ())

-- Empty widget
[W]   blank :: m ()
```

### Dynamic widgets

In the Dynamic cases so far, the *content* of a widget is dynamic but the *definition* of the widget is static.  The functions below enable the definition and/or structure of the widget itself to change over time.

Note the "list" functions do not imply particular HTML tags (ul, li, etc), though the widgets they create can have those tags if you construct them appropriately.

```haskell
-- Create a dynamically-redefined widget from a Dynamic of widget actions.
[W]   dyn        ::        Dynamic (m a) -> m (Event a)

-- Same as dyn, but takes initial value and an update Event instead of a Dynamic.
[W]   widgetHold :: m a ->   Event (m a) -> m (Dynamic a)
```

Also see the "Collection management functions" section in the `reflex` Quick Reference.

### Utility widgets

These are useful widgets that are implemented (or could be implemented) in terms of the low-level widgets above.

Some of these widget builders take a configuration record and return a record containing Events or other useful data associated with the created widget (similar to 'El').  The configuration records have default values, so you can just supply 'def'.  See Reflex/Dom/Widget/Input.hs for record fields (Lenses are provided).

```haskell
-- Text input.
[W]   textInput :: TextInputConfig -> m TextInput
[ ]   textInputGetEnter :: TextInput -> Event ()
[W]   textArea :: TextAreaConfig -> m TextArea

-- Range input (slider with float values).
[W]   rangeInput :: RangeInputConfig -> m RangeInput

-- Checkbox.  The Bool supplies the initial state.
[W]   checkbox :: Bool -> CheckboxConfig -> m Checkbox

-- Dropdown with Dynamic options.  First argument is initial state.
[W]   dropdown :: (Ord k, Show k, Read k) =>
          k -> Dynamic (Map k Text) -> DropdownConfig k -> m (Dropdown k)

-- Table with static columns and dynamic rows.
[W]   tableDynAttr :: ...        -- See Reflex.Dom.Widget.Basic

-- Tabbed view that shows only one of its child widgets at a time.
[W]   tabDisplay :: (Show k, Ord k) => Text -> Text -> Map k (Text, m ()) -> m ()

-- Widget to efficiently display long scrolling lists.
[W]   virtualListWithSelection :: ...        -- See Reflex.Dom.Widget.Lazy
```

## Connecting to the real world (I/O)

```haskell
-- Extract the specified Event from an 'El'.  See Reflex.Dom.Widget.Basic
[ ]   domEvent :: EventName en -> El -> Event (EventResultType en)
```

Also see the "Connection to the real world" and "Time" sections in the `reflex` Quick Reference.

### XMLHttpRequest

Convenience functions for XMLHttpRequest.  see Reflex.Dom.Xhr

```haskell
-- Given method, URL, and config record (with default instance), construct a request.
[ ]   xhrRequest :: Text -> Text -> XhrRequestConfig a -> XhrRequest a

-- Given Event of requests, issue them and produce Event of responses.
[W]   performRequestAsync :: Event (XhrRequest a) -> m (Event XhrResponse)

-- Issue a collection of requests, wait for them ALL to complete, return collected results.
[W]   performRequestsAsync :: Traversable f => Event (f (XhrRequest a)) -> m (Event (f XhrResponse))

-- Convenience function to decode JSON-encoded responses.
[ ]   decodeXhrResponse :: FromJSON a => XhrResponse -> Maybe a

-- Simplified interface to "GET" URLs and return decoded results.
[W]   getAndDecode :: FromJSON a => Event Text -> m (Event (Maybe a))
```

## Startup

```haskell
-- Reflex-Dom entry point.  Takes a monadic widget-building action of lengthy
-- type and turns it into an IO action.
[I]   mainWidget ::
          Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()
[I]   mainWidgetWithHead ::
          Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () ->
          Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()
[I]   mainWidgetWithCss ::
          ByteString ->
          Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()
```

Also see the corresponding section in the `reflex` Quick Reference.
