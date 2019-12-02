# Frequently Asked Questions

### How do I configure DOM events with flags like `preventDefault` and `stopPropagation`?

Using `element`, a low-level DOM builder function. Its second argument, `ElementConfig`, has a `_elementConfig_eventSpec` field that can be modified using `addEventSpecFlags`. It takes a `Proxy` that specifies the `DomSpace`, an event name, and a callback which takes the event result and returns an event flag, which can enable behavior like `preventDefault` and `stopPropagation`.

For example:

```haskell
linkPreventDefault :: forall t m a. DomBuilder t m => m a -> m (Event t (), a)
linkPreventDefault c = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_initialAttributes .~ ("href" =: "https://reflex-frp.org")
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const preventDefault)
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)
  (link, a) <- element "a" cfg c
  return (domEvent Click link, a)
```