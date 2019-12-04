# Frequently Asked Questions

### How do I configure DOM events with flags like `preventDefault` and `stopPropagation`?

Using `element`, a low-level DOM builder function. Its second argument, `ElementConfig`, has a `_elementConfig_eventSpec` field that can be modified using `addEventSpecFlags`. It takes a `Proxy` that specifies the `DomSpace`, an event name, and a callback which takes the event result and returns an event flag, which can enable behavior like `preventDefault` and `stopPropagation`.

The type signature on the `def` value is needed because `EventSpec` is a non-injective type family, meaning that the input type can't be inferred from the result. In this case, the unknown input is the `DomBuilderSpace m`, which is why it needs to be specified manually.

For example:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens ((&), (%~), (.~))
import Data.Proxy
import Reflex.Dom

main :: IO ()
main = mainWidget $ do
  (click, ()) <- linkPreventDefault $ text "reflex-frp"
  display =<< count click

linkPreventDefault :: forall t m a. DomBuilder t m => m a -> m (Event t (), a)
linkPreventDefault c = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_initialAttributes .~ ("href" =: "https://reflex-frp.org")
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const preventDefault)
  (link, a) <- element "a" cfg c
  return (domEvent Click link, a)
```