## [Reflex-DOM](https://reflex-frp.org)

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/reflex-dom.svg)](https://hackage.haskell.org/package/reflex-dom) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/reflex-dom/badge)](https://matrix.hackage.haskell.org/#/package/reflex-dom) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/reflex-frp/reflex-dom/blob/master/LICENSE)

Web applications without callbacks or side-effects. Reflex-DOM brings the power of [Functional Reactive Programming (FRP)](https://wiki.haskell.org/Functional_Reactive_Programming) to the web. Build [HTML](https://developer.mozilla.org/en-US/docs/Web/HTML) and other [Document Object Model (DOM)](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model) data with a pure functional interface.

Reflex-DOM is a framework for building web applications in [Haskell](https://www.haskell.org/), based on the [Functional Reactive Programming](https://wiki.haskell.org/Functional_Reactive_Programming) library [Reflex](https://github.com/reflex-frp/reflex).

**Visit https://reflex-frp.org/ for more information, tutorials, documentation and [examples](https://examples.reflex-frp.org/).**

### Additional resources

* [Official Website](https://reflex-frp.org/)
* [Quick Reference](Quickref.md)
* [Obelisk](https://github.com/obsidiansystems/obelisk#obelisk): A framework built on Reflex and Reflex-DOM for functional reactive web and mobile applications, with batteries included.
* [Get started with Reflex](https://github.com/reflex-frp/reflex-platform)
* [/r/reflexfrp](https://www.reddit.com/r/reflexfrp)
* [irc.freenode.net #reflex-frp](http://webchat.freenode.net?channels=%23reflex-frp&uio=d4)

### Hacking

Use the `./scripts/hack-on haskell-overlays/reflex-packages/dep/reflex-dom` script in [Reflex Platform](https://github.com/reflex-frp/reflex-platform) to checkout the source code of `reflex-dom` locally in `./dep/reflex-dom` directory.
Then do modifications to the source in place, and use the `./try-reflex` or `./scripts/work-on` scripts to create the shell to test your changes.
