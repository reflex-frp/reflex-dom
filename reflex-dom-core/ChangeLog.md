# Revision history for reflex-dom-core

## 0.6.0.0

* ([#374](https://github.com/reflex-frp/reflex-dom/pull/374)) Provide text clipboard data as value of Paste event.
* ([#353](https://github.com/reflex-frp/reflex-dom/pull/353)) Support GHC 8.8.
* ([#358](https://github.com/reflex-frp/reflex-dom/pull/358)) Fix attribute support for explicitly namespaced elements.
* ([#363](https://github.com/reflex-frp/reflex-dom/pull/363)) Remove deprecation warnings for the following widgets in
  `Reflex.Dom.Widget.Basic`:

  * `Link`
  * `button`
  * `dtdd`
  * `linkClass`
  * `link`
  * `tabDisplay`
  * `tableDynAttr`
* ([#361](https://github.com/reflex-frp/reflex-dom/pull/361)) Fix bug in hydration causing the JavaScript to crash when dealing with unexpected HTML.
* ([#364](https://github.com/reflex-frp/reflex-dom/pull/364)) Export attributes used for controlling hydration at the element level:
  * "data-ssr" is now available as `Reflex.Dom.Builder.Immediate.hydratableAttribute`.
  * "data-hydration-skip" is now available as `Reflex.Dom.Builder.Immediate.skipHydrationAttribute`.
* ([#348](https://github.com/reflex-frp/reflex-dom/pull/348)) **(Breaking change)** Make XHR response headers case insensitive by changing `_xhrResponse_headers :: Map Text Text` to `_xhrResponse_headers :: Map (CI Text) Text`.
* ([#225](https://github.com/reflex-frp/reflex-dom/pull/225)) **(Breaking change)** Add a functional dependency to `HasDomEvent`.
* ([#366](https://github.com/reflex-frp/reflex-dom/pull/366)) Bump bounds for `reflex` to include 0.7.


## 0.5.3

* Deprecate a number of old inflexible widget helpers in `Reflex.Dom.Widget.Basic`:

   * `Link`
   * `button`
   * `dtdd`
   * `linkClass`
   * `link`
   * `tabDisplay`
   * `tableDynAttr`

  And in `Reflex.Dom.Widget.Input`:

   * `TextInput`
   * `TextAreaConfig`
   * `CheckboxConfig`
   * `FileInput`

 * Add `< 0.7` upper bound for reflex

* *(Breaking change)* The mouse wheel event now is a `WheelEventResult` rather than `()`.
  This provides information about the wheel's motion beyond the fact that it merely moved.

## 0.5.2

* Update to use new dependent-sum/map packages and drop dependency on `*Tag` classes (e.g., `ShowTag`).

* Update version bounds of base, containers, and stm

* Update to use the newly split `these`/`semialign` packages. To use the pre-split `these` package, set the `split-these` flag to false.

* Reintroduce "data-ssr": elements without this attribute are skipped during
  hydration.

* Fix an issue in the hydration tests that prevented the test from finding the chromium executable

* Relax constraints on `dyn` and `widgetHold` to match the ones in `networkView` and `networkHold` respectively

* Fix prerender for RequesterT so that it doesn't accidentally discard a request that is made at the same moment as getPostBuild's Event fires

## 0.5.1

* Added support for marking elements with a "data-skip-hydration" attribute, which will cause hydration to ignore and skip over them.

* Removed "data-ssr" attributes from statically rendered output.

## 0.5

* Add HydrationDomBuilderT to support hydration of statically rendered DOM nodes. See the note at the top of Reflex.Dom.Builder.Immediate.

* As a result of the hydration changes, the Prerender class has changed, the type of `prerender` has changed and it is now a class method.

* Add the Reflex.Dom.Xhr.FormData module to make posting formdata over xhr more convenient.
