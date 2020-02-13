# Revision history for reflex-dom-core

## Unreleased

* Support GHC 8.8.

* Fix attribute support for explicitly namespaced elements

* Remove deprecation warnings for the following widgets in
  `Reflex.Dom.Widget.Basic`:

   * `Link`
   * `button`
   * `dtdd`
   * `linkClass`
   * `link`
   * `tabDisplay`
   * `tableDynAttr`

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
