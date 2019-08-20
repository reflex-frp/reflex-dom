# Revision history for reflex-dom-core

## Unreleased

* Reintroduce "data-ssr": elements without this attribute are skipped during
  hydration.

## 0.5.2

* Update to use new dependent-sum/map packages and drop dependency on `*Tag` classes (e.g., `ShowTag`).

## 0.5.1

* Added support for marking elements with a "data-skip-hydration" attribute, which will cause hydration to ignore and skip over them.
* Removed "data-ssr" attributes from statically rendered output.:q

## 0.5

* Add HydrationDomBuilderT to support hydration of statically rendered DOM nodes. See the note at the top of Reflex.Dom.Builder.Immediate.
* As a result of the hydration changes, the Prerender class has changed, the type of `prerender` has changed and it is now a class method.
* Add the Reflex.Dom.Xhr.FormData module to make posting formdata over xhr more convenient.
