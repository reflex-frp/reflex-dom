# Revision history for reflex-dom

## 0.6.3.1

* Don't depend on webkit2gtk on android

## 0.6.3.0

* Use android-activity 0.2
* Fix bug in android back button handler

## 0.6.2.0

* [#452](https://github.com/reflex-frp/reflex-dom/pull/452) Allow file access on newer android, following usage recommended by the [official docs](https://developer.android.com/reference/androidx/webkit/WebViewAssetLoader)
* [#453](https://github.com/reflex-frp/reflex-dom/pull/453) History management for android

## 0.6.1.1-r1

* Loosen reflex-dom-core version bounds to allow reflex-dom-core 0.7.*

## 0.6.1.1

* Compile with -fPIC

## 0.6.1.0-r1

* Loosen reflex-dom-core version bounds

## 0.6.1.0

* Update for new dependent-map and dependent-sum (after the "some" package split)

## 0.6.0.0

* ([#379](https://github.com/reflex-frp/reflex-dom/pull/379)) Re-export all modules from `reflex-dom-core`. The newly re-exported modules are:
    * `Foreign.JavaScript.Utils`
    * `Reflex.Dom.Builder.Hydratable`
    * `Reflex.Dom.Modals.Class`
    * `Reflex.Dom.Prerender`
    * `Reflex.Dom.Time`
    * `Reflex.Dom.WebSocket.Query`
    * `Reflex.Dom.Xhr.FormData`
* ([#366](https://github.com/reflex-frp/reflex-dom/pull/366)) Bump bounds for `reflex` to include 0.7.

## 0.5.3

* On Android, enable prompting the user for geolocation
  permissions on demand by default. See
  https://developer.android.com/reference/android/webkit/WebChromeClient.html#onGeolocationPermissionsShowPrompt(java.lang.String,%20android.webkit.GeolocationPermissions.Callback)
  for details.

 * Add `< 0.7` upper bound for reflex

## 0.5.2

* The default jsaddle backend on macOS when built from nix
  is now jsaddle-wkwebview, matching the behaviour of cabal
  builds.

## 0.5

* Re-export new hydration widget "mainWidget" functions.

* The use-warp flag now properly takes precedence on macOS.
