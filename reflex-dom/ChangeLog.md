# Revision history for reflex-dom

## 0.5.3.1

* ([#366](https://github.com/reflex-frp/reflex-dom/pull/366)) Bump bounds for `reflex` to include 0.7.
* Bump bounds for `reflex-dom-core` to include 0.6.

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
