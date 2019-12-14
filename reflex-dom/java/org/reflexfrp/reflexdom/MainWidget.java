package org.reflexfrp.reflexdom;

import android.annotation.TargetApi;
import android.net.Uri;
import android.os.Build;
import android.os.Handler;
import android.util.Log;
import android.view.ViewGroup.LayoutParams;
import android.view.Window;
import android.webkit.CookieManager;
import android.webkit.GeolocationPermissions;
import android.webkit.JavascriptInterface;
import android.webkit.MimeTypeMap;
import android.webkit.PermissionRequest;
import android.webkit.WebChromeClient;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.graphics.Bitmap;
import java.io.IOException;
import java.io.InputStream;
import android.content.Intent;
import android.content.ActivityNotFoundException;
import java.util.concurrent.atomic.AtomicBoolean;

import java.nio.charset.StandardCharsets;

import systems.obsidian.HaskellActivity;

public class MainWidget {
  private static Object startMainWidget(final HaskellActivity a, String url, long jsaddleCallbacks, final String initialJS) {
    CookieManager.setAcceptFileSchemeCookies(true); //TODO: Can we do this just for our own WebView?

    // Remove title and notification bars
    a.requestWindowFeature(Window.FEATURE_NO_TITLE);

    final WebView wv = new WebView(a);
    wv.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
    a.setContentView(wv);
    final WebSettings ws = wv.getSettings();
    ws.setJavaScriptEnabled(true);
    ws.setAllowFileAccessFromFileURLs(true);
    ws.setAllowUniversalAccessFromFileURLs(true);
    ws.setDomStorageEnabled(true);
    wv.setWebContentsDebuggingEnabled(true);
    // allow video to play without user interaction
    wv.getSettings().setMediaPlaybackRequiresUserGesture(false);
    final AtomicBoolean jsaddleLoaded = new AtomicBoolean(false);

    wv.setWebViewClient(new WebViewClient() {
        @Override
        public void onPageFinished(WebView _view, String _url) {
          Log.i("reflex", "onPageFinished");
          boolean alreadyLoaded = jsaddleLoaded.getAndSet(true);
          if(!alreadyLoaded) {
            Log.i("reflex", "loading jsaddle");
            wv.evaluateJavascript(initialJS, null);
          }
        }

        // Re-route / to /android_asset
        @Override
        public WebResourceResponse shouldInterceptRequest (WebView view, WebResourceRequest request) {
            Uri uri = request.getUrl();
            if(!uri.getScheme().equals("file"))
                return null;

            String path = uri.getPath();
            path = getAssetPath(path);

            String mimeType = getMimeType(uri.toString());
            String encoding = "";

            try {
                InputStream data = a.getApplicationContext().getAssets().open(path);
                return new WebResourceResponse(mimeType, encoding, data);
            }
            catch (IOException e) {
                Log.i("reflex", "Opening resource failed, Webview will handle the request ..");
                e.printStackTrace();
            }

            return null;
        }

        @Override
        public boolean shouldOverrideUrlLoading(WebView view, String url) {
            if( url != null && !url.startsWith("http://") && !url.startsWith("https://") && !url.startsWith("file://")) {
                try {
                    view.getContext().startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(url)));
                }
                catch(ActivityNotFoundException  e) {
                    Log.e("reflex", "Starting activity for intent '" + url + "' failed!");
                }
                return true;
            } else {
                return false;
            }
        }
    });

    wv.setWebChromeClient(new WebChromeClient() {
        // Need to accept permissions to use the camera and audio
        @Override
        public void onPermissionRequest(final PermissionRequest request) {
            if(request.getOrigin().toString().startsWith("file://")) {
                a.requestWebViewPermissions(request);
            }
            else {
                a.runOnUiThread(new Runnable() {
                        @TargetApi(Build.VERSION_CODES.LOLLIPOP)
                        @Override
                        public void run() {
                            request.deny();
                        }
                    });
            }
        }

        @Override
        public Bitmap getDefaultVideoPoster() {
            return Bitmap.createBitmap(10, 10, Bitmap.Config.ARGB_8888);
        }

        @Override
        public void onGeolocationPermissionsShowPrompt(String origin, GeolocationPermissions.Callback callback) {
            callback.invoke(origin, true, false);
        }
    });

    wv.addJavascriptInterface(new JSaddleCallbacks(jsaddleCallbacks), "jsaddle");

    wv.loadUrl(url);

    final Handler hnd = new Handler();
    return new Object() {
      public final void evaluateJavascript(final byte[] js) {
        final String jsStr = new String(js, StandardCharsets.UTF_8);
        hnd.post(new Runnable() {
            @Override
            public void run() {
              wv.evaluateJavascript(jsStr, null);
            }
          });
      }
    };
  }

  private static String getMimeType(String url) {
      String type = "";
      String extension = MimeTypeMap.getFileExtensionFromUrl(url);
      if (extension != null) {
          type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(extension);
      }
      return type;
  }

  /** Get the path of an asset. Strips leading / and leading /android_asset/ */
  private static String getAssetPath(String path) {
      path = path.startsWith("/android_asset") ? path.substring("/android_asset".length()) : path;
      path = path.startsWith("/") ? path.substring(1) : path;
      return path;
  }

  private static class JSaddleCallbacks {
    private final long callbacks;
    private native void startProcessing(long callbacks);
    private native void processMessage(long callbacks, byte[] msg);
    private native byte[] processSyncMessage(long callbacks, byte[] msg);

    public JSaddleCallbacks(long _callbacks) {
      callbacks = _callbacks;
    }

    @JavascriptInterface
    public boolean postReady() {
      startProcessing(callbacks);
      return true;
    }

    @JavascriptInterface
    public boolean postMessage(final String msg) {
      processMessage(callbacks, msg.getBytes(StandardCharsets.UTF_8));
      return true;
    }

    @JavascriptInterface
    public String syncMessage(final String msg) {
      return new String(processSyncMessage(callbacks, msg.getBytes(StandardCharsets.UTF_8)),
                        StandardCharsets.UTF_8);
    }
  }
}
