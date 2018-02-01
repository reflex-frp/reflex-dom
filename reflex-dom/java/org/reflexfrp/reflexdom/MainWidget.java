package org.reflexfrp.reflexdom;

import android.app.Activity;
import android.os.Handler;
import android.util.Log;
import android.view.ViewGroup.LayoutParams;
import android.view.Window;
import android.webkit.CookieManager;
import android.webkit.JavascriptInterface;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.webkit.WebChromeClient;
import android.webkit.PermissionRequest;
import android.webkit.MimeTypeMap;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.net.Uri;
import android.annotation.TargetApi;
import android.os.Build;
import java.io.InputStream;
import java.io.IOException;

import java.nio.charset.StandardCharsets;

public class MainWidget {
  private static Object startMainWidget(final Activity a, String url, long jsaddleCallbacks, final String initialJS) {
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

    wv.setWebViewClient(new WebViewClient() {
        @Override
        public void onPageFinished(WebView _view, String _url) {
          wv.evaluateJavascript(initialJS, null);
        }

        // Re-route / to /android_asset
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
    });

    wv.setWebChromeClient(new WebChromeClient() {
        // Need to accept permissions to use the camera and audio
        @Override
        public void onPermissionRequest(final PermissionRequest request) {
            a.runOnUiThread(new Runnable() {
                @TargetApi(Build.VERSION_CODES.LOLLIPOP)
                @Override
                public void run() {
                    // Make sure the request is coming from the file system ...
                    if(request.getOrigin().toString().startsWith("file://")) {
                        request.grant(request.getResources());
                    }
                    else {
                        request.deny();
                    }
                }
            });
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
