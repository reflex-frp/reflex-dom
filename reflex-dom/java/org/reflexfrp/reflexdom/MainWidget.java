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

import java.nio.charset.StandardCharsets;

public class MainWidget {
  private static Object startMainWidget(Activity a, String url, long jsaddleCallbacks, final String initialJS) {
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
