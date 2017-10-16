#include <jni.h>
#include <assert.h>
#include <android/log.h>
#include <HaskellActivity.h>
#include "MainWidget.h"

jobject Reflex_Dom_Android_MainWidget_start(jobject activity, const char *url, const JSaddleCallbacks *jsaddleCallbacks) {
  assert(activity);
  assert(url);

  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, &env, NULL);
  assert(attachResult == JNI_OK);

  jclass cls = (*env)->FindClass(env, "org/reflexfrp/reflexdom/MainWidget");
  assert(cls);
  jmethodID startMainWidget = (*env)->GetStaticMethodID(env, cls, "startMainWidget", "(Landroid/app/Activity;Ljava/lang/String;JLjava/lang/String;)Ljava/lang/Object;");
  assert(startMainWidget);

  jstring jurl = (*env)->NewStringUTF(env, url);
  assert(jurl);
  jstring initialJS = (*env)->NewStringUTF(env, jsaddleCallbacks->jsaddleJsData);
  jobject result = (*env)->CallStaticObjectMethod(env, cls, startMainWidget, activity, jurl, (jlong)jsaddleCallbacks, initialJS);
  (*env)->DeleteLocalRef(env, initialJS);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "MainWidget", "startMainWidget exception");
    (*env)->ExceptionDescribe(env);
  }
  return (*env)->NewGlobalRef(env, result);
}

void Reflex_Dom_Android_MainWidget_runJS(jobject jsExecutor, const char* js) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, &env, NULL);
  assert (attachResult == JNI_OK);

  (*env)->PushLocalFrame(env, 5);

  //TODO: Don't search for this method every time
  jclass cls = (*env)->GetObjectClass(env, jsExecutor);
  assert(cls);
  jmethodID evaluateJavascript = (*env)->GetMethodID(env, cls, "evaluateJavascript", "(Ljava/lang/String;)V");
  assert(evaluateJavascript);
  jstring js_str = (*env)->NewStringUTF(env, js);
  (*env)->CallVoidMethod(env, jsExecutor, evaluateJavascript, js_str, 0);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "MainWidget", "runJS exception");
    (*env)->ExceptionDescribe(env);
  }

  (*env)->PopLocalFrame(env, 0);
}

JNIEXPORT void JNICALL Java_org_reflexfrp_reflexdom_MainWidget_00024JSaddleCallbacks_startProcessing (JNIEnv *env, jobject thisObj, jlong callbacksLong) {
  const JSaddleCallbacks *callbacks = (const JSaddleCallbacks *)callbacksLong;
  (*(callbacks->jsaddleStart))();
  return;
}

JNIEXPORT void JNICALL Java_org_reflexfrp_reflexdom_MainWidget_00024JSaddleCallbacks_processMessage (JNIEnv *env, jobject thisObj, jlong callbacksLong, jstring msg) {
  const JSaddleCallbacks *callbacks = (const JSaddleCallbacks *)callbacksLong;
  const char *msg_str = (*env)->GetStringUTFChars(env, msg, NULL);
  (*(callbacks->jsaddleResult))(msg_str);
  (*env)->ReleaseStringUTFChars(env, msg, msg_str);
  return;
}

JNIEXPORT jstring JNICALL Java_org_reflexfrp_reflexdom_MainWidget_00024JSaddleCallbacks_processSyncMessage (JNIEnv *env, jobject thisObj, jlong callbacksLong, jstring msg) {
  const JSaddleCallbacks *callbacks = (const JSaddleCallbacks *)callbacksLong;
  const char *msg_str = (*env)->GetStringUTFChars(env, msg, NULL);
  char *next_str = (*(callbacks->jsaddleSyncResult))(msg_str);
  jstring next_jstr = (*env)->NewStringUTF(env,next_str);
  free(next_str);
  (*env)->ReleaseStringUTFChars(env, msg, msg_str);
  return next_jstr;
}
