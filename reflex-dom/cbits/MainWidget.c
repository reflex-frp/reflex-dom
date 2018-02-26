#include <jni.h>
#include <assert.h>
#include <stdlib.h>
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
  jmethodID startMainWidget = (*env)->GetStaticMethodID(env, cls, "startMainWidget", "(Lsystems/obsidian/HaskellActivity;Ljava/lang/String;JLjava/lang/String;)Ljava/lang/Object;");
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

void Reflex_Dom_Android_MainWidget_runJS(jobject jsExecutor, const char* js, size_t js_len) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, &env, NULL);
  assert (attachResult == JNI_OK);

  (*env)->PushLocalFrame(env, 5);

  //TODO: Don't search for this method every time
  jclass cls = (*env)->GetObjectClass(env, jsExecutor);
  assert(cls);
  jmethodID evaluateJavascript = (*env)->GetMethodID(env, cls, "evaluateJavascript", "([B)V");
  assert(evaluateJavascript);
  jbyteArray js_str = (*env)->NewByteArray(env, js_len);
  (*env)->SetByteArrayRegion(env, js_str, 0, js_len, (const jbyte*)js);
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

JNIEXPORT void JNICALL Java_org_reflexfrp_reflexdom_MainWidget_00024JSaddleCallbacks_processMessage (JNIEnv *env, jobject thisObj, jlong callbacksLong, jbyteArray msg) {
  const JSaddleCallbacks *callbacks = (const JSaddleCallbacks *)callbacksLong;

  jbyte *msg_str = (*env)->GetByteArrayElements(env, msg, NULL);
  jsize msg_str_len = (*env)->GetArrayLength(env, msg);
  (*(callbacks->jsaddleResult))(msg_str, msg_str_len);
  (*env)->ReleaseByteArrayElements(env, msg, msg_str, JNI_ABORT);
  return;
}

JNIEXPORT jbyteArray JNICALL Java_org_reflexfrp_reflexdom_MainWidget_00024JSaddleCallbacks_processSyncMessage (JNIEnv *env, jobject thisObj, jlong callbacksLong, jbyteArray msg) {
  const JSaddleCallbacks *callbacks = (const JSaddleCallbacks *)callbacksLong;

  char const *next_str;
  size_t next_str_len;

  jbyte *msg_str = (*env)->GetByteArrayElements(env, msg, NULL);
  jsize msg_str_len = (*env)->GetArrayLength(env, msg);
  (*(callbacks->jsaddleSyncResult))(msg_str, msg_str_len, &next_str, &next_str_len);
  (*env)->ReleaseByteArrayElements(env, msg, msg_str, JNI_ABORT);

  jbyteArray next_jstr = (*env)->NewByteArray(env, next_str_len);
  (*env)->SetByteArrayRegion(env, next_jstr, 0, next_str_len, next_str);

  free(next_str);

  return next_jstr;
}
