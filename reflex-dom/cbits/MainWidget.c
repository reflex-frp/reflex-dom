#include <jni.h>
#include <assert.h>
#include <android/log.h>
#include <HaskellActivity.h>

void Reflex_Dom_Android_MainWidget_start(jobject activity, const char *url) {
  assert(activity);
  assert(url);

  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, &env, NULL);
  assert(attachResult == JNI_OK);

  jclass cls = (*env)->FindClass(env, "org/reflexfrp/reflexdom/MainWidget");
  assert(cls);
  jmethodID startMainWidget = (*env)->GetStaticMethodID(env, cls, "startMainWidget", "(Landroid/app/Activity;Ljava/lang/String;)V");
  assert(startMainWidget);

  jstring jurl = (*env)->NewStringUTF(env, url);
  assert(jurl);
  (*env)->CallStaticVoidMethod(env, cls, startMainWidget, activity, jurl);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "MainWidget", "startMainWidget exception");
    (*env)->ExceptionDescribe(env);
  }
}
