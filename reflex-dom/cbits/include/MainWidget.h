#ifndef MAINWIDGET_H_INCLUDED
#define MAINWIDGET_H_INCLUDED

#include <stddef.h>

typedef struct JSaddleCallbacks {
  void (* jsaddleStart) ();
  void (* jsaddleResult) (const char *, size_t);
  void (* jsaddleSyncResult) (const char *, size_t, const char**, size_t*);
  char * jsaddleJsData;
} JSaddleCallbacks;

#endif
