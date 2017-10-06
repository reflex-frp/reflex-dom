#ifndef MAINWIDGET_H_INCLUDED
#define MAINWIDGET_H_INCLUDED

typedef struct JSaddleCallbacks {
  void (* jsaddleStart) ();
  void (* jsaddleResult) (const char *);
  char * (* jsaddleSyncResult) (const char *);
  char * jsaddleJsData;
} JSaddleCallbacks;

#endif
