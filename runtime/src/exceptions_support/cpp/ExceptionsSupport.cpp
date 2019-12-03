#include "Memory.h"
#include "Porting.h"

#ifndef KONAN_NO_EXCEPTIONS

ObjHeader* obj;

// Just some DCE-surviving code referencing ExceptionObjHolder.
// This is needed during compilation to cache.
void foo() {
  try {
    int* p = new int(42);
  } catch (ExceptionObjHolder& e) { obj = e.obj(); }
}

#endif