#include <caml/alloc.h>
#include <caml/mlvalues.h>

#ifndef _WIN32
#include <sys/resource.h>
#endif

/* The peak resident memory of the waited-for children, in megabytes: 0 where
   getrusage is missing. ru_maxrss is in bytes on macOS, in kilobytes
   elsewhere. */
value wire_fstar_children_maxrss(value unit)
{
  double mb = 0.0;
  (void)unit;
#ifndef _WIN32
  struct rusage ru;
  if (getrusage(RUSAGE_CHILDREN, &ru) == 0) {
#ifdef __APPLE__
    mb = (double)ru.ru_maxrss / 1048576.0;
#else
    mb = (double)ru.ru_maxrss / 1024.0;
#endif
  }
#endif
  return caml_copy_double(mb);
}
