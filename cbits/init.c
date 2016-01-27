#include <HsFFI.h>

static void init_rts() __attribute__((constructor));

void init_rts() {
  static char *argv[] = { "sudbury", 0 };
  static char **argv_ = argv;
  static int argc = 1;
  hs_init(&argc, &argv_);
}
