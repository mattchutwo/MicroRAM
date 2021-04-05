
// clang-9 -S -emit-llvm -o varArgs.ll varArgs.c

#include <stdarg.h>

// typedef struct {
//   unsigned int gp_offset;
//   unsigned int fp_offset;
//   void* overflow_arg_area;
//   void* reg_save_area;
// } va_list[1];

// TODO: Move to libfromager
void __va_start(char* raw_list) {
    // TODO: Endianness, alignment?

    // va_list* list = (va_list*) raw_list;
    // list->gp_offset = 999;

    // Set gp_offset to 999.
    // va_list[0] = 0;
    // unsigned int* gp_offset = ((unsigned int*)va_list);
    // *gp_offset = 0;

    // TODO
    // Set fp_offset to 999.
    // Set overflow_arg_area to first variable argument.
    // Set reg_save_area to 0xffff_0000.
}

int sum(int n, ...) {
  va_list p;
  va_start(p,n);
  int sum = 0;

  for (int i = 0; i < n; i++) {
    sum += va_arg(p, int);
  }
  va_end(p);

  return sum;
}

int main() {
    return sum(5, 1, 2, 3, 4, 5);
}
