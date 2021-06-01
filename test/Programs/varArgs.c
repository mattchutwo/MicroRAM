
// clang-9 -S -emit-llvm -o varArgs.ll varArgs.c

#include <stdarg.h>

// typedef struct {
//   unsigned int gp_offset;
//   unsigned int fp_offset;
//   void* overflow_arg_area;
//   void* reg_save_area;
// } va_list[1];

// TODO: Move to libfromager
void __cc_va_start(char* raw_list, char* bp, int offset) {
    // TODO: Endianness, alignment?

    // va_list* list = (va_list*) raw_list;
    // list->gp_offset = 999;

    // Set gp_offset to 999.
    unsigned int* gp_offset = (unsigned int*) raw_list;
    *gp_offset = 999;

    // Set fp_offset to 999.
    unsigned int* fp_offset = gp_offset + 1;
    *fp_offset = 999;

    // Set overflow_arg_area to first variable argument.
    void** overflow_arg_area = (void**) (fp_offset + 1);
    *overflow_arg_area = bp + offset;

    // Set reg_save_area to 0xffff_0000.
    void** reg_save_area = overflow_arg_area + 1;
    *reg_save_area = (void*) 0xffff0000;
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
