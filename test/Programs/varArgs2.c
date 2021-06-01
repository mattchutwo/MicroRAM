
// clang -S -emit-llvm -o varArgs2.ll varArgs2.c

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>

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

void __llvm__memcpy__p0i8__p0i8__i64(uint8_t *dest, const uint8_t *src, uint64_t len) {
    for (uint64_t i = 0; i < len; ++i) {
      dest[i] = src[i];
    }
}

typedef struct {
  int  f1;
  bool f2;
  int* f3;
} test_struct;

int test(int n, ...) {
  va_list p;
  va_start(p,n);

  int sum = n;

  test_struct arg2 = va_arg(p, test_struct);
  sum += arg2.f1;
  if (arg2.f2) {
      sum += 3;
  }
  sum += *(arg2.f3);

  int* arg3 = va_arg(p, int*);
  sum += arg3[0];
  sum += arg3[1];
  sum += arg3[2];

  int arg4 = va_arg(p, int);
  sum += arg4;

  va_end(p);

  return sum;
}

int main() {
  int arg1 = 1;
  test_struct arg2;
  arg2.f1 = 2;
  arg2.f2 = true;
  arg2.f3 = &arg1;

  int arg3[3] = {0,5,0};
  arg3[0] = 4;
  arg3[2] = 6;
  
  return test( arg1, arg2, arg3, arg1);
}
