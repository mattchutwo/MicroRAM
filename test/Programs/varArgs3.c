
// clang-9 -S -emit-llvm -o varArgs3.ll varArgs3.c

#include <stdarg.h>
#include <stdint.h>

// We can't use va_list, so redefine it here.
typedef struct {
  unsigned int gp_offset;
  unsigned int fp_offset;
  void* overflow_arg_area;
  void* reg_save_area;
} __cc_va_list;

// TODO: Move to libfromager
void __cc_va_start(char* raw_list, char* bp, int offset) {
    __cc_va_list* list = (__cc_va_list*) raw_list;

    // Set gp_offset to 999.
    list->gp_offset = 999;

    // Set fp_offset to 999.
    list->fp_offset = 999;

    // Set overflow_arg_area to first variable argument.
    list->overflow_arg_area = bp + offset;

    // Set reg_save_area to 0xffff_0000.
    list->reg_save_area = (void*) 0xffff0000;
}

void __cc_va_copy(char* dest, char* src) {
    __cc_va_list* dest_list = (__cc_va_list*) dest;
    __cc_va_list* src_list = (__cc_va_list*) src;
    *dest_list = *src_list;
}

void __llvm__memcpy__p0i8__p0i8__i64(uint8_t *dest, const uint8_t *src, uint64_t len) {
    for (uint64_t i = 0; i < len; ++i) {
      dest[i] = src[i];
    }
}

int sum(int n, ...) {
  va_list p;
  va_start(p,n);
  va_list q;
  va_copy(q,p);

  int sum = 0;

  for (int i = 0; i < n; i++) {
    sum += va_arg(p, int);
  }
  va_end(p);

  for (int i = 0; i < n; i++) {
    sum += va_arg(q, int);
  }
  va_end(q);

  return sum;
}

int main() {
    return sum(5, 1, 2, 3, 4, 5);
}
