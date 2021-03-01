// Build with:
// clang-9 -O1 -emit-llvm -S -o - test/Programs/memcpy.c >test/Programs/memcpy.ll -fno-builtin
// sed -i -e 's/nofree//g' test/Programs/memcpy.ll
#include <stdlib.h>

void* my_memcpy(void* dest, const void* src, size_t n) {
    char* dest_c = dest;
    const char* src_c = src;
    const char* src_end_c = src_c + n;
    while (src_c != src_end_c) {
        *dest_c = *src_c;
        *dest_c = *src_c;
        *dest_c = *src_c;
        ++src_c;
        ++dest_c;
    }
    return dest;
}

int main() {
    size_t buf = 123;
    size_t buf2 = 0;
    my_memcpy(&buf2, &buf, sizeof(size_t));
    return buf2;
}
