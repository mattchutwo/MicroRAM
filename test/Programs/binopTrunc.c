#include <stdint.h>

int main() {
    uint64_t two = 0x100000002ull;

    int ok = 0;
    ok += (uint32_t)two == 2;
    ok += (uint32_t)3 / (uint32_t)two == 1;
    ok += (uint32_t)3 / (uint32_t)two != 0;
    ok += (uint32_t)3 % (uint32_t)two == 1;
    ok += (uint32_t)3 % (uint32_t)two != 3;
    ok += (uint32_t)two >> 2 == 0;
    ok += (uint32_t)two >> 2 != (uint32_t)0x40000000;
    ok += (int32_t)two >> 2 == 0;
    ok += (int32_t)two >> 2 != (uint32_t)0x40000000;
    return ok;
}
