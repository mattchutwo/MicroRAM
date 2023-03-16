// Compile with:
// clang-11 --target=riscv64-unknown-elf -march=rv64im test/Programs/RISCV/sraw.c -S -o test/Programs/RISCV/sraw.s -O2
// Note -O2 is required to ensure we get the right asm for sraw1 and sraw2.

int sraw1(int x, int y) __attribute__((noinline));

int sraw1(int x, int y) {
    // Since the first argument and the return address both use register `a0`,
    // this should produce `sraw a0, a0, a1`
    return x >> y;
}

int sraw2(int y, int x) __attribute__((noinline));

int sraw2(int y, int x) {
    // Since the first argument and the return address both use register `a0`,
    // this should produce `sraw a0, a1, a0`
    return x >> y;
}

int test_both_sraw(int x, int y, int expected) {
    return sraw1(x, y) == expected && sraw2(y, x) == expected;
}

int main()
{
    int ok = 1;
    ok &= test_both_sraw(12, 1, 6);
    ok &= test_both_sraw(-1, 1, -1);
    ok &= test_both_sraw(-1, 3, -1);
    return ok;
}
