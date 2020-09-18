int __attribute__((noinline)) test_ashr(int x, int n, int expect) {
    return (x >> n) == expect;
}

int main() {
    int ok = 0;
    ok += test_ashr(-4, 0, -4);
    ok += test_ashr(-4, 1, -2);
    ok += test_ashr(-4, 2, -1);
    ok += test_ashr(-4, 3, -1);
    ok += test_ashr(4, 0, 4);
    ok += test_ashr(4, 1, 2);
    ok += test_ashr(4, 2, 1);
    ok += test_ashr(4, 3, 0);
    // Should return 8, as all 8 tests pass.
    return ok;
}
