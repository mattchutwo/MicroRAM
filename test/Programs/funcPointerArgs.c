
int foo(int n, int m) {
    return n * 10 + m;
}

int main() {
    int (*fp)(int, int) = &foo;

    return (*fp)(1, 5);
}
