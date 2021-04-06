
int foo(int n) {
    if (n == 1) {
        return 0;
    }
    
    return n;
}

int main() {
    int (*fp)(int) = &foo;

    return (*fp)(5);
}
