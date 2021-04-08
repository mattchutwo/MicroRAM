
int foo(int n) {
    if (n == 1) {
        return 0;
    }
    
    return n;
}

int (*fp)(int) = foo;

int main() {
    return fp(5);
}
