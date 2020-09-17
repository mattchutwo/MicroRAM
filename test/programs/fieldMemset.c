struct s {
    int a;
    int x[3];
    int b;
};

void __attribute__((noinline)) f(struct s* x) {
    x->a = 1;
    x->b = 2;
    for (int i = 0; i < 3; ++i) {
        x->x[i] = 0;
    }
}

int main() {
    struct s x = {0};
    f(&x);
    return x.a + x.b;
}
