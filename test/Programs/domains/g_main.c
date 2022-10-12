int f(int x);

int g(int x) {
    return x * 2;
}

int main() {
    return f(g(100));
}
