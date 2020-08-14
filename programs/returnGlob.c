/* int twice(int a); */
static int SECRET_NUMBER __attribute__((section("__DATA,__secret"))) = 42;
  
int main() {
  return SECRET_NUMBER;
}

