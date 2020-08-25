/******************************************************************************

                            Recursive factorial.

*******************************************************************************/
static int SECRET_NUMBER __attribute__((section("__DATA,__secret"))) = 2;

int factorial(int n) {
   if(n == 0){
      return 21;
   } else if(n == 1) {
      return 42;
   } else {
     return (n*factorial(n-1));
   }
}

int main() {
   return factorial(SECRET_NUMBER);
}
