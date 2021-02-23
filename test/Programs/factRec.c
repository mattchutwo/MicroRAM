/******************************************************************************

                            Recursive factorial.

*******************************************************************************/
static int SECRET_NUMBER __attribute__((section("__DATA,__secret"))) = 5;

int factorial(int n) {
   if(n == 0){
      return 1;
   } else if(n == 1) {
      return 1;
   } else {
     return (n*factorial(n-1));
   }
}

int main() {
   return factorial(SECRET_NUMBER);
}
