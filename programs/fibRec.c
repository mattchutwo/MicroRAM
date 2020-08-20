/******************************************************************************

                            Recursive fibonacci.

*******************************************************************************/
static int SECRET_NUMBER __attribute__((section("__DATA,__secret"))) = 10;

int fibbonacci(int n) {
   if(n == 0){
      return 0;
   } else if(n == 1) {
      return 1;
   } else {
      return (fibbonacci(n-1) + fibbonacci(n-2));
   }
}

int main() {
   return fibbonacci(SECRET_NUMBER);
}
