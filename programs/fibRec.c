/******************************************************************************

                            Recursive fibonacci.

*******************************************************************************/

int fibbonacci(int n) {
   if(n == 0){
      return 0;
   } else if(n == 1) {
      return 1;
   } else {
      return (fibbonacci(n-1) + fibbonacci(n-2));
   }
}

int main(int argc, char *argv[]) {
   return fibbonacci(argc);
}
