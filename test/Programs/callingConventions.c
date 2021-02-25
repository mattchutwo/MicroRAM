/******************************************************************************

                            More functions example.
This example is designed to test the calling convention: Some variables are expected to be preserved through function calls.

Note: This program will be trivial if we optimize it, we can add an "input" to avoid that.

*******************************************************************************/
int add21(int n) {
  return n + 21; 
}

int main() {
  int a = 1;
  int b = 20;
  int c;
  c = add21 (a);
  b = c + b;
  return b;
}
