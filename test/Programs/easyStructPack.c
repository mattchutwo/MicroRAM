/******************************************************************************

                            (Very) Simple use of Structs.

Simplest use of structs. If you optimize, this becomes trivial.

*******************************************************************************/
static int SECRET_NUMBER1 __attribute__((section("__DATA,__secret"))) = 3;
static int SECRET_NUMBER2 __attribute__((section("__DATA,__secret"))) = 5;

struct Point 
{ 
  int x;
  char y;
  long z;
} __attribute__((packed)); 

int main() {
  struct Point mypoint = {SECRET_NUMBER2, 'b', SECRET_NUMBER1};
  return mypoint.z;
};
