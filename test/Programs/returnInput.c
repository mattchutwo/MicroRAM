/******************************************************************************

Return the first argument (argv[1]).
Transform the arguemnts from char* to int and return.

*******************************************************************************/

static char SECRET_NUMBER[] __attribute__((section("__DATA,__secret"))) = "42";

int
main ()
{
  int i = 0;
  int res=0;
  char* str;
  
  str = SECRET_NUMBER;
  for (int i = 0; str[i] != '\0'; ++i) {
    res = res * 10 + str[i] - '0'; 
  }
  return res;
}
