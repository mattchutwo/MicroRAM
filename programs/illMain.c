/******************************************************************************

Return the first argument (argv[1]).
Transform the arguemnts from char* to int and return.

*******************************************************************************/

int
main (int argc, char argv[])
{
  int i = 0;
  int res=0;
  for (int i = 0; argv[i] != '\0'; ++i) {
    res = res * 10 + argv[i] - '0'; 
  }
  return res;
}
