/******************************************************************************

Return the first argument (argv[1]).
Transform the arguemnts from char* to int and return.

*******************************************************************************/

int
main (int argc, char *argv[])
{
  int i = 0;
  int res=0;
  char* str;
  if (argc < 2) return 0;
  
  str = argv[1];
  for (int i = 0; str[i] != '\0'; ++i) {
    res = res * 10 + str[i] - '0'; 
  }
  return res;
}
