/******************************************************************************

Return the first argument (argv[1]).
Transform the arguemnts from char* to int and return.

*******************************************************************************/

int
main (int argc, char *argv[])
{
  char numStr[] = "123";
  int res = 0;
  for (int i = 0; numStr[i] != '\0'; ++i) {
    res = res * 10 + numStr[i] - '0'; 
  }
  return res;
}

