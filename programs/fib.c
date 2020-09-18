extern int SECRET_NUMBER __attribute__((section("__DATA,__secret"))) = 10;

int main()
{
  int n, first = 0, second = 1, next, c;
  n = SECRET_NUMBER; 
  for (c = 0; c <= n; c++)
  {
    if (c <= 1)
      next = c;
    else
    {
      next = first + second;
      first = second;
      second = next;
    }
  }

  return next;
}
