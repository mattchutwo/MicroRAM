int main(int argc, char *argv[])
{
  int n, first = 0, second = 1, next, c;
  n = argc;  // stupid way to pass an int
  for (c = 0; c < n; c++)
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
