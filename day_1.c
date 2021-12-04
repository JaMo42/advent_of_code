#include <stdio.h>
#include <stdlib.h>

int
main ()
{
  long n, prev = -1;
  unsigned inc = 0;
  char *end;
  char buf[8] = {0};
  while (fgets (buf, 8, stdin))
    {
      n = strtol (buf, &end, 10);
      if (prev == -1)
        prev = n;
      if (n > prev)
        ++inc;
      prev = n;
    }
  printf ("%u\n", inc);
}

