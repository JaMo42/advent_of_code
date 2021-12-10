#include <stdlib.h>
#include <stdio.h>
#include "vector.h"

void
read_input (long **out)
{
  char buf[8];
  char *end;
  long n;
  while (fgets (buf, 8, stdin))
    {
      n = strtol (buf, &end, 10);
      vector_push (*out, n);
    }
}

void
part_one (long *input)
{
  unsigned inc = 0;
  long prev = *input;
  size_t i;

  for (i = 0; i < vector_size (input); ++i)
    {
      if (input[i] > prev)
        ++inc;
      prev = input[i];
    }

  printf ("Part one: \x1b[92m%u\x1b[0m\n", inc);
}

void
part_two (long *input)
{
  unsigned inc = 0;
  long a, d;
  size_t i;

  for (i = 0; i < vector_size (input) - 3; ++i)
    {
      a = input[i];
      // These are in both sides of the comparison and can be factored out
      // b = input[i+1];
      // c = input[i+2];
      d = input[i+3];

      if (d > a)
        ++inc;
    }

  printf ("Part two: \x1b[92m%u\x1b[0m\n", inc);
}

int
main ()
{
  long *input = NULL;

  read_input (&input);

  part_one (input);
  part_two (input);

  vector_free (input);
}

