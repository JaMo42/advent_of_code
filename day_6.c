#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include "vector.h"

/* Highest number a fish can have */
#define MAXSTATE 8

static char bigbuf[1024];

int8_t *
read_initial_state ()
{
  int8_t *state = vector_create (int8_t, 8), v;
  int r;
  ptrdiff_t off = 0;
  /* Read intial state from stdin, append a comma to the end so we can parse
     it in '%d,' chunks until there is nothing left. */
  fgets (bigbuf, sizeof (bigbuf) - 1, stdin);
  bigbuf[strlen (bigbuf)] = ',';
  while ((r = sscanf (bigbuf + off, "%hhd,", &v)))
    {
      vector_push (state, v);
      off += 2;
    }
  return state;
}

size_t
solve_simulate (int8_t *init, int days)
{
  int8_t *current = vector_copy_construct (init);
  int8_t *next = vector_create (int8_t, 2 * vector_size (init));
  int8_t *swap;
  int sim_step;
  size_t result;

  vector_for_each (init, f)
    vector_push (current, *f);

  for (sim_step = 0; sim_step < days; ++sim_step)
    {
      vector_clear (next);
      vector_for_each (current, fishy)
        {
          if (*fishy >= 1)
            vector_push (next, *fishy - 1);
          else
            {
              vector_push (next, 6);
              vector_push (next, 8);
            }
        }
      swap = current;
      current = next;
      next = swap;
    }

  result = vector_size (current);
  vector_free (current);
  vector_free (next);
  return result;
}

size_t
solve_calculate (int8_t *init, int days)
{
  /* Number of fish with state 0,1,...,8 */
  size_t current_buf[MAXSTATE+1] = {0}, next_buf[MAXSTATE+1] = {0};
  size_t *current = current_buf, *next = next_buf, *swap, count;
  int sim_step, i;

  vector_for_each (init, f)
    {
      switch (*f)
        {
        case 0: ++current[0]; break;
        case 1: ++current[1]; break;
        case 2: ++current[2]; break;
        case 3: ++current[3]; break;
        case 4: ++current[4]; break;
        case 5: ++current[5]; break;
        case 6: ++current[6]; break;
        case 7: ++current[7]; break;
        case 8: ++current[8]; break;
        default: printf ("Invalid input: %d\n", *f); exit (1);
        }
    }

  for (sim_step = 0; sim_step < days; ++sim_step)
    {
      next[MAXSTATE] = 0;
      count = current[0];

      for (i = 1; i <= MAXSTATE; ++i)
        next[i-1] = current[i];
      next[8] += count;
      next[6] += count;

      swap = current;
      current = next;
      next = swap;
    }

  count = 0;
  for (i = 0; i<= MAXSTATE; ++i)
    count += current[i];

  return count;
}

int
main ()
{
  const int DAYS_1 = 80, DAYS_2 = 256;
  int8_t *init = read_initial_state ();

  printf ("After %3d days, there are \x1b[92m%zu\x1b[0m fishies\n",
          DAYS_1, solve_simulate (init, DAYS_1));

  printf ("After %3d days, there are \x1b[92m%zu\x1b[0m fishies\n",
          DAYS_2, solve_calculate (init, DAYS_2));

  vector_free (init);
}

