#include <stdio.h>
#include <ctype.h>
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include "vector.h"

#define Min(a, b) (((a) < (b)) ? (a) : (b))

typedef VECTOR(char) Stack;
typedef VECTOR(Stack) Stacks;

bool read_line (FILE *fp, VECTOR(char) *line) {
  vector_clear (*line);
  bool eof = false;
  for (;;) {
    const char ch = fgetc (fp);
    if (ch == EOF) {
      eof = vector_empty (*line);
      break;
    } else if (ch == '\n') {
      break;
    } else {
      vector_push (*line, ch);
    }
  }
  vector_push (*line, '\0');
  return !eof;
}

VECTOR(char*) chunks (const char *str, const size_t size) {
  size_t left = strlen (str);
  VECTOR(char*) result = vector_create (char *, left / size);
  while (left > 0) {
    const size_t chunk_size = Min (size, left);
    char *const chunk = malloc (chunk_size + 1);
    memcpy (chunk, str, chunk_size);
    chunk[chunk_size] = '\0';
    str += chunk_size;
    left -= chunk_size;
    vector_push (result, chunk);
  }
  return result;
}

char first_non_space (const char *str) {
  while (*str && isspace (*str)) {
    ++str;
  }
  return *str;
}

void reverse (Stack *s) {
  Stack elems = vector_clone (*s);
  vector_clear (*s);
  for (size_t i = vector_size (elems); i > 0; --i) {
    vector_push (*s, elems[i-1]);
  }
  vector_free (elems);
}

Stacks read_initial_state () {
  VECTOR(char) line = NULL;
  Stacks result = NULL;
  for (;;) {
    assert (read_line (stdin, &line));
    if (result == NULL) {
      const size_t count = (vector_size (line) + 1) / 4;
      result = vector_create (Stack, count);
      for (size_t i = 0; i < count; ++i) {
        vector_push (result, NULL);
      }
    } else if (first_non_space (line) != '[') {
      // got the " 1  2  3"... line
      // Skip blank line after
      read_line (stdin, &line);
      break;
    }
    VECTOR(char*) elems = chunks (line, 4);
    size_t stack_idx = 0;
    vector_for_each (elems, elem) {
      // elem is "[x] " or "    ".
      const char elem_or_none = (*elem)[1];
      if (elem_or_none != ' ') {
        vector_push (result[stack_idx], elem_or_none);
      }
      stack_idx += 1;
    }
    vector_for_each (elems, elem) {
      free (*elem);
    }
    vector_free (elems);
  }
  vector_for_each (result, stack) {
    reverse (stack);
  }
  vector_free (line);
  return result;
}

Stacks stacks_clone (Stacks s) {
  Stacks c = vector_create (Stack, vector_size (s));
  vector_for_each (s, stack) {
    vector_push (c, vector_clone (*stack));
  }
  return c;
}

void stacks_drop (Stacks s) {
  vector_for_each (s, stack) {
    vector_free (*stack);
  }
  vector_free (s);
}

void stacks_process_move (Stacks s, const char *const command, const bool block_move) {
  int count, source, destination;
  assert (sscanf (command, "move %d from %d to %d", &count, &source,
                  &destination)
          == 3);
  source -= 1;
  destination -= 1;
  if (block_move) {
    const size_t source_size = vector_size (s[source]);
    assert (source_size >= (size_t)count);
    VECTOR(char) block = vector_slice (s[source], -count, VECTOR_SLICE_REST);
    vector_erase (s[source], source_size - count, count);
    vector_push_vector (s[destination], block);
    vector_free (block);
  } else {
    for (int i = 0; i < count; ++i) {
      assert (!vector_empty (s[source]));
      const char elem = vector_pop (s[source]);
      vector_push (s[destination], elem);
    }
  }
}

void stacks_print (Stacks s) {
  int i = 1;
  vector_for_each (s, stack) {
    printf ("%d:", i++);
    vector_for_each (*stack, elem) {
      printf (" %c", *elem);
    }
    putchar ('\n');
  }
}

char* stacks_collect_top (Stacks s) {
  char *result = malloc (vector_size (s) + 1);
  size_t i = 0;
  vector_for_each (s, stack) {
    result[i++] = vector_back (*stack);
  }
  result[i] = '\0';
  return result;
}

int main () {
  Stacks state1 = read_initial_state ();
  Stacks state2 = stacks_clone (state1);
  puts ("Initial:");
  stacks_print (state1);

  VECTOR(char) line = NULL;
  while (read_line (stdin, &line)) {
    //puts (line);
    stacks_process_move (state1, line, false);
    stacks_process_move (state2, line, true);
  }
  vector_free (line);

  puts ("\nFinal 1:");
  stacks_print (state1);
  char *top = stacks_collect_top (state1);
  printf ("Top: \x1b[92m%s\x1b[0m\n", top);
  free (top);

  puts ("\nFinal 2:");
  stacks_print (state2);
  top = stacks_collect_top (state2);
  printf ("Top: \x1b[92m%s\x1b[0m\n", top);
  free (top);

  stacks_drop (state1);
  stacks_drop (state2);
}
