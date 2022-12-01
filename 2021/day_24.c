#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>
#include <inttypes.h>
#include <errno.h>
#include <unistd.h>
#include <sys/wait.h>
#include "vector.h"

#define REG_IDX(reg_sym) ((reg_sym) - 'w')

#define GET_ARG(ins)\
  ((ins)->literal_arg \
   ? (ins)->arg_literal \
   : G_reg[REG_IDX ((ins)->arg_reg)])

enum
{
  INS_INP,
  INS_ADD,
  INS_MUL,
  INS_DIV,
  INS_MOD,
  INS_EQL,
  MNEMONIC_COUNT
};
static const char *MNEMONICS[] = {"inp", "add", "mul", "div", "mod", "eql"};

struct instruction
{
  int opcode;
  char reg;
  union
    {
      char arg_reg;
      int64_t arg_literal;
    };
  bool literal_arg;
};

int64_t G_reg[4];
int64_t *G_input_queue;
size_t G_input_at;
size_t G_input_avail;

struct instruction *
read_program ()
{
  static char buf[32];
  struct instruction *program = NULL;
  int opcode;
  char reg_1, reg_2;
  int64_t literal;
  char *_endptr;

  while (fgets (buf, 32, stdin))
    {
      // opcode
      for (literal = 0; literal < MNEMONIC_COUNT; ++literal)
        {
          if (strncmp (buf, MNEMONICS[literal], 3) == 0)
            {
              opcode = literal;
              break;
            }
        }
      // 1st argument (always register)
      reg_1 = buf[4];
      reg_2 = 0;
      if (opcode != INS_INP)
        {
          // 2nd argument
          reg_2 = buf[6];
          if (!isalpha (reg_2))
            {
              reg_2 = 0;
              literal = strtoll (buf + 6, &_endptr, 10);
            }
        }
      if (reg_2)
        vector_emplace_back (program, opcode, reg_1, .arg_reg=reg_2, false);
      else
        vector_emplace_back (program, opcode, reg_1, .arg_literal=literal, true);
    }
  return program;
}

void
put_input (int64_t x)
{
  vector_push (G_input_queue, x);
  ++G_input_avail;
}

void
get_input (int64_t *out)
{
  if (G_input_avail == 0)
    {
      fputs ("Error: No more arguments\n", stderr);
      exit (1);
    }
  --G_input_avail;
  *out = G_input_queue[G_input_at++];
}

void
clear_input ()
{
  vector_clear (G_input_queue);
  G_input_avail = 0;
  G_input_at = 0;
}

void
do_ins (struct instruction *ins)
{
  int64_t *reg = &G_reg[REG_IDX(ins->reg)];
  int64_t arg = GET_ARG (ins);
  switch (ins->opcode)
    {
      break; case INS_INP: get_input (reg);
      break; case INS_ADD: *reg = arg + *reg;
      break; case INS_MUL: *reg = *reg * arg;
      break; case INS_DIV: *reg = *reg / arg;
      break; case INS_MOD: *reg = *reg % arg;
      break; case INS_EQL: *reg = *reg == arg;
    }
}

void
exec (struct instruction *program)
{
  G_reg[0] = 0;
  G_reg[1] = 0;
  G_reg[2] = 0;
  G_reg[3] = 0;
  vector_for_each (program, ins)
    {
      do_ins (ins);
    }
}

void
list (struct instruction *program)
{
  vector_for_each (program, ins)
    {
      printf ("\x1b[38;5;246m%s \x1b[38;5;254m%c ",
              MNEMONICS[ins->opcode], ins->reg);

      if (ins->opcode == INS_INP)
        putchar ('\n');
      else if (ins->literal_arg)
        printf ("\x1b[38;5;74m%" PRId64 "\n", ins->arg_literal);
      else
        printf ("\x1b[38;5;254m%c\n", ins->arg_reg);
    }
  fputs ("\x1b[0m", stdout);
}

void
transpile (struct instruction *program, FILE *stream)
{
  static const char *OP_TABLE[] = { "+", "*", "/", "%", "==" };
  static char arg_buf[32];
  fputs ("#include <stdio.h>\n"
         "#include <stdlib.h>\n\n"
         "long long get_input (int *i, int argc, const char **argv)\n"
         "{\n"
         "  char *endptr;\n"
         "  long long n;\n"
         "  if (*i == argc)\n"
         "    {\n"
         "      fputs (\"Error: No more arguments\\n\", stderr);\n"
         "      exit (1);\n"
         "    }\n"
         "  n = strtoll (argv[*i], &endptr, 10);\n"
         "  ++(*i);\n"
         "  return n;\n"
         "}\n\n"
         "int\n"
         "main (int argc, const char **argv)\n"
         "{\n"
         "  int input_idx = 1;\n"
         "  long long w=0, x=0, y=0, z=0;\n",
         stream);
  vector_for_each (program, ins)
    {
      if (ins->opcode == INS_INP)
        {
          fprintf (stream, "  %c = get_input (&input_idx, argc, argv);\n",
                   ins->reg);
          continue;
        }
      if (ins->literal_arg)
        sprintf (arg_buf, "%" PRId64, ins->arg_literal);
      else
        sprintf (arg_buf, "%c", ins->arg_reg);
#if 1
      if (ins->opcode == INS_EQL)
        {
          fprintf (stream, "  %c = (%c == %s);\n",
                   ins->reg, ins->reg, arg_buf);
        }
      else
        {
          fprintf (stream, "  %c %c= %s;\n",
                   ins->reg, OP_TABLE[ins->opcode-1][0], arg_buf);
        }
#else
      fprintf (stream, "  %c = %c %s %s\n",
               ins->reg, ins->reg, OP_TABLE[ins->opcode-1], arg_buf);
#endif
    }
  fputs ("  if (input_idx < argc)\n"
         "    fputs (\"Warning: Unused arguments\\n\", stderr);\n"
         "  printf (\"w=%lld x=%lld y=%lld z=%lld\\n\", w, x, y, z);\n"
         "}\n",
         stream);
}

void
compile (struct instruction *program, char *name, bool keep)
{
  char *source_name;
  FILE *fp;
  pid_t pid, wpid;
  int status;
  source_name = malloc (strlen (name) + 3);
  sprintf (source_name, "%s.c", name);

  fp = fopen (source_name, "w");
  transpile (program, fp);
  fclose (fp);
  // Need to fork since `execlp` replaces the current program (unless
  // there is an error)
  pid = fork ();
  if (pid == 0)
    {
      execlp ("cc", "cc", source_name, "-o", name, NULL);
      fprintf (stderr, "Could not run compilation command: %s\n",
               strerror (errno));
    }
  else
    {
      while ((wpid = wait (&status)) > 0) ;
      if (!keep)
        remove (source_name);
    }
}

int
main (int argc, const char **argv)
{
  enum { CMD_HELP, CMD_LIST, CMD_RUN, CMD_CONVERT, CMD_COMPILE };
  struct instruction *program;
  bool compile_keep_source = false;
  char *compile_name;
  int cmd = CMD_HELP, i;
  int64_t v;

  for (i = 1; i < argc; ++i)
    {
      if (argv[i][0] == '-')
        {
          if (strcmp (argv[i], "-keep") == 0)
            compile_keep_source = true;
          else
            {
              fprintf (stderr, "Invalid argument: '%s'\n", argv[i]);
              return 1;
            }
        }
      else
        {
          if (strcmp (argv[i], "list") == 0)
            cmd = CMD_LIST;
          else if (strcmp (argv[i], "run") == 0)
            {
              cmd = CMD_RUN;
              goto stop_args;
            }
          else if (strcmp (argv[i], "convert") == 0)
            cmd = CMD_CONVERT;
          else if (strcmp (argv[i], "compile") == 0)
            {
              cmd = CMD_COMPILE;
              if (i+1 == argc || argv[i+1][0] == '-')
                {
                  fputs ("No program name for compile command\n", stderr);
                  return 1;
                }
              else
                {
                  // Can't be const for execlp
                  compile_name = (char *)argv[i+1];
                  ++i;
                }
            }
          else
            {
              fprintf (stderr, "Invalid command: '%s'\n", argv[i]);
              return 1;
            }
        }
    }
stop_args:

  program = read_program ();

  switch (cmd)
    {
    break; case CMD_HELP:
      puts ("Usage:");
      puts ("day_24 list                  Parse the program and print it with syntax highlighting");
      puts ("day_24 run [INPUTS...]       Run the program with the list of inputs");
      puts ("day_24 convert               Convert the program to C and print it to stdout");
      puts ("day_24 compile NAME [-keep]  Convert the program to C and compile it;");
      puts ("                             specify -keep to keep the generated file.");
    break; case CMD_LIST:
      list (program);
    break; case CMD_RUN:
      ++i;
      clear_input ();
      for (; i < argc; ++i)
        {
          v = strtoll (argv[i], &compile_name, 10);
          put_input (v);
        }
      exec (program);
      if (G_input_avail)
        fprintf (stderr, "Warning: %d unused argument%s\n", G_input_avail,
                 G_input_avail > 1 ? "s" : "");
      printf ("w=%ld x=%ld y=%ld z=%ld\n",
              G_reg[0], G_reg[1], G_reg[2], G_reg[3]);
    break; case CMD_CONVERT:
      transpile (program, stdout);
    break; case CMD_COMPILE:
      compile (program, compile_name, compile_keep_source);
    }

  vector_free (program);
}

