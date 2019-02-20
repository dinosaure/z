#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/callback.h>

#include "miniz.h"

int max(int a, int b) { return (a < b) ? b : a; }

int
main(int argc, const char **argv)
{
  char *i;
  char *o;
  FILE *fh = (strcmp(argv[1], "-i") == 0) ? fopen(argv[2], "rb") : fopen(argv[1], "rb");

  if (fh != NULL)
    {
      fseek(fh, 0L, SEEK_END);
      long i_len = ftell(fh);
      long o_len = i_len * 100; // XXX(dinosaure): lol!
      rewind(fh);

      i = malloc(i_len);
      o = malloc(o_len);

      if (i != NULL && o != NULL)
        {
          long i_len_ = fread(i, sizeof(char), i_len, fh);
          fclose(fh);
          fh = NULL;

          long o_len_ = 0;

          o_len_ = miniz_inflate(i, i_len, o, o_len);

          for (int i = 0; i < o_len_; ++i)
            printf("%c", o[i]);

          free(i);
          free(o);
          i = NULL;
          o = NULL;
        }

      if (i != NULL) free(i);
      if (o != NULL) free(o);
      if (fh != NULL) fclose(fh);
    }

  return EXIT_SUCCESS;
}
