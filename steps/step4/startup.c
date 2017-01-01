#include <stdio.h>
/* define all scheme constants */
#define bool_f 0x2F
#define bool_t 0x6F
#define fx_mask 0x03
#define fx_tag 0x00
#define char_tag 0x0F
#define fx_shift 2

int scheme_entry(void);

/* all scheme values are of type ptrs */
typedef unsigned int ptr;

static const char *getCharString(unsigned char c) {
  static struct charTable_t {
    unsigned char tag;
    const char *str;
  } table[] = {
    {0,    "nul"       },
    {7,    "alarm"     },
    {8,    "backspace" },
    {'\t', "tab"       },               /* 9 */
    {'\n', "newline"   },               /* 10 */
    {11,   "vtab"      },
    {12,   "page"      },
    {'\r', "return"    },               /* 13 */
    {' ',  "space"     },
    {127,  "delete"    },
    {0,    NULL        }                /* terminator */
  };
  struct charTable_t *t;
  for (t = table; t->str != NULL; t++) {
    if (t->tag == c) { return t->str; }
  }
  return NULL;
}


static void print_ptr (ptr x) {
  if ((x & fx_mask) == fx_tag) {
    printf ("%d", ((int) x) >> fx_shift);
  } else if (x == bool_f) {
      printf ("#f");
  } else if (x == bool_t) {
    printf ("#t");
  } else if ((x & 0xFF) == char_tag && (x & 0xFFFF0000) == 0) {
    /* Chars should be only one byte in the 2nd byte */
    const char *s;
    unsigned char c = (unsigned char)(x >> 8);
    if ((s = getCharString(c)) != NULL) {
      printf("#\\%s", s);
    } else {
      printf("#\\%c", c);
    }
  } else if (x == 0x3F) {
    printf("()");
  } else {
    printf ("#<unknown 0x%08x>", x);
  }
  fputc('\n', stdout);
}

int main (int argc, char **argv) {
  print_ptr (scheme_entry());
  return 0;
}
