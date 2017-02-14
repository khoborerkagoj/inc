#include <windows.h>
#include <stdio.h>
/* define all scheme constants */
#define bool_f      0x2F
#define bool_t      0x6F
#define fx_mask     0x03
#define fx_tag      0x00
#define char_tag    0x0F
#define data_mask   0x07
/* 0x00 and 0x04 are for fixnums, 0x07 is all other immediates (which use
 * further differentiation to distinguish amongst themselves */
#define pair_tag    0x01
#define closure_tag 0x02
#define symbol_tag  0x03
#define vector_tag  0x05
#define string_tag  0x06

#define fx_shift 2

typedef struct {
    void* eax;       /* 0  scratch  */
    void* ebx;       /* 4  preserve */
    void* ecx;       /* 8  scratch  */
    void* edx;       /* 12 scratch  */
    void* esi;       /* 16 preserve */
    void* edi;       /* 20 preserve */
    void* ebp;       /* 24 preserve */
    void* esp;       /* 28 preserve */
} context;

int scheme_entry(context *ctx, char *stack, char *heap);

/* all scheme values are of type ptrs */
typedef unsigned int ptr;

/** Prints partial expressions, without the trailing '\n' */
static void print_partial(ptr x);

/** Prints contents of a pair, without the enclosing '(' and ')' */
static void print_pair_contents(ptr x);

static const char *getCharString(unsigned char c) {
    static char cStr[2] = {0, 0};
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
    /* NULL termination is already present */
    cStr[0] = (char)c;
    return cStr;
}

/* x is an unigned int which is actually a pointer. Dereference it */
#define DEREF(x)  (((ptr *)(x))[0])
#define ISPAIR(x) (((x) & data_mask) == pair_tag)
#define ISNULL(x) ((x) == 0x3F)

static void print_pair_contents(ptr x) {
#define CAR(x) DEREF(x - 1)
#define CDR(x) DEREF(x + 3)
    ptr cdr;

    print_partial(CAR(x));
    cdr = CDR(x);
    if (ISPAIR(cdr)) {                  /* list expression */
        fputc(' ', stdout);
        print_pair_contents(cdr);
    } else if (ISNULL(cdr)) {           /* end of list */
        return;
    } else {                            /* improper list */
        fputs(" . ", stdout);
        print_partial(cdr);
    }
}

/* Used to print partial expressions */
static void print_partial(ptr x) {
    if ((x & fx_mask) == fx_tag) {
        printf ("%d", ((int) x) >> fx_shift);
    } else if (x == bool_f) {
        printf ("#f");
    } else if (x == bool_t) {
        printf ("#t");
    } else if ((x & 0xFF) == char_tag && (x & 0xFFFF0000) == 0) {
        /* Chars should be only one byte in the 2nd byte */
        unsigned char c = (unsigned char)(x >> 8);
        printf("#\\%s", getCharString(c));
    } else if (ISNULL(x)) {
        printf("()");
    } else if (ISPAIR(x)) {
        fputc('(', stdout);
        print_pair_contents(x);
        fputc(')', stdout);
    } else {
        printf ("#<unknown 0x%08x>", x);
    }
}

static void print_ptr(ptr x) {
    print_partial(x);
    fputc('\n', stdout);
}


static int getPageSize() {
    SYSTEM_INFO sysInfo;
    GetSystemInfo(&sysInfo);
    return (int)sysInfo.dwPageSize;
}

static void printError(const char *who) {
    fprintf(stderr, "%s failed with error %lu\n", who, GetLastError());
}

static char *allocateProtectedSpace(int size) {
    char *region;
    DWORD oldProt;
    int page;
    page = getPageSize();
    size = ((size + page - 1)/page + 2) * page;
    if ((region = VirtualAlloc(NULL, size,
                    MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE)) == NULL) {
        printError("VirtualAlloc");
        return region;
    }
    /* Protect the first and last pages */
    if (VirtualProtect(region, page, PAGE_NOACCESS, &oldProt) == 0) {
        printError("VirtualProtect");
        return NULL;
    }
    if (VirtualProtect(region + (size - page), page,
                PAGE_NOACCESS, &oldProt) == 0) {
        printError("VirtualProtect");
        return NULL;
    }
    return (region + page);
}

static void deallocateProtectedSpace(char *p, int size) {
    int page = getPageSize();
    size += 2 * page;
    if (VirtualFree(p - page, size, MEM_RELEASE)) {
        printError("VirtualFree");
    }
}

int main (int argc, char **argv) {
    int stackSize   = (16 * 4096);
    char *stackTop  = allocateProtectedSpace(stackSize);
    char *stackBase = stackTop + stackSize;
    char *heapBase  = allocateProtectedSpace(stackSize * 4);
    context ctxt;

    print_ptr (scheme_entry(&ctxt, stackBase, heapBase));
    deallocateProtectedSpace(stackTop, stackSize);
    deallocateProtectedSpace(heapBase, stackSize * 4);
    return 0;
}
