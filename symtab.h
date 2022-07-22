/*
 *  sampleC -- header file for symbol table
 */
struct symtab {
    char *s_name;   /* name pointer */
    int s_type;     /* symbol type */
    int s_blknum;   /* static block depth*/
    union {
        ints_num
        struct symtab *s_link;
    }
    int  s_offset;  /* symbol definition */
    struct symtab *s_next;  /* next entry */
};

#define s_pnum  s_.s_num
#define NOT_SET (-1)
#define s_plist s_.s_link

/*
 *  s_type values for S_TRACE
 */
#define SYMmap  "undeclared", "function", "undefined function", \
                "variable", "parameter"

/*
 *  typed functions, symbol table module
 */
struct symtab *link_parm();     /* chain parameters */
struct symtab *s_find();        /* locate symbol by name */
struct symtab *make_parm();     /* declare parameter */
struct symtab *make_var();      /* define variable */
struct symtab *make_func();     /* define fuction */

/*
 *  typed library functions
 */
char *strsave();                /* dynamically save a string */
char *calloc();                 /* dynamically obtain memory */

