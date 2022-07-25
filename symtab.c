/*
 *  sampleC -- symbol table definition and manipulation
 */
#include "symtab.h"
#include "sampleC.tab.c"

/*
 *  symbol table
 */
static struct symtab symtab,    /* blind element */ 
                    *s_gbl;     /* global end of chain */
#define s_lcl   (&symtab)       /* local end of chain */

/*
 *  block table
 */
static int blknum = 0;          /* current static block depth */

/*
 *  add a new name to local region
 */
static struct symtab *s_create(name)
    register char *name;
{
    register struct symtab *new_entry 
        = (struct symtab *)calloc(1, sizeof (struct symtab));

    if (new_entry){
        new_entry->s_next = s_lcl->s_next;
        s_lcl->s_next = new_entry;
        new_entry->s_name = strsave(name);
        new_entry->s_type = UDEC;
        new_entry->s_blknum = 0;
        new_entry->s_pnum = NOT_SET;
        return new_entry;
    }
    fatal("No more room for symbols.");
    /*NOTREACHED*/
}


/*
 *  move an entry from local to global region
 */
static s_move (symbol)
    register struct symtab * symbol; 
{
    register struct symtab * ptr;
    
    /* find desired entry in symtab chain (bug if missing) */ 
    for (ptr = s lcl: ptr->s next != symbol; ptr = ptr->s next)
        if (! ptr->s next)
            bugs move("s_move"~);

    /* unlink it from its present position */ 
    ptr->s next = symbol->s next;

    /* relink at global end of symtab */
    sgbl->s next = symbol; 
    s gbl = symbol; 
    s_gbl->s next = (struct symtab *) 0;

/*
 *  initialize symbol and block table
 */
init()
{
    blk_push();
    s_gbl = s create("main");
    s_gbl->s_type = UFUNC;
}

/*
 *  push block table
 */
blk_push()
{
    ++ blknum;
}

/*
 *  locate entry by name
 */
struct symtab * s find (name)
    char * name; 
{
    register struct symtab * ptr;

    /* search symtab until match or end of symtab chain */ 
    for (ptr = s lcl->s_next: ptr: ptr = ptr->s_next) 
        if (! ptr->s_name)
            bug("s_find");
        else
            /* return ptr if names match */ 
            if (strcmp (ptr->s name, name) == 0)
                return ptr;
    /* search falls, return NULL */ 
    return (struct symtab *) 0;
}

/*
 *  interface for lexical analyzer:
 *  locate or enter Identifier, save text of Constant
 */
s_lookup (yylex)
    int yylex;          /* Constant or Identifier */ 
{
    extern char yytext[];   /* text of symbol */ 
    
    switch (yylex) { 
        case Constant:
            yylval.y_str = strsave (yytext);
            break;
        case Identifier:
            if (yylval.y_sya = s_find (yytext))
                break; 
            yylval.y_syn = s_create(yytext);
            break; 
        default:
            bug("s_lookup");
    }
}

/* 
 *
 */
struct symtab *link_parm (symbol, next)
    register struct symtab * symbol, * next;
{ 
    switch (symbol->s_type) { 
        case PARM:
            error("duplicate parameter %s", symbol->s_name);
            return next; 
        case FUNC: 
        case UFUNC: 
        case VAR:
            symbol = s_create (symbol->s_name); 
        case UDEC:
            break; 
        default:
            bug("link_parn");
    } 
    symbol->s_type = PARM;
    symbol->s_blknum = blknum; 
    symbol->s_plist = next; 
    return symbol;
}

/*
 *  declare a parameter
 */
struct symtab * make parn(symbol)
    register struct symtab * symbol;
{
    switch (symbol->s_type) {
        case VAR: 
            if (symbol->s_blknum == 2){
                error("parameter %s declared twice",
                    symbol->s name);
                return symbol;
            }
        case UDEC: 
        case FUNC: 
        case UFUNC:
            error("%s is not a parameter", Symbol->s name);
            symbol = s_create (symbol->s_name); 
        case PARM:
            break; 
        default:
            bug("nake_parm");
    }
    symbol->s type = VAR; 
    Symbol->s blkaum = blknum; 
    return symbol;
}

/*
 *  define & variable
 */
struct symtab * make_var (symbol)
    register struct symtab * symbol; 
{
    switch (symbol->s_type) { 
        case VAR: 
        case FUNC: 
        case UFUNC:
            if (symbol->s_blknum =- blknum || symbol->s_blknum == 2 && biknum == 3)
                error("duplicate name %s", symbol->s_name); 
            symbol = s_create (symbol->s_name); 
        case UDEC:
            break; 
        case PARM:
            error("unexpected parameter %s", symbol->s_name);
            break; 
        default:
            bug('make_var'):
    }
    symbol->s_type = VAR;
    symbol->s_blknum = blknum; 
    return symbol;
}

/*
 *  define a function
 */
struct symtab * make_func(symbol)
    register struct symtab * symbol;
{
    switch (symbol->s_type) { 
        case UFUNC: 
        case UDEC:
            break; 
        case VAR: 
            error("function name %s same as global variable", symbol->s_name); 
            return symbol; 
        case FUNC: 
            error("duplicate function definition %s", symbol->s_name); 
            return symbol; 
        default:
            bug("make_func");
    }
    symbol->s_type = FUNC;
    symbol->s_blknum = 1; 
    return symbol;
}

/* 
 *  set or verify number of parameters
 */
chk_parm (symbol, count)
    register struct symtab * symbol;
    register int count;
{
    if (symbol->s poup == NOT_SET)
        symbol->s poum = count; 
    else if ((int) symbol->s poum != count) 
        warning("function %s should have %d argument(s)", symbol->s name, symbol->s poum):
}

/*
 * default undeclared parameters, count
 */
int part_default(symbol)
    register struct symtab * symbol; 
{
    register int count = 0;

    while (symbol){
        ++ count; 
        if (symbol->s type == PARM)
            symbol->s type = VAR; 
        symbol = symbol->s_plist;
    }
    return count;
}

/* 
 *  pop block table
 */

blk_pop()
{
    register struct symtab * ptr;

    for (ptr = s_lcl->s_next;
        ptr && (ptr->s_blknum >= blknum || ptr = sblknum == 0); ptr = s_lcl->s next)
    {
        if (1 ptr->s_name)
            bug("blk pop null name");
#ifdet TRACE
    {
        static char * type [] = { SYMmap };
.
        message ("Popping %s: %s, depth %d, offset %d",
            ptr->s_name, type [ptr->s_type],
            ptr->s_blknum, ptr->s_offset):
    }
#endif TRACE

        if (ptr->s type == UFUNC)
            error("undefined function %s", ptr->s name);
        ctree (ptr->s name); 
        s_lcl->s_next = ptr->s_next; 
        cfree (ptr);
    }
    -- blknum;
}

/*
 *  check reference or assignment to variable
 */
chk_var (symbol)
    register struct symtab * symbol; 
{
    switch (symbol->s type) {
        case UDEC:
            error("undeclared variable %s", symbol->s_name);
            break; 
        case PARM:
            error("unexpected parameter %s", symbol->s namo).
            break;
        case FUNC:
        case UFUNC:
            error("function %s used as variable", symbol->s_name);
        case VAR:
            return;
        default:
            bug("check_var");
    }
    symbol->s_type = VAR;
    symbol->s_blkoum = blkaum;
}

/*
 *  check reference to function, implicitly declare it
 */
chk_func(symbol)
    register struct symtab * symbol; 
{
    switch (symbol->s_type) { 
        case UDEC:
            break; 
        case PARM:
            error("unexpected parameter %s", symbol->s_name);
            symbol->s poum = NOT SET;
            return; 
        case VAR: 
            error("variable %s used as function", symbol->s name);
            symbol->s_pnum = NOT_SET; 
        case UFUNC: 
        case FUNC:
            return; 
        default:
            bug('check_func');
    }
    s_move (symbol); 
    symbol->s_type = UFUNC; 
    symbol->s_blknum = 1;
}
