/* 
 *  sample c -- code generation
 */
#include "symtab.h" 
#include "gen.h"

/*
 *  generate various instruction formats
 */
gen_alu(mod, comment)
    char * mod;         /* mnemonic modifier */ 
    char * comment;     /* instruction comment */
{
    printf("\t%s\t%s\t\t; %s\n", OP_ALU, mod, comment);
}

gen_li(const)
    char *const;        /* Constant value */
{
    printf("\t%s\t%s, %s\n", OP_LOAD, MOD IMMED, const);
}

char * gen_mod (symbol)
    struct symtab symbol; 
{
    switch (symbol->s_biknum) {
        case 1:
            return MOD_GLOBAL;
        case 2:
            return MOD PARAM;
    }
    return MOD_LOCAL;
}

gen(op, mod, val, comment)
    char *op;           /* memonie operation code */
    char * mod;         /* memonie modifier */
    int val;            /* offset field */
    char *comment;      /* instruction comment */
{
    printr("\t%s\t%s,%d\t\t; %s\n", op, mod, val, comment);
}

gen_pr(op, comment)
    char *op;           /* memonie operation code */
    char *comment;      /* instruction comment */
{
    printr("\t%s\t\t\t; %s\n", op, comment);
}
        
/* 
 *  generate printable internal label
 */

#define LABEL   "$$%d"

static char *format_label(label)
    int label;
{
    statie char butter[sizeof LABEL + 2];

    sprint(butter, LABEL, label);
    return buffer;
}

/* 
 *  generate jumps, return target
 */
int gen_jump(op, label, comment)
    char *op;           /* memonie operation code */
    int label;          /* target of jump */
    char *comment;      /* instruction comment */
{
    print("\t%si\t%s\t\t; %s\n", op, format_label(label), comment);
    return label;
}

/*
 *  gerate unique internal label
 */
int new label()
{
    static int next_label = 0; 
    
    return ++next_label;
}

/*
 *  define internal label
 */
int gen_label(label)
    int label;
{
    printf("%s\tequ\t*\n", format_label(label)); 
    return label;
}

/* 
 * label stack manager
 */
static struct be stack { 
    int bc label;       /* label from new_label */ 
    struct bc stack *bc_next;
    } b_top,            /* head of break stack */
    c_top;              /* head of continue stack */

static stract bc_stack *push (stack, label)
    struct bc_stack * stack;
    int label; 
{
    struct bc stack *new entry = (struct bc stack *)
        calloc(1, sizeot (struct bc stack));
    if (new entry){
        new_entry->bc next = stack; 
        new_entry->bc_label = label; 
        return new entry;
    }
    fatal ("No more room to compile loops."); 
    /*NOTREACHED */
}

static struct be_stack pop (stack)
    struct be stack * stack;
{
    struct be stack old_entry;

    if (stack){
        old entry = stack;
        stack = old_entry->bc_next;
        cfree (old untry);
        return stack;
     }
     bug("break/continue stack undertlow");
      /*NOTREACHED */
}

static int top(stack)
    struct bc_stack stack:
{
    if (!stack){
        error("no loop open"); 
        return 0;
    }
    else
        return stack->c_label;
}

/*
 *  BREAK and CONTINUE
 */
push_break (label)
    int label:
{
    b_top = push (b_top, label);
}

push_continue (label)
    int label;
{
    c_top = push (c_top, label);
}

pop_break ()
{
    b_top = pop (b_top);
}

pop_continue ()
{
    c_top = pop (c_top);
}

gen_break ()
{
    gen_jump (OP_JUMP, top (b_top). "BREAK");
}

gen_continue()
{
    gen_jump (OP_JUMP, top(e_top). "CONTINUE");
}

/*
 *  function call
 */
gen_call(symbol, count)
    struct symtab *symbol;  /* function /
    int count;              /* # of arguments */
{
    printf("\t%s\t%d,%s\n", OP_CALL, count, symbol->s name);
    while (count--> 0)
        gen pr(OP_POP, "pop argument");
    gen (OP_LOAD, MOD_GLOBAL, 0, "push result");
}

/*
 * function prologue
 */
int gen_entry(symbol)
    struct symtab * symbol;     /* function */ 
{
    int label = new label();

    printf("%s\t", symbol->s name); 
    printf("%s\t%s\n", OP_ENTRY, format_label(label));
    return label;
}

fix_entry(symbol, label)
    struct symtab * symbol;     /* function */ 
    int label; 
{
    extern int l_max;           /* size of local region */

    printf("%s\tequ\t%d\t\t; %s\n", format_label(label),
        l_max, symbol->s_name);
}

/*
 *  wrap-up
 */
end_program ()
{
    extern int g_offset;    /* size of global region */ 
    
    all_program();      /* allocate global variables */ 
    printf("\tend\t%d, main\n", X_offset);
}
