/*
 *  sample.c -- code generator for simulator
 */
#include <stdlib.h>
#include "sim.h"
#include "symtab.h"

/*
 *  program memory
 */
struct prog prog[L_PROG];
static int pc = 1;      /* current program counter / 
                        /* HALT () is at adress o */

/*
 *  generate a single instruction
 */
int gen(op, mod, val, comment)
    int op;     /* operation code */ 
    int mod;    /* modifier */ 
    int val;    /* offset field */ 
    char * comment; /* instruction comment */
{
    if (pc >= DIM(prog))
        fatal("Not enough program memory.");
        prog[pc].p_op = op;
        prog[pc].p_mod = mod;
        prog[pc].p_val = val;
        printf("%d:\t%d\t%d,%d\t; %s\n", pc, op, mod, val, comment);
        return pc++;
}
    
char * gen_mod (symbol)
    struct symtab *symbol; 
{
    switch (symbol->s_blknum) {
        case 1:
            return MOD_GLOBAL;
        case 2:
            return MOD_PARAM;
    }
    return MOD_LOCAL;
}

/*
 *  generate  instructions
 */
gen_alu(mod, comment)
    char * mod;         /* mnemonic modifier */ 
    char * comment;     /* instruction comment */
{
    gen(OP_ALU, mod, 0, comment);
}

gen_li(constx)
   char *constx;        /* Constant value */
{
    gen(OP_LOAD, MOD_IMMED, atoi(constx), constx);
}

gen_pr(op, comment)
    char *op;           /* memonie operation code */
    char *comment;      /* instruction comment */
{
    gen(op, 0, 0, comment);
}
   
/* 
 *  generate jumps, return target
 */
int gen_jump(op, label, comment)
    char *op;           /* memonie operation code */
    int label;          /* target of jump */
    char *comment;      /* instruction comment */
{
    int pc = gen(op, 0, label, comment);

    if (label <= 0)
        return -pc;     /* new head of chain */
    else   
        return label;   /* already defined */
}

/*
 *  gerate tail of forward branch chain
 */
int new_label()
{
    return 0;       /* end of chain */
}

/*
 *  resolve forward branch chain
 */
int gen_label(chain)
    int chain;
{
    int next;
    while (chain <0)
    {
        chain = -chain;
        next = prog[chain].p_val;
        if (next > 0)
            break;      /* already ok */
        prog[chain].p_val = pc;
        printf("%d:\t(fixup)\t%d\n", chain, pc);
        chain = next;
    }
    return pc;
}

/* 
 * label stack manager
 */
static struct bc_stack { 
    int bc_label;       /* label from new_label */ 
    struct bc_stack *bc_next;
    } *b_top,           /* head of break stack */
      *c_top;           /* head of continue stack */

static struct bc_stack *push (stack, label)
    struct bc_stack * stack;
    int label; 
{
    struct bc_stack *new_entry = (struct bc_stack *)
        calloc(1, sizeof (struct bc_stack));
    if (new_entry){
        new_entry->bc_next = stack; 
        new_entry->bc_label = label; 
        return new_entry;
    }
    fatal ("No more room to compile loops."); 
    /*NOTREACHED */
}

static struct be_stack *pop (stack)
    struct bc_stack * stack;
{
    struct bc_stack *old_entry;

    if (stack){
        old_entry = stack;
        stack = old_entry->bc_next;
        cfree (old_entry);
        return stack;
     }
     bug("break/continue stack undertlow");
      /*NOTREACHED */
}

static int *top(stack)
    struct bc_stack **stack;
{
    if (!stack){
        error("no loop open"); 
        *stack = push(*stack, 0);
    }
    return &(*stack)->bc_label;
}

/*
 *  BREAK and CONTINUE
 */
push_continue (label)
    int label;
{
    c_top = push (c_top, label);
}

push_break (label)
    int label;
{
    b_top = push (b_top, label);
}


gen_break ()
{
    *top(&b_top) = gen_jump (OP_JUMP, *top (&b_top), "BREAK");
}


gen_continue()
{
    *top(&c_top) = gen_jump (OP_JUMP, *top(&c_top), "CONTINUE");
}

pop_break ()
{
    gen_label(*top(&b_top));
    b_top = pop (b_top);
}

pop_continue ()
{
    gen_label(*top(&c_top));
    c_top = pop (c_top);
}

/*
 *  function call
 */
gen_call(symbol, count)
    struct symtab *symbol;  /* function /
    int count;              /* # of arguments */
{
    int pc;

    chk_parm(symbol, count);
    pc = gen(OP_CALL, count, symbol->s_offset, symbol->s_name);
    if (symbol->s_offset <= 0)
    symbol->s_offset = -pc;     /* head of chain */
    while (count--> 0)
        gen_pr(OP_POP, "pop argument");
    gen (OP_LOAD, MOD_GLOBAL, 0, "push result");
}


/*
 * function prologue and definition
 */
int gen_entry(symbol)
    struct symtab * symbol;     /* function */ 
{
    symbol->s_offset = gen_label(symbol->s_offset);
    gen(OP_ENTRY, 0, 0, symbol->s_name);
    return symbol->s_offset;
}

fix_entry(symbol, label)
    struct symtab * symbol;     /* function */ 
    int label; 
{
    extern int l_max;           /* size of local region */

    prog[label].p_val = l_max;
    printf("%d:\tentry\t%d\t; %s\n", label, l_max, symbol->s_name);
}

/*
 *  wrap-up
 */
end_program ()
{
    extern int g_offset;    /* size of global region */ 
    extern struct symtab *s_find();
    int main = s_find("main")->s_offset;

    all_program();            /* allocate global variables */ 
    printf("%d\tend\t%d,%d\n", pc, g_offset, main);
    simulate(pc, g_offset, main);
}
