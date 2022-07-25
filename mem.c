/*
 *  sample c -- memory allocation
 */
#include "symtab.h"

/*
 *  global counters
 */


int g_offset = 1,   /* offset in global region */ 
    l_offset = 0,   /* ofiset in local region */ 
    l_max;          /* size ot local region */

/*
 *  allocate a (global or local) variable
 */
all_var (symbol)
    register struct symtab * symbol; 
{
    extern struct symtab *make_var();
    
    symbol = make_var (symbol); 
    
    /* if not in parameter region, assiga suitable ofiset */
    switch (symbol->s_blknum) { 
        default:    /* local region */ 
            symbol->s_offset = l_offset++; 
        case 2:     /* parameter region */ 
            break; 
        case 1:     /* global region */ 
            symbol->s_offset = g_offset++;
            break; 
        case 0:
            bug("all_var");
    }
}

/*
 *  complete allocation
 */
all_program ()
{
    blk_pop(); 
    
#ifdef TRACE
    message ("global region has %d word(s)", g_offset); 
#endif
}

/*
 *  allocate all parameters
 */
all_parm (symbol)
    register struct symtab * symbol; 
{
    register int p_offset = 0; 
    
    while (symbol){
        symbol->s_offset = p_offset ++;
        symbol = symbol->s_plist;
    }

#ifdef  TRACE
    message("parameter region has %d word(s)", p_offset);
#endif
}

/*
 *  complete allocation of a function
 */
all_func(symbol)
    struct symtab * symbol;
{
    blk_pop();

#ifdef  TRACE
    message("local region has %d word(s)", l_max);
#endif
}
