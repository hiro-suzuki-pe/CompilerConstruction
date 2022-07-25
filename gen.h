/*
 *   sample c -- header file for code generation
 */

/*
 *  operation codes for psendo machine
 */

#define OP_ALU "alu"        /* arithmetic-logic-op */
#define OP_DEC "dee"        /* region offset */
#define OP_INC "inc"        /* region offset */
#define OP_LOAD "load"      /* region offset  */
#define OP_STORE "store"    /* region offset */
#define OP_POP_"pop" 
#define OP_JUMPZ "jump"     /* label */
#define OP_JUMP "jump"      /* label */
#define OP_CALL "call"      /* parn-count, address */
#define OP_ENTRY "entry"    /* local-frame-size */
#define OP_RETURN "return"

#define MOD_GLOBAL "gb1"    /* global region */
#define MOD_PARAM "par"     /* parameter region */
#define MOD_LOCAL "lel"     /* local region */
#define MOD_IMMED    "con"   /* load only: Constant */

/* 
 *  OP_ALU modifiers
 */
#define ALU ADD "+"     /* addition */
#define ALU SUB "-"     /* subtraction */
#define ALU MUL "*"     /* multiplication */
#define ALU DIV "/"     /* division */
#define ALU MOD "%"     /* remainder */
#define ALU LT  "<"     /* compares as: < */
#define ALU GT  ">"     /*              > */
#define ALU_LE  "<="    /*              <= */
#define ALU GE  ">="    /*              >= */
#define ALU EQ  "=="    /*              == */
#define ALU NE  "!="    /*              != */ 
#define ALU AND "&"     /* bit-wise and */
#define ALU OR  "|"     /* bit-wise or */
#define ALU XOR "^"     /* bit-vise excl. or */

/* 
 *  typed functions, code generator
 */
char * gen_mod();       /* region modifier */
