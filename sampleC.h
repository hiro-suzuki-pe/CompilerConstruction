/*
 *  sample C -- header file
 */


#define    GE      1
#define    LE      2
#define    EQ      3
#define    NE      4
#define    PE      5
#define    ME      6
#define    TE      7
#define    DE      8
#define    RE      9
#define    PP      10
#define    MM      11

#define    BREAK     101
#define    CONTINUE     102 
#define    ELSE      103
#define    IF      104
#define    INT      105
#define    RETURN      106
#define    WHILE      107

#define    Constant     201
#define    Identifier   202

void s_lookup(int yylex);
