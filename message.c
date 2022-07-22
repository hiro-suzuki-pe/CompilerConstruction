/*
 *  message routines
 */
#include <stdio.h>

#define VARARG  fmt, v1, v2, v3, v4, v5
#define VARPARM (VARARG)    char *fmt;

extern FILE*yyerfp;

                         /*VARARGS1*/
message VARPARM{
    yywhere();
    fprintf(yyerfp, VARARG);
    putc('\n', yyerfp);
}

                         /*VARARGS1*/
error VARPARM{
    extern int yyerrs;

    fprintf(yyerfp, "[error %d] ", ++yynerrs);
    message(VARARG);
}

                         /*VARARGS1*/
warning VARPARM{
    fputs("[warning] ", yyerfp);
    message(VARARG);
}
                         /*VARARGS1*/
fatal VARPARM{
    fputs("[fatal error] ", yyerfp);
    message(VARARG);
    exit(1);
}

                         /*VARARGS1*/
bug VARPARM{
    fputs("BUG: ", yyerfp);
    message(VARARG);
    exit(1);
}

char *strsave(s)
    register char *s;
{
    register char *cp = calloc(strlen(s) + 1, 1);

    if (cp) {
        strcpy(cp, s);
        return cp;
    }
    fatal("No more room to save strings.");
}