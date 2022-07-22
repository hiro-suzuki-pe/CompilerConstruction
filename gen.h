
#endir
message ('local region has $d vord(s). 1 max):
Code generation
includes the mnemonies for symbolic code generation required in the
mpilation of samplec.y.
sample c -- header file for code generation
operation codes for psendo machine
/* arithmetic-logic-op /* region offset /* region offset /* region offset /* region offset
#define OP ALU alu" #define OP DEC 'dee #define OP INC 'ine #define OP LOAD "load" #define OP STORE 'store" #define OP POP "pop" #define OP JUMPZ "jump" #define OP JUMP "jump" #define OP CALL *call #define OP ENTRY "entry" #define OP RETURN 'return
/* label 1 label
/* parn-count, address * local-frame-size
region modifiers
#define MOD GLOBAL *gb1" #define MOD PARAM 'par' #define HOD LOCAL "iel" #define NOD INED *con"
global region /parameter region /* local region * load only: Constant


APPENDIX A
COMPILER CONSTRUCTION
168
OP ALU modifiers
/* addition /* subtraction
* multiplication /* division /* remainder /* compares as: <
#define ALU ADD #define ALU SUB #define ALU MUL #define ALU DIV #define ALU MOD #define ALU LT #define ALU GT #define ALU_LE #define ALU GE #define ALU EQ #define ALU NE
"!= #define ALU AND "X" #define ALU OR I #define ALU XOR
=>
<= ·>='
/* bit-wise and /* bit-wise or /* bit-vise excl. or
typed functions, code generator
char * gen_mod();
/* region modifier */ File gen.c includes the C functions which generate assembler code for our ficti tious sampleC machine. These routines were introduced in chapter 7. gen.c
sample c -- code generation
#include 'symtab.b #include 'gen.no
generate various instruction formats
*/
gen_alu(mod, comment)
char * mod; char * comment;
/* mnemonic modifier */ /* instruction comment */
printf("\t%s\t%s\t\t; %s\n", OP_ALU, mod, comment);
gen_if(const)
char † const;
/* Constant value */
printf("\t%s\t%s, %s\n", OP_LOAD, MOD IMMED, const);
char * gen_mod (symbol)
struct symtab symbol; svitet (symbol->s_biknum) {


Samplec" COMPILER LISTING
169
Petera NOD_GLOBAL return NOD PARAN
return NOP_LOCAL
d. val. comment
ar * mod:
memonie operation code memonie modifier ottset tiela / instruction comment
10
.
00: Isla'. op. mod. Tal comment:
mnemonie operation code instruction comment /
printf("\ts10if: sia', op. comment):
generate printable internal label
metine LABEL. *$$$4* statie ear format_label(label)
statie car butter(sizeot LABEL. 9: spriatt(etter. Lase, Isha): ratura butter:
generate jemps, retara target
int gua_jup(op. label. comment ebar + op:
m onte operation code at label:
target of jup char comment:
instruction comment print("\tsiusitle: la". ep. terut a del).
comment): return 1:
gerate unique internal label


APPENDIX A
COMPILER CONSTRUCTION
170
int new label
static int next_label = 0; return ++Bext_label;
define internal label
int gen label(label)
int label;
printf("%s\tequ\t*\n', format_label(label)); return label;
label stack manager
static struct be stack { int bc label;
/* label from new_label */ struct bc stack + bc next; } _top.
/* head of break stack *) • c_top:
/* head of continue stack */
static stract bc stack # push (stack, label)
struct bc stack * stack: int label; struct bc stack # new entry = (struct bc stack #
calloc(1, sizeot (struct bc stack));
1! (new entry)
nev_entry->bc next = stack; Dev_entry->bc_label = label; return new entry:
tatal ("No more room to compile loops.); /*NOTREACHED */
statie struct be_stack pop (stack)
struct be stack * stack: struct be stack old_entry:
1t (stack)
old entry = stack: stack = old_entry->bc_next: cfree (old untry):
return stack: bug('break/continue stack undertlov"); /*NOTREACHED /


 ________________________________________
PPENDIX A
static int top(stack)
"amplec* COMPILER LISTING
171
struct bc_stack stack:
if (1 stack)
error("no loop open"); return 0;
else
return stack->c_label;
BREAK and CONTINUE
push_break (label)
int label:
b_top = push (b_top. label);
push_continue (label)
int label;
c_top = push (c_top, label):
pop_break ()
b_top = pop (b_top);
pop_continue ()
c_top = pop (c_top);
gen_break ()
gen_jump (OP_JUMP, top (b_top). "BREAK");
gen_continue()
gen_jump (OP_JUMP, top(e_top). "CONTINUE");
function call
gen_call(symbol, count)
symbol; /* function /
/* # of arguments */
struct symtab
int count;
onnt):



APPENDIX A
COMPILER CONSTRUCTION
172
printf("\t%s\t%a,%s\n", OP_CALL, count, symbol->s Dame). while (count--> 0)
gen pr(OP_POP. "pop argument'); gen (OP_LOAD, MOD_GLOBAL, 0, "push result');
function prologue
int gen_entry(symbol)
struct symtab * symbol; /* function */ int label = new label();
printf("%s\t", symbol->s Dame); printf("%s\t%s\n", OP_ENTRY, format_label(label)). return label;
fix_entry(symbol, label)
struct symtab * symbol; /* function */ int label; extern int 1_mas;
/* size of local region */
printf("%s\tequ\t%d\t\t; %s\", format_label(label).
1_max, symbol->s_name):
wrap-up
end program ()
extern int g_offset; /* size of global region */ all_program();
/* allocate global variables */ printf("\tend\t%d, main\n', X_offset):
The code generator for our fictitious machine language can be compiled using the following command: cc gen.c mem.c symtab.c message.c y. tab.c lex.yy.cl
colib -if -o gen
colib contains the yace support functions explained in section A.8.
and-go
A.7 A load-and-go system
File sim.h replaces gen.h in the compilation of samplec.v to make the load version of the sample compiler. It defines the same mnemonics as gen. them definitions needed by the simulator.
aics as gen.h, but gives






APPENDIX A
"samplec" COMPILER LISTING
173
.
sample
-- header file for simulation
operation codes for pseudo maching
#define OP ALU #define OP DEC #define OP INC a #define OP LOAD #define OP STORE 5 #define OP POP 6
A QON
de tine OP JUMPZ 7 detine OP_JUMP 8
ne OP CALL 9
Ana OP ENTRY 10 #define OP_RETURN if
/* alu
arithmetic-logic-op /* dec
region,ottset /* inc
region,otiset 7* load region,otiset /* store region ,otiset /* pop /* jumpz label /* jump label /* call routine-address /* entry local-frame-size /* return
region modifiers
#define MOD GLOBAL 1 #define MOD PARAM 2 #define MOD LOCAL #define MOD IMMED 4
/* global region /* parameter region /* local region /* load only: Constant */
/*
OP_ALU modifiers
addition /* subtraction /* multiplication /* division /* remainder /* compares as: <
VA
#define ALU ADD 1 #define ALU SUB 2 #define ALU MUL 3 #define ALU DIV 4 #define ALU MOD 6 #define ALU LT 6 #define ALU GT 7 #define ALU LE 8 #define ALU GE 9 #define ALU EQ 10 #define ALU NE #define ALU AND 12 #define ALU OR 13 #define ALU XOR 14
! !
/* bit-wise and /* bit-wise or /* bit-wise excl. or
/*
program memory structure
struct prog {
/* operation code */
short p_op:



COMPILER CONSTRUCTION
174
/* modifier */ / oftest or other value /
short p mod, int P val:
tunable limits
#define L PROG 200 /* max. program size / #define L DATA 100 /* max. area for stack, etc. */
#define DIM(x) (sizeot x / sizeot x[0) / extent */ File sim.c contains the actual simulator for our fictitious machine in chapter 8
for our fictitious machine, as introduced
sim.c
sample c -- machine simulator
*/
#include "sim.h"
data and program memory
static int data(L DATA); extern struct prog prog[];
registers
static struct prog * inst; #define G static int P; static int L; static int T;
/* -> current instruction / /* global segment */ /* current parameter segment */ /* current local segment */ /* top of stack */
shorthand notations
#define TOP data (T-1]
/* right operand: top of stack */ #define NEXT data (T-2] /* left operand: below TOP */ #define PUSH data (T++]
/* new cell to come onto stack */ #define POP
/* -> discarded cell from stack */ #define MEMORY data[address] /* effectively addressed cell */ #define RESULT data (G1
/* result value of function */
address decoder
static int address() /* effective current data address */
register int ad;



sample
COMPLER LISTING
175
swite (inst-> case NOD GLOBAL
mod) *
case MOD_PARAM
ad = P:
break: case MOD_LOCAL:
ad = L:
break: default:
bug("invalid p_208):
s += inst-> Tal: 1+ (ad < 0 Il ad >= 1)
bug("invalid ettective address): return ad:
sinalator
niste (pe_limit, global. pe)
ist pe linit, global. pe: /* initialize */ 17 (global >= DIN(ata))
fatal ("Not enough room for global esta.): T = global + 2; printf("\nExecution begins... lala):
for (::)
/* fetch / 17 (pe < 0 Il pe >= pe linit)
bag("pe not in program area'): inst = {prog (pe++): /* decode operation and dispatch / svitch (inst->p_op) { default:
printf($4:\thaltla'. inst-prog):
return: case OP ALU: 17 (T <=L+1)
bag('simulator stack undertlov): svitch Ciast->p_mod) { default:
bug(*if1egal ALU instruction): case ALU ADD: NEXT += TOP: break; case ALU SUB: NEXT - TOP: break: case ALU MUL: NEXT = TOP: break; case ALU DIV: NEXT /= TOP: breat: case ALU MOD:
NEXT = TOP: breat: case ALU LT: NEXT = NEXT TOP: breat:


AP
APPENDIX A
COMPLER CONSTRUCTION
IT
• ALU GT: cade ALU LE
a ALU GE ose ALU EQ e ALU NE:
. ALV AND : R. ALU OR ea ALU XOR
T NIKT * TOP rss NEXT - NEXT TOP; break; NEXT NEXT TOP; break NEXT NEXT TOP: breu NEXT NEXT TOP: bresk: NEXT TOP; break; NEXT - TOP; break; NEXT TOP; break;
POP:
break; en OP LOAD: if (T > DIM(data))
fatal("Too much data."); 1t (inet->p mod MOD IMMED)
PUSH-inst->pval: olee
PUSH MEMORY: break; cas. OP STORE:
if ( TL)
bag(simulator stack undertlov): printf("%d\tatore\t%.%d\tto %d\n".
inst prog. inst->p mod.
inst->p. val. MEMORY TOP): break; ens. OP INC: if (T > DIM(data))
fatal("Too much data."): printf("%d\tinc\t%.Xd\tto %d\n".
inst-prog. inst->p_mod.
inst->p.val, PUSH = ++ MEMORY): break;
case OP DEC: 17 (T > DIM(data))
tatal ("Too much data."): printf("%d\tdec\t%d. \tto %d\n".
inst prog. inst->p mod.
inut->p. val. PUSH -- MEMORY): break: cu OP POP
if ( TL)
bug("simulator stack underflow"); POP:
break: en OP JUMP: printf("%d\tjump\u\n", inst-prog.
inst->p.al): pe inst->p.val;
break; N. OP JUMPZ:
if ( TL)
bug("simulator stack undertlow) it (data (POP) - 0)
printf("%d\jump\u\n".



wop of de
A
PENDIX A
DX
A
"samplec" COMPILER LISTINO
177
):
inst-prog, inst i pc = inst-> val; break; case OP CALL: printt(*%:\tcall\ **, instprop
inst->p.val); PUSH = pe: pc = inst->p val; PUSH = P: P = T - 2 - insta ;
break; case OP ENTRY:
PUSH = L; L = T: T = inst-> val; if (T Y= DIM (data))
tatal("Too much lata."); break;
case OP RETURN: if (1 <L)
bug('simulator stack undertlo) T = L; L = data (POP): P = data (POP): pc = data (POP): print: 8:\treturn to to
inst-prog. RESULT, p.): break;
The code generation routines in simgen.c replace the corresponding routines in file and section A6
szaplec -- cote generator for simulator
#include *sin.be
progra resory */ struct prog progIL PROG): static int pe = 1;
/* current program conster / /* FALT () is at adress o */
generate a single instruction
Ist es (op. mod.
, commest)


APPENDIX A
COMPILER CONSTRUCTION
178
int op: int mod: int val: char * comment:
/operation code / /* modifier / /* offset field */ /* instruction comment */
** (pe >= DIN (prog))
fatal (Not enough program memory."): prog (pe).p op = op: prog[pe).p mod = mod: prog (pe).P ral = val; printr($:\t \c54.\t: %s\n".
pe, op. mod. val. comment): return pe ++:
***
region modifier
int gen mod (symbol)
struet syutab * symbol:
switch (symbol->s biknum) { case 1:
return MOD_GLOBAL: case 2:
return MOD PARAN: return MOD_LOCAL;
general instructions
gen alu(mod, comment)
int mod; char * comment:
/* modifier */ /* instruction comment /
gen (OP_ALU, mod, o, comment):
gen_if(const) char * const:
/* Constant value */ gen (OP_LOAD, MOD_IMMED. stoi(const). const):
gen_pr (op. coment) int op:
/* operation code / char comment:
/* instruction comment / gen (op. o. o. comment):



SZL-96SDA
APPENDIX A
XZ168708 WISZ OS# 4*
"sampleC" COMPILER LISTING
179
generate jump. return target or chain
int gen_jump (op. label, comment) int op:
/* operation code */ int label;
/* target of jump */ char * comment:
/* instruction comment */ int pe = gen (op. O, label, comment);
if (label <= 0)
return -pc;
pc:
else
etur
/* new head of chain */
return label;
/* already defined */
*/
generate tail of forward branch chain
int new label
return 0;
/* end of chain */
resolve forward branch chain
int gen label (chain)
int chain; int next;
while (chain < 0)
chain = - chain; next = prog (chain).p_val; if (next > 0)
break; /* already ok */ prog[chain).pval = pc; printf("%d:\t(fixup) it%d\n". chain, po); chain = next;
return pc;
label stack manager
static struct bc stack { int bc label;
/* label from new_label / struct bc stack # be next; ) .
/* head of break stack */ _top.
/* head of continue stack */ * c top:
static struct bc stack # push (stack, label)
struct bc stack * stack;
020


APPENDIX A
COMPILER CONSTRUCTION
180
int label; struct be stack * new entry = (struct bc stack *)
calloc(1, sizeof (struct bc stack)):
if (new entry)
new entry->be next = stack; new entry->bc label = label; return new_entry:
fatal ("No more room to compile loops."); /*NOTREACHED*/
static struct bc_stack # pop (stack)
struct bc stack * stack; struct bc stack * old entry:
if (stack)
old_entry = stack; stack = old entry->bc next; cfree (old_entry); return stack;
bug (break/continue stack underflow"); /*NOTREACHEDU/
static int * top(stack)
struct bc stack ** stack;
if (1 *stack)
error('no loop open'); *stack = push(*stack, 0);
return & (*stack) -> bc_label;
BREAK and CONTINUE
push continue (label)
int label;
c_top= push(c_top. label);
push break (label)
int label:
_top = push()_top. label):
*** break
*top (@b_top) genJump (OP_JUMP, *top (@b_top). "BREAK");


APPENDIX A
sampleC" COMPLER LISTING
181
er_continue o
*top (ke_top) = gen_jump (OP_JUMP, top (Xc_top). CONTINUE"):
pop breato
gen_label (*top (@b_top)): b_top = pop(_top):
POP_continue
gen_label("top(ke_top)): c_top = pop (c_top);
function call
gen_call(syabol, count)
struct syatab * sabol; /* fonction / int count:
/* # of arguments */ int pe:
chk_pan(syabol, count): pc = gen (OP_CALL, count, syabol->s offset, syabol->s baze): 1(symbol->s offset <= 0)
symbol->softset = -pe; /* head of chais / while (count--> 0)
gen pr(OP POP POP argument'); gen (OP_LOAD, MOD_GLOBAL, 0, 'push result');
function prologue and definition
int gen_entry(syabol)
struct syatab syabol; /* function / syabol->s_offset = gen_label'syabol->s_offset): gen (OP ENTRY, 0, 0. symbol->s_name): return syabol->s offset;
fix_entry(symbol, label)
struct syntab symbol; /* function / int label; extern int I max; /* size of local region */
prog[label].p_val = 1_mar; printf("%d\tentry\t\t; $s\a'.



182
COMPILER CONSTRUCTION
APPENDIX A
label, 1_max, symbol->s_name):
wrap-up
end program ()
extern int g offset; /* size of global region */ extern struct symtab * s find (: int main = s_find('main') -> s_offset;
.
all_program():
/* allocate global variables */ printf("%d:\tend\t%d.%d\n", pc, & offset, main); simulate (pc, & offset, main);
A load-and-go compiler and simulator for our fictitious machine can be compiled using the following command: cc sin.c simgen.c mem.c symtab.c message.c y.tab.c lex.yy.cl
colib -if -o sim
A.8 Improved error messages from "yaccpar"
The error messages issued by a yace parser can be improved. yace builds a parser using a model stored in the file /usr/lib/yaccpar. The normal behavior of yaccpar in case of an error is:
switch (gyerrflag) { case 0:
/* brand new error */ yyerror("syntax error'); yyerrlab:
**yguerrs; case 1: case 2:
7* incompletely recovered */
/* ... try again */ Jyerrflag = 3;
The switch is reached if the transition matrix contains the error operation. At this point the message syntax error is issued, regardless of the actual nature of the error.
yyerrflag is used to avoid cascades of messages. It is normally zero. If there is an error, it is set to three. During each shift operation, yyerrflag is counted down. An error message is only issued if yyerrflag is zero.
We can attempt to improve the error message as follows: in the current state, we should consider all possible terminal symbols in place of the actual input, to discover those for which the transition matrix does not contain the error action. A suitably formatted list of these symbols can be issued in place of the simple syntax error mes
sage.
input:
/usr/lib/yacepar is always available in source. We compute the desired list of symbols essentially by copying the algorithm employed by the model parser for actual



09.
ap-di-ud-pi
1909
LORSSZL-966
APPENDIX A
Weusaz og'WXZT
svitch (errslag) {
case 0:
"sampleC" COMPILER LISTING
183
/* brand new error */ 1t (lyyn = yypact[yystate]) > YYFLAG & y < TILAST)
register int i;
for (x = yya>0? yy: 0; I < YYLAST: ++1)
if (yychk(yyact[x]] == x - yo 2 x - yy != YYERRCODE)
yyerror(0, yydisplay(3-y)):
yyerror(0,0);
Jyerrlab:
++yynerTs;
yystate is the as yyact[], and yy
rate is the current state, YYLAST is the extent of the transition matrix encoded -if and yyin is a temporary variable used by yaccpar. yypact I) is a filter to de certain simple cases. This information is provided by yace. Tee are in a situation where yyact [] is actually inspected, we let x range over all
ible indices into yyact(). These must at least be zero, and the algorithm in Croer shows that they must also exceed yypact (yystate). If we find a value for x Chich does not correspond to the error symbol YYERRCODE and which would not produce an error operation from yyact [], we display the corresponding input symbol.
If a list of possible input symbols can be computed in this fashion, yacepar issues
3 call
yyerror(0,1) for each terminal symbol t in the list; t is represented as a character string. The end of the list is indicated by a call
Jyerror(0,0) This call is also issued if the list cannot be computed. yyerror() can be extended as follows to produce improved error messages:
yyerror() -- [detailed) error message for
parse
#include <stdio.b>
/* error stream */
FILE + gyerfp = stdout;
/*VARARGS1*/ yJerror(s, t)
/* *message' or 0,"token' */ register char * s, * t; extern int yyuerrs,
/* total number of errors */ static int list = 0; /* sequential calls */ if (s II ! ifst)
/* header necessary?? */ fprintf(yertp. [error $4] . Terrs+1); Jywhere: 1(s)
/* simple message?? */ fputs(s. yyerip):



184
COMPILER CONSTRUCTION
APPENDIX A
if (6)
pute('\n', yyerfp): return;
/. first token? / fpute("expecting: . yyerfp): fputs (t, yyerfp): list = 1; return;
/* no tokens acceptable / fputs (syntax error\n". yyerfp): return;
/
/* subsequent token?t pute(' '. yyerfp): fputs(t. yyertp): return;
/* end of enhanced message */
pute('\n'. yyerfp); list = 0;
The implementation of the new error message expecting: symbols in yyerror() is quite simple. Basically we dispatch different calls depending on the presence of null pointers. The static variable list maintains its value between successive calls; it is used to distinguish the first terminal symbol in a series from the remaining symbols so that an appropriate message header may be issued.
In yacepar we have employed a function yydisplay() which must convert the encoding of a symbol as an integer into a printable string:
#include <etype.h> #define DIM(x) (sizeof / sizeof x[0])
static char yydisplay(ch)
register int ch; static char buf(16):
static char * token[] = { #include "y.tok...
/* token names 0 ):
/
svitch (ch) { case 0:
return (end of if0)': cas. YYERRCODE:
return [error]: case '\b":
return '\\b: cas. '\':
return *\\ cas. '\n':
return '\\": case \r*:
return '\\r case 'It':


"sampleC" COMPILER LISTING
185
return It'; if (ch > 256 ax ch < 256 + DIM(token))
return token[ch - 257]: 1t (isasci1(ch) a isprint(ch))
sprintr (buf. c .ch): else 1ť (ch < 256)
sprintf(buľ, "char %04.30", ch): else
sprintf(but, token %d", ch); return but;
If the symbol ch is a printable ASCII character or if it can be represented by a control character escape sequence, we show the corresponding C-style character constant. If the symbol value exceeds 256, it was normally defined by yace in reponse to a Stoken statement; in this case we try to decode it using an array token [] with appropriate character strings. If neither approach is possible, we return the numerical value. The entire function yydisplay() should be inserted near the beginning of the file /usr/lib/yaccpar; it can also be used to improve the output generated under the YYDEBUG option.
The following shell procedure can be used to construct the entries for the token() vector from the file y.tab.h:
grep '*#.*define' y.tab. ĭ sed 's/# define \([]*V) []*$/ 'if'./' > y.tok.
The strings are placed into a file y.tok.h. So that things work even if the file does not exist, an empty file /usr/include/y.tok.h should be present in the system to be included for yydisplay() as a default.
The new version of yyerror() shown here and the functions which were shown in section 3.3 are best collected into a library colib as follows:
cc -c main.c cpp.c yywhere.c yyerror.c ar r colib main.o cpp. yywhere.o yyerror.o ranlib colib
A.9 Improved debugging output from "yaccpar"
Debugging output from /usr/lib/yaccpar also can be improved. One fairly essential defect is the fact that some inputs are used and discarded before they are displayed. The basic architecture of yaccpar in this respect is as follows:
#17det YYDEBUG int yydebug = 0; #endit int yychar; /* current input token number */
parse()
register short yystate;
yychar = -1;


188
APPENDIX A
COMPILER CONSTRUCTION
/ put a state and value onto the stack
/
yyatack:
#ifdef YYDEBUG if (yydebug)
printr('state %d, char Oxo\n". yystate, yychar): #endir
18 (yychar < 0) /* lookahead available? if ((yyehar = yylex() < 0)
yyehar = 0; /* end of tif. if ( valid shirt */)
/ /
yychar = -1: goto yystack:
In order to improve things, we need to print each terminal symbol as soon as it is obtained from yylex(). Since the nested if statements above appear twice in yacepar, it is best to replace them by calling a new routine yyyylex() defined as follows: statie yyyylexo
if (yychar < 0) ( if (lyychar = yylex 0) < 0)
yychar = 0; *ifdet YYDEBUG if (yydebug)
printf("Cyydebug] reading X8\n". Wendir
yydisplay(yychar));
where we have used yydisplay() to construct a reasonably legible representation of the terminal symbol.
A.10 Regression testing
One should generally save the input files used to test the compiler during develop ment. If the compiler is changed, the test files can be run again to ascertain that at least some of the old features of the compiler are still functioning. The following Bourne shell script automates this testing procedure:




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
    register struct syntab * symbol; 
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

1.6 Memory allocation
Le C functions for memory allocation, introduced in chapter 6, are contained in
file mem.c.
mem.
sample c -- memory allocation
#include "symtab.b'
global counters



APPENDIX A
COMPILER CONSTRUCTION
166
*
/* offset in global region */ /* ofiset in local region */ /* size ot local region /
int
6_offset = 1, 1 otteet = 0, 1 max:
allocato a (global or local) variable
all var (symbol)
register struct symtab * symbol; extern struct symtab + make_var (:
symbol = make_var (symbol); /* if not in parameter region, assiga suitable ofiset / switch (symbol->s blknum) { default:
/* local region */ symbol->s_offset = 1_of1set++; case 2:
/* parameter region */ break; case 1:
/* global region */ symbol->s_offset = g_ottset++;
break; case 0:
bug('all_var");
complete allocation
al1_program ()
blk_pop(); #ifdet TRACE
message ("global region has %d word(s)", g_offset); #endir
allocate all parameters
all_parn (symbol)
register struct symtab * symbol; register int p ofiset = 0; while (symbol)
symbol->softset = poliset ++; symbol = symbol->s plist;


sampleC" COMPILER LISTING
16*
**rder
TRACE message paramet
parameter region has a word(s). P offset
complete allocation of a function
all tune(symbol)
struct syntab * symbol:
pix_pop O: Hirdet TRACE
#endir
message ('local region has $d vord(s). 1 max):
Code generation
includes the mnemonies for symbolic code generation required in the
mpilation of samplec.y.
sample c -- header file for code generation
operation codes for psendo machine
/* arithmetic-logic-op /* region offset /* region offset /* region offset /* region offset
#define OP ALU alu" #define OP DEC 'dee #define OP INC 'ine #define OP LOAD "load" #define OP STORE 'store" #define OP POP "pop" #define OP JUMPZ "jump" #define OP JUMP "jump" #define OP CALL *call #define OP ENTRY "entry" #define OP RETURN 'return
/* label 1 label
/* parn-count, address * local-frame-size
region modifiers
#define MOD GLOBAL *gb1" #define MOD PARAM 'par' #define HOD LOCAL "iel" #define NOD INED *con"
global region /parameter region /* local region * load only: Constant


APPENDIX A
COMPILER CONSTRUCTION
168
OP ALU modifiers
/* addition /* subtraction
* multiplication /* division /* remainder /* compares as: <
#define ALU ADD #define ALU SUB #define ALU MUL #define ALU DIV #define ALU MOD #define ALU LT #define ALU GT #define ALU_LE #define ALU GE #define ALU EQ #define ALU NE
"!= #define ALU AND "X" #define ALU OR I #define ALU XOR
=>
<= ·>='
/* bit-wise and /* bit-wise or /* bit-vise excl. or
typed functions, code generator
char * gen_mod();
/* region modifier */ File gen.c includes the C functions which generate assembler code for our ficti tious sampleC machine. These routines were introduced in chapter 7. gen.c
sample c -- code generation
#include 'symtab.b #include 'gen.no
generate various instruction formats
*/
gen_alu(mod, comment)
char * mod; char * comment;
/* mnemonic modifier */ /* instruction comment */
printf("\t%s\t%s\t\t; %s\n", OP_ALU, mod, comment);
gen_if(const)
char † const;
/* Constant value */
printf("\t%s\t%s, %s\n", OP_LOAD, MOD IMMED, const);
char * gen_mod (symbol)
struct symtab symbol; svitet (symbol->s_biknum) {


Samplec" COMPILER LISTING
169
Petera NOD_GLOBAL return NOD PARAN
return NOP_LOCAL
d. val. comment
ar * mod:
memonie operation code memonie modifier ottset tiela / instruction comment
10
.
00: Isla'. op. mod. Tal comment:
mnemonie operation code instruction comment /
printf("\ts10if: sia', op. comment):
generate printable internal label
metine LABEL. *$$$4* statie ear format_label(label)
statie car butter(sizeot LABEL. 9: spriatt(etter. Lase, Isha): ratura butter:
generate jemps, retara target
int gua_jup(op. label. comment ebar + op:
m onte operation code at label:
target of jup char comment:
instruction comment print("\tsiusitle: la". ep. terut a del).
comment): return 1:
gerate unique internal label


APPENDIX A
COMPILER CONSTRUCTION
170
int new label
static int next_label = 0; return ++Bext_label;
define internal label
int gen label(label)
int label;
printf("%s\tequ\t*\n', format_label(label)); return label;
label stack manager
static struct be stack { int bc label;
/* label from new_label */ struct bc stack + bc next; } _top.
/* head of break stack *) • c_top:
/* head of continue stack */
static stract bc stack # push (stack, label)
struct bc stack * stack: int label; struct bc stack # new entry = (struct bc stack #
calloc(1, sizeot (struct bc stack));
1! (new entry)
nev_entry->bc next = stack; Dev_entry->bc_label = label; return new entry:
tatal ("No more room to compile loops.); /*NOTREACHED */
statie struct be_stack pop (stack)
struct be stack * stack: struct be stack old_entry:
1t (stack)
old entry = stack: stack = old_entry->bc_next: cfree (old untry):
return stack: bug('break/continue stack undertlov"); /*NOTREACHED /


 ________________________________________
PPENDIX A
static int top(stack)
"amplec* COMPILER LISTING
171
struct bc_stack stack:
if (1 stack)
error("no loop open"); return 0;
else
return stack->c_label;
BREAK and CONTINUE
push_break (label)
int label:
b_top = push (b_top. label);
push_continue (label)
int label;
c_top = push (c_top, label):
pop_break ()
b_top = pop (b_top);
pop_continue ()
c_top = pop (c_top);
gen_break ()
gen_jump (OP_JUMP, top (b_top). "BREAK");
gen_continue()
gen_jump (OP_JUMP, top(e_top). "CONTINUE");
function call
gen_call(symbol, count)
symbol; /* function /
/* # of arguments */
struct symtab
int count;
onnt):



APPENDIX A
COMPILER CONSTRUCTION
172
printf("\t%s\t%a,%s\n", OP_CALL, count, symbol->s Dame). while (count--> 0)
gen pr(OP_POP. "pop argument'); gen (OP_LOAD, MOD_GLOBAL, 0, "push result');
function prologue
int gen_entry(symbol)
struct symtab * symbol; /* function */ int label = new label();
printf("%s\t", symbol->s Dame); printf("%s\t%s\n", OP_ENTRY, format_label(label)). return label;
fix_entry(symbol, label)
struct symtab * symbol; /* function */ int label; extern int 1_mas;
/* size of local region */
printf("%s\tequ\t%d\t\t; %s\", format_label(label).
1_max, symbol->s_name):
wrap-up
end program ()
extern int g_offset; /* size of global region */ all_program();
/* allocate global variables */ printf("\tend\t%d, main\n', X_offset):
The code generator for our fictitious machine language can be compiled using the following command: cc gen.c mem.c symtab.c message.c y. tab.c lex.yy.cl
colib -if -o gen
colib contains the yace support functions explained in section A.8.
and-go
A.7 A load-and-go system
File sim.h replaces gen.h in the compilation of samplec.v to make the load version of the sample compiler. It defines the same mnemonics as gen. them definitions needed by the simulator.
aics as gen.h, but gives






APPENDIX A
"samplec" COMPILER LISTING
173
.
sample
-- header file for simulation
operation codes for pseudo maching
#define OP ALU #define OP DEC #define OP INC a #define OP LOAD #define OP STORE 5 #define OP POP 6
A QON
de tine OP JUMPZ 7 detine OP_JUMP 8
ne OP CALL 9
Ana OP ENTRY 10 #define OP_RETURN if
/* alu
arithmetic-logic-op /* dec
region,ottset /* inc
region,otiset 7* load region,otiset /* store region ,otiset /* pop /* jumpz label /* jump label /* call routine-address /* entry local-frame-size /* return
region modifiers
#define MOD GLOBAL 1 #define MOD PARAM 2 #define MOD LOCAL #define MOD IMMED 4
/* global region /* parameter region /* local region /* load only: Constant */
/*
OP_ALU modifiers
addition /* subtraction /* multiplication /* division /* remainder /* compares as: <
VA
#define ALU ADD 1 #define ALU SUB 2 #define ALU MUL 3 #define ALU DIV 4 #define ALU MOD 6 #define ALU LT 6 #define ALU GT 7 #define ALU LE 8 #define ALU GE 9 #define ALU EQ 10 #define ALU NE #define ALU AND 12 #define ALU OR 13 #define ALU XOR 14
! !
/* bit-wise and /* bit-wise or /* bit-wise excl. or
/*
program memory structure
struct prog {
/* operation code */
short p_op:



COMPILER CONSTRUCTION
174
/* modifier */ / oftest or other value /
short p mod, int P val:
tunable limits
#define L PROG 200 /* max. program size / #define L DATA 100 /* max. area for stack, etc. */
#define DIM(x) (sizeot x / sizeot x[0) / extent */ File sim.c contains the actual simulator for our fictitious machine in chapter 8
for our fictitious machine, as introduced
sim.c
sample c -- machine simulator
*/
#include "sim.h"
data and program memory
static int data(L DATA); extern struct prog prog[];
registers
static struct prog * inst; #define G static int P; static int L; static int T;
/* -> current instruction / /* global segment */ /* current parameter segment */ /* current local segment */ /* top of stack */
shorthand notations
#define TOP data (T-1]
/* right operand: top of stack */ #define NEXT data (T-2] /* left operand: below TOP */ #define PUSH data (T++]
/* new cell to come onto stack */ #define POP
/* -> discarded cell from stack */ #define MEMORY data[address] /* effectively addressed cell */ #define RESULT data (G1
/* result value of function */
address decoder
static int address() /* effective current data address */
register int ad;



sample
COMPLER LISTING
175
swite (inst-> case NOD GLOBAL
mod) *
case MOD_PARAM
ad = P:
break: case MOD_LOCAL:
ad = L:
break: default:
bug("invalid p_208):
s += inst-> Tal: 1+ (ad < 0 Il ad >= 1)
bug("invalid ettective address): return ad:
sinalator
niste (pe_limit, global. pe)
ist pe linit, global. pe: /* initialize */ 17 (global >= DIN(ata))
fatal ("Not enough room for global esta.): T = global + 2; printf("\nExecution begins... lala):
for (::)
/* fetch / 17 (pe < 0 Il pe >= pe linit)
bag("pe not in program area'): inst = {prog (pe++): /* decode operation and dispatch / svitch (inst->p_op) { default:
printf($4:\thaltla'. inst-prog):
return: case OP ALU: 17 (T <=L+1)
bag('simulator stack undertlov): svitch Ciast->p_mod) { default:
bug(*if1egal ALU instruction): case ALU ADD: NEXT += TOP: break; case ALU SUB: NEXT - TOP: break: case ALU MUL: NEXT = TOP: break; case ALU DIV: NEXT /= TOP: breat: case ALU MOD:
NEXT = TOP: breat: case ALU LT: NEXT = NEXT TOP: breat:


AP
APPENDIX A
COMPLER CONSTRUCTION
IT
• ALU GT: cade ALU LE
a ALU GE ose ALU EQ e ALU NE:
. ALV AND : R. ALU OR ea ALU XOR
T NIKT * TOP rss NEXT - NEXT TOP; break; NEXT NEXT TOP; break NEXT NEXT TOP: breu NEXT NEXT TOP: bresk: NEXT TOP; break; NEXT - TOP; break; NEXT TOP; break;
POP:
break; en OP LOAD: if (T > DIM(data))
fatal("Too much data."); 1t (inet->p mod MOD IMMED)
PUSH-inst->pval: olee
PUSH MEMORY: break; cas. OP STORE:
if ( TL)
bag(simulator stack undertlov): printf("%d\tatore\t%.%d\tto %d\n".
inst prog. inst->p mod.
inst->p. val. MEMORY TOP): break; ens. OP INC: if (T > DIM(data))
fatal("Too much data."): printf("%d\tinc\t%.Xd\tto %d\n".
inst-prog. inst->p_mod.
inst->p.val, PUSH = ++ MEMORY): break;
case OP DEC: 17 (T > DIM(data))
tatal ("Too much data."): printf("%d\tdec\t%d. \tto %d\n".
inst prog. inst->p mod.
inut->p. val. PUSH -- MEMORY): break: cu OP POP
if ( TL)
bug("simulator stack underflow"); POP:
break: en OP JUMP: printf("%d\tjump\u\n", inst-prog.
inst->p.al): pe inst->p.val;
break; N. OP JUMPZ:
if ( TL)
bug("simulator stack undertlow) it (data (POP) - 0)
printf("%d\jump\u\n".



wop of de
A
PENDIX A
DX
A
"samplec" COMPILER LISTINO
177
):
inst-prog, inst i pc = inst-> val; break; case OP CALL: printt(*%:\tcall\ **, instprop
inst->p.val); PUSH = pe: pc = inst->p val; PUSH = P: P = T - 2 - insta ;
break; case OP ENTRY:
PUSH = L; L = T: T = inst-> val; if (T Y= DIM (data))
tatal("Too much lata."); break;
case OP RETURN: if (1 <L)
bug('simulator stack undertlo) T = L; L = data (POP): P = data (POP): pc = data (POP): print: 8:\treturn to to
inst-prog. RESULT, p.): break;
The code generation routines in simgen.c replace the corresponding routines in file and section A6
szaplec -- cote generator for simulator
#include *sin.be
progra resory */ struct prog progIL PROG): static int pe = 1;
/* current program conster / /* FALT () is at adress o */
generate a single instruction
Ist es (op. mod.
, commest)


APPENDIX A
COMPILER CONSTRUCTION
178
int op: int mod: int val: char * comment:
/operation code / /* modifier / /* offset field */ /* instruction comment */
** (pe >= DIN (prog))
fatal (Not enough program memory."): prog (pe).p op = op: prog[pe).p mod = mod: prog (pe).P ral = val; printr($:\t \c54.\t: %s\n".
pe, op. mod. val. comment): return pe ++:
***
region modifier
int gen mod (symbol)
struet syutab * symbol:
switch (symbol->s biknum) { case 1:
return MOD_GLOBAL: case 2:
return MOD PARAN: return MOD_LOCAL;
general instructions
gen alu(mod, comment)
int mod; char * comment:
/* modifier */ /* instruction comment /
gen (OP_ALU, mod, o, comment):
gen_if(const) char * const:
/* Constant value */ gen (OP_LOAD, MOD_IMMED. stoi(const). const):
gen_pr (op. coment) int op:
/* operation code / char comment:
/* instruction comment / gen (op. o. o. comment):



SZL-96SDA
APPENDIX A
XZ168708 WISZ OS# 4*
"sampleC" COMPILER LISTING
179
generate jump. return target or chain
int gen_jump (op. label, comment) int op:
/* operation code */ int label;
/* target of jump */ char * comment:
/* instruction comment */ int pe = gen (op. O, label, comment);
if (label <= 0)
return -pc;
pc:
else
etur
/* new head of chain */
return label;
/* already defined */
*/
generate tail of forward branch chain
int new label
return 0;
/* end of chain */
resolve forward branch chain
int gen label (chain)
int chain; int next;
while (chain < 0)
chain = - chain; next = prog (chain).p_val; if (next > 0)
break; /* already ok */ prog[chain).pval = pc; printf("%d:\t(fixup) it%d\n". chain, po); chain = next;
return pc;
label stack manager
static struct bc stack { int bc label;
/* label from new_label / struct bc stack # be next; ) .
/* head of break stack */ _top.
/* head of continue stack */ * c top:
static struct bc stack # push (stack, label)
struct bc stack * stack;
020


APPENDIX A
COMPILER CONSTRUCTION
180
int label; struct be stack * new entry = (struct bc stack *)
calloc(1, sizeof (struct bc stack)):
if (new entry)
new entry->be next = stack; new entry->bc label = label; return new_entry:
fatal ("No more room to compile loops."); /*NOTREACHED*/
static struct bc_stack # pop (stack)
struct bc stack * stack; struct bc stack * old entry:
if (stack)
old_entry = stack; stack = old entry->bc next; cfree (old_entry); return stack;
bug (break/continue stack underflow"); /*NOTREACHEDU/
static int * top(stack)
struct bc stack ** stack;
if (1 *stack)
error('no loop open'); *stack = push(*stack, 0);
return & (*stack) -> bc_label;
BREAK and CONTINUE
push continue (label)
int label;
c_top= push(c_top. label);
push break (label)
int label:
_top = push()_top. label):
*** break
*top (@b_top) genJump (OP_JUMP, *top (@b_top). "BREAK");


APPENDIX A
sampleC" COMPLER LISTING
181
er_continue o
*top (ke_top) = gen_jump (OP_JUMP, top (Xc_top). CONTINUE"):
pop breato
gen_label (*top (@b_top)): b_top = pop(_top):
POP_continue
gen_label("top(ke_top)): c_top = pop (c_top);
function call
gen_call(syabol, count)
struct syatab * sabol; /* fonction / int count:
/* # of arguments */ int pe:
chk_pan(syabol, count): pc = gen (OP_CALL, count, syabol->s offset, syabol->s baze): 1(symbol->s offset <= 0)
symbol->softset = -pe; /* head of chais / while (count--> 0)
gen pr(OP POP POP argument'); gen (OP_LOAD, MOD_GLOBAL, 0, 'push result');
function prologue and definition
int gen_entry(syabol)
struct syatab syabol; /* function / syabol->s_offset = gen_label'syabol->s_offset): gen (OP ENTRY, 0, 0. symbol->s_name): return syabol->s offset;
fix_entry(symbol, label)
struct syntab symbol; /* function / int label; extern int I max; /* size of local region */
prog[label].p_val = 1_mar; printf("%d\tentry\t\t; $s\a'.



182
COMPILER CONSTRUCTION
APPENDIX A
label, 1_max, symbol->s_name):
wrap-up
end program ()
extern int g offset; /* size of global region */ extern struct symtab * s find (: int main = s_find('main') -> s_offset;
.
all_program():
/* allocate global variables */ printf("%d:\tend\t%d.%d\n", pc, & offset, main); simulate (pc, & offset, main);
A load-and-go compiler and simulator for our fictitious machine can be compiled using the following command: cc sin.c simgen.c mem.c symtab.c message.c y.tab.c lex.yy.cl
colib -if -o sim
A.8 Improved error messages from "yaccpar"
The error messages issued by a yace parser can be improved. yace builds a parser using a model stored in the file /usr/lib/yaccpar. The normal behavior of yaccpar in case of an error is:
switch (gyerrflag) { case 0:
/* brand new error */ yyerror("syntax error'); yyerrlab:
**yguerrs; case 1: case 2:
7* incompletely recovered */
/* ... try again */ Jyerrflag = 3;
The switch is reached if the transition matrix contains the error operation. At this point the message syntax error is issued, regardless of the actual nature of the error.
yyerrflag is used to avoid cascades of messages. It is normally zero. If there is an error, it is set to three. During each shift operation, yyerrflag is counted down. An error message is only issued if yyerrflag is zero.
We can attempt to improve the error message as follows: in the current state, we should consider all possible terminal symbols in place of the actual input, to discover those for which the transition matrix does not contain the error action. A suitably formatted list of these symbols can be issued in place of the simple syntax error mes
sage.
input:
/usr/lib/yacepar is always available in source. We compute the desired list of symbols essentially by copying the algorithm employed by the model parser for actual



09.
ap-di-ud-pi
1909
LORSSZL-966
APPENDIX A
Weusaz og'WXZT
svitch (errslag) {
case 0:
"sampleC" COMPILER LISTING
183
/* brand new error */ 1t (lyyn = yypact[yystate]) > YYFLAG & y < TILAST)
register int i;
for (x = yya>0? yy: 0; I < YYLAST: ++1)
if (yychk(yyact[x]] == x - yo 2 x - yy != YYERRCODE)
yyerror(0, yydisplay(3-y)):
yyerror(0,0);
Jyerrlab:
++yynerTs;
yystate is the as yyact[], and yy
rate is the current state, YYLAST is the extent of the transition matrix encoded -if and yyin is a temporary variable used by yaccpar. yypact I) is a filter to de certain simple cases. This information is provided by yace. Tee are in a situation where yyact [] is actually inspected, we let x range over all
ible indices into yyact(). These must at least be zero, and the algorithm in Croer shows that they must also exceed yypact (yystate). If we find a value for x Chich does not correspond to the error symbol YYERRCODE and which would not produce an error operation from yyact [], we display the corresponding input symbol.
If a list of possible input symbols can be computed in this fashion, yacepar issues
3 call
yyerror(0,1) for each terminal symbol t in the list; t is represented as a character string. The end of the list is indicated by a call
Jyerror(0,0) This call is also issued if the list cannot be computed. yyerror() can be extended as follows to produce improved error messages:
yyerror() -- [detailed) error message for
parse
#include <stdio.b>
/* error stream */
FILE + gyerfp = stdout;
/*VARARGS1*/ yJerror(s, t)
/* *message' or 0,"token' */ register char * s, * t; extern int yyuerrs,
/* total number of errors */ static int list = 0; /* sequential calls */ if (s II ! ifst)
/* header necessary?? */ fprintf(yertp. [error $4] . Terrs+1); Jywhere: 1(s)
/* simple message?? */ fputs(s. yyerip):



184
COMPILER CONSTRUCTION
APPENDIX A
if (6)
pute('\n', yyerfp): return;
/. first token? / fpute("expecting: . yyerfp): fputs (t, yyerfp): list = 1; return;
/* no tokens acceptable / fputs (syntax error\n". yyerfp): return;
/
/* subsequent token?t pute(' '. yyerfp): fputs(t. yyertp): return;
/* end of enhanced message */
pute('\n'. yyerfp); list = 0;
The implementation of the new error message expecting: symbols in yyerror() is quite simple. Basically we dispatch different calls depending on the presence of null pointers. The static variable list maintains its value between successive calls; it is used to distinguish the first terminal symbol in a series from the remaining symbols so that an appropriate message header may be issued.
In yacepar we have employed a function yydisplay() which must convert the encoding of a symbol as an integer into a printable string:
#include <etype.h> #define DIM(x) (sizeof / sizeof x[0])
static char yydisplay(ch)
register int ch; static char buf(16):
static char * token[] = { #include "y.tok...
/* token names 0 ):
/
svitch (ch) { case 0:
return (end of if0)': cas. YYERRCODE:
return [error]: case '\b":
return '\\b: cas. '\':
return *\\ cas. '\n':
return '\\": case \r*:
return '\\r case 'It':


"sampleC" COMPILER LISTING
185
return It'; if (ch > 256 ax ch < 256 + DIM(token))
return token[ch - 257]: 1t (isasci1(ch) a isprint(ch))
sprintr (buf. c .ch): else 1ť (ch < 256)
sprintf(buľ, "char %04.30", ch): else
sprintf(but, token %d", ch); return but;
If the symbol ch is a printable ASCII character or if it can be represented by a control character escape sequence, we show the corresponding C-style character constant. If the symbol value exceeds 256, it was normally defined by yace in reponse to a Stoken statement; in this case we try to decode it using an array token [] with appropriate character strings. If neither approach is possible, we return the numerical value. The entire function yydisplay() should be inserted near the beginning of the file /usr/lib/yaccpar; it can also be used to improve the output generated under the YYDEBUG option.
The following shell procedure can be used to construct the entries for the token() vector from the file y.tab.h:
grep '*#.*define' y.tab. ĭ sed 's/# define \([]*V) []*$/ 'if'./' > y.tok.
The strings are placed into a file y.tok.h. So that things work even if the file does not exist, an empty file /usr/include/y.tok.h should be present in the system to be included for yydisplay() as a default.
The new version of yyerror() shown here and the functions which were shown in section 3.3 are best collected into a library colib as follows:
cc -c main.c cpp.c yywhere.c yyerror.c ar r colib main.o cpp. yywhere.o yyerror.o ranlib colib
A.9 Improved debugging output from "yaccpar"
Debugging output from /usr/lib/yaccpar also can be improved. One fairly essential defect is the fact that some inputs are used and discarded before they are displayed. The basic architecture of yaccpar in this respect is as follows:
#17det YYDEBUG int yydebug = 0; #endit int yychar; /* current input token number */
parse()
register short yystate;
yychar = -1;


188
APPENDIX A
COMPILER CONSTRUCTION
/ put a state and value onto the stack
/
yyatack:
#ifdef YYDEBUG if (yydebug)
printr('state %d, char Oxo\n". yystate, yychar): #endir
18 (yychar < 0) /* lookahead available? if ((yyehar = yylex() < 0)
yyehar = 0; /* end of tif. if ( valid shirt */)
/ /
yychar = -1: goto yystack:
In order to improve things, we need to print each terminal symbol as soon as it is obtained from yylex(). Since the nested if statements above appear twice in yacepar, it is best to replace them by calling a new routine yyyylex() defined as follows: statie yyyylexo
if (yychar < 0) ( if (lyychar = yylex 0) < 0)
yychar = 0; *ifdet YYDEBUG if (yydebug)
printf("Cyydebug] reading X8\n". Wendir
yydisplay(yychar));
where we have used yydisplay() to construct a reasonably legible representation of the terminal symbol.
A.10 Regression testing
One should generally save the input files used to test the compiler during develop ment. If the compiler is changed, the test files can be run again to ascertain that at least some of the old features of the compiler are still functioning. The following Bourne shell script automates this testing procedure:



