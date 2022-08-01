OBJ	=  sampleC.tab.o lex.yy.o sim.o simgen.o symtab.o message.o mem.o 
#OBJ	= sim.o simgen.o mem.o symtab.o message.o sampleC.tab.o lex.yy.o
#TEST_OBJ = AllTests.o cmtTest.o cmtTestRunner.o
CC	= gcc
YACC = bison
LEX	= flex
INC = 
TEST_INC = 
LIBS =  -lfl  -lc
CFLAGS = -w

sim:	$(OBJ) 
	$(CC) -o sim $(OBJ) $(LIBS) 


sampleC.tab.o:	sampleC.y
#	$(YACC) -d -p yy $<
	$(YACC) -d $<
	$(CC) -c $(CFLAGS) $(INC) sampleC.tab.c
.c.o:
	$(CC) -c $(CFLAGS) $(INC)	$<


lex.yy.c:	sampleC.l
	$(LEX) $<

clean:
	rm -f $(OBJ) sampleC.tab.c  sampleC.tab.h lex.yy.c

all: clean sim
