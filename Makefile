OBJ	= sampleC.tab.o lex.yy.o simgen.o sim.o symtab.o message.o mem.o
#TEST_OBJ = AllTests.o cmtTest.o cmtTestRunner.o
CC	= gcc
YACC = bison
LEX	= flex
INC = 
TEST_INC = 
LIBS = # -lfl   -ll
CFLAGS = -w

sim:	$(OBJ) 
	gcc -o sim.exe $(OBJ) $(LIBS) 

.c.o:
	$(CC) -c	$(INC)	$(CFLAGS) $<

sampleC.tab.c:	sampleC.y
	$(YACC) -d $<

lex.yy.c:	sampleC.l
	$(LEX) $<

clean:
	del $(OBJ) sampleC.tab.c  sampleC.tab.h lex.yy.c

all: clean sim
