


gen.o:	gen.c
	$CC	-c	-w	$

symtab.o:	symtab.c

mem.o:	mem.c

message.o:	message.c

sampleC.tab.o:	sampleC.y
	bison -d sampleC.y
	gcc -c -w sampleC.tab.c

lex.yy.o:	sampleC.l
	flex sampleC.l 
	gcc -c lex.yy.c 

gen:	gen.o mem.o symtab.o message.o  y.tab.o lex.yy.o 
	gcc 	gen.o mem.o symtab.o message.o  y.tab.o lex.yy.o colib -ll -o gen

clean:
	rm gen *.o y.tab.c lex.yy.c