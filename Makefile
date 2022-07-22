

gen.o:	gen.c

symtab.o:	symtab.c

mem.o:	mem.c

message.o:	message.c

y.tab.o:	sampleC.y
	bison sampleC.y
	gcc -c y.tab.c

lex.yy.o:	sampleC.l
	flex sampleC.l 
	gcc -c lex.yy.c 

gen:	gen.o mem.o symtab.o message.o  y.tab.o lex.yy.o 
	gcc 	gen.o mem.o symtab.o message.o  y.tab.o lex.yy.o colib -ll -o gen

clean:
	rm gen *.o y.tab.c lex.yy.c