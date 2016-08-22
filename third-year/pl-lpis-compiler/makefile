ALL: y.tab.o lex.yy.o main.o hashmap.o stack.o vmCompiler.o
	gcc -o play $^ -ll

main.o: main.c
	gcc -c main.c

vmCompiler.o: vmCompiler.c
	gcc -c vmCompiler.c

stack.o: stack.c
	gcc -c stack.c

hashmap.o: hashmap.c
	gcc -c hashmap.c

y.tab.o: y.tab.c lex.yy.c
	gcc -c y.tab.c

y.tab.c: compiler.y
	yacc -d -v  compiler.y 

lex.yy.o: lex.yy.c
	gcc -c lex.yy.c

lex.yy.c: parser.l
	flex parser.l


clean: 
	rm *.o lex.yy.c y.tab.c y.tab.h y.output play

t1:
	./play < test/t1

t2:
	./play < test/t2

t3:
	./play < test/t3

t4:
	./play < test/t4

t5:
	./play < test/t5

t6:
	./play < test/t6
