all: play

play: lex.yy.o album.o
	gcc lex.yy.o album.o -o play -ll

lex.yy.o: parte_lex.l
	flex parte_lex.l
	gcc -c lex.yy.c -o lex.yy.o

musica.o: album.c
	gcc -c album.c

test.o: test.c
	gcc -c test.c
	
test: album.o test.o
	gcc album.o test.o -o test

clean:
	rm *.o