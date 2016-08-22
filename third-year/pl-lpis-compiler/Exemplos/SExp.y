%{
#include <stdio.h>	
int nivel = 0;
%}

%union {char *vals;}

%token <vals>pal 
%token <vals>num

%%

Lisp : SExp
	 ;
SExp : pal                          { printf("%s - %d ", $1, nivel);}
	 | num                          { printf("%s - %d ", $1, nivel);}
	 |'(' { nivel++; } SExpList ')' { nivel--; }  /*nivel incrementa ao entrar na lista e decrementa ao sair*/
	 ;
SExpList : SExp SExpList
	 |
	 ;
%%

#include "lex.yy.c"

int yyerror (char* s){
	fprintf(stderr, "Erro: %s\n",s);
}

int main(){
	yyparse();
	return 0; 
}
