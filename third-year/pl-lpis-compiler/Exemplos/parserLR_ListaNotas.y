%{
#include <stdio.h>
#include <string.h>
int tot, soma;
%}

%union {int vali; char *vals;}

%token NOTAS 
%token <vals>pal 
%token <vali>num

%type  <vals>IdAl
%type  <vali>Corpo 
%type  <vali>Nota

%%

Pauta    : Cabeca Corpo '.'    { printf("Nesta Turma existem %d Alunos\n",$2); }
         ;
Cabeca   : NOTAS
         ;
Corpo    : Aluno               { $$ = 1; }
         | Corpo ';' Aluno     { $$ = $1+1; }
         ;
Aluno    : IdAl '(' Notas ')'  { printf("O Aluno %s teve a Media %f\n",$1,(float)soma/(float)tot); }
         ;
IdAl     : pal                 { $$ = $1; }
         ;
Notas    : Nota                { soma = $1; tot=1; }
         | Notas ',' Nota      { soma += $3; tot++; }
         ;
Nota     : num                 { $$ = $1; }
         ;

%%
#include "lex.yy.c"

int yyerror(char *s) 
     { fprintf(stderr,"Erro Sintático: %s\n", s); }
     
int main(){
	yyparse(); 
	return 0; 
}	
