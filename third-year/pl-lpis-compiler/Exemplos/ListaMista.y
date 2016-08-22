%{
#include <stdio.h>
int compl, sum = 0;
int npals=0, nnums=0;
%}

%union {int vali; char *vals;}

%token INIC FIM 
%token <vals>pal 
%token <vali>num

%%

Frase : INIC Lista FIM { imprimeResult(); }
	  ;
Lista : Elem           { compl = 1;}
	  | Lista ',' Elem { compl = compl + 1;}
	  ;
Elem  : pal            { npals++; sum = sum; }
	  | num            { nnums++; sum += $1; }
	  ;

%%
#include "lex.yy.c"

void imprimeResult()
     { printf("comprimento da lista: %d\n", compl); 
       printf("soma dos elementos: %d\n", sum);
       printf("Numero de palavras: %d\n", npals); 
       printf("Numero de numeros: %d\n", nnums);
     }
     
int yyerror(char *s) 
     { printf("erro sintático: %s\n", s); }
     
int main(){
	yyparse(); 
	return 0; 
}	
