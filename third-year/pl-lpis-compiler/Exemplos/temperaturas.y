%{
#include <stdio.h>
#include <strings.h>
/* Declaracoes C diversas */
#define MDIAS 32
float Temps[MDIAS];
int d;
%}

%union{
   int vali;
   char *vals;
   }

%token TEMPERAT DIA LOCAL
%token <vals>id
%token <vali>num
%token <vals>nome

%type  <vali>TempsDias 
%type  <vali>TempDia 
%type  <vali>Temp

%%
TempsMes  : TEMPERAT IdMes TempsDias
            { printf("Temperatura maxima: %d\n", $3); 
              for( d=1; d<=31; d++ ) 
              if (Temps[d]!=-100.0) printf("Media do dia %d foi %f\n",d,Temps[d]); 
            }
          ;
IdMes     : id
          ;
TempsDias : TempDia            { $$ = $1; }
          | TempsDias TempDia  { /*calculo da temperatura maxima mensal*/
                                 if ($1>=$2) $$ = $1; else $$ = $2; }
          ;
TempDia   : DIA num LOCAL nome Temp ',' Temp ',' Temp Escala
            { /*calculo da temperatura media,se nao houver erros*/
              if (($2<1)||($2>31)) printf("Erro Semântico: Data do dia inválida (%d)\n",$2);
              else if (Temps[$2]!=-100) printf("Erro Semântico: Já ha registo de temperaturas para o dia (%d)\n",$2);
                   else   Temps[$2] = ($5+$7+$9)/3.0;
              /*calculo da temperatura maxima diaria*/
              if ($5>=$7) $$=$5; else $$=$7; 
              if ($9>=$$) $$=$9; 
            }
          ;
Temp      : num  { $$=$1; } 
          ;
Escala    : 'C'
          | 'F'
          ;
%%

#include "lex.yy.c"

int yyerror(char *s)
{
  fprintf(stderr, "ERRO: %s \n", s);
}

int main()
{
  for( d=1; d<=31; d++ ) Temps[d] = -100.0;    /*inicializacao do array com a temperatura Media de cada dia do mes*/      
     yyparse();
  return(0);
}

