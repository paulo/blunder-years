%{
#include<stdio.h>
#include"y.tab.h"

%}

%token IF 
%token int while for if else return void printi scani true false

//falta a variavel
//duvida na lista/conjunto de instruções
//
// aqui está mal
//  Atribuicao -> Abribuicao
//              | ε


%% 
Programa:       ConjuntoInstrucoes      {}
Declaracao:     int variavelAtr         {}
VariavelAtr:    
              Variavel                  {}
            | Variavel '[' Exp ']'      {}
ConjuntoInstrucoes:
              Instrucao                 {}
            | '{' ListaInstrucoes '}'   {}
ListaInstrucoes:
              Instrucao                 {}
            | ListaInstrucoes           {}
Instrucao:  
              If
            | While
            | For
            | Atribuicao
            | Printi
            | Scani
            | DoWhile
Atribuicao: 
              Variavel '=' Exp
            | Variavel '++'
            | Variavel '[' Exp ']' '=' Exp
If:
      IF '(' ExpL ')' '{' ConjuntoInstrucoes '}'
    | IF '(' ExpL ')' Instrucao
While:
      WHILE '(' ExpL ')' '{' ConjuntoInstrucoes '}'
DoWhile: DO '{' ConjuntoInstrucoes '}' WHILE '(' ExpL ')'
For:
      FOR '(' Atribuicao ';' ExpL ';' Atribuicao ')' '{' ConjuntoInstrucoes '}'
    | FOR '(' ';' ExpL ';' Atribuicao ')' '{' ConjuntoInstrucoes '}'
    | FOR '(' Atribuicao ';' ExpL ';' Atribuicao ')' ';'
    | FOR '(' ';' ExpL ';' Atribuicao ')' ';'
Printi: printi '(' Exp ')'
Scani:  scani '(' VariavelAtr ')'
Funcao: Tipo nomeFuncao '(' ListaArg ')' ConjuntoInstrucoes
Tipo:
      void
    | int 
ListaArg: 
          Tipo nomeVar
        | ',' ListaArg


%%

void yyerror(char *s){
    printf("Erro sintatico: %s\n",s);
}


