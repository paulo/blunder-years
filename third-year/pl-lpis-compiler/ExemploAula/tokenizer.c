#include <stdio.h>
#include "cenas.h"

extern int yylex();

int main(){

    int proxSimb;
    while((proxSimb = yylex())){
        switch (proxSimb){
            case ABRE: printf("ABRE "); break;
            case FECHA: printf("FECHA "); break;
            case NUM: printf("NUM "); break;
            case VIRG: printf("VIRG "); break;
            default: break; 
        }
    }
    return 0;
}