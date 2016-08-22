%{
#include "y.tab.h"
#define PONTO '.'
#define PVIRG ';'
#define PE '('
#define PD ')'
#define VIRG ','
%}


%%
(?i:NOTAS) {return(NOTAS);}
"."        {return(PONTO); }
";"        {return(PVIRG);}
"("        {return(PE);}
")"        {return(PD);}
","        {return(VIRG);}
[a-zA-Z]+  {return(pal);}
[0-9]+     {return(num);}
.|\n       { ; } 
%%

int yywrap()
{ return(1); }
