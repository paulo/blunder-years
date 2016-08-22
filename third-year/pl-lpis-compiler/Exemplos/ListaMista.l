%{
%}
%x COMENTARIO

num [0-9]+
pal [a-zA-Z]+

%%

","       											{ return yytext[0]; }
INICIO    											{ return INIC; }
FIM       											{ return FIM; }
{num}     											{ yylval.vali = atoi(yytext); return num; }
{pal}     											{ yylval.vals = strdup(yytext); return pal; }

"/*"      											{ BEGIN COMENTARIO; }
<COMENTARIO>"*/" 									{ BEGIN INITIAL; }
<COMENTARIO>.|\n 									{ ; }

.|\n      											{ ; }

%%

int yywrap()
    { return(1); }
