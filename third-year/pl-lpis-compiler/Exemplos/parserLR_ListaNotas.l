%{
%}

num [0-9]+
pal [a-zA-Z]+

%%
[.;(),]    { return(yytext[0]); }
(?i:NOTAS) { return(NOTAS); }
{pal}      { yylval.vals = strdup(yytext); return(pal); }
{num}      { yylval.vali = atoi(yytext); return(num); }
.|\n       { ; } 
%%

int yywrap()
    { return(1); }
