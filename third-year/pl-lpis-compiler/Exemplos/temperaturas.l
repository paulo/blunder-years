%{
/* Declaracoes C diversas */
%}

%option noyywrap

%%
[CF,]      { return(yytext[0]); }
(?i:DIA)   { return(DIA); }
(?i:LOCAL) { return(LOCAL); }
(?i:TEMPERATURAS)[ ]+(?i:DE) { return(TEMPERAT); }
[0-9]+     { yylval.vali = atoi(yytext); return(num); }
[a-zA-Z]+  { yylval.vals = strdup(yytext); return(id); }
\"[^"]*\"  { yylval.vals = strdup(yytext); return(nome); }
.|\n       { ; }
%%
