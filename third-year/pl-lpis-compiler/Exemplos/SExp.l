%{
	
%}

num [0-9]+
pal [a-zA-Z]+

%%

[()]		{ return yytext[0]; }
{num}		{ yylval.vals = strdup(yytext); return num; }
{pal}    	{ yylval.vals = strdup(yytext); return pal; }
.|\n        { ; }

%%

int yywrap(){ 
	return(1); 
}
