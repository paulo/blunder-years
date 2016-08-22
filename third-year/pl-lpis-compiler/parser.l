%{
#include "vmCompiler.h"
#include "y.tab.h"

int ccLine = 1;

%}



%%

int         {return(INT);}
while       {return(WHILE);}
for         {return(FOR);}
if          {return(IF);}
else        {return(ELSE);}
return      {return(RETURN);}
void        {return(VOID);}
printi      {return(PRINTI);}
scani       {return(SCANI);}
true        {return(TRUE);}
false       {return(FALSE);}
do          {return(DO);}
\=	 		{return('=');}
\.        	{return('.');}
\;        	{return(';');}
\(        	{return('(');}
\)        	{return(')');}
\{  	    {return('{');}
\}      	{return('}');}
\[ 	        {return('[');}
\]	        {return(']');}
\,        	{return(',');}
\<	        {return('<');}
\>	        {return('>');}
\+	        {return('+');}
\-	        {return('-');}
\*	        {return('*');}
\/	        {return('/');}
\%	        {return('%');}
\#          {return('#');}
\|			{return('|');}			
\&			{return('&');}
[a-zA-Z]+  	{yylval.var_name = strdup(yytext); return(var);}
[0-9]+     	{yylval.value = atoi(yytext); return(num);}
[\n]        { ccLine++;}
\/\/.*      { ; }     
.           { ; }
%%

int yywrap()
{ return(1); }