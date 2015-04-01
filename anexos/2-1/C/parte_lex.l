%{
	#include "album.h"
	char* token;
	photo_ptr picture;
%}

%%

\<foto[ \t]+[a-zA-Z]+=\".*\"										{
																		yytext[strlen(yytext)-1]='\0';
																		picture=initPhoto(strchr(yytext,'"')+1);
																	}
\<quando[ \t]+[a-zA-Z]+=\"[0-9]{4}(.|-)[0-9]{2}(.|-)[0-9]{2}		{
																		setDate(picture,strchr(yytext,'"')+1);
																	}
\<quem>[ \t\na-zA-ZÀ-û,\"0-9;:]+									{	
																		token=strchr(yytext,'>')+1;
																		setWho(picture,token);
																	}
\<onde>[ \t\n0-9a-zA-ZÀ-û.,;:\"]+<									{
																		yytext[strlen(yytext)-1]='\0';
																		setLoc(picture,strchr(yytext,'>')+1);
																	}
\<facto>[ \t\n0-9a-zA-ZÀ-û.,;:\"]+<									{
																		yytext[strlen(yytext)-1]='\0';
																		setFact(picture,strchr(yytext,'>')+1);
																	}
\<\/foto>															{
																		if(picture) {
																			token=strdup(picture->who);
																			token=strtok(token,";");
																			while (token){
																				insert_photo_person(token, picture);
																				token=strtok(NULL,";");
																			}
																		}
																		picture=NULL;
																	}
.|\n																;

%%