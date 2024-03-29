%{
#include "musica.h"

char* takeOffAnotations(char* c){
    int i=0;
    while(c[i] != '\0' && !(c[i] == ' ' && c[i + 1] == ' ')) {
        i++;
    }
    c[i] = '\0';
    return c;
}

char* takeOffUnderSccore(char* c){
    int i=0, w=0;
    while(c[i] != '\0') {
        //ignore if '_'
        if(c[i] != '_') {
            c[w] = c[i];
            w++;
        }
        i++;
    }
    c[w] = '\0';
    return c;
}

%}


abc      \<[ ]*(?i:abc)[ ]*\>
abcclose      \<\/[ ]*(?i:abc)[ ]*\>

%%              
^title:.+       appendTitle(yytext + 6);
^from:.+        appendFrom(yytext + 5); 
^author:.+      appendAuthor(yytext + 7); 
^lyrics:.+      appendLyrics(yytext + 7);
^music:.+       appendMusic(yytext + 6);
^singer:.+      appendSinger(yytext + 7);

^[a-zA-Z]+:.+   ;// Ignore other headers:

{abc}(.|\n)*{abcclose}  ;


##-+    {commitCheckNext(); start();}// Music End

^\n     appendWhiteLine(); // white music line.
[ ].*   ; //ignore lines that start with space (they are music anotations);
.*      {appendLine(takeOffAnotations(takeOffUnderSccore(yytext))); 
        //poem line, taking of underscore and music annotations}
.|\n    ;
%%
