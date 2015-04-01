%s country

%{
    #include "tree.h"

    char *aux; 
    char* aux_subcity;
    int aux_len;
    tree_ptr persons=NULL;
    tree_ptr countrys = NULL;
    tree_ptr citys = NULL;
    tree_ptr organizations = NULL;
    tree_ptr otherlocations = NULL;
    char* trimspace(char *str);
%}

pal         [a-zA-Z0-9Ç-ÑÀ-û ]+    
enamex      \<[ \t]*(?i:enamex)[ \t]+(?i:type)=
eclose      \<[ \t]*\/(?i:enamex)[ \t]*\>
org         \"[ \t]*(?i:organization)[ \t]*\"[ \t]*\>
person      \"[ \t]*(?i:person)[ \t]*\"[ \t]*\>
loc         \"[ \t]*(?i:location)[ \t]*\"[ \t]*(\>)?
subcountry  (?i:subtype)=\"[ \t]*(?i:country)[ \t]*\"[ \t]*\>
subcity     (?i:subtype)=\"[ \t]*(?i:city)[ \t]*\"[ \t]*\>

%%

    BEGIN 0;

{enamex}{loc}{subcountry}{pal}{eclose}    {
        aux=strdup(strchr(yytext,'>')+1);
        *strchr(aux,'<')='\0';
        insert(trimspace(aux), &countrys);
        BEGIN country;
    }

<country>\.                      BEGIN 0;

<country>{enamex}{loc}{subcity}{pal}{eclose}   {
        tree_ptr aux_tree = search_tree(aux, countrys);
        aux_subcity=strdup(strchr(yytext,'>')+1);
        *strchr(aux_subcity,'<')='\0';
        insert_subtree(trimspace(aux_subcity), &countrys);
        insert(trimspace(aux_subcity), &citys);
        BEGIN 0;
    }
{enamex}{person}{pal}{eclose}   {
        aux=strdup(strchr(yytext,'>')+1);
        *strchr(aux,'<')='\0';
        insert(trimspace(aux), &persons);
    }
{enamex}{org}{pal}{eclose}  {
        aux=strdup(strchr(yytext,'>')+1);
        *strchr(aux,'<')='\0';
        insert(trimspace(aux), &organizations);
    }
{enamex}{loc}{subcity}{pal}{eclose}   {
       aux=strdup(strchr(yytext,'>')+1);
       *strchr(aux,'<')='\0';
       insert(trimspace(aux), &citys);
   }
{enamex}{loc}{pal}{eclose}  {
        aux=strdup(strchr(yytext,'>')+1);
        *strchr(aux,'<')='\0';
        insert(trimspace(aux), &otherlocations);
    }

<*>.|\n                     ;
    

%%

/*Função que retira os espaços brancos dos nomes*/ 
char* trimspace(char *str)
{
    char *end;

    while(isspace(*str)) str++;

    if(*str == 0)  
        return str;

    end = str + strlen(str) - 1;
    while(end > str && isspace(*end)) end--;

    *(end+1) = 0;
    return str;
}

void printHeader(FILE *f){

    fprintf(f, "<DOCTYPE !html>\n<html>\n\t<head> <meta charset=\"UTF-8\">\n Grupo 7</head>\n");
    fprintf(f, "<link type=\"text/css\" rel=\"stylesheet\" href=\"stylesheet.css\"/>");
    fprintf(f, "\t<title>Processamento de Linguagens</title><body>");

}

void createIndex(FILE *f){
    printHeader(f);
    fprintf(f, "<h1>Processamento de Linguagens</h1></a>\n");
    fprintf(f, "<h2>Processamento de Entidades Nomeadas</h2></a>\n");
    fprintf(f, "<a href=\"persons.html\"><p class=\"ind\">Persons</p></a>\n");
    fprintf(f, "<a href=\"countrys.html\"><p class=\"ind\">Countrys</p></a>\n");
    fprintf(f, "<a href=\"organizations.html\"><p class=\"ind\">Organizations</p></a>\n");
    fprintf(f, "<a href=\"citys.html\"><p class=\"ind\">City's</p></a>\n");
    fprintf(f, "<a href=\"otherlocations.html\"><p class=\"ind\">Other Locations</p></a>\n");
    fprintf(f, "\t</body>\n</html>\n");
}

int yywrap()
{ return(1); }

int main() 
{ 
    yylex(); 

    FILE *i, *p, *c, *o, *ac, *ol;

    i = fopen("web/index.html", "w");
    createIndex(i);
    fclose(i);

    p = fopen("web/persons.html", "w");
    printHeader(p);
    fprintf(p, "<h1>Persons</h1>");
    treeToHtml(persons,p);
    fprintf(p, "<a href=\"index.html\"><p>Voltar</p></a>\n\t</body>\n</html>\n");
    fclose(p);

    c = fopen("web/countrys.html", "w"); 
    printHeader(c);
    fprintf(c, "<h1>Countrys</h1>");
    treeToHtml(countrys,c);
    fprintf(c, "<a href=\"index.html\"><p>Voltar</p></a>\n\t</body>\n</html>\n");
    fclose(c);

    o = fopen("web/organizations.html", "w");
    printHeader(o);
    fprintf(o, "<h1>Organizations</h1>");
    treeToHtml(organizations,o);
    fprintf(o, "<a href=\"index.html\"><p>Voltar</p></a>\n\t</body>\n</html>\n");
    fclose(o);

    ac = fopen("web/citys.html", "w");
    printHeader(ac);
    fprintf(ac, "<h1>City's</h1>");
    treeToHtml(citys,ac);
    fprintf(ac, "<a href=\"index.html\"><p>Voltar</p></a>\n\t</body>\n</html>\n");
    fclose(ac);

    ol = fopen("web/otherlocations.html", "w");
    printHeader(ol);
    fprintf(ol, "<h1>Other Locations</h1>");
    treeToHtml(otherlocations,ol);
    fprintf(ol, "<a href=\"index.html\"><p>Voltar</p></a>\n\t</body>\n</html>\n");
    fclose(ol);

    return 0; 
}