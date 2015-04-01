#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "album.h"

static Album album;

char* trimspace(char *str)
{
	char *end;

  	while(isspace(*str)||(*str)=='\n') str++;

  	if(*str == 0)  
    	return str;

  	end = str + strlen(str) - 1;
  	while(end > str && isspace(*end)) end--;

  	*(end+1) = 0;
  	return str;
}

char* subsPoints(char *str){
	int i;

	for(i=0; i<strlen(str);i++){
		if(str[i]=='.')
			str[i]='-';
	}
	return str;

}

void insert_photo(photo_ptr p, photo_ptr *pics){
    if ((*pics) == NULL)
    {
        (*pics)=p;
        (*pics)->left=NULL;
		(*pics)->right=NULL;
	}
    else{
        int comp=strcmp(p->date, (*pics)->date);
        if(comp < 0)
    	{
        	insert_photo(p, &(*pics)->left);
    	}
    	else {
        	insert_photo(p, &(*pics)->right);
    	}
    }
}


void insert_photo_person(char *name, photo_ptr p){
    
    person_ptr aux, newPers, lastPers;
    int done=0, comp;

    name=trimspace(name);

    if (album.people == NULL)
    {
        album.people=malloc(sizeof(person_node));
        name=trimspace(name);
        album.people->name=strdup(name);
        album.people->href=malloc(16*sizeof(char));
        sprintf(album.people->href,"%d",album.count);
        album.people->href=strcat(album.people->href,".html");
        album.people->pics=p;
        album.count++;
        album.people->next=NULL;
	}
    else{
    	for(aux=album.people;aux && (comp=strcmp(aux->name,name))<0;aux=aux->next){
    		lastPers=aux;
    	}
    	
    	if (comp==0){
			insert_photo(p,&aux->pics);
		}

		else {
			newPers = malloc(sizeof(person_node));
			newPers->name=strdup(name);
			newPers->href=malloc(16*sizeof(char));
			sprintf(newPers->href,"%d",album.count++);
			newPers->href=strcat(newPers->href,".html");
			newPers->pics=p;
			if(!aux) {
				newPers->next=NULL;
				if(lastPers) lastPers->next=newPers;
			}
			else {										
				newPers->next=aux;
				if (lastPers) lastPers->next=newPers;				
				else album.people=newPers;
			} 
		}
	}  
}

void setDate(photo_ptr p, char *photoD){
	photoD=trimspace(photoD);
	photoD=subsPoints(photoD);
	p->date=strdup(photoD);
}

void setLoc(photo_ptr p, char *photoL){
	photoL=trimspace(photoL);
	p->loc=strdup(photoL);
}

void setFact(photo_ptr p, char *photoF){
	photoF=trimspace(photoF);
	p->fact=strdup(photoF);
}

void setWho(photo_ptr p, char *photoW){
	photoW=trimspace(photoW);
	p->who=strdup(photoW);
}

photo_ptr initPhoto(char *photoN){
	photo_ptr pic = malloc(sizeof(photo_node));
	pic->file=strdup(photoN);
	pic->date=strdup("Desconhecida");
	pic->loc=strdup("Desconhecido");
	pic->fact=strdup("Desconhecida");;
	pic->who=strdup("Nao identificados");
	pic->left=NULL;
	pic->right=NULL;
	return pic;
}

void init(){
	album.people=NULL;
	album.count=1;
}

void appendPhoto(photo_ptr p, FILE* file){

	if(p!=NULL) {

		appendPhoto(p->left, file);
		fprintf(file, "  <div class=\"photo\">\n</br>\n");
		fprintf(file, "    <div class=\"fact\"><h3>Descricao:</h3> %s</div>\n</br>\n",p->fact);
		fprintf(file, "    <div class=\"img\" align=\"center\">\n");
		fprintf(file, "      <img src=\"%s\" alt=\"%s\" width=800 height=600>\n    </div>\n</br>\n</br>\n",p->file,p->file);
		fprintf(file, "    <div class=\"data\"><b>Data:</b> %s</div>\n",p->date);
		fprintf(file, "    <div class=\"local\"><b>Local:</b> %s</div>\n",p->loc);
		fprintf(file, "    <div class=\"who\"><b>Quem:</b> %s</div>\n",p->who);
		fprintf(file, "  </div>\n");
		appendPhoto(p->right, file);

	}	
}

void createPage(char* filename, char* name, photo_ptr p){
	FILE* file;
	photo_ptr pic_ptr;

	file=fopen(filename,"w+");

	fprintf(file, "<!DOCTYPE html>\n<html>\n");
	fprintf(file, "<head>\n  <meta charset=\"UTF-8\">\n</head>\n<body>\n");

	fprintf(file, "<a href=\"AlbumGerado.html\">Voltar</a>");

	fprintf(file, "<div class=\"title\" align=\"center\">\n");
	fprintf(file, "  <h1>Album de fotografias</h1>\n");
	fprintf(file, "  <h2>%s</h2>\n</div>\n</br>\n</br>\n", name);

	fprintf(file, "<div class=\"gallery\">\n");
	appendPhoto(p, file);
	fprintf(file, "</div>\n</body>\n</html>");

	fclose(file);
}

void createAlbum(char* filename) {
	FILE* file;
	person_ptr aux = album.people;
	
	file=fopen(filename,"w+");

	fprintf(file, "<!DOCTYPE html>\n<html>\n");
	fprintf(file, "<head>\n  <meta charset=\"UTF-8\">\n</head>\n<body>\n");

	fprintf(file, "<div class=\"title\" align=\"center\">\n");
	fprintf(file, "  <h1>Album de fotografias</h1>\n</div>\n");

	fprintf(file, "<div class=\"index\">\n");
	fprintf(file, "  <div class=\"indextitle\"  align=\"center\">\n");
	fprintf(file, "    <h2>Indice de Pessoas</h2>\n  </div>\n");
	fprintf(file, "  <div class=\"indexitems\">\n    <ul>\n");

	while(aux){
    	fprintf(file, "      <li><a href=\"%s\">%s</a></li>\n", aux->href,aux->name);
    	createPage(aux->href,aux->name, aux->pics);
    	aux=aux->next;
    }
	
	fprintf(file, "    </ul>\n  </div>\n</div>\n");
	fprintf(file, "</body>\n</html>");

	fclose(file);
}

void printst(photo_ptr p){

	if(p!=NULL) {

		printst(p->left);
		printf("%s\n", p->file);
		printst(p->right);
	}	
}

int main(int argc, char* argv[]){
	
	init();
	yylex();

	
	if(argc==2){
		createAlbum(argv[1]);
	}
	else{
		createAlbum("AlbumGerado.html");
	}
	return 0;
}