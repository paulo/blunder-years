#include <stdio.h>
#include <unistd.h>
#include "musica.h"
#include <stdlib.h>
#include <string.h>

// ###########################################################################
//                       private header area
// ###########################################################################

extern int yylex();

typedef struct sMusicLine {
    char* line;
    struct sMusicLine* next;
} MusicLine;

typedef struct sMusic {
    char* _Title;
    char* _From;
    char* _Author;
    char* _Lyrics;
    char* _Music;
    char* _Singer;

    MusicLine* poem;
    MusicLine* poemEnd;
    int error;
} Music;

void writeLatex();

// ###########################################################################
//                       Implementation Area
// ###########################################################################
static Music music;
static char** argv;
static int argc;
static int argIndex;


#define append(name) \
    void append##name(char* t) { \
        if (music._##name != NULL) { \
            music.error = 1; \
        } else { \
            music._##name = strdup(t); \
        } \
    } \


append(Title)
append(From)
append(Author)
append(Lyrics)
append(Music)
append(Singer)

void appendLine(char* line) {
    if (music.poemEnd == NULL){
        music.poem = (MusicLine*) malloc(sizeof(MusicLine));
        music.poemEnd = music.poem;
    } else {
        music.poemEnd->next = (MusicLine*) malloc(sizeof(MusicLine));    
        music.poemEnd = music.poemEnd->next;
    }
    music.poemEnd->next = NULL;
    music.poemEnd->line = strdup(line);
}

void appendWhiteLine() {
    // don't append white line in the init of the file

    if (music.poemEnd != NULL){
        music.poemEnd->next = (MusicLine*) malloc(sizeof(MusicLine));    
        music.poemEnd = music.poemEnd->next;
        music.poemEnd->next = NULL;
        music.poemEnd->line = strdup("");
    }
}

void start() {
    music._Title = NULL;
    music._From = NULL;
    music._Author = NULL;
    music._Lyrics = NULL;
    music._Music = NULL;
    music._Singer = NULL;

    music.poem = NULL;
    music.poemEnd = NULL;
    music.error = 0;
}

void writeLatex(Music m, FILE* f) {
    fprintf(f, "\\title{%s}\n",m._Title);
    if (m._Author != NULL) {
        fprintf(f, "\\author{%s}\n",m._Author);
    } else {
        fprintf(f, "\\author{%s,%s}\n",m._Lyrics,m._Music);
    }

    fprintf(f, "\\documentclass[12pt]{article}\n");
    fprintf(f, "\\begin{document}\n");
    fprintf(f, "\\maketitle\n");
    fprintf(f, "\\section*{Letra}\n");
    fprintf(f, "\\begin{center}\n");
    MusicLine* it;
    fprintf(f, "\\begin{verbatim}\n");
    for (it = m.poem; it ; it = it->next){
        // insert line
        //if (it->line[0] != '\0'){
        fprintf(f, "%s\n",it->line);    
        //} else {
            // insert white space
            
            //fprintf(f, "\\vspace{5mm}\n");
            
        //}
    }
    fprintf(f, "\\end{verbatim}\n");
    if (m._Singer){
        fprintf(f, "\\vspace{5mm}\n");
        fprintf(f, "\\hfill %s\n",m._Singer);    
    }
    fprintf(f, "\\end{center}\n");
    fprintf(f,"\\end{document}\n");
}

int commitCheckNext() {
    if (music.error != 0) {
        // need something here? 
    } else {
        FILE* f;
        if (argIndex < argc) {
            f= fopen(argv[argIndex],"w");
            writeLatex(music,f);
            fflush(f);
            fclose(f);
//            execl("/usr/bin/pdflatex","pdflatex",argv[argIndex], NULL);
        } else {
            char buffer[20];
            sprintf(buffer,"%d.tex",argIndex);
            f = fopen(buffer,"w");
            writeLatex(music,f);
            fflush(f);
            fclose(f);
//            execl("/usr/bin/pdflatex","pdflatex",buffer, NULL);            
        }
        argIndex++;
    }
    return music.error;
}

int main(int _argc, char* _argv[]){
    argc = _argc - 1 ; // first argument is the program name.
    argv = (char**) malloc(sizeof(char) * _argc );
    
    for(_argc--;_argc>0;_argc--){
        argv[_argc - 1] = strdup(_argv[_argc]);
    }
    start();
    yylex();
    commitCheckNext();
}

int yywrap() {

    return -1;
}




