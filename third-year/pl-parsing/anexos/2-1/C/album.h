#ifndef __album_h__
#define __album_h__

typedef struct sPhoto *photo_ptr;

typedef struct sPhoto {
	char* file;
	char* date;
	char* loc;
	char* fact;
	char* who;
	photo_ptr left;
	photo_ptr right;
} photo_node;

typedef struct person *person_ptr;

typedef struct person
{
    char* name;
    char* href;
    photo_ptr pics;
    person_ptr next;
} person_node;

typedef struct sAlbum {
	person_ptr people;
	int count;
} Album;


void setDate(photo_ptr p, char *photoD);
void setLoc(photo_ptr p, char *photoL);
void setFact(photo_ptr p, char *photoF);
void setWho(photo_ptr p, char *photoW);
photo_ptr initPhoto(char *photoN);
void insert_photo_person(char *name, photo_ptr p);

#endif