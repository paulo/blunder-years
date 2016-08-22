#ifndef __VMCOMPILER_H__
#define __VMCOMPILER_H__

typedef struct sEntryVar *EntryVar;

typedef struct sEntryFun *EntryFun;

typedef enum eType{_VOID,_INTS,_INTA} Type;


typedef struct sAddr{
    int addr;
    char scope;
    Type type;
} Addr;


int initVarMap();

EntryFun containsFun(char* varName);

int decVar(char* varName, int size);

int decFun(Type type, char* funName);

int decAddFunArg(Type type, char* name);

void decFunArgRefresh();

int decFunRetAddr();

void endDecFun();

int expFun(char * fun);

int expFunNextArg(Type type);

int expFunNArgs();

Addr getAddr(char* varName);

#endif