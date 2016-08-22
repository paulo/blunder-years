#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hashmap.h"
#include "vmCompiler.h"

#define OK 0
#define ERRO_VAR_ALREADY_EXIST -1
#define ERRO_VAR_DONT_EXIST -2
#define ERRO_VAR_INVALID_TYPE -3
#define ERRO_FUN_DONT_EXIST -4
#define ERRO -5

void yyerror(char *s);

struct sEntryVar{
    Type type;
    char *name;
    int memAdr;
};

typedef struct sFunArg
{
    struct sEntryVar* v; 
    struct sFunArg *next;
}* FunArgL;

struct sEntryFun{
    Type type;
    char *name;
    FunArgL args;
    FunArgL argsEnd;
    int nargs;
};

typedef struct sScope{
    map_t vars;
    int addrCount;
}* Scope;

static EntryFun inUseFun;
static EntryFun decFunAux;
static Scope gloContext;
static Scope funContext;

static map_t mFuncMap;


EntryVar containsVar(Scope fun, char* varName);


int initVarMap()
{
    gloContext = (Scope) malloc(sizeof(struct sScope));
    gloContext->vars = hashmap_new();
    gloContext->addrCount = 0;
	//addressCounter = 0;
	//mVarMap = hashmap_new();
	mFuncMap = hashmap_new();
    return 0;
}


EntryFun containsFun(char* varName)
{
    EntryFun varEntry; //= (Entry) malloc(sizeof(Entry));
    
    if(!(hashmap_get(mFuncMap, varName, (any_t*) &varEntry) == MAP_OK))
        varEntry = NULL;
    return varEntry;    
}

EntryVar containsVar(Scope fun, char* varName)
{
	EntryVar varEntry; //= (Entry) malloc(sizeof(Entry));
    if (fun==NULL)
        fun = gloContext;
	
	if(!(hashmap_get(fun->vars, varName, (any_t*) &varEntry) == MAP_OK))
		varEntry = NULL;
	return varEntry;	
}

int decFun(Type type,char* funName){
    int ret;
    if(!containsFun(funName)) {
        EntryFun newFun = (EntryFun) malloc(sizeof(struct sEntryFun));
        newFun->name = strdup(funName);
        newFun->type = type;
        newFun->args = NULL;
        newFun->argsEnd = NULL;
        newFun->nargs  = 0;
        
        hashmap_put(mFuncMap, funName, (any_t) newFun);
        decFunAux = newFun;

        funContext = (Scope) malloc(sizeof(struct sScope));
        funContext->vars = hashmap_new();
        funContext->addrCount = 0;
        ret = OK;
    } else {
        yyerror("Variável já declarada anteriormente");
        ret = ERRO_VAR_ALREADY_EXIST;
    }
    return ret;
}

int decAddFunArg(Type type, char* name){
    if(decFunAux->argsEnd == NULL){
        decFunAux->argsEnd = (FunArgL) malloc(sizeof(struct sFunArg));
        decFunAux->args = decFunAux->argsEnd;
    } else {
        decFunAux->argsEnd->next = (FunArgL) malloc(sizeof(struct sFunArg));
        decFunAux->argsEnd = decFunAux->argsEnd->next;
    }
    decFunAux->argsEnd->next = NULL;
    //funContext->argsEnd->v
    int err = decVar(name,1);
    if (err == OK){
        (decFunAux->nargs)++;
        hashmap_get(funContext->vars, name, (any_t*) &(decFunAux->argsEnd->v));
    }
    return err;
}

void decFunArgRefresh(){
    FunArgL i = decFunAux->args;
    while(i != NULL){
        i->v->memAdr -= decFunAux->nargs;
        i = i->next;
    }
}

int decFunRetAddr(){
    return -(decFunAux->nargs) - 1;
}

void endDecFun(){
    decFunAux = NULL;
    funContext = NULL;
}

int expFun(char * fun){
    inUseFun = containsFun(fun);
    inUseFun->argsEnd = inUseFun->args;

    if(inUseFun == NULL) {
        yyerror("ERROR Funtion don't exist");
        return ERRO_FUN_DONT_EXIST;
    }
}
int expFunNextArg(Type type){
    if(inUseFun->argsEnd == NULL){
        yyerror("ERROR invalid number of arguments");
        return ERRO;
    }

    if(type != inUseFun->argsEnd->v->type){
        yyerror("ERROR types don't match");
        return ERRO_FUN_DONT_EXIST;
    }


    inUseFun->argsEnd = inUseFun->argsEnd->next;
    return OK;
}
int expFunNArgs(){
    if(inUseFun->argsEnd != NULL){
        yyerror("ERROR invalid number of arguments");
        return ERRO;
    }
    return inUseFun->nargs;
}

int decVar(char* varName, int size)
{
    Scope context; 
    int err = 0;
    if (funContext == NULL) { 
        context = gloContext;
        err += (containsVar(gloContext, varName) != NULL);
    } else {
        context = funContext;        
        err += (containsVar(funContext, varName) != NULL);
        err += (containsVar(gloContext, varName) != NULL);
    }

    if(!err)
	{
		EntryVar newVar = (EntryVar) malloc(sizeof(struct sEntryVar));
		if(size>1)
		{
			newVar->type = _INTA;
		}
		else
		{
            newVar->type = _INTS;
		}
		newVar->name = strdup(varName);
		newVar->memAdr = context->addrCount;
		context->addrCount+=size;

		hashmap_put(context->vars, varName, (any_t) newVar);

		return OK;
	}
    yyerror("Variável já declarada anteriormente");
	return ERRO_VAR_ALREADY_EXIST;
}


Addr getAddr(char* varName)
{
    EntryVar varEntry;
	int memAddr;
    char scope;
    Type type;

    if (funContext == NULL) { 
        varEntry = containsVar(gloContext,varName);
        scope = 'G';
    } else {
        varEntry = containsVar(funContext,varName);
        scope = 'L';    
        if(varEntry == NULL) {
            varEntry = containsVar(gloContext,varName);
            scope = 'G';    
        }
    }

	if(varEntry != NULL) {
		memAddr = varEntry->memAdr;
        type = varEntry->type;
	} else {
        memAddr = ERRO_VAR_DONT_EXIST;
        yyerror("Variável não declarada");
    }


    Addr ret = {memAddr,scope,type};

	return ret;
}
