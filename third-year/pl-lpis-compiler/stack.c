#include <stdio.h>
#include <stdlib.h>
#include "stack.h"

struct sStack {
	int stack[256];
	int p;
}; 


Stack initStack()
{
	Stack s = (Stack) malloc(sizeof(struct sStack));
	s->p = 0;
}

int get(Stack s)
{
	int i;

	i = s->p;
	return s->stack[i];
}

int pop(Stack s)
{
	int i, res;

	i = s->p;
	res = s->stack[i];
	s->p--;
	
	return res;
}

void push(Stack s, int v)
{
	int i;

	s->p++;
	s->stack[s->p] = v;
}