#ifndef __STACK_H__
#define __STACK_H__

typedef struct sStack *Stack;

Stack initStack();

int get(Stack s);

int pop(Stack s);

void push(Stack s, int v);

#endif
