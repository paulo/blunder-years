#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "tree.h"




typedef struct tree
{
    char desig[MAX_SIZE];
    int num;
    tree_ptr left, right, subtree;
} Tree;


void insert(char *novo_desig, tree_ptr *p)
{
    if ((*p) == NULL)
    {
        (*p)=(tree_ptr)malloc(sizeof(Tree));
        strcpy((*p)->desig,novo_desig);
        (*p)->num=1;
        (*p)->left=NULL;
        (*p)->right=NULL;
        (*p)->subtree=NULL;
    }
    else{
        int comp=strcmp(novo_desig,(*p)->desig);
        if(comp < 0)
        {
            insert(novo_desig, &(*p)->left);
        }
        else if (comp > 0)
            {
                insert(novo_desig, &(*p)->right);
            }
            else
                {
                (*p)->num++;
                }
        }
}

void insert_subtree(char *novo_desig, tree_ptr *p){
    if ((*p) != NULL)
    {
        insert(novo_desig, &(*p)->subtree);
    }
}


void subtree_to_html(tree_ptr sub_arv, FILE *f){
    if(sub_arv != NULL){
        subtree_to_html(sub_arv->left, f);
        fprintf(f, "<ul>");
        fprintf(f, "\t<li><p class=\"node\">%s</p></li>\n",sub_arv->desig);
        fprintf(f, "</ul>");
        subtree_to_html(sub_arv->right, f);
    }
}

void treeToHtml(tree_ptr btree, FILE *f){
    if (btree != NULL)
    {
        treeToHtml(btree->left, f); 
        fprintf(f, "<p class=\"root\">%s</p>\n",btree->desig);
        
        if(btree->subtree != NULL) 
            subtree_to_html(btree->subtree, f);
        
        treeToHtml(btree->right, f);
    }
}

void imprime_inorder_tree(tree_ptr p)
{
    if (p!=NULL)
    {
        imprime_inorder_tree(p->left);
        printf("%s -> %d\n",p->desig, p->num);
        if(p->subtree != NULL) imprime_inorder_tree(p->subtree);
        imprime_inorder_tree(p->right);
    }
}


tree_ptr copyTree (tree_ptr p)
{
    tree_ptr aux=NULL;
    if (p!=NULL) {
        insert(p->desig,&aux);
        aux->left=copyTree(p->left);
        aux->right=copyTree(p->right);
    }
    return aux;
}


void imprime_mais_freq(tree_ptr p, int n)                                
{                                                                                       
    if (p!=NULL)
    {
        imprime_mais_freq(p->left,n);
        if ((p->num)>n) printf("%s\n",p->desig);
        imprime_mais_freq(p->right,n);
    }
}


int total_Pubs(tree_ptr p)
{
    if (!p) return 0;
    else return (p->num)+total_Pubs(p->left)+total_Pubs(p->right);
}


int total_elems(tree_ptr p) {
    if (!p) return 0;
    else return 1 + total_elems(p->left)+total_elems(p->right);
}


tree_ptr search_tree(char *desig, tree_ptr p)
{
    if (!p) return NULL;
    else {
        int comp=strcmp(p->desig,desig);
        if (comp==0) return (p);
        else {
            if (comp>0)  return search_tree(desig, p->left);
            else return search_tree(desig, p->right);
        }
    }
}


void imprime_pos_order_by_N(tree_ptr p, int *N) {
    if (p){
        imprime_pos_order_by_N(p->right,N);
        if (*N){
            printf("Elemento: %s\n", p->desig);
            printf("Nmr: %d\n\n", p->num);
            (*N)--;
        }
        imprime_pos_order_by_N(p->left,N);
    }
}


void makeempty(tree_ptr p)
{
    if (p != NULL)
    {
        makeempty(p->left);
        makeempty(p->right);
        free(p);
    }
}