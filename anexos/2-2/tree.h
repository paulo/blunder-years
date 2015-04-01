#ifndef ___TREE_H___
#define ___TREE_H___

#define MAX_SIZE 200

typedef struct tree *tree_ptr;

/*Insere um novo elemento na BS-tree*/
void insert(char *novo_desig, tree_ptr *p);

/*Imprime todos os nodos(elementos) da BS-tree por ordem alfabetica*/
void imprime_inorder_tree(tree_ptr p);

/*Imprime os elementos que aparecem no minimo n vezes*/
void imprime_mais_freq(tree_ptr p, int n);

/*Devolve numero de ocorrencias de todos os elementos da BS-tree*/
int total_Pubs(tree_ptr p);

/*Devolve numero de nodos(elementos) da BS-tree*/
int total_elems(tree_ptr p);

/*Copia uma BS-tree*/
tree_ptr copyTree (tree_ptr p);

/*Procura um elemento numa BS-tree e devolve um apontador ele caso se encontrar.*/
tree_ptr search_tree(char *desig, tree_ptr p);

/*Imprime por ordem decrescente de numero de ocorrencias.*/
void imprime_pos_order_by_N(tree_ptr p, int *N);

/*Liberta memoria de BS-tree*/
void makeempty(tree_ptr p);

/*Insere elementos em subarvore*/
void insert_subtree(char *novo_desig, tree_ptr *p);

/*Imprime para ficheiro uma dada subarvore*/
void treeToHtml(tree_ptr btree, FILE *f);

void subtree_to_html(tree_ptr sub_arv, FILE *f);

#endif