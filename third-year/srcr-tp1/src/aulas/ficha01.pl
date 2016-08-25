% Ficha 1
% Paulo Araújo

% --------------------------------------------------------------------------------------------------------

% i. O João é filho do José;

:- dynamic filho/2.

filho(joao,jose).
filho(i1,i2).
filho(i2,i3).
filho(i3,i4).
filho(i4,i5).
filho(i5,i6).

% ii. O José é filho do Manuel;

filho(jose,manuel).


% iii. O Carlos é filho do José;

filho(carlos,jose).

% iv. O Paulo é pai do Filipe;
% xi. Construir a extensão de um predicado capaz de determinar que o indivíduo P é pai do indivíduo F se existir uma prova de que F seja filho de P;

:- dynamic pai/2.

pai(paulo,filipe).

pai(P,F) :-
	filho(F,P).

% v. O Paulo é pai da Maria;

pai(paulo,maria).


% vi. O António é avô da Nádia;
% xii. Construir a extensão de um predicado capaz de determinar que o indivíduo A é avô de N se existir um indivíduo X de quem N seja filho e de quem A seja pai;
% xiii. Construir a extensão de um predicado capaz de determinar que o indivíduo N é neto do indivíduo A se existir uma prova de que A seja avô de N;

:- dynamic neto/2.
:- dynamic avo/2.

avo(A,N) :-
	neto(N, A).

avo(A,N) :-
	filho(N,X),
	pai(A,X).

avo(antonio,nadia).


neto(N,A) :-
	filho(N,X),
	pai(A,X).

% vii. O João é do sexo masculino;

:- dynamic genero/2.

genero(joao,masculino).


% viii. O José é do sexo masculino;

genero(jose,masculino).


% ix. A Maria é do sexo feminino;

genero(maria,feminino).

% x. A Joana é do sexo feminino;

genero(joana,feminino).


% xiv. Construir a extensão de um predicado que permita determinar se uma pessoa X descende de outra pessoa Y;

:- dynamic descendente/2. 

descendente(X,Y) :-
	filho(X,Y).

descendente(X,Y) :-
	filho(X,Z),
	descendente(Z,Y).


% xv. Construir a extensão de um predicado que permita determinar o grau de descendência entre duas pessoas, X e Y;

grau(D,A,1) :-
	filho(D,A).

grau(D,A,G) :-
	filho(D,X),
	grau(X,A,N),
	G is N+1.


% xvi. Construir a extensão de um predicado capaz de determinar se o indivíduo A é avô de N pela utilização do predicado que determina o grau de descendência entre dois indivíduos;

avo2(A,N) :- 
	grau(A,N,2).


% xvii. Construir a extensão de um predicado capaz de determinar se o indivíduo X é bisavô de Y;

bisavo(B,N) :-
	grau(B,N,3).


% xviii. Construir a extensão de um predicado capaz de determinar se o indivíduo X é trisavô de Y;

trisavo(T,N) :-
	grau(T,N,4).


% xix. Construir a extensão de um predicado capaz de determinar se o indivíduo X é tetraneto de Y.2 de 2

tetraneto(T,N) :-
	grau(N,T,5).

