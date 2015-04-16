r :- consult('ficha03.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

% i. Construir a extensão de um predicado que calcule o maior valor entre dois números;

max3(X,Y,X) :- 	X > Y.
max3(X,Y,Y) :- 	Y >= X.


% ii. Construir a extensão do predicado que implementa a negação por falha na prova;

nao(Q) :-
	Q,!,fail.

nao(Q).

%explicação: o cut(!) obriga a que o predicado Q possua sempre o valor que possui naquela unificaçao... 

% iii. Construir a extensão do predicado «pertence» que verifica se um elemento existe dentro de uma lista de elementos;

pertence(X,[X|L]).

pertence(X,[Y|L]) :-
	pertence(X,L).


% iv. Construir a extensão do predicado «comprimento» que calcula o número de elementos existentes numa lista;

% considerando que o comprimento de uma lista vazia é 0.
comprimento([],0).
comprimento([X|L],C) :-
	comprimento(L,N),
	C is N+1.


% v. Construir a extensão do predicado «quantos» que calcula a quantidade de elementos diferentes existentes numa lista;
quantos([X],1).
quantos([X|L],C):-
	pertence(X,L),
	quantos(L,C).
quantos([X|L],C):-
	quantos(L,Y),
	C is Y+1.

% quantos([X|L], N+1) :-
%		nao(pertence(X,L)),
%		quantos(L,N).


% vi. Construir a extensão do predicado «apagar» que apaga a primeira ocorrência de um elemento de uma lista;
apagar(X,[X|T],T).
apagar(X,[Y|T],[Y|T2]) :- 
		X \== Y,
		apagar(X,T,T2).


% vii. Construir a extensão do predicado «apagartudo» que apaga todas as ocorrências de um dado elemento numa lista;
apagartudo(_, [], []).
apagartudo(X, [X|T], R) :-
    apagartudo(X, T, R).
apagartudo(X, [H|T], [H|Y]) :-
    X \== H,	%apesar de nomes diferentes, mesmo assim é preciso comparar se X é diferente de H (podem ter o mesmo valor unificado)
    apagartudo(X, T, Y). %nao percebo pq é que é Y e não [H|Y] || como é que a lista final fica em R?



% viii. Construir a extensão do predicado «adicionar» que insere um elemento numa lista, sem repetidos;
adicionar(X,[X]).
adicionar(X,[X|L]) :-
		nao(pertence(X,L)).
	

% ix. Construir a extensão do predicado «concatenar», que resulta na concatenação dos elementos da lista L1 com os elementos da lista L2;
concatenar([],L,L).
concatenar([H|T],L,[H|R]) :-
		concatenar(T, L, R).

% x. Construir a extensão do predicado «inverter» que inverte a ordem dos elementos de uma lista;

%inverter([X],[X|_]).
%inverter([H|T], R) :-
%		inverter(T,R).


inverter([],[]).
inverter([H|T], R) :-
    inverter(T,Ri),
    concatenar(Ri,[H],R).



% xi. Construir a extensão do predicado «sublista» que determina se uma lista S é uma sublista de outra lista L;

sublista2(SL,L) :- concatena(L1,SL,L3), concatena(L3,L2,L). %forma feita pelo prof.

s1 :- sublista2([1,2],[1,2]).
s2 :- sublista2([1,2],[1,2,3]).

sublista([],R).
sublista(S,R) :- sublistaInicio(S,R).
sublista(S,[H|T]) :- sublista(S,T).

sublistaInicio([],TB).
sublistaInicio([A|TA],[A|TB]) :-
    sublistaInicio(TA,TB).


% xii. Construir a extensão de um predicado capaz de encontrar todas as possibilidades de prova de um teorema.

% predicado soluçoes/findall