%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- set_prolog_flag(toplevel_print_options, [quoted(true),numbervars(true),portrayed(true),max_depth(100)]).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).

:- dynamic fabricante/2.
:- dynamic marca/2.
:- dynamic modelo/2.
:- dynamic proprietario/2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%          Extenção à programação em logica
%--------------------------------- - - - - - - - - - -  -  -  -  -   -


demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%         Evolucao
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

evolucao( T ) :-
    solucoes(I,+T::I,S), %% aka solucoes
    insere(T),
    teste(S).

remove( T ) :-
    solucoes(I,-T::I,S), %% aka solucoes
    apaga(T),
    teste(S).

whyevo( T , ER) :-
    solucoes(I,+T::I,S),
    assert(T),
    teste(S, ER),
    retract(T).

whyrem( T , ER) :-
    solucoes(I,-T::I,S),
    retract(T),
    teste(S, ER),
    assert(T).

insere( T ) :- assert(T).
insere( T ) :- retract(T) , ! , fail. % para backtrack

apaga( T ) :- retract(T).
apaga( T ) :- assert(T) , ! , fail. % para backtrack

teste([]).
teste([I|L]) :- I, teste(L).

teste([],ok).
teste([I|L], ER) :- I, teste(L, ER).
teste([I|L], I) :- nao(I). %head(L,ER). % por um lado tenho que retornar o backtrack
%head([H|L], H).

nao(X) :- X, !, fail.
nao(X).

solucoes(A,Q,S) :- findall(A,Q,S).
solucoes2(A,Q,S) :-
    Q,
    assert(temp(A)),
    fail.
%solucoes(A,Q,S) :- obter([],S).
%obter(X,S) :-
%    retract(temp(A)), obter([A|X],S).
%obter(S,S).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%         Funções Lista
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

concatena([LH|LT], K, [LH|R]) :-
    concatena(LT,K,R).
concatena([], K, K).


comprimento( [],0 ).
comprimento( [X|L],N ) :-
    comprimento( L,N1 ),
    N is N1+1.

% unicos_aux([],LIST,RES)
unicos(L,S) :- unicosaux([],L,S).


unicosaux(AUX,[H|T],S) :- naoexiste(H,AUX), unicosaux([H|AUX],T,S).
unicosaux(AUX,[H|T],S) :- nao(naoexiste(H,AUX)), unicosaux(AUX,T,S).
unicosaux(S,[],S).

naoexiste(A,[]).
naoexiste(A,[H|T]) :- A\=H, naoexiste(A,T).
