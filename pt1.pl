
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SEMANA 05: 16.MAR.2015 - 20.MAR.2015

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- set_prolog_flag(toplevel_print_options, [quoted(true),numbervars(true),portrayed(true),max_depth(100)]).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic filho/2.
:- dynamic pai/2.

:- dynamic sobrinho/2.
:- dynamic tio/2.

:- dynamic neto/2.
:- dynamic avo/2.

:- dynamic primo/2.
:- dynamic irmao/2.

:- dynamic casado/2.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%         UTILIDADES
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

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado filho: Filho,Pai -> {V,F,D}

rr :- consult('pt2.pl').

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+filho( F,P ) :: (solucoes( (F,P),(clause(filho( F,P ),true)),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

% Invariante Referencial: nao admitir mais do que 2 progenitores
%                         para um mesmo individuo

+filho( F,P ) :: ( solucoes( X, (filho( F, X )),S1),
                  unicos(S1,S),
                  comprimento( S,N ), 
                  N < 3
                  ).

% não deixar que pai tenha info repetida.

+pai(F,P) :: (solucoes((F,P),pai(P,F),S1),
                unicos(S1,S),
                comprimento(S,N),
                N == 0).


+avo(A,N) :: nao(pai(A,N)).
+avo(A,N) :: nao(filho(A,N)).
+avo(A,N) :: nao(neto(A,N)).
+neto(N,A) :: nao(filho(N,A)).
+neto(N,A) :: nao(pai(N,A)).
+neto(N,A) :: nao(avo(N,A)).
+pai(P,F) :: nao(avo(P,F)).
+pai(P,F) :: nao(filho(P,F)).
+pai(P,F) :: nao(neto(P,F)).

+neto(Ne,A) :: (solucoes(X,neto(Ne,X),S),
               comprimento(S,N),
               N =< 4 ).

+neto( Ne,A ) :: (solucoes( (Ne,A),(clause(neto( Ne,A ),true)),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

+naturalidade(A,L,AN,AM) :: ( solucoes(X,naturalidade(A,X,AN,AM),S),
                              comprimento(S,N),
                              N =< 1
                            ).

+naturalidade(A,L,AN,AM) :: AN =< AM.

+tio(X,Y) :: nao(pai(X,Y)).
+tio(X,Y) :: nao(sobrinho(X,Y)).
+tio(X,Y) :: nao(irmao(X,Y)).

+sobrinho(X,Y) :: nao(pai(X,Y)).
+sobrinho(X,Y) :: nao(tio(X,Y)).
+sobrinho(X,Y) :: nao(irmao(X,Y)).

-tio(X,Y) :: (nao(tio(X,Y))).
-sobrinho(X,Y) :: (nao(sobrinho(X,Y))).
-primo(X,Y) :: (nao(primo(X,Y))).
-irmao(X,Y) :: (nao(irmao(X,Y))).

+irmao(X,Y) :: nao(pai(X,Y)).
+irmao(X,Y) :: nao(filho(X,Y)).
+irmao(X,Y) :: nao(primo(X,Y)).
+irmao(X,Y) :: nao(tio(X,Y)).
+irmao(X,Y) :: nao(sobrinho(X,Y)).
+irmao(X,Y) :: nao(avo(X,Y)).
+irmao(X,Y) :: nao(neto(X,Y)).

+primo(X,Y) :: nao(pai(X,Y)).
+primo(X,Y) :: nao(filho(X,Y)).
+primo(X,Y) :: nao(irmao(X,Y)).
+primo(X,Y) :: nao(tio(X,Y)).
+primo(X,Y) :: nao(sobrinho(X,Y)).
+primo(X,Y) :: nao(avo(X,Y)).
+primo(X,Y) :: nao(neto(X,Y)).


% self deterministic 
pai(X,Y) :- clause(filho(Y,X),true).
filho(X,Y) :- pai(Y,X). % nao(clause(filho(X,Y),true)),

avo(X,Y) :- clause(neto(Y,X),true).
neto(X,Y) :- avo(Y,X). % nao(clause(neto(X,Y),true)), 

tio(X,Y) :- clause(sobrinho(Y,X),true).
sobrinho(X,Y) :-tio(Y,X).%clause(tio(X,Y),B), B\=sobrinho(Y,X). nao(clause(sobrinho(X,Y),true))
 
% ligações:
filho(X,Y) :- irmao(X,A), pai(Y,A).

% avo
avo(X,Y) :- pai(X,A), pai(A,Y).
avo(X,Y) :- clause(primo(Y,A),true), clause(avo(X,A),true).
avo(X,Y) :- clause(primo(A,Y),true), clause(avo(X,A),true).

%tio
tio(X,Y) :- irmao(X,A), pai(A,Y), X\=Y.
tio(X,Y) :- pai(X,A), primo(Y,A), X\=Y.
tio(X,Y) :- pai(A,X), avo(A,Y), nao(pai(X,Y)),  X\=Y.

%primo
primo(X,Y) :- clause(primo(Y,X),true).
primo(X,Y) :- pai(A,Y), pai(B,X), irmao(A,B), A\=B, X\=Y.
primo(X,Y) :- pai(A,Y), clause(tio(A,X),true), X\=Y.
primo(X,Y) :- pai(A,X), clause(tio(A,Y),true), X\=Y.
primo(X,Y) :- avo(A,Y), avo(A,X), nao(irmao(X,Y)), X\=Y.

irmao(X,Y) :- clause(irmao(Y,X),true).
irmao(X,Y) :- pai(A,X), pai(A,Y), X\=Y.


%clause(neto(X,Y),(clause(filho(X,T1),true), clause(filho(T1,Y),true),  Y \= X),R).

% clause(neto(X,Y), clause(filho(X,T1),true), clause(filho(T1,Y),true),  Y \= X, R)
load :-
    evolucao(pai(a1,a2)),
    evolucao(pai(a1,a3)),
    evolucao(casado(b1,a2)),
    evolucao(filho(ab1,b1)),
    evolucao(filho(ab1,a2)),
    evolucao(filho(ab2,b1)),
    evolucao(filho(ab2,a2)),
    evolucao(casado(c2,a3)),
    evolucao(filho(ac1,c2)),
    evolucao(filho(ac1,a3)),
    evolucao(filho(ac2,c2)),
    evolucao(filho(ac2,a3)),
    evolucao(neto(c2,c1)),
    evolucao(neto(a1,i1)).

filhos(X,S) :- solucoes(A,filho(A,X),S1), unicos(S1,S).
sobrinhos(X,S) :- solucoes(A,sobrinho(A,X),S1), unicos(S1,S).
netos(X,S) :- solucoes(A,neto(A,X),S1), unicos(S1,S).
primos(X,S) :- solucoes(A,primo(X,A),S1), unicos(S1,S).
tios(X,S) :- solucoes(A,tio(X,A),S1), unicos(S1,S).
irmaos(X,S) :- solucoes(A,irmao(X,A),S1), unicos(S1,S).


descendentes(A,N,R) :- descendentesAux([A],N,R1), unicos(R1,R).

descendentesAux([],_,[]).
descendentesAux(_,N,[]) :- N =< 0.

descendentesAux([A],1,R) :-
    filhos(A,R).

descendentesAux([A],N,R) :- 
    N > 1,
    filhos(A, F1), N1 is N -1,
    netos(A, F2), N2 is N -2,
    descendentesAux(F1,N1,R1),
    descendentesAux(F2,N2,R2),
    concatena(F1,F2,A1), concatena(A1,R1,A2), concatena(A2,R2,R).

descendentesAux([A|T],N,R) :- 
    descendentesAux([A],N,R1),
    descendentesAux(T,N,R2),
    concatena(R1,R2,R).

descendenteAG(A,B,1):- filho(A,B).
descendenteAG(A,B,1):- sobrinho(A,B).
descendenteAG(A,B,2):- neto(A,B).
descendenteAG(A,B,G):-
    filho(A,D),
    descendenteAG(D,B,H),
    G is H + 1.
descendenteAG(A,B,G):-
    sobrinho(A,D),
    descendenteAG(D,B,H),
    G is H + 1.
descendenteAG(A,B,G):-
    neto(A,D),
    descendenteAG(D,B,H),
    G is H + 2.

relacoes(I1,I2,S) :-
   relacoesAux(I1,I2,Q),
   quais(Q,[],S). 

relacoesAux(X,Y,[filho(X,Y),pai(X,Y),primo(X,Y),sobrinho(X,Y),avo(X,Y),neto(X,Y)]).

quais([H|Q],R,S) :-
    H,
    quais(Q,[H|R],S).

quais([H|Q],R,S) :-
    nao(H),
    quais(Q,R,S).

quais([],S,S).