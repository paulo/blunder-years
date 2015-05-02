%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- set_prolog_flag( toplevel_print_options, [quoted(true),numbervars(true),portrayed(true),max_depth(100)]).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- op( 900,xfy,'::' ).
:- op( 500,yfx,'e' ).
:- op( 500,yfx,'ou' ).

:- dynamic fabricante/2.
:- dynamic marca/2.
:- dynamic modelo/2.
:- dynamic proprietario/2.

:- dynamic excecao/1.
:- dynamic interdito/1.
:- dynamic incerto/1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%         Extenção à programação em lógica (disjunção)
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

demo( (A ou B), verdadeiro ) :- demo( A, verdadeiro ), demo( B , verdadeiro). 
demo( (A ou B), verdadeiro ) :- demo( A, verdadeiro ), demo( B , falso). 
demo( (A ou B), verdadeiro ) :- demo( A, falso ), demo( B , verdadeiro). 
demo( (A ou B), falso ) :- demo( A, falso ), demo( B , falso ). 

demo( (A ou B), desconhecido ) :- demo( A, falso ), demo( B, desconhecido ).
demo( (A ou B), desconhecido ) :- demo( A, desconhecido ), demo( B, falso ).
demo( (A ou B), verdadeiro ) :- demo( A, verdadeiro ), demo( B, desconhecido ).
demo( (A ou B), verdadeiro ) :- demo( A, desconhecido ), demo( B, verdadeiro ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%         Extenção à programação em lógica (conjunção)
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

demo( (A e B), verdadeiro ) :- demo( A, verdadeiro ), demo( B , verdadeiro).
demo( (A e B), falso ) :- demo( A, falso ), demo( B , falso). 
demo( (A e B), falso ) :- demo( A, verdadeiro ), demo( B , falso). 
demo( (A e B), falso ) :- demo( A, falso ), demo( B , verdadeiro). 

demo( (A e B), desconhecido ) :- demo( A, verdadeiro ), demo( B, desconhecido ).
demo( (A e B), desconhecido ) :- demo( A, desconhecido ), demo( B, verdadeiro ).
demo( (A e B), falso ) :- demo( A, falso ), demo( B, desconhecido ).
demo( (A e B), falso ) :- demo( A, desconhecido ), demo( B, falso ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%          Extenção à programação em logica (simples)
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

demo( A, verdadeiro ) :- A.
demo( A, falso ) :- -A.
demo( A, desconhecido ) :- nao( A ), nao( -A ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%         Predicados negativos de incerteza
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

-fabricante( Fabricante,Marca ) :-
    nao(fabricante( Fabricante,Marca )),
    nao(excecao( fabricante( Fabricante,Marca ))).
    
-marca( Matricula, Marca ) :-
    nao(marca( Matricula, Marca )),
    nao(excecao( marca( Matricula, Marca ))).

-modelo( Matricula, Modelo ) :-
    nao(modelo( Matricula, Modelo )),
    nao(excecao( modelo( Matricula, Modelo ))).

-proprietario( Matricula, Proprietario ) :-
    nao(proprietario( Matricula, Proprietario )),
    nao(excecao( proprietario( Matricula, Proprietario ))).

excecao(fabricante(X,Y)) :- fabricante(X,A), incerto(A).
excecao(marca(X,Y)) :- marca(X,A), incerto(A).
excecao(modelo(X,Y)) :- modelo(X,A), incerto(A).
excecao(proprietario(X,Y)) :- proprietario(X,A), incerto(A).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%         Evolucao
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

evolucaoNormal( T ) :-
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

insercao( Termo ) :-assert( Termo ).
insercao( Termo ) :-
retract( Termo ),!,fail.


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
%         Invariantes
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+marca( P,B ) :: (solucoes( (P,B),(marca( P,B )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

+modelo( P,M ) :: (solucoes( (P,M),(modelo( P,M )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

+proprietario( P,O ) :: (solucoes( (P,O),(proprietario( P,O )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

+fabricante( B,M ) :: (solucoes( (B,M),(fabricante( B,M )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).


% Invariante ... : não permitir que um modelo pertença a mais de um fabricante

+fabricante( B,M ) :: (solucoes( X,(fabricante( X,M )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).


% Invariante ... : não permitir que uma matricula pertença a mais que uma proprietario

+proprietario( P,O ) :: (solucoes( M,(proprietario( P,M )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).


% Invariante ... : não permitir que uma matricula pertença a mais que uma marca

+marca( P,B ) :: (solucoes( M,(marca( P,M )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

% Invariante ... : não permitir que uma matricula pertença a mais que um modelo

+modelo( P,M ) :: (solucoes( X,(modelo( P,X )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

% Invariante ... : não permitir a inserção de uma matricula num um modelo, sendo que essa matricula já está
%                  associada a uma marca que não é a fabricante do respetivo modelo 
% ver este 

+modelo( P,M ) :: (solucoes( B,(marca( P,B ), fabricante( B,M )), S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

% Invariante ... : não permitir a remoção de um modelo se existirem matriculas daquele modelo registados

-fabricante( B,M ) :: (solucoes( P,(modelo( P,M )), S),
                  comprimento( S,N ),
                  N == 0
                  ).


% Predicado evolucao: aumentar e/ou corrigir a base de conhecimento

evolucao(Q) :-
  rmIncerto(Q),
  rmImpreciso(Q),
  evolucaoNormal(Q).

rmIncerto( modelo(M,P)) :-
  modelo(M,X),
  incerto(X),
  retract(modelo(M,X)),
  retract(modelo(X)).
rmIncerto( fabricante(M,P)) :-
  fabricante(M,X),
  incerto(X),
  retract(fabricante(M,X)),
  retract(fabricante(X)).
rmIncerto( marca(M,P)) :-
  marca(M,X),
  incerto(X),
  retract(marca(M,X)),
  retract(marca(X)).
rmIncerto( proprietario(M,P)) :-
  proprietario(M,X),
  incerto(X),
  retract(proprietario(M,X)),
  retract(proprietario(X)).
rmIncerto(Q).

rmImpreciso( modelo(M,P)) :-
  execao(modelo(M,P)),
  retract(execao(modelo(M,X))).
rmImpreciso( fabricante(M,P)) :-
  execao(fabricante(M,P)),
  retract(execao(fabricante(M,X))).
rmImpreciso( marca(M,P)) :-
  execao(marca(M,P)),
  retract(execao(marca(M,X))).
rmImpreciso( proprietario(M,P)) :-
  execao(proprietario(M,P)),
  retract(execao(proprietario(M,X))).
rmImpreciso(Q).