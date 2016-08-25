%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SEMANA 06: 23.MAR.2015 - 27.MAR.2015

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica estendida

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).


:- op( 900,xfy,'::' ).
:- dynamic jogo/3.
:- dynamic execao/1.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}

r :- consult('pt1.pl').

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

-ola(X).

elemento( X,[X|L] ).
elemento( X,[Y|L] ) :-
    X \== Y,
    pertence( X,L ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   - 
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).


% ficha7
% jogo: id, arb, ajc -> {V,D,F} 
% id numero do jogo
% arb nome do árbitro.
% aju ajudas

%jogo(Id,Arb,Aju) :- jogo(Id,Arb,Aju),
%                    nao(execao(-jogo(Id,Arb,Aju))).


-jogo(Id,Arb,Aju) :- nao(jogo(Id,Arb,Aju)),
                     nao(execao(jogo(Id,Arb,Aju))).

execao(jogo(Id,Arb,Aju)) :- jogo(Id,Arb,nulo).

% i O árbitro Almeida Antunes apitou o primeiro jogo do campeonato, no qual recebeu 500€ como ajudas de custo;
jogo(1,aa,500). 

% ii. O árbitro Baltazar Borges apitou o segundo jogo, tendo recebido a título de ajudas de custo um valor que 
%     ainda ninguém conhece;
jogo(2,bb,nulo). %este valor nulo representa desconhecido e é diferente do valor 

% iii. Consta na ficha de jogo da terceira partida, que o árbitro Costa Carvalho recebeu 500€, 
%      mas a comunicação social alega ter-lhe sido pago mais 2.000€ (como compensação por danos 
%      no seu veículo); instado a pronunciar-se sobre o assunto, o árbitro não confirma nem desmente nenhum dos valores noticiados;
%% Conhecimento imperfeito Impreciso
solucoes(X,Y,Z) :- findall(X,Y,Z).
evolucao( T ) :-
    solucoes(I,+T::I,S), %% aka solucoes
    insere(T),
    teste(S).

insere( T ) :- assert(T).
insere( T ) :- retract(T) , ! , fail. % para backtrack

teste([]).
teste([I|L]) :- I, teste(L).


%      #####################################
%                 Incerta 

t1 :- evolucaoIncerta(jogo(3,cc,X),X,[500,2000]).
evolucaoIncerta(Q,X,[H|T]) :-
    evolucaoIncerta(Q,X,T),
    X is H,
    assert(execao(Q)),
    fail.
evolucaoIncerta(Q,X,L).

evolucaoIncertaRm(Q,ID) :-
    +++
    ++
    -


%     ######################################
%                 Indefinido

t2 :- evolucaoIndefinida(jogo(4,cc,D),D,4).
evolucaoIndefinida(Q,X,ID) :-
    X is ID,
    assert(indefinido(X)),
    assert(Q).

evolucaoIndefinidaRm(Q,ID) :-
    indefinido(ID),
    retract(indefinido(ID)),
    retract(Q).
evolucaoIndefinidaRm(Q,ID) :- 
    nao(indefinido(ID)).


%    ####################################
%                Privado

evolucaoPrivada(Q,X,ID) :-
    X is ID,
    assert(privada(X)),
    assert(Q).

evolucaoPrivadaRm(Q,ID) :-
    indefinido(ID),
    retract(privada(ID)),
    retract(Q).

evolucaoPrivadaRm(Q,ID) :- 
    nao(indefinido(ID)).


% ix. O Guido e a Guida são os pais do Golias, mas como faleceram antes de registarem o filho, nunca será possível saber a data de
% nascimento do Golias;
filho(golias,guida).
filho(golias,guido).
nasceu(golias,privado01).
privado(privado01).





%execao( jogo(3, cc, 500)).
%execao( jogo(3, cc, 2000)).

% iv. O árbitro Duarte Durão apitou o quarto jogo, tendo recebido como ajudas de custo um valor que ronda os 250€ a 750€, 
%     desconhecendo-se qual a quantia exata;
%% Conhecimento imperfeito impreciso
%execao( jogo(4, dd, X)) :-
%    X =< 750, X >= 250.

% v. No quinto jogo apitado pelo árbitro Edgar Esteves, ocorreram tumultos no final do encontro tendo desaparecido as ajudas de
%    custo da carteira do árbitro, pelo que se torna impossível vir-se a conhecer esse valor;
%% confidencial.
jogo( 5,ee, nulo).
nulo( nulo ).

+jogo(Id,Arb,Aju) :: ( solucoes((Id,Arb, Aju),(jogo(Id,Arb,Aju),nao(nulo(Aju))),S),
                    comprimento(S,N),
                    N == 0).


comprimento( [],0 ).
comprimento( [X|L],N ) :-
    comprimento( L,N1 ),
    N is N1+1.

% vi. O árbitro do sexto jogo, Francisco França recebeu, como ajudas de custo, o valor de 250€; 
%     no entanto, entre amigos, refere ter “encaixado” nesse dia para cima de 5.000€;
%  recebeu 250 recebeu 250 euros, conhecimento perfeito positivo
%  recebeu >5000 "entre amigos" == pode ser mentiroso -> conhecimento imperfeito

jogo(6,ff,250).
%execao(jogo(6,ff,X)) :- X >= 5000. % impreciso
%execao(jogo(6,ff,5000)).
%nulo(5000).
% 250 só deixaria de ser verdade quando se soubesse que tinha realmente recebido um valor exato maior que 5000


%  vii. O árbitro Guerra Godinho que apitou o sétimo jogo, declara ser falso que alguma vez tenha recebido os 2.500€ 
%       que a comunicação social refere como tendo entrado na sua conta bancária;
%       contudo, este árbitro nunca confirmou o valor exato das ajudas de custo que recebeu;

-jogo(7,gg,2500).  %perfeito negativo
jogo(7,gg,nulo). %imperfeito do primeiro tipo

% viii. Não se conhecendo com exatidão o valor das ajudas de custo entregues ao árbitro Hélder Heitor no oitavo jogo,
%      aceita-se ter sido um valor cerca dos 1.000€;

execao(jogo(8,hh,1000)). %

% ix. Apesar de não se conhecer o valor exato das ajudas de custo pagas ao árbitro do nono jogo, Ivo Inocêncio, 
%     este terá recebido uma quantia muito próxima dos 3.000€;

jogo(9,ii,3000).    

%     Para além das situações descritas, aplicam-se as seguintes regras de funcionamento ao sistema de gestão e nomeação dos árbitros:

% x. Num mesmo jogo não pode existir mais do que um árbitro nomeado;

% xi. Um árbitro não pode apitar mais do que 3 partidas do campeonato;

% xii. O mesmo árbitro não pode apitar duas partidas consecutivas.

cerca(X,Sup,Inf) :-
    Sup is X * 1.25,
    Inf is X * 0.75.

mproximo(X,Sup,Inf) :-
    Sup is X * 1.1,
    Inf is X * 0.9.

%% criar evolucao de valores nulos
%% Interação apartir de java.


