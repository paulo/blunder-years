r :- consult('ficha02.pl').


% i. Construir a extensão de um predicado que calcule a soma de dois valores;
soma(A,B,R) :-
		R is A+B.


% ii. Construir a extensão de um predicado que aplique uma operação aritmética (adição, subtração, multiplicação, divisão) a dois valores;
operacao(A,B,+,R) :-
		R is A + B.
operacao(A,B,-,R) :-
		R is A - B.
operacao(A,B,*,R) :-
		R is A * B.
operacao(A,B,/,R) :-
		B \== 0,
		R is A / B.


% iii. Construir a extensão de um predicado que calcule a soma de três valores;
soma3(A,B,C,R) :-
	R is A + B + C.


% iv. Construir a extensão de um predicado que calcule a soma de um conjunto de valores;
somalista([], 0).
somalista([H|T], R) :-
		somalista(T,X),
		R is X + H.
 

% v. Construir a extensão de um predicado que aplique uma operação aritmética (adição, subtração, multiplicação, divisão) a um conjunto de valores;
oplista([],+,0).
oplista([],-,0).
oplista([],*,1).
oplista([],/,1).
oplista([H|T],+,R) :- oplista(T,X), R is X + H.
oplista([H|T],-,R) :- oplista(T,X), R is X - H.
oplista([H|T],*,R) :- oplista(T,X), R is X * H.
oplista([H|T],/,R) :- oplista(T,X),
						%falta caso em que divisor é 0 
						R is X / H.

% vi. Construir a extensão de um predicado que calcule o maior valor entre dois números;
max(X,Y,Y) :- X<Y.
max(X,Y,X) :- X>=Y.



% vii. Construir a extensão de um predicado que calcule o maior de um conjunto de valores;
maxlista([X],X).
maxlista([X|T],X):- maxlista(T,Y), X >=Y.
maxlista([X|T],N):- maxlista(T,N), N > X.


% viii. Construir a extensão de um predicado que calcule o menor valor entre dois números;
menor(X,Y,X) :- X<Y.
menor(X,Y,Y) :- Y=<X.


% ix. Construir a extensão de um predicado que calcule o menor de um conjunto de valores;
menorlista([X],X).
menorlista([X|T],X) :- menorlista(T,Y), X=<Y.
menorlista([X|T],N) :- menorlista(T,N), N<X.


% x. Construir a extensão de um predicado que contabilize a quantidade de conjuntos vazios que existem num determinado conjunto de ocorrências;
% vazios([],1).
% vazios([H],0).
% vazios([[]|T],R) :-
% 		vazios(T,G),
% 		R is G+1.


% xi. Construir a extensão de um predicado que calcule o valor de verdade contrário à resposta a uma questão.

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).
