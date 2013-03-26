/*
 *
 *  INFOIIS ASSIGNMENT 4
 *  Tijn Kersjes (3855473)
 *  Jordi Vermeulen (3835634)
 *
 */

:- [world].

% redefined safe/unsafe (using observations only)
possibleDanger(X, Y) :- possibleWumpus(X, Y).
possibleDanger(X, Y) :- possiblePit(X, Y).
safe(X, Y) :- \+ possibleDanger(X, Y).
possibleWumpus(X, Y) :- stench(U, V), aangrenzend(X, Y, U, V).
possiblePit(X, Y) :- breeze(U, V), aangrenzend(X, Y, U, V).
aangrenzend(X, Y, U, V) :- links(X, Y, U, V); rechts(X, Y, U, V); boven(X, Y, U, V); onder(X, Y, U, V).

% run/1
























