/*
 *
 *  INFOIIS world description
 *  Tijn Kersjes (3855473)
 *  Jordi Vermeulen (3835634)
 *   
 */

/*
 *  World:
 * 
 *  4   . . . .
 *  3   w G o o 
 *  2   . . . . 
 *  1   . . o .
 *
 *      1 2 3 4
 *
 */

% pits
breeze(X, Y) :- aangrenzend(X, Y, 3, 1).
breeze(X, Y) :- aangrenzend(X, Y, 3, 3).
breeze(X, Y) :- aangrenzend(X, Y, 4, 3).

% wumpus
stench(X, Y) :- aangrenzend(X, Y, 1, 3).

% gold
glitter(2, 3).

/*
 *  INTERFACE:
 *  kamer(?X,?Y) - geeft aan of (X,Y) een kamer is
 *  rechts(?X,?Y,?U,?V) - geeft aan dat (X,Y) rechts van (U,V) ligt
 *  links(?X,?Y,?U,?V) - geeft aan dat kamer (X,Y) links van kamer (U,V) ligt
 *  boven(?X,?Y,?U,?V) geeft aan dat kamer (X,Y) boven kamer (U,V) ligt
 *  onder(?X,?Y,?U,?V) - geeft aan dat kamer (X,Y) onder kamer (U,V) ligt
 *  glitter(+X,+Y) - slaagt als er op (X,Y) een glitter is waar te nemen
 *  breeze(+X,+Y) - slaagt als er op (X,Y) een breeze waar te nemen is
 *  stench(+X,+Y) - slaagt als er op (X,Y) een stench waar te nemen is
 */

tussen(X,Y,X) :- X =< Y.
tussen(X,Y,Z) :- A is X + 1, A =< Y, tussen(A,Y,Z).

kamer(X, Y) :- tussen(1, 4, X), tussen(1, 4, Y).
rechts(X, Y, U, V) :- kamer(X, Y), kamer(U, V), Y is V, U is X + 1.
links(X, Y, U, V) :- kamer(X, Y), kamer(U, V), Y is V, U is X - 1.
boven(X, Y, U, V) :- kamer(X, Y), kamer(U, V), Y is V - 1, X = U.
onder(X, Y, U, V) :- kamer(X, Y), kamer(U, V), Y is V + 1, X = U.

aangrenzend(X, Y, U, V) :- links(X, Y, U, V); rechts(X, Y, U, V); boven(X, Y, U, V); onder(X, Y, U, V).