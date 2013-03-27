/*
 *  INFOIIS ASSIGNMENT 4
 *  Tijn Kersjes (3855473)
 *  Jordi Vermeulen (3835634)
 */

:- [world].

% redefined safe/unsafe (using observations only)
possibleDanger(X, Y) :- possibleWumpus(X, Y).
possibleDanger(X, Y) :- possiblePit(X, Y).
safe(X, Y) :- \+ possibleDanger(X, Y).
possibleWumpus(X, Y) :- stench(U, V), aangrenzend(X, Y, U, V).
possiblePit(X, Y) :- breeze(U, V), aangrenzend(X, Y, U, V).
aangrenzend(X, Y, U, V) :- links(X, Y, U, V); rechts(X, Y, U, V); boven(X, Y, U, V); onder(X, Y, U, V).

seenSurrounded(X, Y, Z) :-  seenDirection(X, Y, Z, boven), 
							seenDirection(X, Y, Z, onder),
							seenDirection(X, Y, Z, links),
							seenDirection(X, Y, Z, rechts).

% 'Delegates'  --->  call(Z, X, Y)  ---->   Z(X, Y)
seenDirection(X, Y, _, D) :- \+ call(D, X, Y, _, _).
seenDirection(X, Y, Z, D) :- call(D, X, Y, X2, Y2), call(Z, X2, Y2). % and check if agent has seen X2,Y2 !

wumpus(X, Y) :- seenSurrounded(X, Y, stench).
pit(X, Y) :- seenSurrounded(X, Y, breeze).

/*
wumpus(X, Y) :- stench(X1, Y1), boven(X, Y, X1, Y1), 
				stench(X2, Y2), onder(X, Y, X2, Y2), 
				stench(X3, Y3), links(X, Y, X3, Y3), 
				stench(X4, Y4), rechts(X, Y, X4, Y4).

pit(X, Y) :- 	breeze(X1, Y1), boven(X, Y, X1, Y1), 
				breeze(X2, Y2), onder(X, Y, X2, Y2), 
				breeze(X3, Y3), links(X, Y, X3, Y3), 
				breeze(X4, Y4), rechts(X, Y, X4, Y4).
*/

% run/1
























