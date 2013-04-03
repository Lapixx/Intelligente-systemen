/*
 *  INFOIIS ASSIGNMENT 4
 *  Tijn Kersjes (3855473)
 *  Jordi Vermeulen (3835634)
 */

:- [world].

run(Out) :- findGold(1, 1, Out).

aangrenzend(X, Y, U, V) :- links(X, Y, U, V); rechts(X, Y, U, V); boven(X, Y, U, V); onder(X, Y, U, V).

findGold(X, Y, Out) :- safePositions(X, Y, Safe), findGlitter(Safe, (Xt, Yt)), findGold(Xt, Yt, X, Y, Safe, [(Xt, Yt)], Path),
                       convertDirections(Path, Actions), reverse(Safe, RevSafe), append(RevSafe, Actions, Out).
findGold(X, Y, X, Y, _, _, [(X, Y)]) :- !, true.
findGold(X, Y, XFrom, YFrom, Safe, Visited, Path) :- !, aangrenzend(X, Y, Xu, Yu), member((Xu, Yu), Safe), \+ member((Xu, Yu), Visited),
                                                     findGold(Xu, Yu, XFrom, YFrom, Safe, [(Xu, Yu)|Visited], T), append(T, [(X, Y)], Path).

safePositions(X, Y, Out) :- findall((Xn, Yn), aangrenzend(X, Y, Xn, Yn), L), safePositions([(X, Y)], [(X, Y)|L], [(X, Y)|L], Out).
safePositions(Visited, NoPit, NoWumpus, Out) :- member((X, Y), Visited), aangrenzend(X, Y, Xn, Yn), \+ member((Xn, Yn), Visited),
                                                member((Xn, Yn), NoPit), member((Xn, Yn), NoWumpus), !,
                                                checkNoPit(Xn, Yn, Out1), checkNoWumpus(Xn, Yn, Out2),
                                                append(Out1, NoPit, A), append(Out2, NoWumpus, B),
                                                filterUnique(A, AA), filterUnique(B, BB),
                                                safePositions([(Xn, Yn)|Visited], AA, BB, Out).
safePositions(X, _, _, X).

convertDirections([_], [grab]).
convertDirections([(Px, Py),(Lx, Ly)|An], [goNorth|Out]) :- boven(Px, Py, Lx, Ly), convertDirections([(Lx, Ly)|An], Out).
convertDirections([(Px, Py),(Lx, Ly)|An], [goSouth|Out]) :- onder(Px, Py, Lx, Ly), convertDirections([(Lx, Ly)|An], Out).
convertDirections([(Px, Py),(Lx, Ly)|An], [goEast|Out]) :- rechts(Px, Py, Lx, Ly), convertDirections([(Lx, Ly)|An], Out).
convertDirections([(Px, Py),(Lx, Ly)|An], [goWest|Out]) :- links(Px, Py, Lx, Ly), convertDirections([(Lx, Ly)|An], Out).

findGlitter([], []).
findGlitter([(X, Y)|_], (X, Y)) :- glitter(X, Y), !, true.
findGlitter([_|Rest], (X, Y)) :- findGlitter(Rest, (X, Y)).

checkNoPit(X, Y, Out) :- \+ breeze(X, Y), findall((Xn, Yn), aangrenzend(X, Y, Xn, Yn), Out), !, true.
checkNoPit(_, _, []).

checkNoWumpus(X, Y, Out) :- \+ stench(X, Y), findall((Xn, Yn), aangrenzend(X, Y, Xn, Yn), Out), !, true.
checkNoWumpus(_, _, []).

filterUnique([], []).
filterUnique([L|Ist], Out) :- member(L, Ist), !, filterUnique(Ist, Out).
filterUnique([L|Ist], [L|Out]) :- filterUnique(Ist, Out).