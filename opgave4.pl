/*
 *  INFOIIS ASSIGNMENT 4
 *  Tijn Kersjes (3855473)
 *  Jordi Vermeulen (3835634)
 */

% Import world file
:- [world].

% Entry point: run/1 and run/0
run :- findGoldFormatted(1, 1).
run(Out) :- findGold(1, 1, Out).

% findGold/3 wrapper (for run/1)
findGold(X, Y, Out) :- safePositions(X, Y, Safe), findGlitter(Safe, (Xt, Yt)), findPath(Xt, Yt, X, Y, Safe, [(Xt, Yt)], Path),
                       convertDirections(Path, Actions), reverse(Safe, RevSafe), append(RevSafe, Actions, Out).

% Like findGold, but with formatted output (for run/0)
findGoldFormatted(X, Y) :- safePositions(X, Y, Safe), findGlitter(Safe, (Xt, Yt)), findPath(Xt, Yt, X, Y, Safe, [(Xt, Yt)], Path),
                           convertDirections(Path, Actions), reverse(Safe, RevSafe),
                           nl,
                           write('Explored rooms:'), nl,
                           writeRooms(RevSafe), nl,
                           write('Actions:'), nl,
                           writeActions(Actions, 1), nl, nl.

% Output list of rooms (for run/0)
writeRooms([]).
writeRooms([(Lx, Ly)|Ist]) :- write(' • '), write('('), write(Lx), write(', '), write(Ly), write(')'), nl, writeRooms(Ist).

% Output list of actions (for run/0)
writeActions([grab], N) :- write(' '), write(N), write('. '), write('Grab').
writeActions([goNorth|Rest], N) :- write(' '), write(N), write('. '), write('Go North'), nl, M is N + 1, writeActions(Rest, M).
writeActions([goSouth|Rest], N) :- write(' '), write(N), write('. '), write('Go South'), nl, M is N + 1, writeActions(Rest, M).
writeActions([goEast|Rest], N) :- write(' '), write(N), write('. '), write('Go East'), nl, M is N + 1, writeActions(Rest, M).
writeActions([goWest|Rest], N) :- write(' '), write(N), write('. '), write('Go West'), nl, M is N + 1, writeActions(Rest, M).

% Path planning (XTo, YTo, XFrom, YFrom, SafeRooms, VisitedRooms, Path)
findPath(X, Y, X, Y, _, _, [(X, Y)]).
findPath(X, Y, XFrom, YFrom, Safe, Visited, Path) :- aangrenzend(X, Y, Xu, Yu), member((Xu, Yu), Safe), \+ member((Xu, Yu), Visited),
                                                     findPath(Xu, Yu, XFrom, YFrom, Safe, [(Xu, Yu)|Visited], T), append(T, [(X, Y)], Path).

% Listing all safe locations (that are safely reachable from (X, Y))
safePositions(X, Y, Out) :- findall((Xn, Yn), aangrenzend(X, Y, Xn, Yn), L), safePositions([(X, Y)], [(X, Y)|L], [(X, Y)|L], Out).
safePositions(Visited, NoPit, NoWumpus, Out) :- member((X, Y), Visited), aangrenzend(X, Y, Xn, Yn), \+ member((Xn, Yn), Visited),
                                                member((Xn, Yn), NoPit), member((Xn, Yn), NoWumpus), !, % Cut operator forces all
                                                checkNoPit(Xn, Yn, Out1), checkNoWumpus(Xn, Yn, Out2),  % reachable rooms to be explored.
                                                append(Out1, NoPit, A), append(Out2, NoWumpus, B),
                                                filterUnique(A, AA), filterUnique(B, BB), % prevent double entries
                                                safePositions([(Xn, Yn)|Visited], AA, BB, Out).
safePositions(X, _, _, X).

% Converts list of locations to list of actions
convertDirections([_], [grab]).
convertDirections([(Px, Py),(Lx, Ly)|An], [goNorth|Out]) :- boven(Px, Py, Lx, Ly), convertDirections([(Lx, Ly)|An], Out).
convertDirections([(Px, Py),(Lx, Ly)|An], [goSouth|Out]) :- onder(Px, Py, Lx, Ly), convertDirections([(Lx, Ly)|An], Out).
convertDirections([(Px, Py),(Lx, Ly)|An], [goEast|Out]) :- rechts(Px, Py, Lx, Ly), convertDirections([(Lx, Ly)|An], Out).
convertDirections([(Px, Py),(Lx, Ly)|An], [goWest|Out]) :- links(Px, Py, Lx, Ly), convertDirections([(Lx, Ly)|An], Out).

% Searches for gold in list of rooms
findGlitter([], []).
findGlitter([(X, Y)|_], (X, Y)) :- glitter(X, Y).
findGlitter([(X, Y)|Rest], (U, V)) :- \+ glitter(X, Y), findGlitter(Rest, (U, V)).

% Search for rooms without pit with observations made from (X, Y)
checkNoPit(X, Y, Out) :- \+ breeze(X, Y), findall((Xn, Yn), aangrenzend(X, Y, Xn, Yn), Out), !, true. % cut operator prevents backtracking
checkNoPit(_, _, []).                                                                                 % on aangrenzend/4 preventing double results

% Search for rooms without wumpus with observations made from (X, Y)
checkNoWumpus(X, Y, Out) :- \+ stench(X, Y), findall((Xn, Yn), aangrenzend(X, Y, Xn, Yn), Out), !, true. % see above
checkNoWumpus(_, _, []).

% Removes double elements from list
filterUnique([], []).
filterUnique([L|Ist], Out) :- member(L, Ist), !, filterUnique(Ist, Out). % Only interested in the first occurence of L in Ist. Cut operator prevents
filterUnique([L|Ist], [L|Out]) :- filterUnique(Ist, Out).                % looking at rest of list after L was found when backtracking.

% Returns adjacent rooms (either left, right, up or down)
aangrenzend(X, Y, U, V) :- links(X, Y, U, V).
aangrenzend(X, Y, U, V) :- rechts(X, Y, U, V).
aangrenzend(X, Y, U, V) :- boven(X, Y, U, V).
aangrenzend(X, Y, U, V) :- onder(X, Y, U, V).