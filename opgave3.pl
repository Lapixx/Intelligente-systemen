/*
 *
 *  INFOIIS ASSIGNMENT 3
 *  Tijn Kersjes (3855473)
 *  Jordi Vermeulen (3835634)
 *
 */
 
/* Definitions from previous assignment */

coordinate(1).
coordinate(2).
coordinate(3).
coordinate(4).
tile(X,Y) :- coordinate(X), coordinate(Y).
south(tile(X1,Y1), tile(X2,Y2)) :- tile(X1,Y1), tile(X2,Y2), X1 = X2, 1 is Y1 - Y2.
north(tile(X1,Y1), tile(X2,Y2)) :- tile(X1,Y1), tile(X2,Y2), X1 = X2, 1 is Y2 - Y1.
west(tile(X1,Y1), tile(X2,Y2)) :- tile(X1,Y1), tile(X2,Y2), 1 is X1 - X2, Y1 = Y2.
east(tile(X1,Y1), tile(X2,Y2)) :- tile(X1,Y1), tile(X2,Y2), 1 is X2 - X1, Y1 = Y2.
adjacent(A, B) :- north(A, B).
adjacent(A, B) :- south(A, B).
adjacent(A, B) :- east(A, B).
adjacent(A, B) :- west(A, B).

tussen(X,Y,X) :- X =< Y.
tussen(X,Y,Z) :- A is X + 1, A =< Y, tussen(A,Y,Z).

/* Imports */

:-use_module(library(bounds)). %for all_different

/*
 *   1a. plan/2:
 *       Chooses a new adjacent tile that is not in the visited list. This way cycles are avoided.
 */

plan(Xt, Yt, Out) :- plan2(tile(Xt, Yt), Out, [[Xt, Yt]]).
plan2(tile(1,1), [tile(1,1)], _) :- !, true.
plan2(Target, NewT, Closed) :- !, adjacent(tile(Xu, Yu), Target), not(member([Xu,Yu], Closed)), plan2(tile(Xu, Yu), T, [[Xu, Yu]|Closed]), append(T, [Target], NewT). 

/* 
 *  1b. safePlan/3:
 *      Wrapper for previous plan that adds all unsafe tiles to the visited list before starting.
 *      This way, the unsafe tiles are avoided.
 */

% define unsafe and safe tiles
unsafe(X, Y) :- tile(X, Y), \+ safe(X, Y).
safe(X,Y) :- noWumpus(X,Y), noPit(X,Y).
noWumpus(X,Y) :- \+ wumpus(X,Y).
noPit(X,Y) :- \+ pit(X,Y).

% add 3 pits and a wumpus in the game world
pit(3, 1).
pit(3, 3).
pit(4, 4).
wumpus(1, 3).

safePlan(Xt, Yt, Out) :- findall([X, Y], unsafe(X, Y), Unsafe), plan2(tile(Xt, Yt), Plan, [[Xt, Yt]|Unsafe]), convertDirections(Plan, Out).

% converts steps to 'natural language'
convertDirections([_], []).
convertDirections([P,L|An], [goNorth|Out]) :- north(P, L), convertDirections([L|An], Out).
convertDirections([P,L|An], [goSouth|Out]) :- south(P, L), convertDirections([L|An], Out).
convertDirections([P,L|An], [goEast|Out]) :- east(P, L), convertDirections([L|An], Out).
convertDirections([P,L|An], [goWest|Out]) :- west(P, L), convertDirections([L|An], Out).

/*
 * 2. colourSort/2:
 *    Bubble sort adjusted so that instead of comparing 2 items with each other,
 *    the element's position in the provided Order list are compared.
 */

colourSort(List, Order, Sorted) :- swap(List, List1, Order), !, colourSort(List1, Order, Sorted). 
colourSort(Sorted, _, Sorted).

swap([X,Y|Rest],[Y,X|Rest], Order) :- nth1(Ix, Order, X), nth1(Iy, Order, Y), Ix > Iy. %changed part
swap([Z|Rest],[Z|Rest1], Order) :- swap(Rest,Rest1, Order).


/* 
 * 3. quickSort
 *    Just like in real life
 */

quickSort([], []).
quickSort([H|In], Out) :- partition(In, H, L, G), quickSort(L, Out1), quickSort(G, Out2), append(Out1, [H|Out2], Out).

partition([], _, [], []).
partition([H|T], P, [H|Smaller], Larger) :- H =< P, partition(T, P, Smaller, Larger).
partition([H|T], P, Smaller, [H|Larger]) :- H > P, partition(T, P, Smaller, Larger).

/*
 * 4. Eight Queens Puzzle
 *    queensolution/8: Checks if all 8 Y-positions are unique
 *    (because 2 Queens can't be on the same row), and if 2 Queens are
 *    not on the same diagonal line.
 */

queensolution(Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8) :- % has 92 solutions: most likely valid
    % values should be numbers from 1-8
    tussen(1, 8, Y1), tussen(1, 8, Y2), tussen(1, 8, Y3), tussen(1, 8, Y4),
    tussen(1, 8, Y5), tussen(1, 8, Y6), tussen(1, 8, Y7), tussen(1, 8, Y8), 
    % enforce unique Y positions
    all_different([Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8]),
    % check diagonals
    1 =\= abs(Y1 - Y2), 2 =\= abs(Y1 - Y3), 3 =\= abs(Y1 - Y4), 4 =\= abs(Y1 - Y5), 5 =\= abs(Y1 - Y6), 6 =\= abs(Y1 - Y7), 7 =\= abs(Y1 - Y8),
                        1 =\= abs(Y2 - Y3), 2 =\= abs(Y2 - Y4), 3 =\= abs(Y2 - Y5), 4 =\= abs(Y2 - Y6), 5 =\= abs(Y2 - Y7), 6 =\= abs(Y2 - Y8),
                                            1 =\= abs(Y3 - Y4), 2 =\= abs(Y3 - Y5), 3 =\= abs(Y3 - Y6), 4 =\= abs(Y3 - Y7), 5 =\= abs(Y3 - Y8),
                                                                1 =\= abs(Y4 - Y5), 2 =\= abs(Y4 - Y6), 3 =\= abs(Y4 - Y7), 4 =\= abs(Y4 - Y8),
                                                                                    1 =\= abs(Y5 - Y6), 2 =\= abs(Y5 - Y7), 3 =\= abs(Y5 - Y8),
                                                                                                        1 =\= abs(Y6 - Y7), 2 =\= abs(Y6 - Y8),
                                                                                                                            1 =\= abs(Y7 - Y8).



/* 5. BONUS - Sudoku

Example input:

sudoku([
[0,0,0,1,0,0,0,6,0],
[2,9,0,0,0,0,4,7,0],
[0,3,0,0,0,2,0,0,0],
[0,0,7,6,0,0,0,0,5],
[0,0,0,0,0,0,0,0,0],
[5,0,0,0,0,8,7,0,0],
[0,0,0,7,0,0,0,5,0],
[0,5,9,0,0,0,0,1,8],
[0,8,0,0,0,9,0,0,0]
]).

*/

% Finds and prints solution to sudoku
sudoku([A, B, C, D, E, F, G, H, I]) :-
    sudokuSolved([A, B, C, D, E, F, G, H, I], Solution),
    nl, nl, write('Input:'), nl,
    printSudoku([A, B, C, D, E, F, G, H, I]), nl,

    nl, nl, write('Solution:'), nl,
    printSudoku(Solution), nl.

% Prints sudoku matrix
printSudoku([A, B, C, D, E, F, G, H, I]) :-
    write('  -----------------------'), nl,
    maplist(printSudokuRow, [A, B, C]), write('  -----------------------'), nl,
    maplist(printSudokuRow, [D, E, F]), write('  -----------------------'), nl,
    maplist(printSudokuRow, [G, H, I]), write('  -----------------------').

printSudokuRow([]) :- write(' | '), nl.
printSudokuRow([R1, R2, R3|Ow]) :- zeroToSpace(R1, Q1), zeroToSpace(R2, Q2), zeroToSpace(R3, Q3), write(' | '), write(Q1), write(' '), write(Q2), write(' '), write(Q3), printSudokuRow(Ow).

zeroToSpace(A, A) :- A \= 0.
zeroToSpace(A, ' ') :- A = 0.

% Checks if a sudoku is a solution to provided input
sudokuSolved([Ai, Bi, Ci, Di, Ei, Fi, Gi, Hi, Ii], [Ao, Bo, Co, Do, Eo, Fo, Go, Ho, Io]) :- 
    match(Ai, Ao), match(Bi, Bo), match(Ci, Co), %check if solution matches provided 'template'
    match(Di, Do), match(Ei, Eo), match(Fi, Fo),
    match(Gi, Go), match(Hi, Ho), match(Ii, Io), 
    sudokuSolution([Ao, Bo, Co, Do, Eo, Fo, Go, Ho, Io]), %check general constraints
    !, true. % Cut makes sure program stops after finding 1 solution

% Checks if cell matches cell of provided input, i.e. has the same value or input is not defined (0)
match([], []).
match([I|N], [_|Ut]) :- I = 0, match(N, Ut).
match([I|N], [O|Ut]) :- I = O, match(N, Ut).

% checks if matrix is a valid sudoku solution
sudokuSolution([A, B, C, D, E, F, G, H, I]) :-
    maplist(all_different, [A, B, C, D, E, F, G, H, I]), % check if rows contain unique values
    uniqueCols(A, B, C, D, E, F, G, H, I),
    uniqueDiags(A, B, C, D, E, F, G, H, I),
    uniqueDiags(I, H, G, F, E, D, C, B, A),
    uniqueGrid(A, B, C, 1), uniqueGrid(A, B, C, 4), uniqueGrid(A, B, C, 7),
    uniqueGrid(D, E, F, 1), uniqueGrid(D, E, F, 4), uniqueGrid(D, E, F, 7),
    uniqueGrid(G, H, I, 1), uniqueGrid(G, H, I, 4), uniqueGrid(G, H, I, 7),
    maplist(maplist(tussen(1, 9)), [A, B, C, D, E, F, G, H, I]). % each value should be 1-9,
                                                                 % this is placed after all other constraints to prevent
                                                                 % unnecessary backtracking on all values on the grid.

% checks if columns contain unique values
uniqueCols([], [], [], [], [], [], [], [], []).
uniqueCols([A|As], [B|Bs], [C|Cs], [D|Ds], [E|Es], [F|Fs], [G|Gs], [H|Hs], [I|Is]) :-
    all_different([A, B, C, D, E, F, G, H, I]),
    uniqueCols(As, Bs, Cs, Ds, Es, Fs, Gs, Hs, Is).

% checks if diagonals contain unique values
uniqueDiags(A, B, C, D, E, F, G, H, I) :-
    nth1(1, A, Ax), nth1(2, B, Bx), nth1(3, C, Cx), 
    nth1(4, D, Dx), nth1(5, E, Ex), nth1(6, F, Fx), 
    nth1(7, G, Gx), nth1(8, H, Hx), nth1(9, I, Ix), 
    all_different([Ax, Bx, Cx, Dx, Ex, Fx, Gx, Hx, Ix]).

% checks if a 3x3 block contain unique values
% N0 defines the offset of the first column in the grid
uniqueGrid(A, B, C, N0) :- 
    N1 is N0 + 1, N2 is N0 + 2,
    nth1(N0, A, Aa), nth1(N1, A, Ab), nth1(N2, A, Ac), 
    nth1(N0, B, Ba), nth1(N1, B, Bb), nth1(N2, B, Bc), 
    nth1(N0, C, Ca), nth1(N1, C, Cb), nth1(N2, C, Cc), 
    all_different([Aa, Ba, Ca, Ab, Bb, Cb, Ac, Bc, Cc]).