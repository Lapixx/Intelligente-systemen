:- modeh(*,breeze(+world,-xcoordinate,-ycoordinate)).
:- modeb(*,adjacent(+direction,+xcoordinate,+ycoordinate,-xcoordinate,-ycoordinate)).
:- modeb(*,wind(+world,-direction)).
:- modeb(*,pit(+world,-xcoordinate,-ycoordinate)).

:- determination(breeze/3,adjacent/5).
:- determination(breeze/3,wind/2).
:- determination(breeze/3,pit/3).

% Types
world(1).
world(2).

direction(north).
direction(south).
direction(east).
direction(west).

xcoordinate(1).
xcoordinate(2).
xcoordinate(3).
xcoordinate(4).

ycoordinate(1).
ycoordinate(2).
ycoordinate(3).
ycoordinate(4).

% Background knowledge
room(X,Y) :- xcoordinate(X), ycoordinate(Y).

adjacent(north, X1, Y1, X1, Y2) :- room(X1, Y1), room(X1, Y2), Y1 is Y2 - 1.
adjacent(south, X1, Y1, X1, Y2) :- room(X1, Y1), room(X1, Y2), Y1 is Y2 + 1.
adjacent(west, X1, Y1, X2, Y1) :- room(X1, Y1), room(X2, Y1), X1 is X2 + 1.
adjacent(east, X1, Y1, X2, Y1) :- room(X1, Y1), room(X2, Y1), X1 is X2 - 1.

pit(1,2,2).
pit(1,3,3).
pit(1,4,4).
wind(1,south).

pit(2,3,1).
pit(2,3,3).
pit(2,1,2).
pit(2,1,3).
pit(2,1,4).
wind(2,west).