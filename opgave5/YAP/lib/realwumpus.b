:- modeh(*,breeze(+world,-coordinate,-coordinate)).
:- modeb(*,adjacent(+direction,+coordinate,+coordinate,-coordinate,-coordinate)).
:- modeb(*,wind(+world,-direction)).
:- modeb(*,pit(+world,-coordinate,-coordinate)).

:- determination(breeze/3,adjacent/5).
:- determination(breeze/3,wind/2).
:- determination(breeze/3,pit/3).
:- determination(breeze/3,world/1).
:- determination(breeze/3,coordinate/1).

% Types
world(1).
world(2).
world(3).
world(4).
world(5).
world(6).

direction(north).
direction(south).
direction(east).
direction(west).

coordinate(1).
coordinate(2).
coordinate(3).
coordinate(4).

% Background knowledge
room(X,Y) :- coordinate(X), coordinate(Y).

adjacent(north, X1, Y1, X1, Y2) :- room(X1, Y1), room(X1, Y2), Y1 is Y2 - 1.
adjacent(south, X1, Y1, X1, Y2) :- room(X1, Y1), room(X1, Y2), Y1 is Y2 + 1.
adjacent(west, X1, Y1, X2, Y1) :- room(X1, Y1), room(X2, Y1), X1 is X2 + 1.
adjacent(east, X1, Y1, X2, Y1) :- room(X1, Y1), room(X2, Y1), X1 is X2 - 1.

pit(1,1,3).
pit(1,3,1).
pit(1,3,3).
pit(1,4,4).
wind(1,south).

pit(2,4,1).
pit(2,2,2).
pit(2,2,3).
pit(2,2,4).
wind(2,north).

pit(3,1,1).
pit(3,1,4).
pit(3,3,4).
wind(3,north).

pit(4,3,1).
pit(4,3,3).
pit(4,1,2).
pit(4,1,3).
pit(4,1,4).
wind(4,west).

pit(5,2,2).
pit(5,2,3).
pit(5,3,2).
pit(5,3,3).
wind(5,east).

pit(6,2,4).
pit(6,2,2).
pit(6,3,3).
pit(6,4,1).
wind(6,west).