
% Mode declarations

% :- modeh(*,breeze(+world,-coord,-coord)).
% :- modeb(*,wind(+world,-direction)).
% :- modeb(*,pit(+world,-coord,-coord)).
% :- modeb(*,adjacent(+direction,-coord,-coord,+coord,+coord)).

:- modeh(*,breeze(+world,-xcoord,-ycoord)).
:- modeb(*,wind(+world,-direction)).
:- modeb(*,pit(+world,-xcoord,-ycoord)).
:- modeb(*,adjacent(+direction,-xcoord,-ycoord,+xcoord,+ycoord)).

:-determination(breeze/3,wind/2).
:-determination(breeze/3,adjacent/5).
:-determination(breeze/3,pit/3).

% Types

world(1).
world(2).
world(3).
world(4).
world(5).
world(6).

direction(north).
direction(west).
direction(south).
direction(east).

xcoord(1).
xcoord(2).
xcoord(3).
xcoord(4).

ycoord(1).
ycoord(2).
ycoord(3).
ycoord(4).

% Background knowledge

% pit(W,X,Y).  
% wind(W,R).   wind staat naar R in wereld W.
% breeze(W,X,Y) : wind(W,R), pit(A,B), adjacent(R,X,Y,A,B).

room(X,Y) :- 
	xcoord(X),
	ycoord(Y).

adjacent(east,X,Y,X2,Y) :-
	room(X,Y),
	room(X2,Y),
	X is X2+1.
adjacent(west,X,Y,X2,Y) :-
	room(X,Y),
	room(X2,Y),
	X is X2-1.
adjacent(north,X,Y,X,Y2) :-
	room(X,Y),
	room(X,Y2),
	Y is Y2+1.
adjacent(south,X,Y,X,Y2) :-
	room(X,Y),
	room(X,Y2),
	Y is Y2-1.

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

