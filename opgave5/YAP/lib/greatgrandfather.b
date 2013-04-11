
% Mode declarations

:- modeh(*,greatgrandfather(+person,-person)).
:- modeb(*,grandfather(+person,-person)).
:- modeb(*,father(+person,-person)).
:- modeb(*,mother(+person,-person)).
:- modeb(*,male(+person)).
:- modeb(*,female(+person)).

:-determination(greatgrandfather/2,grandfather/2).
:-determination(greatgrandfather/2,person/1).
:-determination(greatgrandfather/2,father/2).
:-determination(greatgrandfather/2,mother/2).
:-determination(greatgrandfather/2,male/1).
:-determination(greatgrandfather/2,female/1).

% Types

person(george).
person(mum).
person(spencer).
person(kydd).
person(elizabeth).
person(philip).
person(margaret).
person(diana).
person(charles).
person(anne).
person(mark).
person(andrew).
person(sarah).
person(edward).
person(sophie).
person(william).
person(harry).
person(peter).
person(zara).
person(beatrice).
person(eugenie).
person(louise).
person(james).

% Background knowledge

father(george,elizabeth).
father(george,margaret).
father(spencer,diana).
father(philip,charles).
father(philip,anne).
father(philip,andrew).
father(philip,edward).
father(charles,william).
father(charles,harry).
father(mark,peter).
father(mark,zara).
father(andrew,beatrice).
father(andrew,eugenie).
father(edward,louise).
father(edward,james).

mother(mum,elizabeth).
mother(mum,margaret).
mother(kydd,diana).
mother(elizabeth,charles).
mother(elizabeth,anne).
mother(elizabeth,andrew).
mother(elizabeth,edward).
mother(diana,william).
mother(diana,harry).
mother(anne,peter).
mother(anne,zara).
mother(sarah,beatrice).
mother(sarah,eugenie).
mother(sophie,louise).
mother(sophie,james).

male(george).
male(philip).
male(edward).

female(elizabeth).
female(diana).
female(sarah).

grandfather(A, B) :- father(A, C), father(C, B).
grandfather(A, B) :- father(A, C), mother(C, B).