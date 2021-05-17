:- ['./royal_family_facts.pl'].

father(X,Y) :- husband(X,Z), mother(Z,Y).

child(X,Y) :- father(Y,X).
child(X,Y) :- mother(Y,X).

grandparent(X,Y) :- child(Y,Z),child(Z,X).

parent(X,Y) :- child(Y,X).

brother(X,Y) :- child(X,Z),child(Y,Z),male(X),X\=Y.
sister(X,Y)  :- child(X,Z),child(Y,Z),female(X),X\=Y.

aunt(X,Y)  :- parent(Z,Y),sister(Z,X).
uncle(X,Y) :- parent(Z,Y),brother(Z,X).

cousin(X,Y) :- aunt(Z,Y),child(X,Z).
cousin(X,Y) :- uncle(Z,Y),child(X,Z).

nephew(X,Y) :- cousin(X,Y),male(X).
niece(X,Y)  :- cousin(X,Y),female(X).

aunt(margriet,mabel).
grandparent(emma,juliana).