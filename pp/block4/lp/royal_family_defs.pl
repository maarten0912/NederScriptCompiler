:- ['./royal_family_facts.pl'].

father(X,Y) :- husband(X,Z), mother(Z,Y).

child(X,Y) :- father(Y,X).
child(X,Y) :- mother(Y,X).

grandparent(X,Y) :- child(Y,Z),child(Z,X).

parent(X,Y) :- child(Y,X).

brother(X,Y) :- mother(Z,X),mother(Z,Y),male(X),not(X=Y).
sister(X,Y)  :- mother(Z,X),mother(Z,Y),female(X),not(X=Y).

aunt(X,Y) :- parent(Z,Y),sister(Z,X).
uncle(X,Y) :- parent(Z,Y),brother(Z,X).

cousin(X,Y) :- aunt(Z,Y),child(X,Z).
cousin(X,Y) :- uncle(Z,Y),child(X,Z).

nephew(X,Y) :- cousin(X,Y),male(X).
niece(X,Y)  :- cousin(X,Y),female(X).

ancestor(X,Y) :- child(Y,X).
ancestor(X,Y) :- child(Y,Z),ancestor(X,Z).