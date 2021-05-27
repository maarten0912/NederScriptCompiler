% 4-LP.12
istree(nil).
istree(t(L,_,R)) :- istree(L), istree(R).

% 4-LP.13
max(t(_, N, nil),X) :- X = N, !.
max(t(_,_,R),X) :- max(R,X).

min(t(nil,N,_),X) :- X = N, !.
min(t(L,_,_),X) :- min(L,X).

% 4-LP.14
issorted(nil) :- !.
issorted(t(nil,_,nil)) :- !.
issorted(t(nil,N,R)) :- issorted(R), min(R,X), X >= N, !.
issorted(t(L,N,nil)) :- issorted(L), max(L,X), X =< N, !.
issorted(t(L,N,R)) :- issorted(L), issorted(R), max(L,X), X =< N, min(R,Q), Q >= N.

% 4-LP.15
find(t(L,X,R),N,S) :- X=N, !, S = t(L,X,R).
find(t(L,X,_),N,S) :- N < X, !, find(L,N,S). 
find(t(_,X,R),N,S) :- N > X, !, find(R,N,S).

% 4-LP.16
insert(nil,N,S) :- S = t(nil,N,nil).
insert(t(nil,X,nil),N,S) :- N > X, !, S = t(nil,X,t(nil,N,nil)).
insert(t(nil,X,nil),N,S) :- N < X, !, S = t(t(nil,N,nil),X,nil).
insert(t(L,X,R),N,S) :- N < X, !, insert(L,N,P), S = t(P,X,R).
insert(t(L,X,R),N,S) :- N > X, !, insert(R,N,P), S = t(L,X,P).

% 4-LP.17
deleteAll(T,N,S) :- find(T,N,_), !, deleteOne(T,N,Z), deleteAll(Z,N,S).
deleteAll(T,N,S) :- not(find(T,N,_)), !, S = T.


deleteOne(nil,_,S) :- S = nil, !.
deleteOne(t(nil,X,nil),N,S) :- X = N, !, S = nil.
deleteOne(t(nil,X,R),N,S) :- X = N, !, S = R.
deleteOne(t(L,X,nil),N,S) :- X = N, !, S = L.
deleteOne(t(L,X,R),N,S) :- X = N, !, max(L,P), deleteOne(L,P,Z), S = t(Z,P,R).
deleteOne(t(L,X,R),N,S) :- not(X = N), X < N, !, deleteOne(R,N,Z), S = t(L,X,Z).
deleteOne(t(L,X,R),N,S) :- not(X = N), X > N, !, deleteOne(L,N,Z), S = t(Z,X,R).
