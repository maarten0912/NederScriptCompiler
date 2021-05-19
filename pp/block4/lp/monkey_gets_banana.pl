goal(state(_,_,_,has)).
init(state(atdoor,onfloor,atwindow,hasnot)).

move(state(inmiddle, onbox, inmiddle, hasnot), grasp, state(inmiddle, onbox, inmiddle, has)).
move(state(Pos, onfloor, Pos, Has), climb, state(Pos, onbox, Pos, Has)).
move(state(Pos1, onfloor, Pos1, Has), push(Pos1,Pos2), state(Pos2, onfloor, Pos2, Has)).
move(state(Pos1, onfloor, Posbox, Has), walk(Pos1, Pos2), state(Pos2, onfloor, Posbox, Has)).

solve(S) :- goal(S).
solve(State1) :- move(State1,_,State2), solve(State2).

solve(S,[]) :- goal(S).
solve(State1,[Move|L]) :- move(State1,Move,State2), solve(State2,L).

yee(A,B,C) :- C = [A|B].