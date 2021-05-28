:- [tests].
:- use_module(library(clpfd)). % Import the module
:- set_prolog_flag(clpfd_monotonic, true). % setting to get useful errors sometimes

% =================
% SNAKE DEFINITION
% =================
% a snake is correct when all the predicates result in true

snake(RowClues, ColClues, Grid, Solution)
    :- copyGrid(Grid,Solution)
    , checkHeadTail(Solution)
    , checkRowClues(Solution,RowClues)
    , checkColClues(Solution,ColClues)
    , nonTouching(Solution) % snake cannot touch itself
    , countNeighbors(Solution) % heads have 1 neighbor, midpoints 2
    %, snakeConnected(Solution) % snake must be connected
    .


% ================
% DEEPCOPY A GRID
% ================
% This code was copied from the pdf about this project

copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).

copyRow([],[]).
copyRow([-1|R],[_|S]) :- copyRow(R,S).
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).

% ================
% CHECK CONNECTED
% ================

% snakeConnected(Solution)
%     :- copyGrid(Solution, TestGrid)
%     , 



% ================
% CHECK ROW CLUES
% ================

% We split the grid up in rows, and take a single value from the clues list, then we count the number of 1s and 2s,
% and compare that value to the clue given by the puzzle. We do this for every row in the grid.
checkRowClues([],[]).
checkRowClues([_|Rows],[-1|Clues])
    :- checkRowClues(Rows,Clues)
    , !.
checkRowClues([Row|Rows],[RowClue|Clues])
    :- countRow(Row,Count)
    , Count #= RowClue
    , checkRowClues(Rows,Clues).

% This uses the count_cell predicate, given in the COUNT NEIGHBOURS section. 
% count_cell will make sure that the second argument is 1 if the first argument
% is a cell with either 1 or 2. Else the second argument must be 0. countRow will
% then count the number of snake parts in a given row.
countRow([],0).
countRow([V|Row], NewCount)
    :- count_cell(V,X)
    , countRow(Row,OldCount)
    , NewCount is OldCount + X.

% ================
% CHECK COL CLUES
% ================

% After transposing the grid, we can use the checkRowClues function.
checkColClues([],[]).
checkColClues(Solution,ColClues)
    :- transpose(Solution, TransGrid)
    , checkRowClues(TransGrid,ColClues).

% =============
% NON TOUCHING
% =============

nonTouching([Row]) :- !.
nonTouching([Row1,Row2|Rows])
    :- nonTouchingRows(Row1,Row2)
    , nonTouching([Row2|Rows]).

nonTouchingRows([_],[_]) :- !.
nonTouchingRows([V1,V2|VRow], [W1,W2|WRow])
    :- not(checkForbiddenPattern([[V1,V2],[W1,W2]]))
    , nonTouchingRows([V2|VRow],[W2|WRow]).

checkForbiddenPattern([[2,0],[0,2]]) :- !.
checkForbiddenPattern([[1,0],[0,2]]) :- !.
checkForbiddenPattern([[2,0],[0,1]]) :- !.
checkForbiddenPattern([[1,0],[0,1]]) :- !.
checkForbiddenPattern([[0,2],[2,0]]) :- !.
checkForbiddenPattern([[0,2],[1,0]]) :- !.
checkForbiddenPattern([[0,1],[2,0]]) :- !.
checkForbiddenPattern([[0,1],[1,0]]) :- !.



% ================
% CHECK HEAD TAIL
% ================

% This will check if the grid has exactly 2 "1" values (those are head or tails)
% and if the grid only has the values 0, 1 or 2
checkHeadTail(Solution)
    :- flattenGrid(Solution, Flattened)
    , checkMemberOf(Flattened, [0,1,2])
    , countHeadTailOccurences(Flattened, Amount)
    , Amount #= 2.

% This will flatten the matrix such that all the values in the grid are placed into a
% single, one-dimensional list. This makes it easier to check the properties we want
flattenGrid([], []).
flattenGrid([Row|Rows],Result) :- is_list(Row), flattenGrid(Rows,ResultRows), !, append(Row,ResultRows,Result).
flattenGrid([Row|Rows],[Row|ResultRows]) :- flattenGrid(Rows,ResultRows).

% This will count the number of `1`s in a given list. These are the head and tail of
% the snake. We have specified in checkHeadTail that we want this to be 2.
countHeadTailOccurences([],0).
countHeadTailOccurences([1|Result],AmountOfOnes) 
    :- countHeadTailOccurences(Result, NewAmountOfOnes)
    , AmountOfOnes is NewAmountOfOnes+1
    , !.
countHeadTailOccurences([_|Result],AmountOfOnes)
    :- countHeadTailOccurences(Result, AmountOfOnes).

% This will check if all numbers in a given list are in a given allowedList
% In checkHeadTail we have given the allowedList [0,1,2]
 checkMemberOf([V|Result], AllowedList)
    :- member(V,AllowedList)
    , checkMemberOf(Result, AllowedList).
checkMemberOf([],_).

% ================
% COUNT NEIGHBORS
% ================

% To help calculate the values of all the pieces, we first extend the grid by adding zeros at all the sides.
% We then use this extended grid in checkGrid to determine if the grid has all the correct values.
countNeighbors(Solution) 
    :- extend_grid(Solution, ExtendedGrid)
    , checkGrid(ExtendedGrid). 

% Doing check_neighbors_rows on every 3 rows in the (extended) grid._
% If there are only 2 rows left, being the last normal row and a row of zeros, we end the recursion.
checkGrid([Row1, Row2, Row3|Rows])
    :- check_neighbors_rows(Row1, Row2, Row3)
    ,checkGrid([Row2, Row3|Rows]), !.
checkGrid([_,_]).

% Extend a row by putting a 0 before it and after it
extend_row(OldRow,NewRow) :- append([0|OldRow],[0],NewRow).

% Extend all the rows in a grid by using the predicate extend_row
extend_grid_rows([], []).
extend_grid_rows([Row|Rows], [ExtendedRow|ExtendedRows])
    :- extend_row(Row, ExtendedRow)    
    , extend_grid_rows(Rows, ExtendedRows).

% Putting a border of zeroes around the whole grid using transpose and extend_grid_rows
extend_grid(OldGrid,NewGrid)
    :- transpose(OldGrid,TransGrid),
    extend_grid_rows(TransGrid,RowTransGrid),
    transpose(RowTransGrid,RowGrid),
    extend_grid_rows(RowGrid,NewGrid).

% Given a cell and its 4 neighbors, we determine if the cell has the correct amount of neighbors
% For a cell with value 1, it should have 1 neighbor with a nonzero value, and the same for value 2.
check_neighbors_pattern(0,_,_,_,_).
check_neighbors_pattern(Piece,N,E,S,W) 
    :- 1 #=< Piece,
    count_cell(N,X1),
    count_cell(E,X2),
    count_cell(S,X3),
    count_cell(W,X4),
    Piece #= X1+X2+X3+X4.

% Every cell with a snake part (tail or midpoint) should be counted as 1
count_cell(0, X) :- X = 0.
count_cell(1, X) :- X = 1.
count_cell(2, X) :- X = 1.

% Use check_neighbors_pattern to check if middle row in the three given rows has the correct amount of neighbors
% We do not check the first and last element of the middle row (they are the border of the extended grid)
check_neighbors_rows([_,A2,_],[ResultRows,B2,B3],[_,C2,_]) :- check_neighbors_pattern(B2,A2,B3,C2,ResultRows).
check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]) 
    :- check_neighbors_pattern(M,N,E,S,W),
    check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).