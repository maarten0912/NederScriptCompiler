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
    %, checkRowClues(Solution,RowClues)
    %, checkColClues(Solution,ColClues)
    %, nonTouching(Solution) % snake cannot touch itself
    %, countNeighbors(Solution) % heads have 1 neighbor, midpoints 2
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
% CHECK HEAD TAIL
% ================

checkHeadTail(Solution) :- checkHeadTailRows(Solution, 0).

checkHeadTailRows([],_).
checkHeadTailRows([Row|Rows], AmountOfOnes)
    :- checkHeadTailValues(Row, AmountOfOnes)
    , checkHeadTailRows(Rows, AmountOfOnes).

checkHeadTailValues([],_).
checkHeadTailValues([1|Row],AmountOfOnes) 
    :- checkHeadTailValues(Row, AmountOfOnes + 1), !.
checkHeadTailValues([V|Row],AmountOfOnes)
    :- checkHeadTailValues(Row, AmountOfOnes)
    , V /= 1.
    
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
checkGrid([Row1,Row2]).

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
check_neighbors_rows([_,A2,_],[B1,B2,B3],[_,C2,_]) :- check_neighbors_pattern(B2,A2,B3,C2,B1).
check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]) 
    :- check_neighbors_pattern(M,N,E,S,W),
    check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).