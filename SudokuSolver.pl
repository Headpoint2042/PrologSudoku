% Sudoku Solver file
solveSudoku(Grid, Solution) :-
copyGrid(Grid, Solution),
lineUnique(Solution),
squareUnique(Solution), !.

copyGrid([], []).
copyGrid([Row|Rest], [SolRow|SolRest]) :- copyRow(Row, SolRow), copyGrid(Rest, SolRest). 

copyRow([], []).
copyRow([-1|Rest], [_|SolRest]) :- copyRow(Rest, SolRest).
copyRow([Clue|Rest], [Clue|SolRest]) :- copyRow(Rest, SolRest).

cell(1).
cell(2).
cell(3).
cell(4).
cell(5).
cell(6).
cell(7).
cell(8).
cell(9).

lineUnique(Solution) :- checkLineUnique(Solution), transpose(Solution, TransSolution), checkLineUnique(TransSolution), !.

checkLineUnique([]).
checkLineUnique([Row|Rest]) :- checkRowUnique(Row), checkLineUnique(Rest).

checkRowUnique([]).
checkRowUnique([Elem|Rest]) :- (cell(Elem), \+ (member(Elem, Rest))), checkRowUnique(Rest).

% Base case: Transpose of an empty matrix is an empty matrix
transpose([], []).

% Recursive case: Transpose the matrix
transpose([[]|_], []).
transpose(Matrix, [Row|Rows]) :-
    first_column(Matrix, Row, RestMatrix),
    transpose(RestMatrix, Rows).

% Helper predicate to extract the first column and the rest of the matrix
first_column([], [], []).
first_column([[H|T]|Rest], [H|Hs], [T|Ts]) :-
    first_column(Rest, Hs, Ts).


squareUnique([]).
squareUnique([Row1, Row2, Row3|Rest]) :- checkSquare([Row1, Row2, Row3]), squareUnique(Rest), !.

checkSquare([]).
checkSquare([[R1, R2, R3|Rest1], [R4, R5, R6|Rest2], [R7, R8, R9|Rest3]]) :- checkRowUnique([R1, R2, R3, R4, R5, R6, R7, R8, R9]), checkSquare([Rest1, Rest2, Rest3]).

init([[3, -1, 6, 5, -1, 8, 4, -1, -1], [5, 2, -1, -1, -1, -1, -1, -1, -1], [-1, 8, 7, -1, -1, -1, -1, 3, 1], [-1, -1, 3, -1, 1, -1, -1, 8, -1], [9, -1, -1, 8, 6, 3, -1, -1, 5], [-1, 5, -1, -1, 9, -1, 6, -1, -1], [1, 3, -1, -1, -1, -1, 2, 5, -1], [-1, -1, -1, -1, -1, -1, -1, 7, 4], [-1, -1, 5, 2, -1, 6, 3, -1, -1]]).