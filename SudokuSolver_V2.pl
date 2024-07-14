% Sudoku Solver file
solveSudoku(Grid, Solution) :-
copyGrid(Grid, IntermidiateRepresentation),
solvingIR(IntermidiateRepresentation, IntermidiateRepresentation2),
transformIR(IntermidiateRepresentation2, Solution).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%        INTERMIDIATE REPRESENTATION CREATION           %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
copyGrid([], []).
copyGrid([Row|Rest], [SolRow|SolRest]) :- copyRow(Row, SolRow), copyGrid(Rest, SolRest). 

copyRow([], []).
copyRow([-1|Rest], [S|SolRest]) :- copyRow(Rest, SolRest), isAll(S).
copyRow([Clue|Rest], [[Clue]|SolRest]) :- copyRow(Rest, SolRest).

isAll([1, 2, 3, 4, 5, 6, 7, 8, 9]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%           INTERMIDIATE REPRESENTATION SOLVING         %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solvingIR(Grid, Grid) :- transformIR(Grid, _).
solvingIR(Grid, NewGrid) :- checkGrid(Grid, 0, 0, IntermidiateGrid), solvingIR(IntermidiateGrid, NewGrid). 

checkGrid(Grid, _, 9, Grid).
checkGrid(Grid, 9, Y, NewGrid) :- checkGrid(Grid, 0, Y+1, NewGrid).
checkGrid(Grid, X, Y, NewGrid) :-  
    (nth0(Grid, X, Row),
    nth0(Row, Y, [Elem])), !, renewGrid(Grid, X, Y, IntermidiateGrid), checkGrid(IntermidiateGrid, X+1, Y, NewGrid).
checkGrid(Grid, X, Y, NewGrid) :- checkGrid(Grid, X+1, Y, NewGrid).


renewGrid(Grid, X, Y, NewGrid) :- 
    nth0(Grid, X, Row),
    nth0(Row, Y, [Elem]),
    removeListsElem(Row, Elem, NewRow),
    updateGrid(Grid, X, NewRow, Grid2),
    transpose(Grid2, TGrid),
    nth0(TGrid, Y, Col),
    removeListElem(Col, Elem, NewCol),
    updateGrid(TGrid, Y, NewCol, TGrid2),
    transpose(TGrid2, Grid3),
    getSquares(Grid3, Squares),
    ResultX is X mod 3,
    ResultY is Y mod 3,
    Result is ResultX + ResultY,
    nth0(Squares, Res)


updateGrid([Row|RestRows], 0, NewRow, [NewRow|RestRows]).
updateGrid([Row|RestRows], X, NewRow, [Row|UpRestRows]) :- updateGrid(RestRows, X-1, NewRow, UpRestRows). 










removeListsElem([], _, []).
removeListsElem([RowElem|Rest], Elem, [NewRowElem|NewRest]) :- member(Elem, RowElem), !, deleteList(RowElem, Elem, NewRowElem), removeListsElem(Rest, Elem, NewRest).
removeListElem([Row|Rest], Elem, [Row|NewRest]) :- removeListElem(Rest, Elem, NewRest).

deleteList([Elem], Elem, [Elem]).
deleteList(List, Elem, NewList) :- delete(List, Elem, NewList).

removeSquareElem([], _, []).
removeSquareElem([Row|RowRest], Elem, [NewRow|NewRowRest]) :- removeListsElem(Row, Elem, NewRow), removeSquareElem(RowRest, Elem, NewRowRest).

getSquares([], []).
getSquares([Row1, Row2, Row3 | RestRows], [Squares|RestSquares]) :- getSquaresRows([Row1, Row2, Row3], Squares), getSquares(RestRows, RestSquares).

getSquaresRows([], []).
getSquaresRows([[X1, X2, X3| Row1], [X4, X5, X6| Row2], [X7, X8, X9| Row3]], resSquares) :- getSquaresRows([Row1, Row2, Row3], restSquares), append([[X1, X2, X3], [X4, X5, X6], [X7, X8, X9]], restSquares, resSquares ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%                  SOLUTION CREATION                    %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transformIR([], []).
transformIR([Row| RowRest], [SolutionRow| SolutionRowRest]) :- transformIRRow(Row, SolutionRow), transformIR(RowRest, SolutionRowRest).

transformIRRow([], []).
transformIRRow([[Elem]| RowRest], [Elem|NewRowRest]) :- transformIRRow(RowRest, NewRowRest).