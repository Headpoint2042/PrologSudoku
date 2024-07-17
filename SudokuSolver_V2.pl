% Sudoku Solver file
solveSudoku(Grid, Solution) :-
copyGrid(Grid, IntermediateRepresentation),
writeln('Copy Grid finished'),
solvingIR(IntermediateRepresentation, IntermediateRepresentation2),
transformIR(IntermediateRepresentation2, Solution).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%        INTERMEDIATE REPRESENTATION CREATION           %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
copyGrid([], []).
copyGrid([Row|Rest], [SolRow|SolRest]) :- copyRow(Row, SolRow), copyGrid(Rest, SolRest). 

copyRow([], []).
copyRow([-1|Rest], [S|SolRest]) :- copyRow(Rest, SolRest), isAll(S).
copyRow([Clue|Rest], [[Clue]|SolRest]) :- copyRow(Rest, SolRest).

isAll([1, 2, 3, 4, 5, 6, 7, 8, 9]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%           INTERMEDIATE REPRESENTATION SOLVING         %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solvingIR(Grid, NewGrid) :- 
    (\+ transformIR(Grid, _)),
    !, 
    checkGrid(Grid, 0, 0, IntermediateGrid),
    solvingIR(IntermediateGrid, NewGrid).
solvingIR(Grid, Grid).

checkGrid(Grid, _, 9, Grid) :-
    !.
checkGrid(Grid, 9, Y, NewGrid) :- 
    Y1 is Y + 1, 
    checkGrid(Grid, 0, Y1, NewGrid), !.
checkGrid(Grid, X, Y, NewGrid) :- 
    (nth0(X, Grid, Row),
    nth0(Y, Row, Elem),
    length(Elem, 1)), 
    !,
    renewGrid(Grid, X, Y, IntermediateGrid), 
    X1 is X + 1, 
    checkGrid(IntermediateGrid, X1, Y, NewGrid).
checkGrid(Grid, X, Y, NewGrid) :- 
    checkGrid2(Grid, X, Y, IntermediateGrid)
    X1 is X + 1, 
    checkGrid(IntermediateGrid, X1, Y, NewGrid).

renewGrid(Grid, X, Y, NewGrid) :- 
    nth0(X, Grid, Row),
    nth0(Y, Row, Elem),
    length(Elem, 1),
    nth0(0, Elem, Num),
    removeListsElem(Row, Num, NewRow),
    updateGrid(Grid, X, NewRow, Grid2),
    transpose(Grid2, TGrid),
    nth0(Y, TGrid, Col),
    removeListsElem(Col, Num, NewCol),
    updateGrid(TGrid, Y, NewCol, TGrid2),
    transpose(TGrid2, Grid3),
    getSquares(Grid3, Squares),
    ResultX is X div 3,
    ResultY is Y div 3,
    Result is 3 * ResultX + ResultY,
    nth0(Result, Squares, Square),
    removeSquareElem(Square, Num, NewSquare),
    replace(Result, NewSquare, Squares, NewSquares),
    gridFromSquares(NewSquares, NewGrid),
    writeln(['Renewed', X, ', ', Y, NewGrid]).



transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).


replace(_, _, [], []).  % Base case: empty list

replace(0, NewElement, [ElementToReplace|Tail], [NewElement|Tail]).
replace(Index, NewElement, [Head|Tail], [Head|NewTail]) :- Index1 is Index - 1,
    replace(Index1, NewElement, Tail, NewTail).


gridFromSquares([], []).
gridFromSquares([[X1, X2, X3], [X4, X5, X6], [X7, X8, X9] | RestSquares], [Row1, Row2, Row3 | RestGrid]) :-
    append(X1, X4, X14),
    append(X14, X7, Row1),
    append(X2, X5, X25),
    append(X25, X8, Row2),
    append(X3, X6, X36),
    append(X36, X9, Row3),
    gridFromSquares(RestSquares, RestGrid).


updateGrid([_|RestRows], 0, NewRow, [NewRow|RestRows]) :- !.
updateGrid([Row|RestRows], X, NewRow, [Row|UpRestRows]) :- X1 is X - 1, updateGrid(RestRows, X1, NewRow, UpRestRows). 


removeListsElem([], _, []).
removeListsElem([RowElem|Rest], Elem, [NewRowElem|NewRest]) :- deleteList(RowElem, Elem, NewRowElem), removeListsElem(Rest, Elem, NewRest).

deleteList([X], _, [X]) :- !.
deleteList(List, Elem, NewList) :- delete(List, Elem, NewList).

removeSquareElem([], _, []).
removeSquareElem([Row|RowRest], Elem, [NewRow|NewRowRest]) :- removeListsElem(Row, Elem, NewRow), removeSquareElem(RowRest, Elem, NewRowRest).

getSquares([], []).
getSquares([Row1, Row2, Row3 | RestRows], Res) :- getSquaresRows([Row1, Row2, Row3], Squares), getSquares(RestRows, RestSquares), append(Squares, RestSquares, Res).

getSquaresRows([[], [], []], []).
getSquaresRows([[X1, X2, X3| Row1], [X4, X5, X6| Row2], [X7, X8, X9| Row3]], [[[X1, X2, X3], [X4, X5, X6], [X7, X8, X9]] | RestSquares]) :- getSquaresRows([Row1, Row2, Row3], RestSquares).








checkGrid2(Grid, X, Y, NewGrid) :- 
    nth0(X, Grid, Row),
    nth0(Y, Row, Cell),
    delete_at(Y, Row, NewRow),
    checkList(Cell, NewRow, NewCell),
    length(NewCell, 1),
    delete_at(Y, UpdatedRow, NewRow),
    nth0(Y, UpdatedRow, NewCell),
    updateGrid(Grid, X, UpdatedRow, NewGrid), !.

checkGrid2(Grid, X, Y, NewGrid) :-
    transpose(Grid, TGrid), 
    nth0(Y, TGrid, Row),
    nth0(X, Row, Cell),
    delete_at(X, Row, NewRow),
    checkList(Cell, NewRow, NewCell),
    length(NewCell, 1),
    delete_at(X, UpdatedRow, NewRow),
    nth0(X, UpdatedRow, NewCell),
    updateGrid(TGrid, Y, UpdatedRow, NewTGrid),
    transpose(NewTGrid, NewGrid), !.


delete_at(0, [_|Tail], Tail) :- !.

delete_at(Index, [Head|Tail], [Head|NewTail]) :-
    Index > 0,
    NewIndex is Index - 1,
    delete_at(NewIndex, Tail, NewTail).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%                  SOLUTION CREATION                    %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transformIR([], []).
transformIR([Row| RowRest], [SolutionRow| SolutionRowRest]) :- transformIRRow(Row, SolutionRow), transformIR(RowRest, SolutionRowRest).

transformIRRow([], []).
transformIRRow([[Elem]| RowRest], [Elem|NewRowRest]) :- transformIRRow(RowRest, NewRowRest).

init([[3, -1, 6, 5, -1, 8, 4, -1, -1], [5, 2, -1, -1, -1, -1, -1, -1, -1], [-1, 8, 7, -1, -1, -1, -1, 3, 1], [-1, -1, 3, -1, 1, -1, -1, 8, -1], [9, -1, -1, 8, 6, 3, -1, -1, 5], [-1, 5, -1, -1, 9, -1, 6, -1, -1], [1, 3, -1, -1, -1, -1, 2, 5, -1], [-1, -1, -1, -1, -1, -1, -1, 7, 4], [-1, -1, 5, 2, -1, 6, 3, -1, -1]]).

solve(X) :- init(S), solveSudoku(S, X).