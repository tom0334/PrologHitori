:- use_module(library(apply)).
:- use_module(library(lists)).

% This is actually the constraint programming libary
% using too much from here feels kinda like cheating (?),
% since im researching Logic Programming.
% For now its only transpose though I think.
% TODO make my own transpose if this is an issue
:- use_module(library(clpfd)). 

% https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/singles.html#4x4de%23722022701905902
% the actual puzzle
puzzle([
    [4, 3, 1, 2], 
    [2, 4, 2, 4], 
    [2, 1, 1, 4], 
    [4, 2, 4, 1]
]).
solvedPuzzle(
	[[4, 3, 1, 2], [0, 4, 2, 0], [2, 1, 0, 4], [0, 2, 4, 1]] 
).

%https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/singles.html#5x5dk%23864813841153333
smallPuzzle([
    [4, 2, 2, 5, 5],
    [2, 1, 1, 4, 5],
    [3, 5, 5, 2, 2],
    [1, 5, 4, 2, 3],
    [2, 2, 1, 4, 4]
]
).

mediumPuzzle([
    [2, 5, 5, 1, 6, 4],
    [4, 5, 6, 2, 4, 6],
    [1, 1, 3, 2, 4, 2],
    [6, 2, 3, 3, 1, 5],
    [6, 4, 6, 4, 2, 2],
    [1, 6, 2, 4, 5, 3]
]).

mediumBigPuzzle(
[
  [1,4,5,7,2,4,1,5],
  [8,1,7,2,4,3,5,3],
  [5,8,5,8,3,3,4,1],
  [2,8,3,1,3,4,6,8],
  [5,6,2,1,8,1,5,3],
  [1,6,4,8,6,8,3,4],
  [8,3,1,5,1,8,2,4],
  [4,2,3,3,1,5,1,7]
]
).

% https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/singles.html#10x10dk%23410746926154546
bigPuzzle([
    [5, 10, 7, 9, 6, 6, 8, 1, 10, 3],
    [6, 5, 5, 7, 6, 3, 10, 10, 7, 2],
    [7, 3, 9, 10, 4, 6, 2, 8, 3, 6],
    [1, 8, 5, 3, 7, 10, 3, 1, 6, 10],
    [8, 3, 3, 5, 2, 7, 6, 6, 5, 9],
    [5, 10, 1, 1, 3, 3, 7, 2, 5, 2],
    [3, 2, 1, 8, 3, 10, 4, 9, 7, 1],
    [10, 7, 2, 3, 8, 7, 6, 10, 9, 9],
    [2, 8, 3, 10, 10, 8, 5, 7, 1, 6],
    [4, 6, 7, 1, 9, 7, 10, 3, 1, 7]
]).



%Todo THIS WILL HELP A LOT I THINK
%make DICT of positions that are duplicate in its row 
% and another DICt of positions that are duplicate in its column.
%use the dict for O(0) access.
% that way we can just do something like:
%validCell(X,Y,0) :- notUniqueInRow(X,Y), notUniqueInColumn(X,Y). %assume these are 0 first   
%validCell(X,Y,0) :- notUniqueInRow(X,Y).
%validCell(X,Y,0) :- notuniqueInColumn(X,Y).
%validCell(_,_,_, _, X, X).
% we can also later ad contraints to not do two next to each other or something if we have the canditeas easily accesible in a list.
% maybe it is even better if you make it a list of lists, so you have each row seperately.


% https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/singles.html#10x10dk%23410746926154546
isSolution(Board, Solution) :-
    isPossible(Board,Solution),
    allRowsValid(Solution),
    allColumnsValid(Solution),
    allNonZeroConnected(Solution).


% gives you the indices as pairs (X,Y) that have a nonzero value at the board
allPositionsWithValue(Board, PositionsWithValue) :-
    findall( (X,Y,V), elementAt(Board,X,Y,V), PositionsWithValue).

isPossible(Board, Solution) :-
    sameShape(Board,Solution),
    allPositionsWithValue(Board, Positions),
    allDuplicateSets(Positions, RowOnly, ColOnly, InBoth),
    maplist(checkPositionFast(RowOnly,ColOnly,InBoth, Solution), Positions).


checkPositionFast(DupsInRows, DupsInColumns,InBoth, Solution, (X,Y,V)):-
    elementAt(Solution, X, Y, SolutionValue), %really need to get rid of this somehow?
    validCellFast((X,Y,V),DupsInRows,DupsInColumns, InBoth, SolutionValue).

%The order matters a lot here. First line is seen as the first option for the solver
% that means we initially make it zero if its double in a row/column.
% that was faster for all cases i tested.
%TODO: add additional constraints to make the order in which the solver considers options in the search space more optimal
% A cell is valid if it keeps its original value or becomes 0 if allowed
validCellFast((X,Y,V),_,_,DupsInBoth,0):- 
    ord_memberchk((X,Y,V),DupsInBoth).

validCellFast((_,_,V),_,_,_,V).

validCellFast((X,Y,V),DupsInRowsOnly,_,_,0):- 
    ord_memberchk((X,Y,V),DupsInRowsOnly).

validCellFast((X,Y,V),_,DupsInColumnsOnly,_,0):- 
    ord_memberchk((X,Y,V),DupsInColumnsOnly).



sameShape([], []).
sameShape([A|As], [B|Bs]) :-
    same_length(A, B),
    sameShape(As, Bs).

% gives you the indices as pairs (X,Y) that have a nonzero value at the board
allPositions(Board, Positions) :-
    findall( (X,Y), elementAt(Board,X,Y,_), Positions).




%%%%%%%%%%
%creates a dictionary that has the following values:
% key: all elements in the list. 
% Value: the number of times that number occurs in the lists
% so countInList([1,1,1,4,7], Result) gives {1:3, 4:1, 7:1}
%exept that the list should have positions 
countInList(PositionList, ResultDict):-
    dict_create(EmptyDict, _, []),
    countOccurancesIn(PositionList,EmptyDict, ResultDict).

countOccurancesIn([], Result, Result).
countOccurancesIn([(_,_,V)|Tail], CountMap, Result):-
    OldCount = CountMap.get(V, 0),   % 0 is default value, if its not in the map yet. We will bump that up to 0, so we only get 1 if its in there multiple times.
    NewCount is OldCount + 1, % 
    put_dict(V, CountMap, NewCount, NewDict),
    countOccurancesIn(Tail, NewDict, Result).

positionIsInRowY(Y, (_, RY, _)) :-
    RY = Y.

positionIsInColumnX(X, (CX, _, _)) :-
    CX = X.

onlyPositionsInRowY(Positions, Y, Filtered) :-
    include(positionIsInRowY(Y), Positions, Filtered).

onlyPositionsInColumnX(Positions, X, Filtered) :-
    include(positionIsInColumnX(X), Positions, Filtered).

%Todo find a better way to do this, its inefficent
%Probs good enough if you only call it once
%Just gives you 0 1 2 3if your board is size 4.
% assumes the board is square!
allIndices(Positions, Sorted) :-
    findall(X, member((X, _, _), Positions), AllIndices),
    sort(AllIndices, Sorted).

allColumns(Positions, ColumnList) :-
    allIndices(Positions, ColumnIndices),  % get 0,1,2,3...
    maplist(onlyPositionsInColumnX(Positions), ColumnIndices, ColumnList).

allRows(Positions, RowList):-
    allIndices(Positions, RowIndices),  % get 0,1,2,3...
    maplist(onlyPositionsInRowY(Positions), RowIndices, RowList).

findDuplicatePositions(PositionList, DuplicatesOnly):-
    countInList(PositionList, Dict),
    include(positionIsDuplicateAccordingToDict(Dict), PositionList, DuplicatesOnly).

positionIsDuplicateAccordingToDict(Dict, (_,_,V)) :-
    get_dict(V, Dict, Count),
    Count > 1.

% Call findDuplicatePositions on each row
duplicatesInAll(RowsOrColumns, AllDuplicatesSet) :-
    maplist(findDuplicatePositions, RowsOrColumns, DuplicatesPerSubList),
    append(DuplicatesPerSubList, AllDuplicatesList),
    list_to_ord_set(AllDuplicatesList, AllDuplicatesSet).

duplicatesInAllRows(AllPositions, DupsInRows):-
    allRows(AllPositions, AllRows),
    duplicatesInAll(AllRows,DupsInRows).

duplicatesInAllColumns(AllPositions, DupsInColumns):-
    allColumns(AllPositions, AllColumns),
    duplicatesInAll(AllColumns,DupsInColumns).

allDuplicateSets(AllPositions, RowOnly, ColOnly, InBoth) :-
    duplicatesInAllRows(AllPositions, DupsInRows),
    duplicatesInAllColumns(AllPositions, DupsInColumns),
    ord_intersection(DupsInRows, DupsInColumns, InBoth),
    ord_subtract(DupsInRows, InBoth, RowOnly),
    ord_subtract(DupsInColumns, InBoth, ColOnly).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VALID ROW AND COLUMN CONSTRAINT: (no duplicates)

%This set of predicates can check if all ROWS or COLUMNS are valid. 
%They are valid if they contain no duplicates, except zeros. Multiple zeros are allowed.
allRowsValid(Board) :- maplist(isListValid, Board).

allColumnsValid(Board) :- 
    transpose(Board, Transposed), 
    maplist(isListValid,Transposed).


% then we can check wether a row or column is valid using this.
% A row or column is valid all the numbers are uniqe, but any amount of zeros is allowed:
isListValid(X) :- 
    \+ nextto(0,0,X), %from std list, nextTo(X,Y,List) is true if x is next to y in List
    subtract(X, [0], ListWithoutZeros), % removes the zeros from the list
    is_set(ListWithoutZeros). %checks if a list contains duplicates
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONNECTED CONSTRAINT

allNonZeroConnected(Board) :-
    nonZeroPositions(Board, Positions),
    Positions = [Start|_],
    findConnectedPositions(Start, Positions, Connected),
    listsHaveSameSize(Positions,Connected).

getRow(Board, X, Row):- nth0(X,Board,Row).

%get from board at X,Y. Top Left is 0,0.
elementAt(Board, X,Y,Element):-
    getRow(Board,Y,Row), 
    % row is now the entire row, get the element from that:
    nth0(X,Row,Element).

elementAtIsNotZero(Board,X,Y):-
    elementAt(Board,X,Y,Value),
    Value\= 0.

% gives you the indices as pairs (X,Y) that have a nonzero value at the board
nonZeroPositions(Board, Positions) :-
    findall( (X,Y), elementAtIsNotZero(Board,X,Y), Positions).

listsHaveSameSize(List1,List2):-
    length(List1, Len),
    length(List2, Len).

% gives you all adjecent (X,Y) pairs for a (X,Y) position.
% use this by calling adjacent((1,1), Pos). Will give (0,1),(2,1), (1,0), (1,2)
adjacentPos((X1,Y1),(X2,Y2)) :- 
    (X1 = X2, adjacentNum(Y1,Y2)); 
    (Y1 = Y2, adjacentNum(X1,X2)).

%can be used like this: adjacentNum(5,X). gives X = 4 and X = 6
adjacentNum(Num, Adj) :- 
    Adj is Num + 1; 
    Adj is Num - 1.


adjacentWithinPostions(Positions, From, Adjacent):-
    adjacentPos(From, Adjacent), 
    member(Adjacent, Positions).


%Todo see if this is a bottleneck and if we can speed it up using sets or something
%allAdjacent is a list of all the positions that are adjecent to From that can be found in the list Positions
findAllAdjacentWithinPostions(From, Positions, AllAdjacent) :-
    findall(Adjacent, 
        adjacentWithinPostions(Positions, From, Adjacent),
        AllAdjacent
    ).



%%%%%%%%%%%%%%%%%%%
% THE BFS ALGORITHM for the connected constraint

%helper predicate you can call easily
findConnectedPositions(Start, Positions, Connected) :-
    %params: [start] is the queue
    % Positions are all the positions the search can use
    % [] are all visited nonZeroPositions
    % Connected is the result
    dfs([Start], Positions, [], Connected).

% If head is already visited, move on to the next in the queue 
dfs([Head|Tail], Positions, Visited, Connected) :-
    member(Head, Visited),
    dfs(Tail, Positions, Visited, Connected).

% if head is NOT visited, find its neigbours from the positions
% and add them to the queue, 
dfs([Head|Tail], Positions, Visited, Connected) :-
    \+ member(Head, Visited),
    findAllAdjacentWithinPostions(Head, Positions, Neighbors),
    append(Tail, Neighbors, Queue), %queue is tail(the prev queue) + neighbours
    dfs(Queue, Positions, [Head|Visited], Connected).

% End case: when the queue is empty, you can return the set of visited as the result
dfs([], _, Visited, Visited).

    


