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

% https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/singles.html#10x10dk%23410746926154546
isSolution(Board, Solution) :-
    findPossibleSolution(Board, PositionsToZero), 
    translateToBoard(Board, PositionsToZero, Solution),
    allNonZeroConnected(Solution).


findPossibleSolution(Board, PositionsToZero):-
    allPositionsWithValue(Board, Positions),
    allRows(Positions, RowList),
    allColumns(Positions, ColumnList),
    append(RowList, ColumnList, AllRowsAndColumns),
    solveAll(AllRowsAndColumns, [], PositionsToZero).


solveAll([], Result, Result).

solveAll([Head|Tail], ChosenSoFar, Result):-
    solveRowOrColumn(ChosenSoFar,Chosen, Head),
    ord_union(ChosenSoFar, Chosen, NewChosen),
    solveAll(Tail, NewChosen, Result).

solveRowOrColumn(AlreadyZerodInOther, Result, RowOrColumn):-
    countInList(RowOrColumn, CountMap),
    include(positionIsDuplicateAccordingToDict(CountMap), RowOrColumn, DuplicatesOnly),
    recursivelySolveRowOrColumn(AlreadyZerodInOther, CountMap, [], Result ,DuplicatesOnly).


% gives you the indices as pairs (X,Y) that have a nonzero value at the board
allPositionsWithValue(Board, PositionsWithValue) :-
    findall( (X,Y,V), elementAt(Board,X,Y,V), PositionsWithValue).

%TODO THIS IS SLOW, since it needs to convert to only positions.
% That means it goes to the list every time... Fix that.
notNextToOther((X,Y),AllPositionsOnly):-
    XNext is X + 1,
    XPrev is X -1,
    YNext is Y+1,
    YPrev is Y-1,
    convertToValueOnlyPositions(AllPositionsOnly, AllWithoutValue),
    \+ ord_memberchk((XNext,Y),AllWithoutValue),
    \+ ord_memberchk((XPrev,Y),AllWithoutValue),
    \+ ord_memberchk((X,YNext),AllWithoutValue),
    \+ ord_memberchk((X,YPrev),AllWithoutValue).




duplicatesInDict(Dict) :- get_dict(_, Dict, V),V > 1. 

%When there are no duplicates left, we are done!
% TODO: see if we can make this more efficent somehow.... Can we keep this info so we dont have to check every time?
recursivelySolveRowOrColumn(_,CountMap,Result,Result,_):-
    \+ duplicatesInDict(CountMap).

recursivelySolveRowOrColumn(AlreadyZerodInOther,CountMap, ChosenZeros, Result,[(X,Y,V)|Tail]) :-
    CountLeft = CountMap.get(V, 0),
    CountLeft > 1,
    notNextToOther((X,Y), ChosenZeros),
    notNextToOther((X,Y), AlreadyZerodInOther),

    %We can pick this one as a zero!
    NewCount is CountLeft - 1, 
    ord_add_element(ChosenZeros, (X,Y,V), NewChosenZeros),
    put_dict(V, CountMap, NewCount, NewCountMap),

    recursivelySolveRowOrColumn(AlreadyZerodInOther, NewCountMap, NewChosenZeros, Result, Tail).


recursivelySolveRowOrColumn(AlreadyZerodInOther,CountMap, ChosenZeros, Result,[(_,_,_)|Tail]) :-
    recursivelySolveRowOrColumn(AlreadyZerodInOther, CountMap, ChosenZeros, Result, Tail).




%For translating a set of chosen duplicates to a board (list of rows).
%%%%%%%%%%%%
translateToBoard(Board, Zerod, Solution):-
    sameShape(Board, Solution),
    allPositionsWithValue(Board, Positions),
    maplist(translatePositionToBoardValue(Solution, Zerod), Positions).

translatePositionToBoardValue(Solution,Zerod,(X,Y,V)):-
    elementAt(Solution, X, Y, SolutionValue),
    boardValueOrZerod((X,Y,V),Zerod, SolutionValue).


boardValueOrZerod((X,Y,V), Zerod, 0) :- ord_memberchk((X,Y,V), Zerod), !.
boardValueOrZerod((_,_,V), _, V).

sameShape([], []).
sameShape([A|As], [B|Bs]) :-
    same_length(A, B),
    sameShape(As, Bs).

% Finding duplicates in a list
%%%%%%%%%%
%creates a dictionary that has the following values:
% key: the value of the position
% Value: the number of times that number occurs in the list
% so countInList([1,1,1,4,7], Result) gives {1:3, 4:1, 7:1} 
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

%This is not an efficient way to do this, so please dont use this in performance critial parts of the solver
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


%Finding duplicate positions in the board
%%%%%%%%%%

findDuplicatePositions(PositionList, DuplicatesOnly):-
    countInList(PositionList, Dict),
    include(positionIsDuplicateAccordingToDict(Dict), PositionList, DuplicatesOnly).

positionIsDuplicateAccordingToDict(Dict, (_,_,V)) :-
    get_dict(V, Dict, Count),
    Count > 1.

%Finds the duplicate positions in list of ALL positions.
% these duplicates will be split over 3 sets:
    % those duplicate ONLY in their column
    % those duplicate ONLY in their Rows
    % those DUPLICATE IN BOTH their row and column
allDuplicateSets(AllPositions, RowOnly, ColOnly, InBoth,AllWithoutValue) :-
    duplicatesInAllRows(AllPositions, DupsInRows),
    duplicatesInAllColumns(AllPositions, DupsInColumns),
    ord_intersection(DupsInRows, DupsInColumns, InBoth),
    ord_subtract(DupsInRows, InBoth, RowOnly),
    ord_subtract(DupsInColumns, InBoth, ColOnly),
    ord_union(DupsInRows, DupsInColumns, DupsInAll),
    convertToValueOnlyPositions(DupsInAll, AllWithoutValue).

stripValue((X,Y,_), (X,Y)).

convertToValueOnlyPositions(PositionsWithValue, Result) :-
    maplist(stripValue, PositionsWithValue, PositionsWithoutValue),
    list_to_ord_set(PositionsWithoutValue, Result).

%finds all duplicates in a list of positions, returns ordered set
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

%TODO replace this elementAt call to a get from the set of chosen duplicates
%should be much faster.
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
    (X1 = X2, Y1 is Y2 + 1);
    (X1 = X2, Y1 is Y2 - 1);  
    (Y1 = Y2,  X1 is X2 + 1);
    (Y1 = Y2,  X1 is X2 - 1).

adjacentWithinPostions(Positions, From, Adjacent):-
    member(Adjacent, Positions),
    adjacentPos(From, Adjacent).


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



%%%%%%%%%%%%%%%%%%%%%
%Utils

loop_through_list(_File, []) :- !.
loop_through_list(File, [Head|Tail]) :-
    write(File, Head),
    nl(File),
    loop_through_list(File, Tail).

write_list_to_file(Filename,List) :-
    open(Filename, write, File),
    loop_through_list(File, List),
    close(File).

    