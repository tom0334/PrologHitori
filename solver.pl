:- use_module(library(apply)).
:- use_module(library(lists)).
:- include('src/examplePuzzles.pl').
:- include('src/utils.pl').
:- include('src/connected.pl').

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

%IMPORTANT: the list in which you check needs to be (X,Y) only, NOT (X,Y,V)
notNextToOther((X,Y),AllWithoutValue):-
    XNext is X + 1,
    XPrev is X -1,
    YNext is Y+1,
    YPrev is Y-1,
    (\+ ord_memberchk((XNext,Y),AllWithoutValue)),
    (\+ ord_memberchk((XPrev,Y),AllWithoutValue)),
    (\+ ord_memberchk((X,YNext),AllWithoutValue)),
    (\+ ord_memberchk((X,YPrev),AllWithoutValue)).


duplicatesInDict(Dict) :- 
    get_dict(_, Dict, V),
    V > 1.  

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
    ord_add_element(ChosenZeros, (X,Y), NewChosenZeros),
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


boardValueOrZerod((X,Y,_), Zerod, 0) :- ord_memberchk((X,Y), Zerod), !.
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

