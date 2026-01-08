%Finding duplicate positions in the board
%%%%%%%%%%

findDuplicatePositions(PositionList, Dict, DuplicatesOnly):-
    include(positionIsDuplicateAccordingToDict(Dict), PositionList, DuplicatesOnly).

positionIsDuplicateAccordingToDict(Dict, (_,_,V)) :-
    get_dict(V, Dict, Count),
    Count > 1.


dupValuesOnly(Result, [], Result).

dupValuesOnly( SoFar, [ (_,_,V) | Tail ], Result):-
    ord_add_element(SoFar,V, NewSoFar),
    dupValuesOnly(NewSoFar, Tail,Result ).


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