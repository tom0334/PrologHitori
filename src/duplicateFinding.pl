% This file contains predicates for finding duplicates in list in an efficient way

% Finds duplicates in a list by creating a dictonary with the amount of times a number occurs
% creates a dictionary that has the following values:
% key: the value of the position
% value: the number of times that number occurs in the list
% So countInList([1,1,1,4,7], Result) gives {1:3, 4:1, 7:1} 
countInList(PositionList, ResultDict):-
    dict_create(EmptyDict, _, []),
    countOccurancesIn(PositionList,EmptyDict, ResultDict).

% Counts the occurances of all numbers in a list, generates a dictonary with the counts. A CountMap!
countOccurancesIn([], Result, Result).
countOccurancesIn([(_,_,V)|Tail], CountMap, Result):-
    % 0 is default value, if its not in the map yet.
    OldCount = CountMap.get(V, 0),   
    NewCount is OldCount + 1,
    put_dict(V, CountMap, NewCount, NewDict),
    countOccurancesIn(Tail, NewDict, Result).



% Uses a CountMap to find which positions are duplicates.
findDuplicatePositions(PositionList, Dict, DuplicatesOnly):-
    include(positionIsDuplicateAccordingToDict(Dict), PositionList, DuplicatesOnly).

positionIsDuplicateAccordingToDict(Dict, (_,_,V)) :-
    get_dict(V, Dict, Count),
    Count > 1.


% Finds the duplicate positions (X,Y,V) in a list, returns only a list of values of the positions that are duplicate.
% So, ([(1,2,3),(1,1,3),(7,8,9)]) will give [3], since there are 2 positions with value 3 so it is duplicate.
dupValuesOnly(Result, [], Result).

dupValuesOnly( SoFar, [ (_,_,V) | Tail ], Result):-
    ord_add_element(SoFar,V, NewSoFar),
    dupValuesOnly(NewSoFar, Tail,Result ).



% Some helper predicates to find all positions on the board, 
% and to convert those positions to lists of positions corresponding to each row and column.
% These are inefficient, but they are only used once per puzzle so that is OK.

% Gives you all positions on the board with value: [(X,Y,V)]
allPositionsWithValue(Board, PositionsWithValue) :-
    findall( (X,Y,V), elementAt(Board,X,Y,V), PositionsWithValue).

% Just gives you [0, 1, 2, 3] if your board is size 4.
% Assumes the board is square!
allIndices(Positions, Sorted) :-
    findall(X, member((X, _, _), Positions), AllIndices),
    sort(AllIndices, Sorted).

allColumns(Positions, ColumnList) :-
    allIndices(Positions, ColumnIndices),  % get 0,1,2,3...
    maplist(onlyPositionsInColumnX(Positions), ColumnIndices, ColumnList).

allRows(Positions, RowList):-
    allIndices(Positions, RowIndices),  % get 0,1,2,3...
    maplist(onlyPositionsInRowY(Positions), RowIndices, RowList).



%Some simple helper predicates to find only positions in a certain row or column
onlyPositionsInRowY(Positions, Y, Filtered) :-
    include(positionIsInRowY(Y), Positions, Filtered).

onlyPositionsInColumnX(Positions, X, Filtered) :-
    include(positionIsInColumnX(X), Positions, Filtered).

positionIsInRowY(Y, (_, RY, _)) :-
    RY = Y.

positionIsInColumnX(X, (CX, _, _)) :-
    CX = X.

