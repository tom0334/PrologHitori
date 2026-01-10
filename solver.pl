:- use_module(library(apply)).
:- use_module(library(lists)).
:- include('src/examplePuzzles.pl').
:- include('src/utils.pl').
:- include('src/connected.pl').
:- include('src/duplicateFinding.pl').
:- include('src/testingUtils.pl').
:- include('src/cutofConnected.pl').
:- include('src/smarts.pl').

solverVersion("2.0").

%still here for backwards compat, or if you just want to see the board...
isSolution(Board, SolutionBoard) :- isSolutionZerodPositions(Board, SolutionBoard, _ ).

isSolutionZerodPositions(Board, SolutionBoard, ZerodPositions) :-
    allPositionsWithValue(Board, AllPositionsWithValue),
    findSolution(AllPositionsWithValue, ZerodPositions), 
    translateToBoard(Board, ZerodPositions, SolutionBoard).

%finds a solution to a hitori puzzle
%Positions is a list of all positions in the board in the (X,Y,V) form
%positionsToZero is an ord_set of all positions to mark as black.
findSolution(Positions, PositionsToZero):-
    allRows(Positions, RowList),
    allColumns(Positions, ColumnList),
    length(ColumnList, N),

    %Prepare rows
    maplist(countInList, RowList, AllCountMapsR),
    maplist(findDuplicatePositions, RowList, AllCountMapsR, AllDuplicateListsR),
    maplist(filterKnownWhitesRow(N), AllDuplicateListsR, AllSmartDuplicateListsR),
    maplist(dupValuesOnly([]), AllSmartDuplicateListsR, AllDupNumsR),

    %Prepare columns
    maplist(countInList, ColumnList, AllCountMapsC),
    maplist(findDuplicatePositions, ColumnList, AllCountMapsC, AllDuplicateListsC),
    %TODO:
    %maplist(filterKnownWhitesColumn, AllDuplicatesListC, AllSmartDuplicateListsC),
    maplist(dupValuesOnly([]), AllDuplicateListsC, AllDupNumsC),

    solveAll(AllDuplicateListsR,AllCountMapsR,AllDupNumsR, N, [], RowResult),
    %solve columns:

    solveAll(AllDuplicateListsC,AllCountMapsC,AllDupNumsC, N, RowResult, PositionsToZero).





%params:
% [DuplicateLists]: ONLY the duplicate positions (X,Y,V) for each row and column. So the first element is a list of all duplicates in the first row
% [Countmaps]: the countmaps for the amount of times each number occurs in a the row or column
% [Dupnums] all duplicate numbers only (V) in each row. This may contain numbers that are already solved, but that is okay. Testing found that to be better than sorting them out again.
% N: puzzle size 
% ChosenSoFar: a bag for the currently chosen positions.
% Result: Will be unified with result when done: a list of all positions to make zero

%end case: when the first 3 params are empty lists, that means that we solved all rows and columns!
solveAll([],[],[], _N, Result, Result).

solveAll([HDupPositions|TDupPositions], [HCm| TCm], [HDupNums | TDupNums], N, ChosenSoFar, Result):-
    recursivelySolveRowOrColumn(ChosenSoFar, HCm, HDupNums, [], Chosen ,HDupPositions),
    ord_union(ChosenSoFar, Chosen, NewChosen),
    isStillConnectedFast(Chosen, N, NewChosen),
    solveAll(TDupPositions, TCm, TDupNums, N, NewChosen, Result).


% gives you the indices as pairs (X,Y) that have a nonzero value at the board
allPositionsWithValue(Board, PositionsWithValue) :-
    findall( (X,Y,V), elementAt(Board,X,Y,V), PositionsWithValue).

%IMPORTANT: the list in which you check needs to be (X,Y) only, NOT (X,Y,V)
notNextToOther((X,Y),AllWithoutValue):-

    YPrev is Y-1,
    (\+ ord_memberchk((X,YPrev),AllWithoutValue)),

    XPrev is X -1,
    (\+ ord_memberchk((XPrev,Y),AllWithoutValue)),

    XNext is X + 1,
    (\+ ord_memberchk((XNext,Y),AllWithoutValue)),

    YNext is Y+1,
    (\+ ord_memberchk((X,YNext),AllWithoutValue)).



duplicatesInDict(Dict) :- 
    get_dict(_, Dict, V),
    V > 1.  


recursivelySolveRowOrColumn(_AlreadyZerodInOther,_CountMap,[],Result,Result,_DuplicatePositions):-
    !.

recursivelySolveRowOrColumn(AlreadyZerodInOther,CountMap,DupNums, ChosenZeros, Result,[(X,Y,V)|TDuplicatePositions]) :-
    CountLeft = CountMap.get(V, 0),
    CountLeft > 1,
        notNextToOther((X,Y), AlreadyZerodInOther),
    notNextToOther((X,Y), ChosenZeros),


    %We can pick this one as a zero!
    NewCount is CountLeft - 1, 
    (   NewCount < 2
    ->  ord_del_element(DupNums, V, NewDupNums)
    ;   NewDupNums = DupNums
    ),

    ord_add_element(ChosenZeros, (X,Y), NewChosenZeros),
    put_dict(V, CountMap, NewCount, NewCountMap),

    recursivelySolveRowOrColumn(AlreadyZerodInOther, NewCountMap,NewDupNums, NewChosenZeros, Result, TDuplicatePositions).


recursivelySolveRowOrColumn(AlreadyZerodInOther,CountMap,DupNums, ChosenZeros, Result,[(_,_,_)|TDuplicatePositions]) :-
    recursivelySolveRowOrColumn(AlreadyZerodInOther, CountMap,DupNums, ChosenZeros, Result, TDuplicatePositions).




