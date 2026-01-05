:- use_module(library(apply)).
:- use_module(library(lists)).
:- include('src/examplePuzzles.pl').
:- include('src/utils.pl').
:- include('src/connected.pl').
:- include('src/duplicateFinding.pl').
:- include('src/testingUtils.pl').


solverVersion("2.0").

%still here for backwards compat, or if you just want to see the board...
isSolution(Board, SolutionBoard) :- isSolutionZerodPositions(Board, SolutionBoard, _ ).

isSolutionZerodPositions(Board, SolutionBoard, ZerodPositions) :-
    findPossibleSolution(Board, ZerodPositions), 
    translateToBoard(Board, ZerodPositions, SolutionBoard),
    allNonZeroConnected(SolutionBoard).


findPossibleSolution(Board, PositionsToZero):-
    allPositionsWithValue(Board, Positions),
    allRows(Positions, RowList),
    allColumns(Positions, ColumnList),
    append(RowList, ColumnList, AllRowsAndColumns),
    maplist(countInList, AllRowsAndColumns, AllCountMaps),
    maplist(findDuplicatePositions, AllRowsAndColumns, AllDuplicateLists),
    solveAll(AllDuplicateLists, AllCountMaps, [], PositionsToZero).


solveAll([],[], Result, Result).

solveAll([Head|Tail], [HCm| TCm], ChosenSoFar, Result):-
    dupValuesOnly(Head, [], DupNums),
    recursivelySolveRowOrColumn(ChosenSoFar, HCm, DupNums, [], Chosen ,Head),
    ord_union(ChosenSoFar, Chosen, NewChosen),
    solveAll(Tail,TCm, NewChosen, Result).


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


recursivelySolveRowOrColumn(_,_,[],Result,Result,_):-
    !.

recursivelySolveRowOrColumn(AlreadyZerodInOther,CountMap,DupNums, ChosenZeros, Result,[(X,Y,V)|Tail]) :-
    CountLeft = CountMap.get(V, 0),
    CountLeft > 1,
    notNextToOther((X,Y), ChosenZeros),
    notNextToOther((X,Y), AlreadyZerodInOther),

    %We can pick this one as a zero!
    NewCount is CountLeft - 1, 
    (   NewCount < 2
    ->  ord_del_element(DupNums, V, NewDupNums)
    ;   NewDupNums = DupNums
    ),

    ord_add_element(ChosenZeros, (X,Y), NewChosenZeros),
    put_dict(V, CountMap, NewCount, NewCountMap),

    recursivelySolveRowOrColumn(AlreadyZerodInOther, NewCountMap,NewDupNums, NewChosenZeros, Result, Tail).


recursivelySolveRowOrColumn(AlreadyZerodInOther,CountMap,DupNums, ChosenZeros, Result,[(_,_,_)|Tail]) :-
    recursivelySolveRowOrColumn(AlreadyZerodInOther, CountMap,DupNums, ChosenZeros, Result, Tail).




