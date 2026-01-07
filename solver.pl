:- use_module(library(apply)).
:- use_module(library(lists)).
:- include('src/examplePuzzles.pl').
:- include('src/utils.pl').
:- include('src/connected.pl').
:- include('src/duplicateFinding.pl').
:- include('src/testingUtils.pl').
:- include('src/cutofConnected.pl').


solverVersion("2.0").

%still here for backwards compat, or if you just want to see the board...
isSolution(Board, SolutionBoard) :- isSolutionZerodPositions(Board, SolutionBoard, _ ).

isSolutionZerodPositions(Board, SolutionBoard, ZerodPositions) :-
    allPositionsWithValue(Board, AllPositionsWithValue),
    convertToValueOnlyPositions(AllPositionsWithValue, AllPositionsNoValue),

    findPossibleSolution(Board,AllPositionsWithValue, ZerodPositions), 
    ord_subtract(AllPositionsNoValue, ZerodPositions, NonZerodPositions),
    allNonZeroConnected(NonZerodPositions),
    translateToBoard(Board, ZerodPositions, SolutionBoard).


findPossibleSolution(Board, Positions, PositionsToZero):-
    allRows(Positions, RowList),
    allColumns(Positions, ColumnList),
    length(ColumnList, N),
    append(RowList, ColumnList, AllRowsAndColumns),
    maplist(countInList, AllRowsAndColumns, AllCountMaps),
    maplist(findDuplicatePositions, AllRowsAndColumns, AllDuplicateLists),
    maplist(dupValuesOnly([]), AllDuplicateLists, AllDupNums),
    solveAll(Board,AllDuplicateLists,AllCountMaps,AllDupNums, N, [], PositionsToZero).


solveAll(_Board, [],[],[], _N, Result, Result).

solveAll(Board, [Head|Tail], [HCm| TCm], [HDupNums | TDupNums], N, ChosenSoFar, Result):-
    recursivelySolveRowOrColumn(ChosenSoFar, HCm, HDupNums, [], Chosen ,Head),
    %TODO Figure out why chosen sometimes has elements that were already chosen?
    ord_union(ChosenSoFar, Chosen, NewChosen),

    %translateToBoard(Board, NewChosen, SolutionBoard),
    %writeln(Board),
    %writeln(""),
    %maplist(writeln,SolutionBoard),
    %writeln(""),

    isStillConnectedFast(Chosen, N, NewChosen),
    solveAll(Board,Tail,TCm,TDupNums,N, NewChosen, Result).


% gives you the indices as pairs (X,Y) that have a nonzero value at the board
allPositionsWithValue(Board, PositionsWithValue) :-
    findall( (X,Y,V), elementAt(Board,X,Y,V), PositionsWithValue).

%IMPORTANT: the list in which you check needs to be (X,Y) only, NOT (X,Y,V)
notNextToOther((X,Y),AllWithoutValue):-
    XPrev is X -1,
    (\+ ord_memberchk((XPrev,Y),AllWithoutValue)),

    YPrev is Y-1,
    (\+ ord_memberchk((X,YPrev),AllWithoutValue)),

    XNext is X + 1,
    (\+ ord_memberchk((XNext,Y),AllWithoutValue)),

    YNext is Y+1,
    (\+ ord_memberchk((X,YNext),AllWithoutValue)).



duplicatesInDict(Dict) :- 
    get_dict(_, Dict, V),
    V > 1.  


recursivelySolveRowOrColumn(_,_,[],Result,Result,_):-
    !.

recursivelySolveRowOrColumn(AlreadyZerodInOther,CountMap,DupNums, ChosenZeros, Result,[(X,Y,V)|Tail]) :-
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

    recursivelySolveRowOrColumn(AlreadyZerodInOther, NewCountMap,NewDupNums, NewChosenZeros, Result, Tail).


recursivelySolveRowOrColumn(AlreadyZerodInOther,CountMap,DupNums, ChosenZeros, Result,[(_,_,_)|Tail]) :-
    recursivelySolveRowOrColumn(AlreadyZerodInOther, CountMap,DupNums, ChosenZeros, Result, Tail).




