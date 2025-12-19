:- use_module(library(apply)).
:- use_module(library(lists)).
:- include('src/examplePuzzles.pl').
:- include('src/utils.pl').
:- include('src/connected.pl').
:- include('src/duplicateFinding.pl').


solverVersion("2.0").

%still here for backwards compat, or if you just want to see the board...
isSolution(Board, SolutionBoard) :- isSolutionZerodPositions(Board, SolutionBoard, _ ).

isSolutionZerodPositions(Board, SolutionBoard, ZerodPositions) :-
    findPossibleSolution(Board, ZerodPositions), 
    translateToBoard(Board, ZerodPositions, SolutionBoard),
    allNonZeroConnected(SolutionBoard).

isSolutionAndWrite(Board,N,Seed, OutputFile, SolutionBoard, SolutionZerod):-
    time(
        isSolutionZerodPositions(Board, SolutionBoard, SolutionZerod )
    ),
    solverVersion(Version),
    string_concat("Solved by: v", Version, Comment),
    writeSolution(OutputFile, N,Seed, Comment, Board, SolutionZerod).

findPossibleSolution(Board, PositionsToZero):-
    allPositionsWithValue(Board, Positions),
    allRows(Positions, RowList),
    allColumns(Positions, ColumnList),
    append(RowList, ColumnList, AllRowsAndColumns),
    maplist(countInList, AllRowsAndColumns, AllCountMaps),
    solveAll(AllRowsAndColumns, AllCountMaps, [], PositionsToZero).


solveAll([],[], Result, Result).

solveAll([Head|Tail], [HCm| TCm], ChosenSoFar, Result):-
    solveRowOrColumn(ChosenSoFar,Chosen, Head, HCm),
    ord_union(ChosenSoFar, Chosen, NewChosen),
    solveAll(Tail,TCm, NewChosen, Result).


%TODO theduplicates never change! It would be better to compute all the duplicates beforehand!
solveRowOrColumn(AlreadyZerodInOther, Result, RowOrColumn, CountMap):-
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
    \+ duplicatesInDict(CountMap)
    ,!.

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




