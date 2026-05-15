% This is the main file for HitoriProlog. It can find solutions to a Hitori puzzle. Use the isSolution(Board, SolutionBoard) predicate to print the solution as a board,
% or if you are satisfied with only a list of positions to mark, you can use the isSolutionZerodPositions predicate. 
% If you want to write the solution to a file too, see the testingUtils.pl file

% Alternatively, you can use the runHitorPrologOnFile.py script to run HitoriProlog on a .singles file.

:- use_module(library(apply)).
:- use_module(library(lists)).
:- include('src/examplePuzzles.pl').
:- include('src/debugPuzzles.pl').
:- include('src/utils.pl').
:- include('src/duplicateFinding.pl').
:- include('src/testingUtils.pl').
:- include('src/cutofConnected.pl').
:- include('src/sandwichPair.pl').
:- include('src/sandwichTriple.pl').
:- include('src/pairIsolation.pl').
:- include('src/redundantConstraints.pl').
:- include('src/neighbours.pl').

modelVersion("3.0.2").

%if you just want to see the solved puzzle board in your console, use this. 
isSolution(Board, SolutionBoard) :- 
    isSolutionZerodPositions(Board, ZerodPositions),
    translateToBoard(Board, ZerodPositions, SolutionBoard).

%Main predicate to find a solution to a hitori puzzle, where the solution is a list of positions to mark.
%Board is a list of lists of numbers corresponding to the puzzle.
%positionsToZero is an ord_set of all positions to mark as black.
isSolutionZerodPositions(Board, PositionsToZero) :-
    allPositionsWithValue(Board, Positions),
    allRows(Positions, RowList),
    allColumns(Positions, ColumnList),
    length(ColumnList, N),
    append(RowList, ColumnList, AllRowsAndColumns),
    maplist(countInList, AllRowsAndColumns, AllCountMaps),
    maplist(findDuplicatePositions, AllRowsAndColumns, AllCountMaps, AllDuplicateLists),
    applyRCsToCountMapsAndDupLists(N, AllCountMaps, AllDuplicateLists, AllCountMapsWithRC, AllDuplicateListsWithRC, PreMarked),
    maplist(dupValuesOnly([]), AllDuplicateListsWithRC, AllDupNums),
    solveAll(AllDuplicateListsWithRC,AllCountMapsWithRC,AllDupNums, N, PreMarked, PositionsToZero).

% Solves a hitori puzzle iteratively row/column wise using a number of precomputed datastructures. 
% [DuplicateLists]: ONLY the duplicate positions (X,Y,V) for each row and column. So the first element is a list of all duplicates in the first row.
% [Countmaps]: the countmaps for the amount of times each number occurs in a the row or column. So the first element is a dictonary containing the counts that each number occurs in the first row
% [Dupnums] all duplicate numbers only (V) in each row. This may contain numbers that are already solved, but that is okay. Testing found that to be better than sorting them out again. 
%The first element contains the duplicate numbers in the first row.

% N: puzzle size 
% ChosenSoFar: a bag for the currently chosen positions.
% Result: Will be unified with result when done: a list of all positions to make zero

%Base case: when the first 3 params are empty lists, that means that we solved all rows and columns!
solveAll([],[],[], _N, Result, Result).

solveAll([HDupPositions|TDupPositions], [HCm| TCm], [HDupNums | TDupNums], N, ChosenSoFar, Result):-
    recursivelySolveRowOrColumn(ChosenSoFar, HCm, HDupNums, [], Chosen ,HDupPositions),
    ord_union(ChosenSoFar, Chosen, NewChosen),
    isStillConnectedFast(Chosen, N, NewChosen),
    solveAll(TDupPositions, TCm, TDupNums, N, NewChosen, Result).


% Base case where we there are no more duplicates left in this row/column. This row/column is now solved!
recursivelySolveRowOrColumn(_AlreadyZerodInOther,_CountMap,[],Result,Result,_DuplicatePositions):-
    !.

%Case where we DO (attempt to) mark the current duplicate. 
recursivelySolveRowOrColumn(AlreadyZerodInOther,CountMap,DupNums, ChosenZeros, Result,[(X,Y,V)|TDuplicatePositions]) :-
    get_dict(V, CountMap, CountLeft),
    CountLeft > 1,
    notNextToOther((X,Y), AlreadyZerodInOther),
    notNextToOther((X,Y), ChosenZeros),

    %We can pick this one to mark!
    NewCount is CountLeft - 1, 
    (   NewCount < 2
    ->  ord_del_element(DupNums, V, NewDupNums)
    ;   NewDupNums = DupNums
    ),

    ord_add_element(ChosenZeros, (X,Y), NewChosenZeros),
    put_dict(V, CountMap, NewCount, NewCountMap),

    recursivelySolveRowOrColumn(AlreadyZerodInOther, NewCountMap,NewDupNums, NewChosenZeros, Result, TDuplicatePositions).

%Case where we do NOT mark the current duplicate. 
recursivelySolveRowOrColumn(AlreadyZerodInOther,CountMap,DupNums, ChosenZeros, Result,[(_,_,_)|TDuplicatePositions]) :-
    recursivelySolveRowOrColumn(AlreadyZerodInOther, CountMap,DupNums, ChosenZeros, Result, TDuplicatePositions).


%Checks if a position is NOT next to any of the positions in AllWithoutValue.
%Implements the adjacency constraint.
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



