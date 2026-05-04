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

modelVersion("3.0.2").

%This is the main file for HitoriProlog. It can find solutions to a Hitori puzzle. Use the isSolution(Board, SolutionBoard) predicate
% Alternatively, you can use the runHitorPrologOnFile.py script

%if you just want to see the solved puzzle board, use this. 
%Board is a 
isSolution(Board, SolutionBoard) :- 
    isSolutionZerodPositions(Board, ZerodPositions),
    translateToBoard(Board, ZerodPositions, SolutionBoard).


%finds a solution to a hitori puzzle, where the solution is a list of positions to mark.
%positionsToZero is an ord_set of all positions to mark as black.
isSolutionZerodPositions(Board, PositionsToZero) :-
    allPositionsWithValue(Board, Positions),
    allRows(Positions, RowList),
    allColumns(Positions, ColumnList),
    length(ColumnList, N),
    append(RowList, ColumnList, AllRowsAndColumns),
    maplist(countInList, AllRowsAndColumns, AllCountMaps),
    maplist(findDuplicatePositions, AllRowsAndColumns, AllCountMaps, AllDuplicateLists),
    applyRedundantConstraints(N, AllCountMaps, AllDuplicateLists, [], [], [], AllCountMapsWithRC, AllDuplicateListsWithRC, PreMarked),
    writeln("premarked:"),
    writeln(PreMarked),
    maplist(dupValuesOnly([]), AllDuplicateListsWithRC, AllDupNums),
    solveAll(AllDuplicateListsWithRC,AllCountMapsWithRC,AllDupNums, N, PreMarked, PositionsToZero).

%Base case, unify the bag with the result
applyRedundantConstraints(_N, [], [], ResCM, ResDL, MarkedRes, ResCM, ResDL, MarkedRes).

%Apply the redundant constraints to every countmap and duplicatelist,  Store results in two bags.
applyRedundantConstraints(N, [HCountMap| TCountMaps] , [HDuplicateList | TDuplicateLists], AllResCountMaps, AllResDuplicateLists, MarkedSofar, CMRes, TLRes, MarkedRes):-
    applyRedundantConstraintsForRowOrColumn(N, HCountMap, HDuplicateList, MarkedSofar, ResCountMap, ResDuplicateList, NewMarkedSoFar),
    applyRedundantConstraints(N, TCountMaps, TDuplicateLists, [ ResCountMap | AllResCountMaps] , [ResDuplicateList | AllResDuplicateLists ], NewMarkedSoFar, CMRes, TLRes, MarkedRes).

applyRedundantConstraintsForRowOrColumn(N, CountMap, DuplicateList, MarkedSofar, ResCountMap, ResDuplicateList, MarkedRes):-
    sandwichPair(N, CountMap, DuplicateList, KnownWhiteSP),
    sandwichTriple(N, DuplicateList, KnownWhiteSP, KnownBlackST),
    pairIsolation(N, CountMap, DuplicateList, KnownBlackPI),
    write("Pair isolation res:"),
    writeln(KnownBlackPI),
    %write("sandwichPair result (known white): "),
    %writeln(KnownWhiteSP),
    %write("sandwichTriple result (known black): "),
    %writeln(KnownBlackST),
    subtract(DuplicateList, KnownWhiteSP, DupListWithoutKnownWhitesSP),
    subtract(DupListWithoutKnownWhitesSP, KnownBlackST, ResDuplicateList),

    updateCountMapForKnownBlackPositions(CountMap, KnownBlackST, ResCountMap),
    maplist(stripValue, KnownBlackST, PositionsToMark),
    list_to_ord_set(PositionsToMark, PositionsToMarkSet),
    ord_union(MarkedSofar, PositionsToMarkSet, MarkedRes).


updateCountMapForKnownBlackPositions(ResCountMap, [],  ResCountMap).
updateCountMapForKnownBlackPositions(Countmap, [HKnownBlack | TKnownBlack], ResCountMap):-
    decrementValueInCountmap(Countmap, HKnownBlack, NewCountMap),
    updateCountMapForKnownBlackPositions(NewCountMap, TKnownBlack, ResCountMap).
    

decrementValueInCountmap(CountMap, (_X,_Y,V), NewCountMap):-
    get_dict(V, CountMap, CountLeft),
    NewCount is CountLeft - 1, 
    put_dict(V, CountMap, NewCount, NewCountMap).



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


recursivelySolveRowOrColumn(_AlreadyZerodInOther,_CountMap,[],Result,Result,_DuplicatePositions):-
    !.

recursivelySolveRowOrColumn(AlreadyZerodInOther,CountMap,DupNums, ChosenZeros, Result,[(X,Y,V)|TDuplicatePositions]) :-
    get_dict(V, CountMap, CountLeft),
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




