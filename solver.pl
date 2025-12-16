:- use_module(library(apply)).
:- use_module(library(lists)).
:- include('src/examplePuzzles.pl').
:- include('src/utils.pl').
:- include('src/connected.pl').
:- include('src/duplicateFinding.pl').

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


%TODO: These countmaps never change, and we do recompute them each time... Can be much faster!
%TODO same for the duplictes. It would be better to compute all the duplicates beforehand!
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



