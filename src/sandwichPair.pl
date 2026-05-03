isWhiteBecauseSandwichPair(N, AllDuplicatePositions, (X,Y,V)):-
    member((X,Y,V), AllDuplicatePositions),
    leftNeighbour(N,(X,Y,V), (LNX,LNY,LNV)),
    rightNeighbour(N,(X,Y,V), (RNX,RNY,RNV)),
    member((LNX, LNY, LNV), AllDuplicatePositions),
    member((RNX, RNY, RNV), AllDuplicatePositions),
    RNV = LNV.

isWhiteBecauseSandwichPair(N, AllDuplicatePositions, (X,Y,V)):-
    member((X,Y,V), AllDuplicatePositions),
    upNeigbour(N,(X,Y,V), (UNX,UNY,UNV)),
    downNeighbour(N,(X,Y,V), (DNX,DNY,DNV)),
    member((UNX, UNY, UNV), AllDuplicatePositions),
    member((DNX, DNY, DNV), AllDuplicatePositions),
    UNV = DNV.

leftNeighbour( _N, (MeX, MeY, _MeV), (LNX,LNY, _LNV) ):-
    LNX is MeX -1,
    LNY is MeY,
    LNX >= 0.

rightNeighbour(N, (MeX, MeY, _MeV), (RNX,RNY, _RNV) ):-
    RNX is MeX +1,
    RNY is MeY,
    RNX < N.

upNeigbour(N, (MeX, MeY, _MeV), (RNX,RNY, _RNV) ):-
    RNY is MeY +1,
    RNX is MeX,
    RNY < N.

downNeighbour( _N, (MeX, MeY, _MeV), (LNX,LNY, _LNV) ):-
    LNY is MeY -1,
    LNX is MeX,
    LNY >= 0.


% we can remove the one in between from the duplicate positions, as we know it will never be marked. 
% do NOT change the countmap, we still need to select another one with the same number!
% This is equivalaent to the recursive solve predicate skipping it.
sandwichPair(N, _CountMap, DuplicatePositions, ToExclude):-
    findall(X, isWhiteBecauseSandwichPair(N, DuplicatePositions, X), ToExclude).

    