%Sandwich pair redundant constraint

% Finds a position that must be white because it is surrounded by two positions with the same value

% HORIZONTAL case
isWhiteBecauseSandwichPair(N, AllDuplicatePositions, (X,Y,V)):-
    member((X,Y,V), AllDuplicatePositions),
    leftNeighbour(N,(X,Y,V), (LNX,LNY,LNV)),
    rightNeighbour(N,(X,Y,V), (RNX,RNY,RNV)),
    member((LNX, LNY, LNV), AllDuplicatePositions),
    member((RNX, RNY, RNV), AllDuplicatePositions),
    RNV = LNV.

% VERTICAL case
isWhiteBecauseSandwichPair(N, AllDuplicatePositions, (X,Y,V)):-
    member((X,Y,V), AllDuplicatePositions),
    upNeigbour(N,(X,Y,V), (UNX,UNY,UNV)),
    downNeighbour(N,(X,Y,V), (DNX,DNY,DNV)),
    member((UNX, UNY, UNV), AllDuplicatePositions),
    member((DNX, DNY, DNV), AllDuplicatePositions),
    UNV = DNV.


% Main sandwichPair predicate. Finds all knownWhite positions (XY) in a row or column, using a set of duplicate positions.
sandwichPair(N, _CountMap, DuplicatePositions, ToExclude, KnownWhiteXY):-
    findall(X, isWhiteBecauseSandwichPair(N, DuplicatePositions, X), KnownWhite),
    maplist(stripValue, KnownWhite, KnownWhiteXY).

    