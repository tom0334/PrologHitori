sandwichTripleBlackPositionsAround(N, AllDuplicatePositions, (X,Y,V), BlackPositions):-
    member((X,Y,V), AllDuplicatePositions),
    upNeigbour(N,(X,Y,V), (UNX,UNY,UNV)),
    downNeighbour(N,(X,Y,V), (DNX,DNY,DNV)),
    member((UNX, UNY, UNV), AllDuplicatePositions),
    member((DNX, DNY, DNV), AllDuplicatePositions),
    UNV = DNV,
    V = UNV,
    BlackPositions = [(UNX,UNY,UNV),(DNX,DNY,DNV)].

sandwichTripleBlackPositionsAround(N, AllDuplicatePositions, (X,Y,V), BlackPositions):-
    member((X,Y,V), AllDuplicatePositions),
    leftNeighbour(N,(X,Y,V), (LNX,LNY,LNV)),
    rightNeighbour(N,(X,Y,V), (RNX,RNY,RNV)),
    member((LNX, LNY, LNV), AllDuplicatePositions),
    member((RNX, RNY, RNV), AllDuplicatePositions),
    RNV = LNV,
    V = RNV,
    BlackPositions = [(LNX,LNY,LNV),(RNX,RNY,RNV)].


sandwichTriple(N, DuplicatePositions, SandwichPairWhitePositions, SandwichTripleBlackPositions):-
    maplist(findBlackPositionsAroundMiddle(N, DuplicatePositions), SandwichPairWhitePositions, Result),
    flatten(Result, SandwichTripleBlackPositions).

findBlackPositionsAroundMiddle(N, DuplicatePositions, WhitePos, BlackPositions):-
    findall(BlackPosForDirection, sandwichTripleBlackPositionsAround(N, DuplicatePositions, WhitePos, BlackPosForDirection), BlackPositions).


    