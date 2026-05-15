%This file contains predicates for finding neigbours within the board. Used for some redundant constraints.

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

neigbour(N, (MeX, MeY), (NX,NY)):-
    (
        leftNeighbour(N, (MeX, MeY, _), (NX,NY,_));
        rightNeighbour(N, (MeX, MeY, _), (NX,NY,_));
        upNeigbour(N, (MeX, MeY, _ ), (NX,NY,_));
        downNeighbour(N, (MeX, MeY, _), (NX,NY,_))
    ).

%These white neigbours may contain duplicates, but that is OK, because we will have to remove the duplicates anyway
% when we apply multiple redundant constraints.
findAllWhiteNeigboursOfPositions(N, BlackPositions, WhiteNeigboursNoValues):-
    maplist(findAllWhiteNeigboursOfBlackPosition(N), BlackPositions, ListOfLists),
    flatten(ListOfLists, WhiteNeigboursNoValues).

findAllWhiteNeigboursOfBlackPosition(N, PositionWithValue, NeighborsWithoutValue):-
    stripValue(PositionWithValue, PositionsWithoutValue),
    findall(Neighbor, neigbour(N, PositionsWithoutValue, Neighbor), NeighborsWithoutValue).