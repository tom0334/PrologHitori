blackPositionsBecauseOfPairIsolationForV(AllDuplicatePositions, V, (BlackX, BlackY, BlackV)):-
    member((X,Y,V), AllDuplicatePositions),
    member((X2,Y2,V), AllDuplicatePositions),
    neighbours((X,Y), (X2,Y2)),
    !,
    member((BlackX, BlackY, V), AllDuplicatePositions),
    notTheSamePosition((BlackX,BlackY), (X,Y)),
    notTheSamePosition((BlackX,BlackY), (X2,Y2)),
    BlackV = V.

findAllBlackPositionsForValue(AllDuplicatePositions, V, AllBlackForPair):-
    findall(Position, 
        blackPositionsBecauseOfPairIsolationForV(AllDuplicatePositions,V, Position),
    AllBlackForPair).

valueOccursMoreThanTwoTimes(CountMap, V):-
    get_dict(V, CountMap, Count),
    Count > 2. % we need a pair and at least one other one

notTheSamePosition((X,_Y), (X2,_Y2)) :- 
    \+(X is X2),
    !. 

notTheSamePosition((_X,Y), (_X2,Y2)) :- 
    \+(Y is Y2),
    !.      

neighbours((X,_Y), (X2, _Y2)):- 
    X is (X2 + 1),
    !.
neighbours((_X,Y), (_X2, Y2)):- 
    Y is (Y2 + 1),
    !.
neighbours((X,_Y), (X2, _Y2)):- 
    X is (X2 - 1),
    !.
neighbours((_X,Y), (_X2, Y2)):- 
    Y is (Y2 - 1),
    !.



pairIsolation(_N, CountMap, DuplicatePositions, KnownBlack):-
    findall(Value, valueOccursMoreThanTwoTimes(CountMap,Value), Values),
    maplist(findAllBlackPositionsForValue(DuplicatePositions), Values, BlackPositionLists),
    flatten(BlackPositionLists, KnownBlack).
