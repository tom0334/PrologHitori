blackPositionsBecauseOfPairIsolation(_N, CountMap, AllDuplicatePositions, (BlackX, BlackY)):-
    get_dict(V, CountMap, Count),
    Count > 2, % we need a pair and at least one other one
    write("found one that occurs more than twice!"),
    writeln(V),
    member((X,Y,V), AllDuplicatePositions),
    member((X2,Y2,V), AllDuplicatePositions),
    neighbours((X,Y), (X2,Y2)),
    member((BlackX, BlackY, V), AllDuplicatePositions),
    notTheSamePosition((BlackX,BlackY), (X,Y)),
    notTheSamePosition((BlackX,BlackY), (X2,Y2)).


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

pairIsolation(N, CountMap, DuplicatePositions, KnownBlack):-
    findall(Pos, blackPositionsBecauseOfPairIsolation(N,CountMap,DuplicatePositions,Pos), KnownBlack).
