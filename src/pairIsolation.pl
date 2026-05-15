%The Pair isolation redundant constraint

%Finds a position that can be marked black because there exists a adjacent pair with the same value in the row/column
blackPositionsBecauseOfPairIsolationForV(AllDuplicatePositions, V, (BlackX, BlackY, BlackV)):-
    member((X,Y,V), AllDuplicatePositions),
    member((X2,Y2,V), AllDuplicatePositions),
    neighbours((X,Y), (X2,Y2)),
    %Cut since we found a pair, so others can be marked black! 
    %(without the cut we would find both (A,B) and (B,A))
    %This will not find cases where 2 adjacent pairs exist. That's OK because puzzles with 2 pairs are unsolvable! 
    !,
    member((BlackX, BlackY, V), AllDuplicatePositions),
    notTheSamePosition((BlackX,BlackY), (X,Y)),
    notTheSamePosition((BlackX,BlackY), (X2,Y2)),
    BlackV = V.

% Finds ALL positions with value V that can be marked black, because there exists a adjacent pair with the same value.
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

%Simple XY neigbour checks used only in this redundant constraint.
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


% Main Pair isolation predicate. Finds all KnownBlack positions that must be marked in a row/column, 
% and also returns all positions that must be white because of adjacency to the black ones.
pairIsolation(N, CountMap, DuplicatePositions, KnownBlack, KnownWhite):-
    % For pair isolation to be possible, we need at least 3 occurances. Otherwise there is nothing to isolate...
    findall(Value, valueOccursMoreThanTwoTimes(CountMap,Value), Values),

    % Now that we know the values that occur at least 3 times in this row/column, we can check each one for possible pair isolation oppurtunities... 
    maplist(findAllBlackPositionsForValue(DuplicatePositions), Values, BlackPositionLists),

    %The result is a list of lists, where each element is a list of positions that we can mark black with some Value. Flatten it
    flatten(BlackPositionLists, KnownBlack),

    %All adjacent white neigbours. Already gives us the positions in XY as desired.
    findAllWhiteNeigboursOfPositions(N, KnownBlack, KnownWhite).

