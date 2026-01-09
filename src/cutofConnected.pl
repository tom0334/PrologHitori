diagonallyAdjacentPos((X1,Y1),(X2,Y2)) :- 
    (X2 is X1 + 1, Y2 is Y1 + 1);
    (X2 is X1 + 1, Y2 is Y1 - 1);
    (X2 is X1 - 1, Y2 is Y1 + 1);
    (X2 is X1 - 1, Y2 is Y1 - 1).

isOnEdge((X,Y),N):- 
    (X = 0,!);
    (Y = 0,!);
    (X is N -1,!);
    (Y is N -1,!).

getPositionToExplore(ChosenPositions, From, (Adjacent,From))  :-
    diagonallyAdjacentPos(From, Adjacent),
    ord_memberchk(Adjacent, ChosenPositions).


findAllPositionsToExplore(ChosenPositions,  From, AllAdjacentFromInfoPairs) :-
    findall(
        Adjacent,
        getPositionToExplore(ChosenPositions, From, Adjacent),
        AllAdjacentFromInfoPairs
    ).

%%%%%%%%%%%%%%%

isStillConnectedFast([], _N, _ChosenPositions).

isStillConnectedFast([Start | TStartPositions], N, ChosenPositions) :-
    dfsCutSearch([(Start,Start)], N, ChosenPositions, 0,[],[]),
    isStillConnectedFast(TStartPositions, N, ChosenPositions),
    !.

%If the current edge is already used, that means we already considered this edge, so we can do nothing and continue...
dfsCutSearch([(Head,From) | Tail], N, ChosenPositions, SidesOfBoardFoundSoFar,UsedEdges,Visited):-
    member( (From, Head), UsedEdges),
    dfsCutSearch(Tail, N, ChosenPositions, SidesOfBoardFoundSoFar, UsedEdges,Visited),
    !.


%End case 1: when we encounter a position on the side of the board, AND  when we already found one before, we are done!
% we know for sure there is a path from start to two sides, so there is a cutof path!
dfsCutSearch( [ (Head,_From) | _Tail], N, _ChosenPositions, 1 , _UsedEdges,_Visited):-
    isOnEdge(Head,N),
    !,
    fail.

%End case 2: when we encounter a position that was already visited, that means we found a cycle!
% So there is at least white element not connected to the rest!
dfsCutSearch([(Head,_From) | _], _N, _ChosenPositions, _SidesOfBoardFoundSoFar, _UsedEdges,Visited):-
    member(Head, Visited),
    !,
    fail.

% End case 3: when the queue is empty, we can say we did NOT find a cutting path
dfsCutSearch([], _N, _ChosenPositions, _SidesOfBoardFoundSoFar, _UsedEdges,_Visited):-
    !.


%Regular case: increment the counter for sidesOfBoard found if needed, find all neigbouring edges to explore, and call yourself recursively.
dfsCutSearch([(Head,From) | Tail], N, ChosenPositions, SidesOfBoardFoundSoFar,UsedEdges,Visited):-
    calcNewSidesOfBoardFoundSoFar(Head, N, SidesOfBoardFoundSoFar, NewSidesOfBoardFoundSoFar),
    findAllPositionsToExplore(ChosenPositions, Head, Neighbors),
    append(Tail, Neighbors, Queue),
    dfsCutSearch(Queue, N, ChosenPositions, NewSidesOfBoardFoundSoFar, [(Head,From)|UsedEdges],[Head|Visited]),
    !.



%Adds one to the counter if it is on the edge of the board...
calcNewSidesOfBoardFoundSoFar(Pos, N, SidesOfBoardFoundSoFar,NewSidesOfBoardFoundSoFar):-
    isOnEdge(Pos,N),
    NewSidesOfBoardFoundSoFar is SidesOfBoardFoundSoFar + 1,
    !.

calcNewSidesOfBoardFoundSoFar(_, _, EdgesSoFar,EdgesSoFar).


