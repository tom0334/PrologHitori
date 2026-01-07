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


%TODO: see if we can get rid of the overhead of this predicate. It does almost nothing and is called very often

isStillConnectedFast(Chosen, N, ChosenSoFar):-
    \+(findConnectedPathsToEdges(Chosen, N, ChosenSoFar, true)).

findConnectedPathsToEdges([StartH | _StartT], N, ChosenPositions, true) :-
    dfsCutSearch([(StartH,StartH)], N, ChosenPositions, 0,[],[], Result),
    Result >= 2,
    !.

findConnectedPathsToEdges([_ | StartT], N, ChosenPositions, IsConnected) :-
    findConnectedPathsToEdges(StartT, N, ChosenPositions, IsConnected).


%If the current head is on an edge, add one to the edges EdgesFoundSoFar
dfsCutSearch([(Head,From) | Tail], N, ChosenPositions, EdgesFoundSoFar,UsedEdges,Visited, Result):-
    member( (From, Head), UsedEdges),
    dfsCutSearch(Tail, N, ChosenPositions, EdgesFoundSoFar, UsedEdges,Visited, Result),
    !.


%end case 1: when we find an edge when we already found one, we are done, and can return true
dfsCutSearch( [ (Head,_From) | _Tail], N, _ChosenPositions, 1 , _UsedEdges,_Visited, 2):-
    isOnEdge(Head,N),
    !.

dfsCutSearch([(Head,_From) | _], _N, _ChosenPositions, _EdgesFoundSoFar, _UsedEdges,Visited, 2):-
    member(Head, Visited),
    !.

% End case 1: when the queue is empty, we can say we did NOT find a cutting path
dfsCutSearch([], _N, _ChosenPositions, _EdgesFoundSoFar, _UsedEdges,_Visited, 0):-
    !.


%If the current head is on an edge, add one to the edges EdgesFoundSoFar
dfsCutSearch([(Head,From) | Tail], N, ChosenPositions, EdgesFoundSoFar,UsedEdges,Visited, Result):-
    calcNewEdgesSoFar(Head, N, EdgesFoundSoFar, NewEdgesSoFar),
    findAllPositionsToExplore(ChosenPositions, Head, Neighbors),
    append(Tail, Neighbors, Queue),
    dfsCutSearch(Queue, N, ChosenPositions, NewEdgesSoFar, [(Head,From)|UsedEdges],[Head|Visited], Result),
    !.

calcNewEdgesSoFar(Pos, N, EdgesSoFar,NewEdgesSoFar):-
    isOnEdge(Pos,N),
    NewEdgesSoFar is EdgesSoFar + 1,
    !.

calcNewEdgesSoFar(_, _, EdgesSoFar,EdgesSoFar).


