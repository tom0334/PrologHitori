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

getPositionToExplore(ChosenPositions, From, Adjacent):-
    diagonallyAdjacentPos(From, Adjacent),
    ord_memberchk(Adjacent,ChosenPositions).

%Todo see if this is a bottleneck and if we can speed it up using sets or something
%allAdjacent is a list of all the positions that are adjecent to From that can be found in the list Positions
findAllPositionsToExplore(ChosenPositions, From, AllAdjacent) :-
    findall(Adjacent, 
        getPositionToExplore(ChosenPositions, From, Adjacent),
        AllAdjacent
    ).

%%%%%%%%%%%%%%%

isStillConnectedFast(Chosen, N, ChosenSoFar):-
        %write("Chosen:"),
        %writeln(Chosen),
        %write("ChosenSofar"),
        %writeln(ChosenSoFar),
    \+(findConnectedPathsToEdges(Chosen, N, ChosenSoFar, true)).

findConnectedPathsToEdges([StartH | _StartT], N, ChosenPositions, true) :-
    %write("searching from: "),
    %writeln(StartH),
    dfsCutSearch([StartH], N, ChosenPositions, 0,[], Result),
    Result = 2,
    %writeln("Conncected found!"),
    !.

findConnectedPathsToEdges([_ | StartT], N, ChosenPositions, IsConnected) :-
    findConnectedPathsToEdges(StartT, N, ChosenPositions, IsConnected).




%end case 2: when we find an edge when we already found one, we are done, and can return true
dfsCutSearch([Head | _Tail], N, _ChosenPositions, 1 , Visited, 2):-
    isOnEdge(Head,N),
    \+ member(Head, Visited),
    %write("END CASE 2: CONNECTION FOUND "),
    %writeln(Head),
    %writeln(""),
    !.

% End case 1: when the queue is empty, we can say we did NOT find a cutting path
dfsCutSearch([], _N, _ChosenPositions, _EdgesFoundSoFar, _Visited, 0):-
    %writeln("END CASE 1: Q empty..."),
    %writeln(""),
    !.

% If head is already visited, move on to the next in the queue 
dfsCutSearch([Head | Tail], N, ChosenPositions, EdgesFoundSoFar, Visited, Result):-
    member(Head, Visited),
    %write("already visited... : "),
    %writeln(Head),
    dfsCutSearch(Tail, N, ChosenPositions, EdgesFoundSoFar, Visited, Result),
    !.

%If the current head is on an edge, add one to the edges EdgesFoundSoFar
dfsCutSearch([Head | Tail], N, ChosenPositions, EdgesFoundSoFar,Visited, Result):-
    %writeln("dfs 3"),
    calcNewEdgesSoFar(Head, N, EdgesFoundSoFar, NewEdgesSoFar),
    %write('new edges: '),
    %write(NewEdgesSoFar),
    %write(" from pos: "),
    %writeln(Head),

    findAllPositionsToExplore(ChosenPositions, Head, Neighbors),
    %write('Neighbors:'),
    %writeln(Neighbors),

    append(Tail, Neighbors, Queue), 
    dfsCutSearch(Queue, N, ChosenPositions, NewEdgesSoFar, [Head|Visited], Result),
    !.


calcNewEdgesSoFar(Pos, N, EdgesSoFar,NewEdgesSoFar):-
    isOnEdge(Pos,N),
    NewEdgesSoFar is EdgesSoFar + 1,
    !.

calcNewEdgesSoFar(_, _, EdgesSoFar,EdgesSoFar).


