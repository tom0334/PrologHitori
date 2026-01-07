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

getPositionToExplore(ChosenPositions, UsedEdges, From, (Adjacent,From))  :-
    diagonallyAdjacentPos(From, Adjacent),
    ord_memberchk(Adjacent, ChosenPositions),
    \+ (member( (From, Adjacent), UsedEdges )),
    \+ (member( (Adjacent, From), UsedEdges )).


findAllPositionsToExplore(ChosenPositions, UsedEdges, From, AllAdjacentFromInfoPairs) :-
    findall(
        Adjacent,
        getPositionToExplore(ChosenPositions, UsedEdges, From, Adjacent),
        AllAdjacentFromInfoPairs
    ).

%%%%%%%%%%%%%%%

%TODO also detect cases where the zero forms a loop around a spot, closing it in.
% These are more rare, so i guess it still better to use both for now.
%example:
%    0 
%  0 5 0
%    0

isStillConnectedFast(Chosen, N, ChosenSoFar):-
        %write("Chosen:"),
        %writeln(Chosen),
        %write("ChosenSofar"),
        %writeln(ChosenSoFar),
    \+(findConnectedPathsToEdges(Chosen, N, ChosenSoFar, true)).

findConnectedPathsToEdges([StartH | _StartT], N, ChosenPositions, true) :-
    %write("searching from: "),
    %writeln(StartH),
    dfsCutSearch([(StartH,StartH)], N, ChosenPositions, 0,[],[], Result),
    Result >= 2,
    %writeln("Conncected found!"),
    !.

findConnectedPathsToEdges([_ | StartT], N, ChosenPositions, IsConnected) :-
    findConnectedPathsToEdges(StartT, N, ChosenPositions, IsConnected).



%If the current head is on an edge, add one to the edges EdgesFoundSoFar
dfsCutSearch([(Head,From) | Tail], N, ChosenPositions, EdgesFoundSoFar,UsedEdges,Visited, Result):-
    member( (From, Head), UsedEdges),

    %writeln("ALREADY USED EDGE in the other direction!"),
    dfsCutSearch(Tail, N, ChosenPositions, EdgesFoundSoFar, UsedEdges,Visited, Result),
    !.


%end case 1: when we find an edge when we already found one, we are done, and can return true
dfsCutSearch( [ (Head,_From) | _Tail], N, _ChosenPositions, 1 , _UsedEdges,_Visited, 2):-
    isOnEdge(Head,N),
    %write("END CASE 1: CONNECTION FOUND "),
    %writeln(Head),
    %writeln(""),
    !.

dfsCutSearch([(Head,_From) | _], _N, _ChosenPositions, _EdgesFoundSoFar, _UsedEdges,Visited, 2):-
    member(Head, Visited),
    %write("END CASE 2: found already visited! (CYCLE detected):"),
    %writeln(Visited),
    %writeln(""),
    !.

% End case 1: when the queue is empty, we can say we did NOT find a cutting path
dfsCutSearch([], _N, _ChosenPositions, _EdgesFoundSoFar, _UsedEdges,_Visited, 0):-
    %writeln("END CASE 3: Q empty..."),
    %writeln(""),
    !.


%If the current head is on an edge, add one to the edges EdgesFoundSoFar
dfsCutSearch([(Head,From) | Tail], N, ChosenPositions, EdgesFoundSoFar,UsedEdges,Visited, Result):-
    %writeln("dfs 3"),
    calcNewEdgesSoFar(Head, N, EdgesFoundSoFar, NewEdgesSoFar),
    %write('new edges: '),
    %write(NewEdgesSoFar),
    %write(" HEAD: "),
    %writeln(Head),
    %write("FROM:"),
    %writeln(Head),
    %write("UsedEdges:"),
    %writeln(UsedEdges),

    findAllPositionsToExplore(ChosenPositions, UsedEdges, Head, Neighbors),
    %write('Neighbors:'),
    %writeln(Neighbors),

    append(Tail, Neighbors, Queue),
    dfsCutSearch(Queue, N, ChosenPositions, NewEdgesSoFar, [(Head,From)|UsedEdges],[Head|Visited], Result),
    !.

calcNewEdgesSoFar(Pos, N, EdgesSoFar,NewEdgesSoFar):-
    isOnEdge(Pos,N),
    NewEdgesSoFar is EdgesSoFar + 1,
    !.

calcNewEdgesSoFar(_, _, EdgesSoFar,EdgesSoFar).


