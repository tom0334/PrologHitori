%%%%%%%%%%%%%%%
%Call this to check if the chosenPositions do NOT cut off any white squares
%Params: 
%[StartPositions]: A list of newly chosen positions(X,Y) to make black. These are the start positions to do DFS from to look for cutting off paths
%N : size of board, used to detect if a position is on the edge of the board or not.
%ChosenPositions: A list of positions (X,Y) that were in previous iterations, and that we already know do not cut off anything
%Run DFS from all startpositions.  
isStillConnectedFast([], _N, _ChosenPositions).

isStillConnectedFast([Start | TStartPositions], N, ChosenPositions) :-
    dfsCutSearch([(Start,Start)], N, ChosenPositions, 0,[],[]),
    isStillConnectedFast(TStartPositions, N, ChosenPositions),
    !.

%%%%%%%%%%%%%%%%%%%%%%%
%DFS Search. This is an edge based DFS search that will explore to find one of two things:
% 1. Two sides of the board. May be the same side, that does not matter. If two sides of the board are found, 
% then that path cuts of some amount of white squares from the rest. 
% 2. A node that it already encountered before. That means there is a cycle encircling some amount of white squares on the board.

%This predicate will FAIL when it finds one of these things, and succeed when it ensures the black squares chosen are OK.
%Params:
%Queue A list of edges to explore. The start has itself as FROM, but that does not really matter
%N: board size
%ChosenPositions: Positions chosen in previous iterations to be black. These can be explored.
%SidesOfBoardFoundSoFar the number of sides of the board found so far. When this reaches 2 we fail.
%UsedEdges: A set of edges that we explored so far, to avoid it going backwards.
%VisitedNodes: All nodes that we explored so far

%If the current edge is already used, that means we already considered this edge, so we can do nothing and continue...
dfsCutSearch([(Head,From) | TQueue], N, ChosenPositions, SidesOfBoardFoundSoFar,UsedEdges,VisitedNodes):-
    member( (From, Head), UsedEdges), %list with member is faster than ord_set here since these are often small lists. I tested it!
    dfsCutSearch(TQueue, N, ChosenPositions, SidesOfBoardFoundSoFar, UsedEdges,VisitedNodes),
    !.


%End case 1: when we encounter a position on the side of the board, AND we already found one,
% There is a cutting of path!
% we know for sure there is a path from start to two sides, so there is a cutof path!
dfsCutSearch( [ (Head,_From) | _TQueue], N, _ChosenPositions, 1 , _UsedEdges,_VisitedNodes):-
    isOnEdge(Head,N),
    !,
    fail.

%End case 2: when we encounter a position that was already VisitedNodes, that means we found a cycle!
% So there is at least white element not connected to the rest!
dfsCutSearch([(Head,_From) | __TQueue], _N, _ChosenPositions, _SidesOfBoardFoundSoFar, _UsedEdges,VisitedNodes):-
    member(Head, VisitedNodes),
    !,
    fail.

% End case 3: when the queue is empty, we can say we did NOT find a cutting path. So we do succeed!
dfsCutSearch([], _N, _ChosenPositions, _SidesOfBoardFoundSoFar, _UsedEdges,_VisitedNodes):-
    !.

%Regular case: increment the counter for sidesOfBoard found if needed, find all neigbouring edges to explore, and call yourself recursively.
dfsCutSearch([(Head,From) | TQueue], N, ChosenPositions, SidesOfBoardFoundSoFar,UsedEdges,VisitedNodes):-
    calcNewSidesOfBoardFoundSoFar(Head, N, SidesOfBoardFoundSoFar, NewSidesOfBoardFoundSoFar),
    findAllPositionsToExplore(ChosenPositions, Head, Neighbors),
    append(TQueue, Neighbors, Queue),
    dfsCutSearch(Queue, N, ChosenPositions, NewSidesOfBoardFoundSoFar, [(Head,From)|UsedEdges],[Head|VisitedNodes]),
    !.


%%%%%%%%%%%%%%%%%%%%
%Smaller helper predicates for the cutof connection detection.

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

%Finds the neigbour edges to explore from a position
findAllPositionsToExplore(ChosenPositions,  From, AllAdjacentFromInfoPairs) :-
    findall(
        Adjacent,
        getPositionToExplore(ChosenPositions, From, Adjacent),
        AllAdjacentFromInfoPairs
    ).

%Adds one to the counter if it is on the edge of the board...
calcNewSidesOfBoardFoundSoFar(Pos, N, SidesOfBoardFoundSoFar,NewSidesOfBoardFoundSoFar):-
    isOnEdge(Pos,N),
    NewSidesOfBoardFoundSoFar is SidesOfBoardFoundSoFar + 1,
    !.

calcNewSidesOfBoardFoundSoFar(_, _, EdgesSoFar,EdgesSoFar).


