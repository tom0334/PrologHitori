%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONNECTED CONSTRAINT

allNonZeroConnected(NonZerodPositions) :-
    writeln("CHECKING CONNECTED"),
    NonZerodPositions = [Start|_],
    findConnectedPositions(Start, NonZerodPositions, Connected),
    listsHaveSameSize(NonZerodPositions,Connected).

listsHaveSameSize(List1,List2):-
    length(List1, Len),
    length(List2, Len).

% gives you all adjecent (X,Y) pairs for a (X,Y) position.
% use this by calling adjacent((1,1), Pos). Will give (0,1),(2,1), (1,0), (1,2)
adjacentPos((X1,Y1),(X2,Y2)) :- 
    (X1 = X2, Y1 is Y2 + 1);
    (X1 = X2, Y1 is Y2 - 1);  
    (Y1 = Y2,  X1 is X2 + 1);
    (Y1 = Y2,  X1 is X2 - 1).

adjacentWithinPostions(Positions, From, Adjacent):-
    member(Adjacent, Positions),
    adjacentPos(From, Adjacent).


%Todo see if this is a bottleneck and if we can speed it up using sets or something
%allAdjacent is a list of all the positions that are adjecent to From that can be found in the list Positions
findAllAdjacentWithinPostions(From, Positions, AllAdjacent) :-
    findall(Adjacent, 
        adjacentWithinPostions(Positions, From, Adjacent),
        AllAdjacent
    ).


%%%%%%%%%%%%%%%%%%%
% THE BFS ALGORITHM for the connected constraint

%helper predicate you can call easily
findConnectedPositions(Start, Positions, Connected) :-
    %params: [start] is the queue
    % Positions are all the positions the search can use
    % [] are all visited nonZeroPositions
    % Connected is the result
    dfs([Start], Positions, [], Connected).

% If head is already visited, move on to the next in the queue 
dfs([Head|Tail], Positions, Visited, Connected) :-
    member(Head, Visited),
    dfs(Tail, Positions, Visited, Connected).

% if head is NOT visited, find its neigbours from the positions
% and add them to the queue, 
dfs([Head|Tail], Positions, Visited, Connected) :-
    \+ member(Head, Visited),
    findAllAdjacentWithinPostions(Head, Positions, Neighbors),
    append(Tail, Neighbors, Queue), %queue is tail(the prev queue) + neighbours
    dfs(Queue, Positions, [Head|Visited], Connected).

% End case: when the queue is empty, you can return the set of visited as the result
dfs([], _, Visited, Visited).
