%Sandwich triple redundant constraint. Depends on the SandwichPair redundant constraint!

% Finds (ALL) sandwich tripple black positions around a single middle known white sandwichpair position.
% There can be up to 4, (both vertical and horizontal).

% VERTICAL CASE
sandwichTripleBlackPositionsAround(N, AllDuplicatePositions, (X,Y,V), BlackPositions):-
    member((X,Y,V), AllDuplicatePositions),
    upNeigbour(N,(X,Y,V), (UNX,UNY,UNV)),
    downNeighbour(N,(X,Y,V), (DNX,DNY,DNV)),
    member((UNX, UNY, UNV), AllDuplicatePositions),
    member((DNX, DNY, DNV), AllDuplicatePositions),
    UNV = DNV,
    V = UNV,
    BlackPositions = [(UNX,UNY,UNV),(DNX,DNY,DNV)].

% HORIZONTAL CASE
sandwichTripleBlackPositionsAround(N, AllDuplicatePositions, (X,Y,V), BlackPositions):-
    member((X,Y,V), AllDuplicatePositions),
    leftNeighbour(N,(X,Y,V), (LNX,LNY,LNV)),
    rightNeighbour(N,(X,Y,V), (RNX,RNY,RNV)),
    member((LNX, LNY, LNV), AllDuplicatePositions),
    member((RNX, RNY, RNV), AllDuplicatePositions),
    RNV = LNV,
    V = RNV,
    BlackPositions = [(LNX,LNY,LNV),(RNX,RNY,RNV)].

% Main predicate to get ALL the sandwich tripple known black positions, around a set of known sandwich pair white positions.
% Also finds all adjecent white positions to the black positions.
sandwichTriple(N, DuplicatePositions, SandwichPairWhitePositions, SandwichTripleBlackPositions, SandWichTripleWhitePositionsXY):-
    % This results in a list of lists, where each element is a list of known black positions around one white sandwichpair position 
    maplist(findBlackPositionsAroundMiddle(N, DuplicatePositions), SandwichPairWhitePositions, Result),
    %Flatten that list of lists into one list.
    flatten(Result, SandwichTripleBlackPositions),
    %Find all neigbours of the black positions (these must be white)
    findAllWhiteNeigboursOfPositions(N, SandwichTripleBlackPositions, SandWichTripleWhitePositionsXY).

% FindAll black positions (up to 4, for both vertical and horizontal) around ONE single white sandwichpair middle position.
findBlackPositionsAroundMiddle(N, DuplicatePositions, WhitePos, BlackPositions):-
    findall(BlackPosForDirection, sandwichTripleBlackPositionsAround(N, DuplicatePositions, WhitePos, BlackPosForDirection), BlackPositions).


    