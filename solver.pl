:- use_module(library(apply)).

% This is actually the constraint programming libary
% using too much from here feels kinda like cheating (?),
% since im researching Logic Programming.
% For now its only transpose though I think.
% TODO make my own transpose if this is an issue
:- use_module(library(clpfd)). 

% https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/singles.html#4x4de%23722022701905902
% the actual puzzle
puzzle(
	[[4, 3, 1, 2], [2, 4, 2, 4], [2, 1, 1, 4], [4, 2, 4, 1]]
).
solvedPuzzle(
	[[4, 3, 1, 2], [0, 4, 2, 0], [2, 1, 0, 4], [0, 2, 4, 1]] 
).

%https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/singles.html#5x5dk%23864813841153333
smallPuzzle([
    [4, 2, 2, 5, 5],
    [2, 1, 1, 4, 5],
    [3, 5, 5, 2, 2],
    [1, 5, 4, 2, 3],
    [2, 2, 1, 4, 4]
]
).

mediumPuzzle([
    [2, 5, 5, 1, 6, 4],
    [4, 5, 6, 2, 4, 6],
    [1, 1, 3, 2, 4, 2],
    [6, 2, 3, 3, 1, 5],
    [6, 4, 6, 4, 2, 2],
    [1, 6, 2, 4, 5, 3]
]).

bigPuzzle([
    [5, 10, 7, 9, 6, 6, 8, 1, 10, 3],
    [6, 5, 5, 7, 6, 3, 10, 10, 7, 2],
    [7, 3, 9, 10, 4, 6, 2, 8, 3, 6],
    [1, 8, 5, 3, 7, 10, 3, 1, 6, 10],
    [8, 3, 3, 5, 2, 7, 6, 6, 5, 9],
    [5, 10, 1, 1, 3, 3, 7, 2, 5, 2],
    [3, 2, 1, 8, 3, 10, 4, 9, 7, 1],
    [10, 7, 2, 3, 8, 7, 6, 10, 9, 9],
    [2, 8, 3, 10, 10, 8, 5, 7, 1, 6],
    [4, 6, 7, 1, 9, 7, 10, 3, 1, 7]
]).


% https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/singles.html#10x10dk%23410746926154546
isSolution(Board, Solution) :-
	sameBoard(Board, Solution),
	allRowsValid(Solution),
	allColumnsValid(Solution),
	noAdjecentZerosInBoard(Solution), 
	allNonZeroConnected(Solution).


%This set of predicates ensures the solution has the same shape and numbers in it as the board.
sameBoard(Board, Solution) :-
    maplist(sameRow, Board, Solution).

sameRow(RowB, RowS) :-
    maplist(cellOK, RowB, RowS).

% a cell is ok if it is either the same in the board and solution, or if it is 0 in the solution.
cellOK(_, 0).
cellOK(X, X).





% True if a list has NO adjacent zeros
noAdjecentZerosInList([]).
noAdjecentZerosInList([_]).
noAdjecentZerosInList([X,Y|T]) :-
    \+ (X = 0, Y = 0),
    noAdjecentZerosInList([Y|T]).

%
noAdjecentZerosInAllRows(Board):-
	maplist(noAdjecentZerosInList, Board).

% Check a board for no adjacent zeros in rows AND columns
noAdjecentZerosInBoard(Board) :-
    noAdjecentZerosInAllRows(Board),
    transpose(Board, Transposed),
    noAdjecentZerosInAllRows(Transposed).



%This set of predicates can check if all ROWS or COLUMNS are valid. 
%They are valid if they contain no duplicates, except zeros. Multiple zeros are allowed.
allRowsValid(Board) :- maplist(isListValid, Board).

allColumnsValid(Board) :- 
	transpose(Board, Transposed), 
	maplist(isListValid,Transposed).

% this is a simple way to check if all elements are unique:
simpleIsUnique(List):- is_set(List).

% for the case where we want to check if the numbers are unique in the list, but multiple black squares are okay, we can use this to get the list without zeros:
listWithoutZeros(List, R) :- subtract(List, [0], R).

% then we can check wether a row or column is valid using this.
% A row or column is valid all the numbers are uniqe, but any amount of zeros is allowed:
isListValid(X) :- listWithoutZeros(X, ListWithoutZeros), simpleIsUnique(ListWithoutZeros).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONNECTED CONSTRAINT

allNonZeroConnected(Board) :-
    nonZeroPositions(Board, Positions),
    Positions = [Start|_],
    findConnectedPositions(Start, Positions, Connected),
    listsHaveSameSize(Positions,Connected).

getRow(Board, X, Row):- nth0(X,Board,Row).

%get from board at X,Y. Top Left is 0,0.
elementAt(Board, X,Y,Element):-
    getRow(Board,Y,Row), 
    % row is now the entire row, get the element from that:
    nth0(X,Row,Element).

elementAtIsNotZero(Board,X,Y):-
    elementAt(Board,X,Y,Value),
    Value\= 0.

% gives you the indices as pairs (X,Y) that have a nonzero value at the board
nonZeroPositions(Board, Positions) :-
    findall( (X,Y), elementAtIsNotZero(Board,X,Y), Positions).

listsHaveSameSize(List1,List2):-
    length(List1, Len),
    length(List2, Len).

% gives you all adjecent (X,Y) pairs for a (X,Y) position.
% use this by calling adjacent((1,1), Pos). Will give (0,1),(2,1), (1,0), (1,2)
adjacentPos((X1,Y1),(X2,Y2)) :- 
    (X1 = X2, adjacentNum(Y1,Y2)); 
    (Y1 = Y2, adjacentNum(X1,X2)).

%can be used like this: adjacentNum(5,X). gives X = 4 and X = 6
adjacentNum(Num, Adj) :- 
    Adj is Num + 1; 
    Adj is Num - 1.


adjacentWithinPostions(Positions, From, Adjacent):-
    adjacentPos(From, Adjacent), 
    member(Adjacent, Positions).

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

    


