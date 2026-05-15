% Obviously not a valid puzzle, but useful for testing the RC
sandwichPairsExample([
    [9,4,9,5], 
    [2,3,2,4], 
    [9,4,9,5], 
    [1,3,1,9]
]).

% Obviously not a valid puzzle, but useful for testing the RC
sandwichTripleExample([
    [9,1,9,2], 
    [1,1,1,2], 
    [9,1,9,2], 
    [8,9,8,9] 
]).

% Obviously not a valid puzzle, but useful for testing the RC
sandwichTripleExample2([
    [1,1,1,9,1,1,1], 
    [3,3,3,9,3,3,3], 
    [1,1,1,9,1,1,1], 
    [2,2,2,6,2,2,2], 
    [1,1,1,9,1,1,1], 
    [3,3,3,9,3,3,3], 
    [1,1,1,9,1,1,1]
]).


% Obviously not a valid puzzle, but useful for testing the RC
pairIsolationExample([
    [1,1,9,1], 
    [4,3,2,1], 
    [1,2,9,5], 
    [4,3,9,2] 
]).

pairIsolationExample2([
    [1,1,9,1,1], 
    [4,3,2,1,8], 
    [1,2,9,5,7], 
    [4,3,9,2,6] 
]).

puzzleWithPairIsolation([
    [3, 2, 2, 1, 3],
    [5, 1, 3, 5, 2],
    [4, 3, 2, 4, 5],
    [4, 2, 5, 3, 5],
    [1, 2, 4, 5, 3]
]).

failingPuzzle([
    [3, 3, 1, 4, 2],
    [4, 2, 2, 3, 5],
    [2, 1, 4, 2, 3],
    [3, 2, 5, 4, 1],
    [4, 4, 2, 2, 2]
]).

failingPuzzle2([
    [1, 4, 9, 4, 8, 6, 8, 8, 7, 7],
    [4, 8, 3, 1, 2, 10, 9, 3, 7, 5],
    [8, 9, 6, 7, 10, 7, 3, 6, 9, 1],
    [6, 7, 4, 9, 1, 2, 5, 9, 4, 10],
    [10, 5, 7, 2, 2, 3, 9, 1, 4, 4],
    [7, 5, 8, 8, 6, 9, 7, 4, 7, 2],
    [3, 6, 5, 10, 4, 6, 6, 7, 8, 9],
    [2, 9, 10, 6, 7, 4, 7, 5, 4, 6],
    [5, 3, 2, 8, 8, 1, 10, 10, 6, 4],
    [7, 5, 1, 8, 5, 8, 8, 6, 2, 1]
]).