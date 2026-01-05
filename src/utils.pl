%%%%%%%%%%%%%%%%%%%%%
%Utils
%%%%%%%%%%%%%5

%For translating a set of chosen duplicates to a board (list of rows).
%%%%%%%%%%%%
translateToBoard(Board, Zerod, Solution):-
    sameShape(Board, Solution),
    allPositionsWithValue(Board, Positions),
    maplist(translatePositionToBoardValue(Solution, Zerod), Positions).

translatePositionToBoardValue(Solution,Zerod,(X,Y,V)):-
    elementAt(Solution, X, Y, SolutionValue),
    boardValueOrZerod((X,Y,V),Zerod, SolutionValue).


boardValueOrZerod((X,Y,_), Zerod, 0) :- ord_memberchk((X,Y), Zerod), !.
boardValueOrZerod((_,_,V), _, V).

sameShape([], []).
sameShape([A|As], [B|Bs]) :-
    same_length(A, B),
    sameShape(As, Bs).


