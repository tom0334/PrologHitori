%%%%%%%%%%%%%%%%%%%%%
%Utils
%%%%%%%%%%%%%5


stripValue((X,Y,_), (X,Y)).

convertToValueOnlyPositions(PositionsWithValue, Result) :-
    maplist(stripValue, PositionsWithValue, PositionsWithoutValue),
    list_to_ord_set(PositionsWithoutValue, Result).



getRow(Board, X, Row):- nth0(X,Board,Row).

%get from board at X,Y. Top Left is 0,0.
%WARNING: This is slow!
elementAt(Board, X,Y,Element):-
    getRow(Board,Y,Row), 
    % row is now the entire row, get the element from that:
    nth0(X,Row,Element).


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


