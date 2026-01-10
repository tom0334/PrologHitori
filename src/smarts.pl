
canIgnoreBecauseIsKnownWhite(N, AllDuplicatePositionsInRow, (X,Y,V)):-
	leftNeighbour(N,(X,Y,V), (LNX,LNY,LNV)),
	rightNeighbour(N,(X,Y,V), (RNX,RNY,RNV)),
	member((LNX, LNY, LNV), AllDuplicatePositionsInRow),
	member((RNX, RNY, RNV), AllDuplicatePositionsInRow),
	RNV = LNV,
	!.

leftNeighbour( _N, (MeX, MeY, _MeV), (LNX,LNY, _LNV) ):-
	LNX is MeX -1,
	LNY is MeY,
	LNX >= 0.

rightNeighbour(N, (MeX, MeY, _MeV), (RNX,RNY, _RNV) ):-
	RNX is MeX +1,
	RNY is MeY,
	RNX < N.


filterKnownWhitesRow(N, RowDuplicateList, Excluded):-
	exclude(canIgnoreBecauseIsKnownWhite(N, RowDuplicateList), RowDuplicateList, Excluded).

