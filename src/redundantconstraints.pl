%Applies all RCs to the countmaps, duplicate lists, and also returns which tiles can be preMarked (known black (without values))
applyRCsToCountMapsAndDupLists(N, AllCountMaps, AllDuplicateLists, AllCountMapsWithRC, AllDuplicateListsWithRC, PreMarked):-
    redundantConstraints(N,AllCountMaps,AllDuplicateLists,[],[],KnownWhiteWithoutValues,KnownBlackWithValues),
    writeln("Known white:"),
    writeln(KnownWhiteWithoutValues),
    writeln("Known black:"),
    writeln(KnownBlackWithValues),

    %A tile may be marked black by multiple RCs, filter out those duplicates
    list_to_ord_set(KnownBlackWithValues, KnownBlackWithValuesSet),

    %Now we will update the countmaps and duplicate lists to account for the newly known black tiles.
    %Change the lists of countmaps and the lists of duplists to a list of (CountList, DupList)
    %So that we can use maplist on the pair
    zipped(AllCountMaps, AllDuplicateLists, Pairs),
    maplist(updateCMAndDupList(KnownBlackWithValuesSet), Pairs, ResultPairs),
    zipped( AllCountMapsWithRC, AllUpdatedDuplicateLists, ResultPairs),

    %Remove the known white tiles from the duplists, since we know that we never have to consider them to be marked.
    maplist(removeThisFromListNoValues(KnownWhiteWithoutValues), AllUpdatedDuplicateLists, AllDuplicateListsWithRC),
    
    %Remove the values for black tiles. This may now contain duplicates again, so we have remove them again.
    maplist(stripValue,KnownBlackWithValuesSet, KnownBlackWithoutValues),

    %After removing those duplicates, we know the premarked
    list_to_ord_set(KnownBlackWithoutValues, PreMarked).


%Base case, unify the bag with the result
redundantConstraints(_N, [], [], KnownWhiteBag, KnownBlackBag, KnownWhiteBag, KnownBlackBag).

%Find known values because of redundant constraints in every countmap and duplicatelist,  Store results in two bags.
redundantConstraints(N, [HCountMap| TCountMaps] , [HDuplicateList | TDuplicateLists], KnownWhiteBag, KnownBlackBag, KnownWhiteRes, KnownBlackRes):-
    redundantConstraintsForRowOrColumn(N, HCountMap, HDuplicateList, KnownWhiteBag, KnownBlackBag, NewKnownWhiteBag, NewKnownBlackBag),
    redundantConstraints(N, TCountMaps, TDuplicateLists, NewKnownWhiteBag, NewKnownBlackBag, KnownWhiteRes, KnownBlackRes).

%Find all known values because of RCS on one row/column.
redundantConstraintsForRowOrColumn(N, CountMap, DuplicateList, KnownWhiteSofar, KnownBlackSoFar, KnownWhiteRes, KnownBlackRes):-
    sandwichPair(N, CountMap, DuplicateList, KnownWhiteSPWithValues, KnownWhiteSP),
    sandwichTriple(N, DuplicateList, KnownWhiteSPWithValues, KnownBlackST, KnownWhiteST),
    pairIsolation(N, CountMap, DuplicateList, KnownBlackPI, KnownWhitePI),
    
    %write("Pair isolation res (black):"),
    %writeln(KnownBlackPI),
    %write("white:"),
    %writeln(KnownWhitePI),
    %write("sandwichPair result (white): "),
    %writeln(KnownWhiteSP),
    %write("sandwichTriple result (black): "),
    %writeln(KnownBlackST),
    %write("sandwichTriple result (white): "),
    %writeln(KnownWhiteST),

    flatten([KnownBlackSoFar, KnownBlackPI, KnownBlackST ], KnownBlackRes),
    flatten([KnownWhiteSofar, KnownWhitePI, KnownWhiteST, KnownWhiteSP], KnownWhiteRes),
    !.

%Updates the pair of a countmap and a duplist for a row or column, according to now known black values
% Does so by updating the counts in the countmap, 
%and then removing the tiles that are now no longer duplicates from the duplist.
updateCMAndDupList(KnownBlackWithValuesSet, (CountMap,DupList), (ResCountMap, ResDupList) ):-
    updateCM(KnownBlackWithValuesSet, DupList,CountMap, ResCountMap),
    
    %remove all known black tiles from the duplist
    subtract(DupList, KnownBlackWithValuesSet, DupListWithoutMarked),
    
    %Remove all that are now no longer duplicates from the duplist
    findall(Pos, noLongerNeedsToBeconsidered(ResCountMap, DupList, Pos), PositionsToBeRemoved),
    subtract(DupListWithoutMarked, PositionsToBeRemoved, ResDupList).

%A tile no longer needs to be considered to be marked ifs no longer a duplicate in the row/column
noLongerNeedsToBeconsidered(CountMap, DupList, (X,Y,V)):-
    member((X,Y,V), DupList ),
    get_dict(V, CountMap, CountLeft),
    CountLeft < 2.

%Base case. This predicate updates a countMap to account for tiles that we determined have to be black.
updateCM([],_, ResCountMap, ResCountMap).

updateCM([HKnownBlack | TKnownBlack], DupList, Countmap, ResCountMap):-
    member(HKnownBlack, DupList),
    decrementValueInCountmap(Countmap, HKnownBlack, NewCountMap),
    updateCM(TKnownBlack, DupList, NewCountMap, ResCountMap),
    !.

updateCM([_HKnownBlack | TKnownBlack], DupList, CountMap, ResCountMap):-
    updateCM(TKnownBlack, DupList, CountMap, ResCountMap),
    !.

decrementValueInCountmap(CountMap, (_X,_Y,V), NewCountMap):-
    get_dict(V, CountMap, CountLeft),
    NewCount is CountLeft - 1, 
    put_dict(V, CountMap, NewCount, NewCountMap).


%different order than regular subtract, useful for when using it with maplist
removeThisFromList(ToRemove, From, Result):-
    subtract(From, ToRemove, Result).

removeThisFromListNoValues([], Result ,Result).

%Removes positions without value from a lists with positions with value
% Example: remove (1,1) from  [(1,1,1), (5,5,5)] becomes [(5,5,5)] 
removeThisFromListNoValues( [ PosXY | TRemove ], From, Result):-
    exclude( equalIgnoringValue(PosXY) , From, PartialResult),
    removeThisFromListNoValues(TRemove, PartialResult, Result ).

equalIgnoringValue( (X, Y), (X,Y,_)).

