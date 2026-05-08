%Applies all RCs to the countmaps, duplicate lists, and also returns which tiles can be preMarked (known black)
applyRCsToCountMapsAndDupLists(N, AllCountMaps, AllDuplicateLists, AllCountMapsWithRC, AllDuplicateListsWithRC, PreMarked):-
    redundantConstraints(N,AllCountMaps,AllDuplicateLists,[],[],KnownWhiteWithValues,KnownBlackWithValues),
    writeln("Known white:"),
    writeln(KnownWhiteWithValues),
    writeln("Known black:"),
    writeln(KnownBlackWithValues),

    %A tile may be marked black by multiple RCs, filter out those duplicates:
    list_to_ord_set(KnownBlackWithValues, KnownBlackWithValuesSet),

    %change the lists of countmaps and the lists of duplists to a list of (CountList, DupList)
    %(So that we can use maplist on the pair)
    zipped(AllCountMaps, AllDuplicateLists, Pairs),
    maplist(updateCMAndDupList(KnownBlackWithValuesSet), Pairs, ResultPairs),
    zipped( AllCountMapsWithRC, AllUpdatedDuplicateLists, ResultPairs),

    maplist(removeThisFromList(KnownBlackWithValuesSet), AllUpdatedDuplicateLists, AllDuplicateListsWithoutMarked),
    maplist(removeThisFromList(KnownWhiteWithValues), AllDuplicateListsWithoutMarked, AllDuplicateListsWithRC),
    %writeln("Updated count maps and dup lists"),
    
    %Remove the values for black tiles. This may now contain duplicates again, so remove them again.
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
    sandwichPair(N, CountMap, DuplicateList, KnownWhiteSP),
    sandwichTriple(N, DuplicateList, KnownWhiteSP, KnownBlackST),
    pairIsolation(N, CountMap, DuplicateList, KnownBlackPI),
    
    %write("Pair isolation res:"),
    %writeln(KnownBlackPI),
    %write("sandwichPair result (known white): "),
    %writeln(KnownWhiteSP),
    %write("sandwichTriple result (known black): "),
    %writeln(KnownBlackST),

    append(KnownBlackPI, KnownBlackST, AllKnownBlack),
    append(KnownBlackSoFar, AllKnownBlack, KnownBlackRes),
    append(KnownWhiteSofar, KnownWhiteSP, KnownWhiteRes),
    !.

%Updates the pair of a countmap and a duplist for a row or column, according to now known black values
% Does so by updating the counts in the countmap, 
%and then removing the tiles that are now no longer duplicates from the duplist.
updateCMAndDupList(KnownBlackWithValuesSet, (CountMap,DupList), (ResCountMap, ResDupList) ):-
    updateCM(KnownBlackWithValuesSet, DupList,CountMap, ResCountMap),
    findall(Pos, noLongerNeedsToBeconsidered(ResCountMap, DupList, Pos), PositionsToBeRemoved),
    %write("Removing: "),
    %writeln(PositionsToBeRemoved),
    subtract(DupList, PositionsToBeRemoved, ResDupList).

%If 
noLongerNeedsToBeconsidered(CountMap, DupList, (X,Y,V)):-
    member((X,Y,V), DupList ),
    get_dict(V, CountMap, CountLeft),
    CountLeft < 2.


updateCM([],_, ResCountMap, ResCountMap).

updateCM([HKnownBlack | TKnownBlack], DupList, Countmap, ResCountMap):-
    member(HKnownBlack, DupList),
    %write("Decrementing "),
    %write(HKnownBlack),
    %write(" in "),
    %writeln(DupList),
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

%Same as a zip function in any functional programming language.
%but enforces the lists to be the same length!
zipped([], [], []).
zipped([X|Xs], [Y|Ys], [(X,Y)|Zs]) :-
    zipped(Xs, Ys, Zs).