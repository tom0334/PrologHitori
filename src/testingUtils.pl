%Testing and writing to file utilities.

%Finds a solution to a puzzle, and writes it to a file in the .singlesSol format
isSolutionAndWrite(Board,N,Seed, OutputFile, Verbose, Solution):-
    (Verbose = 1, writeln("PROLOG: Solving..."); true),
    call_time(
        isSolutionZerodPositions(Board,Solution),
        TimeDict
    ),
    modelVersion(Version),
    string_concat("Solved by: v", Version, Comment),
    translateToBoardWithBlackLetter(Board, Solution, SolutionInGenericFormat), 
    writeSolution(OutputFile, N,Seed, Comment, SolutionInGenericFormat, TimeDict),

    %Print some info to the console so you can see live what is happening:
    (Verbose = 1,
        write("Solved! Took "),
        write(TimeDict.cpu),
        write(" seconds, "),
        write(TimeDict.inferences),
        writeln(" inferences"),
        maplist(writeln, SolutionInGenericFormat),
        writeln(""),
        writeln(Solution);
        true
    ).



%Writing board with chosen zeros to solution output file
%%%%%%%%%%%%
translateToBoardWithBlackLetter(Board, Zerod, SolutionInGenericFormat):-
    sameShape(Board, SolutionInGenericFormat),
    allPositionsWithValue(Board, Positions),
    maplist(translateToBoardValueOrBoardBlack(SolutionInGenericFormat, Zerod), Positions).

translateToBoardValueOrBoardBlack(Solution,Zerod,(X,Y,V)):-
    elementAt(Solution, X, Y, SolutionValue),
    boardValueOrBlackedBoardValue((X,Y,V),Zerod, SolutionValue).

boardValueOrBlackedBoardValue((X,Y,V), Zerod, Res) :- ord_memberchk((X,Y), Zerod), string_concat(V,"B", Res), !.

boardValueOrBlackedBoardValue((_,_,V), _, V).



writeSolution(Filename, N, Seed, Comment, SolutionInGenericFormat, TimeDict) :-
    open(Filename, write, File),
    writeln(File, N),
    writeln(File, ""),
    writeMatrix(File, SolutionInGenericFormat),
    writeln(File, ""),
    writeln(File, Seed),
    writeln(File, Comment),
    write(File, "#prologTime="),
    writeln(File, TimeDict.cpu),
    write(File, "#prologInferences="),
    writeln(File, TimeDict.inferences),
    close(File).


writeMatrix(File, Matrix):-
    maplist( writeRow(File), Matrix).

%Write the elements (1W, 2B, 3W...etc to a file),
%with a space as seperator between elements and a \n at the end of the row 
writeRow(File, [Head | Tail]):- 
    write(File, Head),
    writeSeperatorAndContinue(File, Tail). 

writeSeperatorAndContinue(File, []):-
    writeln(File, ""), !.

writeSeperatorAndContinue(File, NonEmptyList):-
    write(File, " "),
    writeRow(File, NonEmptyList). 


