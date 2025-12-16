%%%%%%%%%%%%%%%%%%%%%
%Utils

loop_through_list(_File, []) :- !.
loop_through_list(File, [Head|Tail]) :-
    write(File, Head),
    nl(File),
    loop_through_list(File, Tail).

write_list_to_file(Filename,List) :-
    open(Filename, write, File),
    loop_through_list(File, List),
    close(File).