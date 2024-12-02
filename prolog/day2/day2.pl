:- module(day2, [increasing/1, decreasing/1, diff_in_range/1, is_safe/1, read_file/2, permute/2]).
:- initialization(main, main).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(list_util)).

read_file(File, N) :-
    open(File, read, Stream),
    count_safe_2(Stream, N, 1000).

% part 1
count_safe(_, 0, 0).
count_safe(Stream, Accum, Lines) :-
    Lines > 0,
    Lines1 is Lines - 1,
    line_to_list(Stream, List),
    (is_safe(List) -> (
        count_safe(Stream, Accum1, Lines1),
        Accum is Accum1 + 1
        );
        count_safe(Stream, Accum, Lines1)
    ).

line_to_list(Stream, List) :-
    read_line_to_codes(Stream, Line),
    atom_codes(A, Line),
    atomic_list_concat(As, ' ', A),
    maplist(atom_number, As, List).

is_safe(List) :- diff_in_range(List), (increasing(List); decreasing(List)).

increasing([]).
increasing([_]).
increasing([X,Y|Xs]) :- X < Y, increasing([Y|Xs]).

decreasing([]).
decreasing([_]).
decreasing([X,Y|Xs]) :- Y < X, decreasing([Y|Xs]).

diff_in_range([]).
diff_in_range([_]).
diff_in_range([X,Y|Xs]) :-
    Diff is X - Y,
    Diff \= 0,
    between(-3, 3, Diff),
    diff_in_range([Y|Xs]).

% part 2
count_safe_2(_, 0, 0).
count_safe_2(Stream, Accum, Lines) :-
    Lines > 0,
    Lines1 is Lines - 1,
    line_to_list(Stream, List),
    (is_safe_2(List) -> (
        count_safe_2(Stream, Accum1, Lines1),
        Accum is Accum1 + 1
        );
        count_safe_2(Stream, Accum, Lines1)
    ).

permute([], []).
permute(List, Sublist) :-
    append(As, [_|Bs], List),
    append(As, Bs, Sublist).

is_safe_2(List) :- is_safe(List); (permute(List, Sublist), is_safe(Sublist)).

main :-
    read_file("day2.hidden", N),
    format("Safe: ~d\n", N).
