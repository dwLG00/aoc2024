:- module(day1, [read_file/3, historian_diff/3, similarity_array/3]).
:- initialization(main, main).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(list_util)).

% Before running, install list_util via :- pack_install(list_util).
% You must have `day1.hidden` in your working directory.

% Input/Output IO

read_file(File, List1, List2) :-
    open(File, read, Stream),
    read_n_lines(Stream, List1, List2, 1000).

read_n_lines(_, [], [], 0).
read_n_lines(Stream, [Int1|Rest1], [Int2|Rest2], N) :-
    N > 0,
    N1 is N - 1,
    read_line(Stream, Int1, Int2),
    length([Int1|Rest1], N),
    length([Int2|Rest2], N),
    read_n_lines(Stream, Rest1, Rest2, N1).

read_line(Stream, Int1, Int2) :-
    read_line_to_codes(Stream, Line),
    atom_codes(A, Line),
    atomic_list_concat(As, '   ', A), %3 spaces separator
    maplist(atom_number, As, [Int1, Int2]).

% Actual logic

absdiff([], [], []).
absdiff([X|Xs], [Y|Ys], [Z|Zs]) :-
    abs(X - Y) #= Z,
    absdiff(Xs, Ys, Zs).

historian_diff(List1, List2, Diffs) :-
    sort(0, @=<, List1, SortedList1),
    sort(0, @=<, List2, SortedList2),
    absdiff(SortedList1, SortedList2, Diffs).

count([], _, 0).
count([X|Xs], X, N) :- count(Xs, X, N1), N is 1 + N1.
count([X1|Xs], X, N) :- X1 \= X, count(Xs, X, N).

rep(0, _, []).
rep(N, X, [X|Xs]) :-
    N > 0,
    N1 is N - 1,
    rep(N1, X, Xs).

lt(A, B) :- A #>= B.

similarity_array(_, [], []).
similarity_array([L1|List1], List2, Arr) :- 
    span(lt(L1), List2, Prefix, Suffix),
    count(Prefix, L1, Count),
    similarity_array(List1, Suffix, SubArr),
    rep(Count, L1, Append),
    append(Append, SubArr, Arr).

% main

main :-
    read_file("day1.hidden", L1, L2),
    historian_diff(L1, L2, Diffs),
    sumlist(Diffs, DiffSum),
    format('DiffSum: ~d\n', DiffSum),
    sort(0, @=<, L1, List1),
    sort(0, @=<, L2, List2),
    similarity_array(List1, List2, SimilarityArr),
    sumlist(SimilarityArr, SimSum),
    format('Similarity: ~d\n', SimSum).
