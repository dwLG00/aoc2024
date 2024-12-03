:- module(day3, [read_file/2, match/2, do_dont_split/2, do_dont_split2/2, do_dont/2, concat/2, take_alternating/2, matchtest/2]).
:- initialization(main, main).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(list_util)).
:- use_module(library(pcre)).

% IO and part 1

read_file(File, Str) :-
    open(File, read, Stream),
    read_string(Stream, _, Str).

match(Str, N) :- re_foldl(match_unravel, "mul\\((?<N1>[0-9]{1,3}),(?<N2>[0-9]{1,3})\\)", Str, 0, N, []).

match_unravel(Dict, A, B) :-
    Dict = re_match{0: _, 'N1': N1, 'N2': N2},
    number_string(N1Int, N1),
    number_string(N2Int, N2),
    B #= A + N1Int * N2Int.

% part 2
% I'm ashamed to admit that I cheated.
% Prolog's pcre doesn't have a built-in "replace-all" regex.
% And I couldn't figure out how to manually implement replace-all.
% Below are my ill-fated attempts to do such a thing. They have all ended in failure.
% At the end, I decided to just use the regex expression that I knew was correct,
% and just run replace_all using a regex implementation I found online,
% then run match/2 on that output ("day3.manual").
% This method worked. However, it feels like it goes against the spirit
% to use an external tool instead of writing the code myself.

% Update as of 12/03/2024: I've figured out the bug.
% It turns out that newlines were not being matched by .*
% by default, and I needed to prepend my regex string
% by "(?s)". Whoops

concat([], "").
concat([X], X).
concat([X|Xs], Str) :-
    concat(Xs, Str2),
    string_concat(X, Str2, Str).

take_alternating([], []).
take_alternating([X], [X]).
take_alternating([X,_|Xs], [X," "|Ys]) :- take_alternating(Xs, Ys).

do_dont(String, ElimString) :-
    re_split("(?s)don't\\(\\)(.)*do\\(\\)"/x, String, Splits, [greedy(false)]),
    take_alternating(Splits, SplitsAlt),
    concat(SplitsAlt, ElimString).

% Graveyard
do_dont_split(String, String) :- re_replace("don't\\(\\).*do\\(\\)"/x, " ", String, String, [greedy(false)]).
do_dont_split(String, ElimString) :-    
    re_replace("don't\\(\\).*do\\(\\)"/x, " ", String, String2, [greedy(false)]),
    do_dont_split(String2, ElimString).

do_dont_split2(String, ElimString) :-
    re_replace("don't\\(\\).*do\\(\\)"/x, "\n", String, String2, [greedy(false)]), % Now we just need to take off the last don't()
    (String == String2 -> ElimString = String;
        do_dont_split(String2, ElimString)
    ).


% Main

main :- 
    read_file("day3.hidden", S),
    match(S, N),
    format("Mults: ~d\n", N),
    read_file("day3.hidden", S2),
    do_dont(S2, S3),
    match(S3, N2),
    format("Part 2 Mults: ~d\n", N2).
