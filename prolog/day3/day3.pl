:- module(day3, [read_file/2, match/2, do_dont_split/2, do_dont_split2/2, do_dont/2, concat/2, take_alternating/2]).
:- initialization(main, main).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(list_util)).
:- use_module(library(pcre)).

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
% I will most likely come back to this code and eventually get a working version that
% gives me the correct answer.

concat([], "").
concat([X], X).
concat([X|Xs], Str) :-
    concat(Xs, Str2),
    string_concat(X, Str2, Str).

take_alternating([], []).
take_alternating([X], [X]).
take_alternating([X,_|Xs], [X," "|Ys]) :- take_alternating(Xs, Ys).

do_dont(String, ElimString) :-
    re_split("don't\\(\\)((?!do\\(\\)).)*do\\(\\)"/x, String, Splits, [greedy(true)]),
    take_alternating(Splits, SplitsAlt),
    concat(SplitsAlt, ElimString).

do_dont_split(String, String) :- re_replace("don't\\(\\)((?!do\\(\\)).)*do\\(\\)"/x, " ", String, String, [greedy(true)]).
do_dont_split(String, ElimString) :-    
    re_replace("don't\\(\\)((?!do\\(\\)).)*do\\(\\)"/x, " ", String, String2, [greedy(true)]),
    do_dont_split(String2, ElimString).

do_dont_split2(String, ElimString) :-
    re_replace("don't\\(\\).*do\\(\\)"/x, "\n", String, String2, [greedy(false)]), % Now we just need to take off the last don't()
    (String == String2 -> ElimString = String;
        do_dont_split(String2, ElimString)
    ).


main :- 
    read_file("day3.hidden", S),
    match(S, N),
    format("Mults: ~d\n", N).
    %read_file("day2.hidden", N),
    %format("Safe: ~d\n", N).
