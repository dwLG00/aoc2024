:- module(day4, [is_xmas/4, count_matches/2, get_column/3, columns/2, grid/2, count_matches_2d/2, into_grid/3, read_file/2, diagonals/2, test/2, count_xmas/2]).
:- initialization(main, main).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(list_util)).
:- use_module(library(pcre)).

% IO and part 1

read_file(File, Grid) :-
    open(File, read, Stream),
    into_grid(Stream, Grid, 140).

into_grid(_, [], 0).
into_grid(Stream, [Row|Grid], N) :-
    N #> 0,
    N1 #= N - 1,
    read_line_to_codes(Stream, Line),
    maplist(char_code, Row, Line),
    into_grid(Stream, Grid, N1).

% part 1 logic

% count all matches
count_matches_2d([], 0).
count_matches_2d([Row|Rows], N) :-
    count_matches(Row, N1),
    count_matches_2d(Rows, N2),
    N #= N1 + N2.

% From a 2d array Xs, construct a list of all rows, columns, and diagonals ("Grid")
grid(Xs, Grid) :-
    columns(Xs, Columns),
    diagonals(Xs, Diags),
    append(Xs, Columns, Interm),
    append(Interm, Diags, Grid).

% Compute diagonals of grid
diagonals(Xs, Diags) :-
    length(Xs, Length),
    stagger(Xs, Diags1, Length, Length),
    reverse(Xs, XRevs),
    stagger(XRevs, Diags2, Length, Length),
    columns(Diags1, Cols1),
    columns(Diags2, Cols2),
    append(Cols1, Cols2, Diags).

stagger(_, [], 0, _).
stagger([X|Xs], [Diag|Diags], N, Tot) :-
    N #> 0,
    N1 #= N - 1,
    M #= Tot - N,
    pad(X, N, M, Diag),
    stagger(Xs, Diags, N1, Tot).

rep(_, 0, []).
rep(X, N, [X|Xs]) :-
    N #> 0,
    N1 #= N - 1,
    rep(X, N1, Xs).

pad(Xs, N, M, Padded) :-
    rep('.', N, LeftPad),
    rep('.', M, RightPad),
    append(LeftPad, Xs, Inter),
    append(Inter, RightPad, Padded).

% Compute columns of grid
columns([], []).
columns([X|Xs], Columns) :-
    length(X, N),
    columns_helper([X|Xs], Columns, N).

columns_helper(_, [], 0).
columns_helper(Xs, [Col|Columns], N) :-
    N #> 0,
    N1 #= N - 1,
    get_column(Xs, N1, Col),
    columns_helper(Xs, Columns, N1).

get_column(Xs, N, Column) :- maplist(nth(N), Xs, Column).

nth(0, [X|_], X).
nth(N, [_|Xs], X) :-
    N #> 0,
    N1 #= N - 1,
    nth(N1, Xs, X).

% Count matches of "XMAS"
count_matches(Xs, 0) :- length(Xs, N), between(0, 3, N).
count_matches([X1, X2, X3, X4], 1) :- is_xmas(X1, X2, X3, X4).
count_matches([X1, X2, X3, X4], 0) :- \+ is_xmas(X1, X2, X3, X4).
count_matches([X1, X2, X3, X4 | Xs], N) :-
    (is_xmas(X1, X2, X3, X4) -> (
        N1 #= N - 1,
        count_matches([X2,X3,X4|Xs], N1)
        );
        count_matches([X2,X3,X4|Xs], N)
    ).

is_xmas('X', 'M', 'A', 'S').
is_xmas('S', 'A', 'M', 'X').

% part 2

count_xmas(Xs, N) :-
    grab_three(Xs, TripleRows),
    triple_rows_count_xmas(TripleRows, N).

% For each tuple of 3 rows, compute number of xmas'es and sum
triple_rows_count_xmas([], 0).
triple_rows_count_xmas([[R1, R2, R3]|Rest], N) :-
    rows_count_xmas(R1, R2, R3, Count),
    N #= Count + N1,
    triple_rows_count_xmas(Rest, N1).

% sliding window of 3
grab_three(Xs, []) :- length(Xs, N), between(0, 2, N).
grab_three([A, B, C|Xs], [[A, B, C]|Ys]) :-
    grab_three([B, C|Xs], Ys).

% given tuple of 3 rows, compute # of xmas'es on that "row"
rows_count_xmas(Row1, Row2, Row3, 0) :- length(Row1, N), length(Row2, N), length(Row3, N), between(0, 2, N).
rows_count_xmas([A, B, C|Row1], [_, E, F|Row2], [G, H, I|Row3], N) :-
    ((x_mas(A, C, E, G, I); x_mas(I, G, E, C, A)) -> (
        N #> 0,
        N1 #= N - 1,
        rows_count_xmas([B, C|Row1], [E, F|Row2], [H, I|Row3], N1)
        );
        rows_count_xmas([B, C|Row1], [E, F|Row2], [H, I|Row3], N)
    ).

% check for x-mas. the other two cases are transposition of the others
x_mas('M', 'S', 'A', 'M', 'S').
x_mas('M', 'M', 'A', 'S', 'S').
%x_mas(['M', _, 'S'], [_, 'A', _], ['M', _, 'S']).
%x_mas(['M', _, 'M'], [_, 'A', _], ['S', _, 'S']).

% Main

main :- 
    read_file("day4.hidden", Xs),
    grid(Xs, Grid),
    count_matches_2d(Grid, N),
    format("part 1 matches: ~d\n", N),
    count_xmas(Xs, N2),
    format("part 2 matches: ~d\n", N2).
