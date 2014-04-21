% @doc Checkers board data structure and operations.
-module(toogie_checkers_board).
-export_type([board/0]).

-define(NUM_CELLS, 32).
-type board() :: tuple().
-type pos() :: 1..?NUM_CELLS.
-type color() :: 1 | 2.
-type cell() :: color() | 0.

-export([new/0, play/3, color/2]).

-spec new() -> board().
new() ->
    {2, 2, 2, 2,
     2, 2, 2, 2,
     0, 0, 0, 0,
     0, 0, 0, 0,
     0, 0, 0, 0,
     0, 0, 0, 0,
     1, 1, 1, 1,
     1, 1, 1, 1}.
     
%     01    02    03    04
%  05    06    07    08   
%     09    10    11    12
%  13    14    15    16
%     17    18    19    20
%  21    22    23    24
%     25    26    27    28
%  29    30    31    32              

-spec play(color(), [pos()], board()) -> {ok, board()} | {error, _}.
play(Color, [_, _ | _] = Move, Board) ->
    do_play(Color, Move, Board);
play(_, _, _) ->
    {error, invalid_move}.

-spec play_one(color(), pos(), pos(), board()) -> {ok|win, board()} | {error, _}.
play_one(Color, P1, P2, Board) when
        P1 >= 1, P1 =< ?NUM_CELLS, P2 >= 1, P2 =< ?NUM_CELLS,
        is_tuple(Board), size(Board) == ?NUM_CELLS,
        element(P1, Board) == Color, element(P2, Board) == 0 ->
    % replaces element in position P2 with element in P1
    % TODO remove any intermediate piece jumped over
    B1 = setelement(P1, Board, 0),
    {ok, setelement(P2, B1, Color)};
play_one(_, _, _, _) ->
    {error, invalid_move}.

-spec do_play(color(), [pos()], board()) -> {ok, board()} | {error, _}.
do_play(_, [_], Board) ->
    {ok, Board};
do_play(Color, [P1 | [P2 | _] = Rest], Board) ->
    case play_one(Color, P1, P2, Board) of
        {ok, NewBoard} ->
            do_play(Color, Rest, NewBoard);
        {error, Err} ->
            {error, Err}
    end.

-spec color(pos(), board()) -> cell().
color(P, Board) ->
    element(P, Board).
