% @doc Checkers board data structure and operations.
-module(toogie_checkers_board).
-export_type([board/0]).

-define(NUM_CELLS, 32).
-type board() :: tuple().
-type pos() :: 1..?NUM_CELLS.
-type color() :: 1 | 2.
-type cell() :: color() | 0.

-export([new/0,
         play/3,
         color/2,
         to_str/1]).

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
    B1 = setelement(P1, Board, 0),
    B2 =
    case is_simple_move(Color, P1, P2) of
        true ->
            B1;
        false ->
            case is_jump(Color, P1, P2) of
                false ->
                    {error, invalid_move};
                J ->
                    setelement(J, B1, 0)
            end
    end,
    case B2 of
        {error, invalid_move} ->
            {error, invalid_move};
        _ ->
            {ok, setelement(P2, B2, Color)}
    end;
play_one(_, _, _, _) ->
    {error, invalid_move}.

is_simple_move(1, S, E) when E == S - 4 ->
    true;
is_simple_move(1, S, E) when E == S - 3, S rem 4 /= 0 ->
    true;
is_simple_move(2, S, E) when E == S + 4 ->
    true;
is_simple_move(2, S, E) when E == S + 5, S rem 4 /= 0 ->
    true;
is_simple_move(_, _, _) ->
    false.

is_jump(1, S, E) when E == S - 7, S rem 4 /= 0 ->
    Row = (S - 1) div 4,
    S - 3 - (Row rem 2);
is_jump(1, S, E) when E == S - 9, S rem 4 /= 1 ->
    Row = (S - 1) div 4,
    S - 4 - (Row rem 2);
is_jump(2, S, E) when E == S + 9, S rem 4 /= 0 ->
    Row = (S-1) div 4,
    S + 5 - (Row rem 2);
is_jump(2, S, E) when E == S + 7, S rem 4 /= 1 ->
    Row = (S - 1) div 4,
    S + 4 - (Row rem 2);
is_jump(_, _, _) ->
    false.

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

to_str(B) ->
    to_str(1, B).

to_str(N, _) when N > 32->
    "";
to_str(N, B) when N >= 0; N < 32 ->
    [io_lib:format("    ~2..0B    ~2..0B    ~2..0B    ~2..0B\n"
                   " ~2..0B    ~2..0B    ~2..0B    ~2..0B\n",
                   [element(N, B), element(N+1, B),
                    element(N+2, B), element(N+3, B),
                    element(N+4, B), element(N+5, B),
                    element(N+6, B), element(N+7, B)])
     | to_str(N+8, B)].
