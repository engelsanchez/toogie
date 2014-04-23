% @doc Checkers board data structure and operations.
-module(toogie_checkers_board).
-export_type([board/0]).

-define(NUM_CELLS, 32).
-type board() :: tuple().
-type pos() :: 1..?NUM_CELLS.
-type color() :: 1 | 2.
-type piece() :: 1..4.
-type cell() :: color() | 0.

-export([new/0,
         play/3,
         color/2,
         to_str/1]).

-spec new() -> board().
new() ->
    {2, 2, 2, 2,
     2, 2, 2, 2,
     2, 2, 2, 2,
     0, 0, 0, 0,
     0, 0, 0, 0,
     1, 1, 1, 1,
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
play(Color, Move, Board) ->
    play_seq(Color, Move, Board).

play_seq(Color, [P1 | [P2 | Tail] = Rest], Board) ->
    Piece = element(P1, Board),
    case Color == Piece orelse Color + 2 == Piece of
        true ->
            case play_first(Piece, P1, P2, Board) of
                {error, Err} ->
                    {error, Err};
                {simple, _} when Tail /= []->
                    % If playing many, first must be a jump
                    {error, invalid_move};
                {_, NewBoard} ->
                    play_more(Piece, Rest, NewBoard)
            end;
        false ->
            {error, invalid_move}
    end;
play_seq(_, _, _) ->
    % TODO Verify non-user inputs are valid. Otherwise should crash.
    {error, invalid_move}.

-spec play_first(piece(), pos(), pos(), board()) ->
    {simple|jump, board()} | {error, _}.
play_first(Piece, P1, P2, Board) when
        P1 >= 1, P1 =< ?NUM_CELLS, P2 >= 1, P2 =< ?NUM_CELLS,
        is_tuple(Board), size(Board) == ?NUM_CELLS,
        element(P1, Board) == Piece, element(P2, Board) == 0 ->
    % replaces element in position P2 with element in P1
    B1 = setelement(P1, Board, 0),
    case is_simple_move(Piece, P1, P2) of
        true ->
            {simple, setelement(P2, B1, Piece)};
        false ->
            case is_jump(Piece, P1, P2) of
                false ->
                    {error, invalid_move};
                J ->
                    B2 = setelement(J, B1, 0),
                    {jump, setelement(P2, B2, Piece)}
            end
    end;
play_first(_, _, _, _) ->
    {error, invalid_move}.

% @doc Playing an extra jump
-spec play_more(piece(), [pos()], board()) ->
    {ok, board()} | {error, _}.
play_more(Piece, [P1 | [P2|_]=Rest], Board) when
        P1 >= 1, P1 =< ?NUM_CELLS, P2 >= 1, P2 =< ?NUM_CELLS,
        element(P1, Board) == Piece, element(P2, Board) == 0 ->
    % replaces element in position P2 with element in P1
    B1 = setelement(P1, Board, 0),
    case is_jump(Piece, P1, P2) of
        false ->
            {error, invalid_move};
        J ->
            B2 = setelement(J, B1, 0),
            B3 = setelement(P2, B2, Piece),
            play_more(Piece, Rest, B3)
    end;
play_more(Piece, [P], Board) ->
    % If regular piece reaches the other end, crown it
    Row = (P+3) div 4, % Top = 1, Bottom = 8
    case {Piece, Row} of
        {1, 1} ->
            {ok, setelement(P, Board, 3)};
        {2, 8} ->
            {ok, setelement(P, Board, 4)};
        _ ->
            {ok, Board}
    end;
play_more(_, _, _) ->
    {error, invalid_move}.

-spec is_simple_move(piece(), pos(), pos()) -> boolean().
is_simple_move(P, S, E) when E == S - 4, P == 1 orelse P > 2 ->
    true;
is_simple_move(P, S, E) when E == S - 3, S rem 4 /= 0, P == 1 orelse P > 2 ->
    true;
is_simple_move(P, S, E) when E == S + 4, P >= 2 ->
    true;
is_simple_move(P, S, E) when E == S + 5, S rem 4 /= 0, P >= 2 ->
    true;
is_simple_move(_, _, _) ->
    false.

-spec is_jump(piece(), pos(), pos()) -> false | pos().
is_jump(P, S, E) when E == S - 7, S rem 4 /= 0, P == 1 orelse P > 2 ->
    Row = (S - 1) div 4,
    S - 3 - (Row rem 2);
is_jump(P, S, E) when E == S - 9, S rem 4 /= 1, P == 1 orelse P > 2 ->
    Row = (S - 1) div 4,
    S - 4 - (Row rem 2);
is_jump(P, S, E) when E == S + 9, S rem 4 /= 0, P >= 2 ->
    Row = (S - 1) div 4,
    S + 5 - (Row rem 2);
is_jump(P, S, E) when E == S + 7, S rem 4 /= 1, P >= 2 ->
    Row = (S - 1) div 4,
    S + 4 - (Row rem 2);
is_jump(_, _, _) ->
    false.


-spec color(pos(), board()) -> cell().
color(P, Board) ->
    element(P, Board).

-spec to_str(board()) -> iolist().
to_str(B) ->
    to_str(1, B).

-spec to_str(pos(), board()) -> iolist().
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
