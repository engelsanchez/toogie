% @doc Handles checkers games.
-module(toogie_checkers_game).
-behaviour(toogie_game).
-export([new/1,
         play/3,
         state_string/1,
         is_valid_seek/1]).

-record(checkers_state, {board :: toogie_checkers_board:board()}).

-include("toogie_common.hrl").

-define(num(D1, D2), ((D1 - $0) * 10 + D2 - $0)).
-define(num(D), (D - $0)).

new(<<>>) ->
    #checkers_state{board=toogie_checkers_board:new()};
new(_) ->
    invalid.

is_valid_seek(<<>>) ->
    true;
is_valid_seek(_) ->
    false.

state_string(#checkers_state{board=Board}) ->
    list_to_binary(io_lib:format("~w", [Board])).

play(Player, MoveStr, State) ->
    case parse_move(MoveStr) of
        {ok, Move} ->
            do_play(Player, Move, State);
        {error, _} ->
            ?log("Invalid move received ~s", [MoveStr]),
            {error, invalid_move}
    end.

parse_move(Str) ->
    parse_move(Str, []).

parse_move(<<>>, [_, _ | _]=L) ->
    {ok, lists:reverse(L)};
parse_move(<<D1, D2, "-", Rest/binary>>, L)
        when D1 >= $1, D1 =< $9, D2 >= $0, D2 =< $9,
        ?num(D1, D2) >= 1, ?num(D1, D2) =< 32 ->
    parse_move(Rest, [?num(D1, D2) | L]);
parse_move(<<D, "-", Rest/binary>>, L)
        when D >= $1, D =< $9 ->
    parse_move(Rest, [?num(D) | L]);
parse_move(<<D1, D2>>, L)
        when D1 >= $1, D1 =< $9, D2 >= $0, D2 =< $9,
        ?num(D1, D2) >= 1, ?num(D1, D2) =< 32 ->
    parse_move(<<>>, [?num(D1, D2) | L]);
parse_move(<<D>>, L)
        when D >= $1, D =< $9 ->
    parse_move(<<>>, [?num(D) | L]);
parse_move(_, _) ->
    {error, invalid_move}.

do_play(Player, Move, State=#checkers_state{board=Board})
        when is_list(Move) ->
    case toogie_checkers_board:play(Player, Move, Board) of
        {ok, NewBoard} ->
            ?log("Played ~p : ~p\n", [Move, NewBoard]),
            {ok, State#checkers_state{board=NewBoard}};
        {error, _} ->
            {error, invalid_move}
    end.
