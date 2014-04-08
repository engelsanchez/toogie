% @doc Handles 4-inline game commands and seeks.
-module(toogie_c4_game).
-export([new/1,
         play/3,
         state_string/1,
         is_valid_seek/1]).

-record(c4_game_state, {
        board,
        game_var}).

-include("toogie_common.hrl").
-include("c4_board.hrl").

new(SeekStr) ->
    {BoardSpec, GameVar} = parse_seek(SeekStr),
    Board = c4_board:new(BoardSpec),
    #c4_game_state{board=Board, game_var=GameVar}.

play(Player, {drop, Col}, Board) ->
	?log("Player ~w played ~w~n", [Player, Col]),
	case c4_board:add_piece(Board, Player, Col) of
		{ok, NewBoard, Row} ->
			?log("Board ~w~n", [NewBoard]),
			case c4_board:check_win(NewBoard, Row, Col) of
				true ->
                    {win, NewBoard};
				false -> 
					case c4_board:is_full(NewBoard) of
						false ->
                            {ok, NewBoard};
						true ->
                            {draw, NewBoard}
					end
			end;
		invalid_move -> 
            invalid_move
	end;
play(Player, MoveStr, Board) ->
    case parse_move(MoveStr) of
        invalid_move ->
            invalid_move;
        Move ->
            play(Player, Move, Board)
    end.

state_string(Board) ->
    list_to_binary(io_lib:format("~w", [Board])).

% @doc Reads board dimensions from text (07x06,08x07,etc).
parse_board_size(<<W1:8/integer, W2:8/integer,
                   "x",
                   H1:8/integer, H2:8/integer>>) 
        when
        W1 >= $0, W1 =< $9,
        W2 >= $0, W2 =< $9,
        H1 >= $0, H1 =< $9,
        H2 >= $0, H2 =< $9 ->
    #board_size{rows= (H1-$0)*10+(H2-$0), cols=(W1-$0)*10+(W2-$0)};
parse_board_size(_) -> undefined.

parse_seek(<<GVar:3/binary, " ", BSpec:5/binary>>) ->
    BoardSpec = parse_board_size(BSpec),
    GameVar = case GVar of
        <<"STD">> -> std;
        <<"POP">> -> pop;
        _ -> undefined
    end,
    {BoardSpec, GameVar}.

% validate seek strings
is_valid_seek(SeekStr) ->
    case parse_seek(SeekStr) of
        {undefined, _} -> false;
        {_, undefined} -> false;
        _ -> true
    end.

parse_move(<<"DROP ", ColStr/binary>>) ->
    case (catch binary_to_integer(ColStr)) of
        badarg ->
            invalid_move;
        IntVal ->
            {drop, IntVal}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_board_size_test() ->
    ?assertEqual(#board_size{cols=7,rows=6}, parse_board_size(<<"07x06">>)),
    ?assertEqual(#board_size{cols=8,rows=7}, parse_board_size(<<"08x07">>)),
    ?assertEqual(#board_size{cols=9,rows=7}, parse_board_size(<<"09x07">>)),
    ?assertEqual(#board_size{cols=10,rows=7}, parse_board_size(<<"10x07">>)).

-endif.
