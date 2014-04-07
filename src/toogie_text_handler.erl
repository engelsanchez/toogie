% @doc Handles text messages and translates them
% into toogie_player commands (join a game, play, quit, etc).

-module(toogie_text_handler).
-export([new/0,
         handle/2]).

-include("toogie_common.hrl").

new() ->
    #player_state{}.

% @doc Translates websocket messages from the client into toogie_player commands.	
% Messages: SEEK, CANCEL_SEEK, PLAY, QUIT_GAME.
handle(<<"CONNECT">>,
       #player_state{player_pid=undefined} = State) ->
    ?log("Processing CONNECT message", []),
    case toogie_player_master:connect() of
        {ok, Pid, <<PlayerId:36/binary>>} when is_pid(Pid) ->
            {reply,
             <<"WELCOME ", PlayerId/binary>>,
             State#player_state{player_pid=Pid}};
		R ->
			?log("Bad reply ~w", [R]),	
			{reply, <<"INTERNAL_ERROR">>, State}
	end;
handle(<<"CONNECT AS ", PlayerId:36/binary>>,
       #player_state{player_pid=undefined} = State) ->
	?log("Processing CONNECT AS message", []),
	case toogie_player_master:connect(PlayerId) of
		{ok, Pid, <<NewPlayerId:36/binary>>} when is_pid(Pid) -> 
			?log("New player id = ~s", [NewPlayerId]),
            {reply,
             <<"WELCOME ", NewPlayerId/binary>>,
             State#player_state{player_pid=Pid}};
		_ -> 
			{reply, <<"INTERNAL_ERROR">>, State}
	end;
handle(Msg,
       #player_state{player_pid=Pid} = State)
        when is_pid(Pid) ->
	?log("Received : ~s", [Msg]),
	Reply = toogie_player:text_cmd(Pid, Msg),
	State2 = case Reply of
        ok_quit -> State#player_state{player_pid=undefined};
        _ -> State
    end,
	{reply, toogie_player:text_reply(Reply), State2};
handle(Msg, State) ->
	?log("Unexpected message ~w with state ~w", [Msg, State]),
    % TODO: reply with invalid command message maybe
	{noreply, State}.
