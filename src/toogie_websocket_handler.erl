% @doc Handles websocket text messages and translates them
% into toogie_player commands (join a game, play, quit, etc).

-module(toogie_websocket_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-include("toogie_common.hrl").

-record(state, {
        player_state :: #player_state{}
        }).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

% @doc When the websocket is created, a child toogie_player process is spawned to
% which we'll forward the user requests.
-spec(websocket_init(term(), term(), term()) -> {ok, term(), #state{}} ).
websocket_init(_Any, Req, []) ->
    ?log("Starting ws connection", []),
    {ok, Req, #state{player_state=toogie_text_handler:new()}}.

websocket_handle({text, Msg}, Req, State = #state{player_state=PlayerState}) ->
    ?log("Got message ~s", [Msg]),
    case toogie_text_handler:handle(Msg, PlayerState) of
        {reply, Reply, NewPlayerState} ->
            ?log("Sending message ~s", [Reply]),
            {reply, {text, Reply}, Req, State#state{player_state=NewPlayerState}};
        {noreply, NewPlayerState} ->
            {noreply, Req, State#state{player_state=NewPlayerState}}
    end.

% @doc Handles messages sent from toogie_player process and replies to
% websocket client.
% TODO: type player messages to distinguish from others
websocket_info(Event, Req, State) ->
    {reply, {text, toogie_player:text_reply(Event)}, Req, State}.

% @doc It terminates the child toogie_player process when the websocket is closed.
websocket_terminate(_Reason, _Req, #state{player_state=#player_state{player_pid=PlayerPid}}) 
        when is_pid(PlayerPid) ->
	?log("Websocket connection closing, notifying player process of disconnect",[]),
    toogie_player:disconnected(PlayerPid),
	ok;
websocket_terminate(_Reason, _Req, _State) ->
	?log("Websocket connection closing without a player process",[]),
	ok.
