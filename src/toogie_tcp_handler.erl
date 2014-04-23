% @doc Handles game commands sent over raw TCP connections.
% This is useful for testing with telnet.
-module(toogie_tcp_handler).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("toogie_common.hrl").
%% ranch_protocol API.
-export([start_link/4]).

%% gen_server API.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([init/4]).

-record(state, {socket,
                transport,
                player_state :: #player_state{},
                partial= <<>> :: string()}).

%% API.
%% Used by Ranch to start connection handler.
start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% gen_server.

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    PlayerState = toogie_text_handler:new(),
    gen_server:enter_loop(?MODULE, [],
                          #state{socket=Socket,
                                 transport=Transport,
                                 player_state=PlayerState}).

do_cmd(Cmd, State=#state{socket=Socket,
                         transport=Transport,
                         player_state=PlayerState}) ->
    ?log("Got ~p\n", [Cmd]),
    case toogie_text_handler:handle(to_upper(Cmd), PlayerState) of
        {reply, Reply, NewPlayerState} ->
            Transport:send(Socket, <<Reply/binary, "\r\n">>),
            State#state{player_state=NewPlayerState};
        {noreply, NewPlayerState} ->
            State#state{player_state=NewPlayerState}
    end.

-spec to_upper(binary()) -> binary().
to_upper(Bin) ->
    << <<(if C >= $a,C=<$z -> C + $A-$a; true -> C end):8>> || <<C:8>> <= Bin >>.

do_cmds(Text, State) ->
    case binary:split(Text, [<<"\r">>, <<"\n">>, <<"\r\n">>]) of
        [Partial] ->
            State#state{partial=Partial};
        [Cmd, Partial] ->
            State2 = do_cmd(Cmd, State),
            do_cmds(Partial, State2)
    end.

handle_info({tcp, Socket, Data},
            State=#state{socket=Socket,
                         transport=Transport,
                         partial=Partial}) ->
    Transport:setopts(Socket, [{active, once}]),
    ?log("Plain doing ~p and ~p", [Partial, Data]),
    State2 = do_cmds(<<Partial/binary, Data/binary>>, State),
    {noreply, State2};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(Msg, State=#state{socket=Socket, transport=Transport}) ->
    Msg2 = toogie_player:text_reply(Msg),
    Transport:send(Socket, <<Msg2/binary, "\r\n" >>),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{
                player_state=#player_state{player_pid=PlayerPid}})
       when is_pid(PlayerPid) ->
    ?log("Plain socket connection closing, notifying player process", []),
    toogie_player:disconnected(PlayerPid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
