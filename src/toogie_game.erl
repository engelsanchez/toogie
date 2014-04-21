%% @doc Game process.
%% It receives moves from players and notifies the opponent of the move
%% and whether the game has been won or the board is full.
%% The public functions are {@link start/1}, {@link start_link/1},
%% {@link play/3}, {@link disconnect/2}, {@link abandon/1} and {@link quit/2}.
%% The other functions are FSM callbacks.
-module(toogie_game).
-behaviour(gen_server).
% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3]).
% Public API
-export([start/1,
         start_link/1,
         start_link/4,
         play/3,
         game_state/2,
         disconnect/2,
         reconnect/2,
         abandon/1,
         quit/2]).

-type color() :: 1 | 2.
-type seek_string() :: binary().

% The toogie_game behaviour
-callback new(seek_string()) -> term().
-callback is_valid_seek(seek_string()) -> boolean().
-callback state_string(term()) -> binary().
-callback play(color(), binary(), term()) ->
    invalid_move | {turn_change, term()} | {win, term()} | {draw, term()}.

% gen_server State data structure
-record(state, {game_id :: integer(),
                game_module :: atom(),
                game_type :: binary(),
                game_desc :: binary(),
                game_state :: any(),
                p1 :: pid(),
                p1conn=false :: boolean(),
                color1 :: color(),
                p2 :: pid(),
                p2conn=false :: boolean(),
                color2 :: color()}).

-include("toogie_common.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
% @doc Starts unsupervised game process, mostly for testing.
-spec(start(#game_info{}) -> {ok, pid()} | ignore | {error, string()}).
start(GameArgs) ->
	gen_server:start(?MODULE, GameArgs, []).

% @doc Starts game process linked to the current process.
-spec(start_link(#game_info{}) -> {ok, pid()} | ignore | {error, string()}).
start_link(GameArgs) ->
	gen_server:start_link(?MODULE, GameArgs, []).

start_link(Pid1, Pid2, Mod, Seek = #seek{})
        when is_pid(Pid1), is_pid(Pid2), is_atom(Mod) ->
    gen_server:start_link(?MODULE, [Pid1, Pid2, Mod, Seek], []).

% @doc Called when a player makes a move
-spec(play(pid(), pid(), tuple()) ->
      invalid_move | not_your_turn | ok | you_win ).
play(GamePid, PlayerPid, Move) ->
	gen_server:call(GamePid, {play, PlayerPid, Move}, ?INTERNAL_TIMEOUT).

% @doc Returns information about the game relevant to the calling player
game_state(GamePid, PlayerPid) ->
	gen_server:call(GamePid, {game_state, PlayerPid}, ?INTERNAL_TIMEOUT).

% @doc Marks player as disconnected from the game and notifies other player.
-spec(disconnect(pid(), pid()) -> ok).
disconnect(GamePid, PlayerPid) ->
	gen_server:call(GamePid, {disconnected, PlayerPid}, ?INTERNAL_TIMEOUT).

% @doc Marks player as connected again and notifies other player.
-spec(reconnect(pid(), pid()) -> ok).
reconnect(GamePid, PlayerPid) ->
	gen_server:call(GamePid, {reconnected, PlayerPid}, ?INTERNAL_TIMEOUT).

% @doc Stops the game
abandon(GamePid) ->
	gen_server:call(GamePid, {abandon}, ?INTERNAL_TIMEOUT).

% @doc Called when a player actively quits a game (not just disconnects).
-spec(quit(pid(), pid()) -> ok).
quit(GamePid, PlayerPid) ->
	gen_server:call(GamePid, {quit, PlayerPid}, ?INTERNAL_TIMEOUT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM functions

% @doc Creates the empty board and starts monitoring the player processes.
% FSM initialization callback.
-spec(init(#game_info{}) -> {ok, playing, #state{}}).
init([P1, P2, Mod, #seek{id=GameId, seek_str=SeekStr, game_type=GameType}])
        when is_pid(P1), is_pid(P2)  ->
	?log("Starting~n", []),
	monitor(process, P1),
	monitor(process, P2),
    GameState = Mod:new(SeekStr),
    {ok, #state{game_id=GameId,
                game_module=Mod,
                game_desc=SeekStr,
                game_type=GameType,
                game_state=GameState,
                p1=P1, p1conn=true, color1=1,
                p2=P2, p2conn=true, color2=2}}.

% @doc Waiting for a move
% returns: 
%   invalid_move
%   not_your_turn
%   ok
%   you_win
% May send message to other player:
%   played, Col, you_lose|your_turn
handle_call({play, P2, _Move}, _From, #state{p2=P2} = State) ->
	{reply, not_your_turn, State};
handle_call({play, P1, Move}, _From, #state{game_module=Mod, p1=P1, p1conn=true, color1=Color, p2=P2, game_state=GameState} = State) ->
    case Mod:play(Color, Move, GameState) of
        {win, NewGameState} ->
            toogie_player:other_played(P2, self(), Move, you_lose), 
            {stop, normal, you_win, State#state{game_state=NewGameState}};
        {turn_change, NewGameState} ->
            toogie_player:other_played(P2, self(), Move, your_turn),
            {reply, ok, turn_change(State#state{game_state=NewGameState})};
        {ok, NewGameState} ->
            toogie_player:other_played(P2, self(), Move, your_turn),
            {reply, ok, turn_change(State#state{game_state=NewGameState})};
        {draw, NewGameState} ->
            toogie_player:other_played(P2, self(), Move, game_draw),
            State2 = turn_change(State#state{game_state=NewGameState}),
            {stop, normal, game_draw, State2}; 
        {error, invalid_move} ->
			{reply, invalid_move, State}
    end;
% @doc One or both players have disconnected and moves are not allowed.
% Players may reconnect at any point.
handle_call({join, P1}, _From, #state{p1=P1, p1conn=false, p2=P2, p2conn=P2Conn} = State) ->
	toogie_player:other_returned(P2, self()),
	State2 = State#state{p1conn=true},
	case P2Conn of
		true ->
			{reply, {in_game, your_turn}, State2};
		false ->
			{reply, {in_game, wait}, State2}
	end;
handle_call({join, P2}, _From, #state{p1=P1, p1conn=P1Conn, p2=P2, p2conn=false} = State) ->
	toogie_player:other_returned(P1, self()),
	State2 = State#state{p2conn=true},
	case P1Conn of
		true ->
			{reply, {in_game, other_turn}, State2};
		false ->
			{reply, {in_game, wait}, State2}
	end;
% @doc Handles disconnections or requests to abandon a game.
handle_call({quit, Pid}, _From, #state{p1=P1, p2=P2} = State) ->
	toogie_player:other_quit(case Pid of P1->P2; P2->P1 end, self()),
	{stop, normal, ok, State};
handle_call({disconnected, P1}, _From, #state{p1=P1, p1conn=true, p2=P2} = State) ->
	toogie_player:other_disconnected(P2, self()),
	{reply, ok, State#state{p1conn=false}};
handle_call({disconnected, P2}, _From, #state{p1=P1, p2=P2, p2conn=true} = State) ->
	toogie_player:other_disconnected(P1, self()),
	{reply, ok, State#state{p2conn=false}};
handle_call({reconnected, P1}, _From, #state{p1=P1, p1conn=false, p2=P2} = State) ->
	toogie_player:other_returned(P2, self()),
	{reply, ok, State#state{p1conn=true}};
handle_call({reconnected, P2}, _From, #state{p1=P1, p2=P2, p2conn=false} = State) ->
	toogie_player:other_returned(P1, self()),
	{reply, ok, State#state{p2conn=true}};
handle_call({abandon}, _From, State) ->
	{stop, normal, ok, State};
handle_call({game_state, Pid}, _From, 
            #state{game_id=GameId, game_module=Mod, game_type=GameType,
                   game_state=GameState, game_desc=GameDesc,
                   p1=P1, p2=P2, color1=C1,color2=C2} = State)
	when Pid =:= P1;Pid =:= P2 ->
    {Turn, Color} = if
        Pid =:= P1 -> {your_turn, C1};
        Pid =:= P2 -> {other_turn, C2}
    end,
    StateStr = Mod:state_string(GameState),
    {reply, #game_info{turn = Turn,
                       color = Color,
                       id = GameId,
                       game_desc = GameDesc,
                       game_type = GameType,
                       game_state = StateStr}, State}; 
handle_call(Msg, _From, State) ->
	?log("Unexpected message ~w : ~w", [Msg, State]),
	{reply, {error, invalid_command}, State}.

handle_cast(Msg, State) ->
	?log("Unexpected message ~w : ~w", [Msg, State]),
	{noreply, State}.

% @doc Handles player process disconnections. 
% FSM callback for non-FSM process messages.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{p1=Pid} = Data) ->
	{stop, normal, Data#state{p1conn=false}};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{p2=Pid} = Data) ->
	{stop, normal, Data#state{p2conn=false}};
handle_info(Event, Data) ->
	?log("Unexpected event ~w : ~w", [Event, Data]),
	{next_state, Data}.

% @doc Does nothing as there is no real cleanup needed upon game end.
% Generic FSM termination callback.
terminate(_Reason, _State) ->
	ok.

% @doc Code hot swapping callback, does nothing.
code_change(_OldVsn, StateData, _Extra) ->
	{ok, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions

% @doc Returns a state record with the players swapped 
turn_change(#state {p1=P1, p1conn=P1C, color1=Color1, p2=P2, p2conn=P2C, color2=Color2} = State) ->
	State#state{p1=P2, p1conn=P2C, color1=Color2, p2=P1, p2conn=P1C, color2=Color1}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

turn_change_test() ->
	S1 = #state{p1=p1,p1conn=p1conn,color1=color1,p2=p2,p2conn=p2conn,color2=color2},
	?assertEqual(#state{p1=p2,p1conn=p2conn,color1=color2,p2=p1,p2conn=p1conn,color2=color1}, turn_change(S1)).

-endif.
