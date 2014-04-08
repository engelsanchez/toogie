% @doc Coordinates the creation of games between pairs of players.
% It mostly sits in a loop receiving join requests.  When the first
% join request comes, the player is told to wait for another. When the
% next one comes, the pair are put into a newly created game and
% the process repeats over and over.
-module(toogie_game_master).
-behaviour(gen_server).
% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
% Public API
-export([start/0,
         start_link/0,
         stop/0,
		 seek/1,
         cancel_seek/1,
         cancel_seek/2,
         accept_seek/1,
		 game_finished/1, 
		 register_for_seeks/1,
         seek_list/0,
         game_list/0]).

-include("toogie_common.hrl").

-record(state, {game_modules :: orddict:orddict(),
                seeks :: dict(),                    % seek -> id
                seeks_by_player :: dict(),          % player pid -> seek
                seeks_by_id :: dict(),              % id -> seek
                parent :: pid(),
                seed=now() :: erlang:timestamp()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API

% @doc Starts a standalone toogie_game_master process. Mostly for testing.
-spec(start() ->  {ok, pid()} | ignore | {error, binary()}).
start() ->
	gen_server:start({local, toogie_game_master}, ?MODULE, self(), []).

% @doc Starts a toogie_game_master process that is linked to the current
% process.
-spec(start_link() -> {ok, pid()} | ignore | {error, binary()}).
start_link() ->
	gen_server:start_link({local, toogie_game_master}, ?MODULE, self(), []).

% @doc Call when a player requests to join a game. 
-spec(seek(#seek{}) -> {seek_pending, seek_id()} | {duplicate_seek, seek_id()}
      | {new_game, #game_info{}}).
seek(Seek) ->
	gen_server:call(?MODULE, {seek, Seek}, ?INTERNAL_TIMEOUT).

% @doc Cancels all pending seeks for the given player.
-spec(cancel_seek(pid()) -> seek_canceled | no_seek_found).
cancel_seek(Pid) ->
	gen_server:call(?MODULE, {cancel_seek, Pid}, ?INTERNAL_TIMEOUT).

% @doc Call when a player requests to cancel all pending seeks.
-spec(cancel_seek(pid(), seek_id()) -> seek_canceled | no_seek_found).
cancel_seek(Pid, SeekId) ->
	gen_server:call(?MODULE, {cancel_seek, Pid, SeekId}, ?INTERNAL_TIMEOUT).

% @doc User request to accept a pending seek.
-spec(accept_seek(pos_integer()) -> ok | no_seek_found).
accept_seek(SeekId) ->
	gen_server:call(?MODULE, {accept_seek, SeekId}, ?INTERNAL_TIMEOUT).

% @doc Returns a list with info for all current games.
game_list() ->
	gen_server:call(?MODULE, game_list, ?INTERNAL_TIMEOUT).

% @doc Removes game from our database
game_finished(GamePid) ->
	gen_server:call(?MODULE, {game_finished, GamePid}, ?INTERNAL_TIMEOUT).

% @doc Returns the list of all pending seeks.
seek_list() ->
	gen_server:call(?MODULE, seek_list, ?INTERNAL_TIMEOUT).

% @doc Registers a user to receive seek notifications and returns the current list of seeks.
register_for_seeks(PlayerPid) ->
	gen_server:call(?MODULE, {register_for_seeks, PlayerPid}, ?INTERNAL_TIMEOUT).

% @doc Requests the game master process to stop.
-spec(stop() -> ok).
stop() ->
	gen_server:call(?MODULE, stop, ?INTERNAL_TIMEOUT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks

% @doc Starts trapping exits, creates player and game tables
% and starts FSM in loop state.
% Callback method for the OTP gen_fsm behavior.
init(ParentPid) when is_pid(ParentPid) ->
	?log("Starting", []),
	process_flag(trap_exit, true),
	ets:new(toogie_game_id_tbl, [named_table, private, set]),
	ets:new(toogie_game_pid_tbl, [named_table, private, set]),
	ets:new(toogie_game_priv_tbl, [named_table, private, set]),
	ets:new(toogie_player_priv_tbl, [named_table, private, set]),
	Seeks = dict:new(),
	SeeksByPlayer= dict:new(),
	SeeksById = dict:new(),
    {ok, GameMods} = application:get_env(game_modules),
    ?log("Starting with game modules ~p", [GameMods]),
	erlang:start_timer(?LOG_STATE_INTERVAL, self(), log_state),
    {ok, #state{game_modules=GameMods, seeks=Seeks,
                seeks_by_player=SeeksByPlayer, seeks_by_id=SeeksById,
                parent=ParentPid, seed=now()}}.

% @doc Deletes player and game tables upon termination.
% Callback function for the OTP gen_fsm behavior.
terminate(_Reason, _Data) ->
	ets:delete(toogie_game_id_tbl),
	ets:delete(toogie_game_pid_tbl),
	ets:delete(toogie_game_priv_tbl),
	ets:delete(toogie_player_priv_tbl),
	ok.

game_mod(GameType, GameMods) ->
    case orddict:find(GameType, GameMods) of
        error ->
            undefined;
        {ok, GameModVal} ->
            GameModVal
    end.

% @doc The main 'loop' state of this FSM handles all messages 
% (seeks, seek cancellations, player registrations ).
handle_call({seek, #seek{pid=Pid, game_privacy=anon, game_type=GameType} = Seek}, 
            _From, 
            #state{game_modules=GameMods, seeks=Seeks,
                   seeks_by_player=SeeksByPlayer, seeks_by_id=SeeksById,
                   seed=Seed} = State) ->
	% Do not use issuer pid when looking up seeks
	Key = Seek#seek{pid=none},
	?log("Processing anonymous game seek ~w ~w ~w", [Seek, Key, State]),
	% Match with currents seeks. Start game if matched, add to seeks if not
    case {game_mod(GameType, GameMods), dict:find(Key, Seeks)} of
        {undefined, _} ->
            ?log("Invalid game type ~p", [GameType]),
            {error, {invalid_game_type, GameType}};
        {GameMod, {ok, SeekId}} ->
			case dict:find(SeekId, SeeksById) of  
				{ok, #seek{pid=Pid}} ->
					?log("Seek ~w is a duplicate", [SeekId]),
					{reply, {duplicate_seek, SeekId}, State}; 
				{ok, #seek{pid=OPid, game_type=GameType, seek_str=SeekStr}} ->
					?log("Seek has match. Starting new game", []),
                    {ok, GamePid} = toogie_game:start_link(OPid, Pid, GameMod,
                                                           Seek),
					toogie_player:joined(OPid, #game_info{pid=GamePid, id=SeekId, game_privacy=anon, turn=your_turn, color=1}),
					ets:insert(toogie_game_id_tbl, {SeekId, GamePid}),
					ets:insert(toogie_game_pid_tbl, {GamePid, SeekId}),
					toogie_player_master:unregister_player(OPid),
					toogie_player_master:unregister_player(Pid),
					{_SeekIds, Seeks2, SeeksById2, SeeksByPlayer2} =
						remove_player_seeks([Pid, OPid], Seeks, SeeksById, SeeksByPlayer),
					NewState = State#state{seeks=Seeks2, seeks_by_player=SeeksByPlayer2, seeks_by_id=SeeksById2},
					?log("State ~w", [NewState]),
					{
						reply,
                        {new_game, #game_info{pid=GamePid, id=SeekId,
                                              game_type=GameType,
                                              game_desc=SeekStr,
                                              game_privacy=anon,
                                              turn=other_turn, color=2}}, 
						NewState
					}
			end;
		{_, error} ->
			?log("No match. Seek -> Pending ", []),
			{SeekId, Seed2} = next_game_id(Seed),
			NewSeek = Seek#seek{id=SeekId},
			Seeks2 = dict:store(Key, SeekId, Seeks),
			SeeksByPlayer2 = dict:append(Pid, SeekId, SeeksByPlayer),
			SeeksById2 = dict:store(SeekId, NewSeek, SeeksById),
			toogie_player_master:notify_seek_issued(NewSeek),
			NewState = State#state{seeks=Seeks2, seeks_by_player=SeeksByPlayer2, seeks_by_id=SeeksById2, seed=Seed2},
			%?log("State ~w", [NewState]),
			{reply, {seek_pending, SeekId}, NewState}
	end;
% Process play with friend request (private seek).
handle_call({seek, #seek{game_privacy=priv} = Seek}, {Pid, _Tag} = _From, 
			#state{seed=Seed,seeks=Seeks, seeks_by_player=SeeksByPlayer, seeks_by_id=SeeksById} = State) ->
	?log("Adding new private game seek ~w", [Seek]),
	% Removing all other pending seeks when requesting private game
	{_, Seeks2, SeeksById2, SeeksByPlayer2} = remove_player_seeks(Pid, Seeks, SeeksById, SeeksByPlayer),
	{SeekId, Seed2} = next_game_id(Seed),
	ets:insert(toogie_game_priv_tbl, {SeekId, Seek#seek{id=SeekId, pid=Pid}}),
	ets:insert(toogie_player_priv_tbl, {Pid, SeekId}),
	{reply, 
	 {seek_pending, SeekId}, 
	 State#state{seed=Seed2, seeks=Seeks2, seeks_by_id=SeeksById2, seeks_by_player=SeeksByPlayer2}};
handle_call({accept_seek, SeekId}, {Pid, _Tag}, #state{game_modules=GameMods,
                                             seeks_by_id=SeeksById, seeks=Seeks, seeks_by_player=SeeksByPlayer} = State) ->
	?log("Processing accept seek ~w", [SeekId]),
	case dict:find(SeekId, SeeksById) of
		{ok, #seek{pid=OPid, game_type=Type, seek_str=SeekStr} = Seek} -> 
			?log("Seek has match. Starting new game", []),
            case game_mod(Type, GameMods) of
                undefined ->
                    {reply, invalid_command, State};
                Mod ->
                    {ok, GamePid} = toogie_game:start_link(OPid, Pid, Mod,  Seek),
                    GameId=SeekId,
                    GameInfo = #game_info{pid=GamePid,
                                          id=GameId,
                                          turn=your_turn,
                                          color=1,
                                          game_type=Type,
                                          game_privacy=anon,
                                          game_desc=SeekStr},
                    toogie_player:joined(OPid, GameInfo),
                    ets:insert(toogie_game_id_tbl, {GameId, GamePid}),
                    ets:insert(toogie_game_pid_tbl, {GamePid, GameId}),
                    % Stop bugging these two players with seek notifications
                    toogie_player_master:unregister_player(OPid),
                    toogie_player_master:unregister_player(Pid),
                    {_SeekIds, Seeks2, SeeksById2, SeeksByPlayer2} =
                                                                     remove_player_seeks([Pid, OPid], Seeks, SeeksById, SeeksByPlayer),
                    NewState = State#state{seeks=Seeks2, seeks_by_player=SeeksByPlayer2, seeks_by_id=SeeksById2},
                    ?log("State ~w", [NewState]),
                    {reply, {new_game, GameInfo#game_info{turn=other_turn, color=2}}, NewState}
            end;
		error ->
			case ets:lookup(toogie_game_priv_tbl, SeekId) of
				[{SeekId, #seek{pid=OPid, game_type=Type, seek_str=SeekStr} = Seek}] ->
					?log("Seek matches private seek ~w. Starting new game", [Seek]),
					ets:delete(toogie_game_priv_tbl, SeekId),
					% @todo Change below when multiple private seeks are possible
					ets:delete(toogie_player_priv_tbl, OPid),
                    GameMod = game_mod(Type, GameMods),
					{ok, GamePid} = toogie_game:start_link(OPid, Pid, GameMod, Seek),
					GameId=SeekId,
					toogie_player:joined(OPid, #game_info{pid=GamePid, id=GameId,game_privacy=anon, turn=your_turn, color=1}),
					ets:insert(toogie_game_id_tbl, {GameId, GamePid}),
					ets:insert(toogie_game_pid_tbl, {GamePid, GameId}),
					% Stop bugging these two players with seek notifications
					toogie_player_master:unregister_player(OPid),
					toogie_player_master:unregister_player(Pid),
					{_SeekIds, Seeks2, SeeksById2, SeeksByPlayer2} =
						remove_player_seeks([Pid, OPid], Seeks, SeeksById, SeeksByPlayer),
					NewState = State#state{seeks=Seeks2, seeks_by_player=SeeksByPlayer2, seeks_by_id=SeeksById2},
					?log("State ~w", [NewState]),
					{
						reply,
                        {new_game, #game_info{pid=GamePid, id=GameId,
                                              game_type=Type,
                                              game_desc=SeekStr,
                                              game_privacy=anon,
                                              turn=other_turn, color=2}},
						NewState
					};
				[] ->
					{reply, no_seek_found, State}
			end
	end;
handle_call({cancel_seek, Pid}, _From, 
			#state{seeks=Seeks, seeks_by_id=SeeksById, seeks_by_player=SeeksByPlayer} = State) 
  when is_pid(Pid) ->
	?log("Canceling seeks for player ~w", [Pid]),
	{SeekIds, Seeks2, SeeksById2, SeeksByPlayer2} = 
		remove_player_seeks(Pid, Seeks, SeeksById, SeeksByPlayer),
	Reply = case SeekIds of 
				[] ->
					?log("No seeks found for player ~w", [Pid]),
					no_seek_found;
				_ -> 
					?log("Removed seeks player seeks ~w ~w", [Pid, SeekIds]),
					seek_canceled
			end,
	NewState = State#state{seeks=Seeks2,seeks_by_id=SeeksById2,seeks_by_player=SeeksByPlayer2},
	?log("State ~w", [NewState]),
	{reply, Reply, NewState};
handle_call({cancel_seek, Pid, SeekId}, _From, State) when is_integer(SeekId) ->
	case do_remove_seek(Pid, SeekId, State) of
		{ok, NewState} -> 
			%?log("State ~w", [NewState]),
			{reply, seek_canceled, NewState};
		no_seek_found -> {reply, no_seek_found, State}
	end;
handle_call(seek_list, _From, State) ->
	{reply, get_seek_list(State), State};
handle_call({register_for_seeks, PlayerPid}, _From, State) ->
	% Synchronously register the player and then return current seek list
	% To guarantee that player sees all current seeks and any coming afterwards
	toogie_player_master:register_player(PlayerPid),
	{reply, get_seek_list(State), State};
handle_call(game_list, _From, State) ->
	% Go over the entire game table and build a list
	GameList = ets:foldl(fun(X, L) -> [X | L] end, [], toogie_game_pid_tbl),
	{reply, GameList, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

% @doc Handles game processes ending and users disconnecting.
% Callback function for miscellaneous messages received by the FSM process.
handle_info({'EXIT', Pid, _Reason}, #state{parent=Pid} = State) ->
	?log("Parent process went down, goodbye!", []),
	{stop, parent_died, State};
handle_info({'EXIT', GamePid, _Reason}, State) ->
	?log("Game process ~w went down", [GamePid]),
	case ets:lookup(toogie_game_pid_tbl, GamePid) of
		[{GamePid, GameId}] -> 
			ets:delete(toogie_game_id_tbl, GameId);
		_ -> ok
	end,
	ets:delete(toogie_game_pid_tbl, GamePid),
	{noreply, State};
% On user disconnect, remove from player and seeks data.
handle_info({'DOWN', _Ref, process, P1, _Reason}, 
			#state{seeks=Seeks, 
				   seeks_by_id=SeeksById, 
				   seeks_by_player=SeeksByPlayer} = State) ->
	?log("Player process ~w went down", [P1]),
	{_SeekIds, Seeks2, SeeksById2, SeeksByPlayer2} = 
		remove_player_seeks(P1, Seeks, SeeksById, SeeksByPlayer),
	{noreply, State#state{seeks=Seeks2, 
						  seeks_by_id=SeeksById2, 
						  seeks_by_player=SeeksByPlayer2}};
handle_info({timeout, _Ref, log_state}, 
			#state{seeks=Seeks,seeks_by_id=SeeksById, seeks_by_player=SeeksByPlayer} = State) ->
	IdTblSize = ets:info(toogie_game_id_tbl, size),
	PidTblSize = ets:info(toogie_game_pid_tbl, size),
	PrivTblSize = ets:info(toogie_game_priv_tbl, size),
	PlayerPrivTblSize = ets:info(toogie_game_priv_tbl, size),
	S = dict:to_list(Seeks),
	SI = dict:to_list(SeeksById),
	SP = dict:to_list(SeeksByPlayer),
	?log("~w state: ~nid entries = ~w ~npid entries = ~w~n"
	"Priv seeks = ~w~nPlayer priv seeks = ~w~nSeeks = ~w~n"
		"Seeks by Id = ~w~nSeeks by player = ~w", 
		[?MODULE, IdTblSize, PidTblSize, PrivTblSize, PlayerPrivTblSize, S, SI, SP]),
	erlang:start_timer(?LOG_STATE_INTERVAL, self(), log_state),
	{noreply, State}.

% @doc Handles the (asynchronous) stop message
% Generic FSM callback for synchronous messages.
handle_cast(stop, State)->
	{stop, normal, State};
handle_cast(Event, State) ->
	?log("Unexpected event ~w : ~w", [Event, State]),
	{noreply, State}.

% @doc Hot code swap callback (does nothing now).
code_change(_OldVsn, StateData, _Extra) ->
	{ok, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions

% @doc Computes the next number to be used as game id, which is guaranteed
% not to be in use at this very moment.
-spec(next_game_id({integer(), integer(), integer()}) -> {pos_integer(), {integer(), integer(), integer()}}).
next_game_id(Seed) ->
	{GameId, Seed2} = random:uniform_s(?MAX_GAME_ID, Seed),
	% If in use, try again.
	case ets:member(toogie_game_id_tbl, GameId) of
		true -> next_game_id(Seed2);
		false -> {GameId, Seed2}
	end.

get_seek_list(#state{seeks_by_id=Seeks} = _State) ->
	% Get seeks from seekers dictionary values, as those still have the sender's Pid
	% unlike the keys in the seeks dictionary.
	dict:fold(fun(_K, V, L) -> [V | L] end, [], Seeks).

% @doc Removes all seeks for the given players and notifies everybody else about it
remove_player_seeks(PlayerPid, Seeks, SeeksById, SeeksByPlayer) when is_pid(PlayerPid) ->
	remove_player_seeks([PlayerPid], Seeks, SeeksById, SeeksByPlayer);
remove_player_seeks(PlayerPids, Seeks, SeeksById, SeeksByPlayer) when is_list(PlayerPids) ->
	% Compile list of seek ids
	remove_player_priv_seeks(PlayerPids),
	SeekIds = 
		dict:fold(fun(K, V, L) -> case lists:member(K, PlayerPids) of true-> V ++ L;false -> L end end, 
				  [],
				  SeeksByPlayer),
	SeeksByPlayer2 =
		dict:filter(fun(K, _V) -> not lists:member(K, PlayerPids) end, SeeksByPlayer),
	SeeksById2 =
		dict:filter(fun(K, _V) -> not lists:member(K, SeekIds) end, SeeksById),
	Seeks2 = 
		dict:filter(fun(_K, V) -> not lists:member(V, SeekIds) end, Seeks),
	toogie_player_master:notify_seek_removed(SeekIds, PlayerPids),
	{SeekIds, Seeks2, SeeksById2, SeeksByPlayer2}.

% @doc Removes all private game seeks for a given player or list of players.
remove_player_priv_seeks([]) -> ok;
remove_player_priv_seeks([PlayerPid | More]) ->
	remove_player_priv_seeks(PlayerPid),
	remove_player_priv_seeks(More);
remove_player_priv_seeks(PlayerPid) when is_pid(PlayerPid) ->
	case ets:lookup(toogie_player_priv_tbl, PlayerPid) of
		[{PlayerPid, SeekId}] ->
			ets:delete(toogie_player_priv_tbl, PlayerPid),
			ets:delete(toogie_game_priv_tbl, SeekId);
		_ -> ok
	end.

	

-spec(do_remove_seek(pid(), seek_id, #state{}) ->{ok, #state{}} | {no_seek_found, #state{}}).
do_remove_seek(Pid, SeekId, #state{seeks=Seeks,seeks_by_id=SeeksById,seeks_by_player=SeeksByPlayer} = State) ->
	case dict:find(SeekId, SeeksById) of
		{ok, #seek{pid=Pid} = Seek} ->
			SeeksById2 = dict:erase(SeekId, SeeksById),
			Seeks2 = dict:erase(Seek#seek{pid=none, id=none}, Seeks),
			{ok, PlayerSeeks} = dict:find(Pid, SeeksByPlayer),
			PlayerSeeks2 = lists:delete(SeekId, PlayerSeeks),
			SeeksByPlayer2 = 
				case PlayerSeeks2 of
					[] -> dict:erase(Pid, SeeksByPlayer);
					_ -> dict:store(Pid, PlayerSeeks2, SeeksByPlayer)
				end,
			toogie_player_master:notify_seek_removed(SeekId, Pid),
			NewState = State#state{seeks=Seeks2, 
								   seeks_by_id=SeeksById2, 
								   seeks_by_player=SeeksByPlayer2},
			%?log("State ~w", [NewState]),
			{ok, NewState};
		{ok, _Seek} -> no_seek_found;
		error ->
			% A private seek maybe?
			case ets:lookup(toogie_game_priv_tbl, SeekId) of
				[{SeekId, #seek{pid=Pid}}] ->
				ets:delete(toogie_game_priv_tbl, SeekId),
				ets:delete(toogie_player_priv_tbl, Pid),
				{ok, State};
				[] -> no_seek_found;
				[{SeekId, #seek{}}] -> no_seek_found
			end
	end.
