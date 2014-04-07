% @doc Main application and top supervisor module.
-module(toogie_app).
-behavior(application).
-behavior(supervisor).

-export([start/0, start/2, stop/1]).
-export([init/1]).
-export([start_link/0]).
-include("toogie_common.hrl").

% @doc Manual application start.
-spec(start() -> {ok, pid()} | {ok, pid(), term()}).
start() ->
	error_logger:logfile({open, "toogie_server.log"}),
	error_logger:tty(false),
	?log("Starting application ~w", [?MODULE]),
    application:ensure_all_started(toogie_server).

% @doc Starts our game server and the cowboy listeners.
% This is the initialization callback called from application:start
-spec(start(normal | {failover, string()} | {takeover, string()}, term()) -> {ok, pid()} | ignore | {error, term()}).
start(_Type, _Args) ->
    Result = start_link(),
    Dispatch = cowboy_router:compile([
                {'_', [
                        {"/websocket", toogie_websocket_handler, []},
                        {"/", default_handler, []}
                        ]}
                ]),
    ?log("Starting cowboy listener on port 8080", []),
    cowboy:start_http(toogie_http_listener, 100, [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]),
    {ok, _} = ranch:start_listener(tcp_reverse, 10,
                                   ranch_tcp, [{port, 5555}], toogie_tcp_handler, []),
    Result.

% @doc Starts the game server and links it to the current process.
% It does not start the cowboy listeners. For that use {@link start/2}
-spec(start_link() -> {ok, pid()} | ignore | {error, term()}).
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% @doc No-op as there is nothing to clean up when the application is stopped.
% This is the callback called by application:stop/1
-spec(stop(term()) -> ok).
stop(_State) ->
	ok.

% @doc Sets up the supervisor tree with the toogie_game_master process as the lone child
% of the main toogie_server process that will be automatically restarted if it crashes. 
% Callback called by supervisor:start_link/3.
init(_Args) ->
	SupSpec = {one_for_all, 1, 5},
	GameMasterSpec = {toogie_game_master, {toogie_game_master, start_link, []}, permanent, 2000, worker, [toogie_game_master]},
	PlayerMasterSpec = {toogie_player_master, {toogie_player_master, start_link, []}, permanent, 2000, worker, [toogie_player_master]},
	{ok, {SupSpec, [GameMasterSpec, PlayerMasterSpec]}}.	
