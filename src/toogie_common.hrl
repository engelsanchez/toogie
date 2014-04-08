-ifndef(NOLOG).
-define(log(Format, Args), error_logger:info_msg("~w:~w ~w : " ++ Format ++ "~n", [?MODULE, ?LINE, self()] ++ Args)).
-else.
nolog(_Format, _Args) -> ok.
-define(log(Format, Args), nolog(Format, Args)).
-endif.

-define(ends_with(Bin, Str), Str == binary_part(Bin, {byte_size(Bin), -byte_size(Str)})).
-define(INTERNAL_TIMEOUT, 1000000).
% Log internal state every N minutes
-define(LOG_STATE_INTERVAL, 1 * 60 * 1000).
-define(MAX_GAME_ID,1000000).
-define(MAX_PLAYER_ID,1000000).

%%%%%%%%%%%%
% Common types
% TODO: Move to modules
-type game_privacy() :: anon | priv.
% 2 character game type
-type game_type() :: <<_:16>>.
-type turn() :: your_turn | other_turn.
-type seek_id() :: pos_integer().
-type game_id() :: pos_integer().

-record(game_info,{
		id :: pos_integer(),
		pid = none :: pid() | none, 
        game_privacy :: game_privacy(),
		game_type :: game_type(), 
        game_desc :: binary(), % Seek string
        game_state,
        turn,
        color,
		ppid1 = none :: none | pid(),
		ppid2  = none :: none | pid()
		}). 

-record(seek, {
		id = none :: none | pos_integer(),
		pid = none :: none | pid(),
        game_privacy :: game_privacy(),
        game_type :: game_type(),
        seek_str :: string()
		}).

-record(player_state, {
        player_pid :: undefined|pid()
        }).
