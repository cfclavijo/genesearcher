%%%-------------------------------------------------------------------
%% @doc genesearcher top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(genesearcher_sup).

-behaviour(supervisor).

-include("genesearcher.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_POOL_SIZE, 5).
-define(DEFAULT_POOL_MAX_OVERFLOW, 10).

-define(DEFAULT_PORT, 8080).
-define(DEFAULT_KEEP_ALIVE, true).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {PoolOptions, MysqlOptions} = mysql_poolboy_options(),
    MySQLPoolSrv = mysql_poolboy:child_spec(pool1, PoolOptions, MysqlOptions),
    Procs = [MySQLPoolSrv],
    {ok, { {one_for_all, 0, 1}, Procs} }.

%%====================================================================
%% Internal functions
%%====================================================================

mysql_poolboy_options() ->
    PoolOptions = [
                   {size, genesearcher:get_env(ensembldb_pool_size), ?DEFAULT_POOL_SIZE},
                   {max_overflow, genesearcher:get_env(ensembldb_pool_max_overflow, ?DEFAULT_POOL_MAX_OVERFLOW)}
                  ],
    MysqlOptions = [
                    {host, genesearcher:get_env(ensembldb_host)},
                    {port, genesearcher:get_env(ensembldb_port, ?DEFAULT_PORT)},
                    {user, genesearcher:get_env(ensembldb_user)},
                    {database, genesearcher:get_env(ensembldb_database)},
                    {keep_alive, genesearcher:get_env(ensembldb_keep_alive, ?DEFAULT_KEEP_ALIVE)}
                   ],
    Password = case genesearcher:get_env(ensembldb_password) of
                   undefined -> [];
                   Pswd -> [{password, Pswd}]
               end,
    {PoolOptions, MysqlOptions ++ Password}.
