%%%-------------------------------------------------------------------
%% @doc hums top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hums_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
        stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


stop() ->
    exit(whereis(?SERVER), normal).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
