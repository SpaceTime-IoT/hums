%%%-------------------------------------------------------------------
%% @doc hums projection supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hums_proj_sup).

-behaviour(supervisor).

%% API
-export([start_link/2,
         make_proj_supname/1,
         make_fitness_regname/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(FluName, Props0) ->
    supervisor:start_link(?MODULE, [FluName, Props0]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([FluName, Props0]) ->
    DataDir = get_data_dir(FluName, Props0),

    RestartStrategy = one_for_all,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ProjRegName = make_proj_supname(FluName),
    Props = Props0 ++ [{projection_store_registered_name, ProjRegName},
                       {use_partition_simulator,false}],
    ProjSpec = {ProjRegName,
                {hums_projection_store, start_link,
                 [ProjRegName, DataDir, FluName]},
                permanent, 5000, worker, []},
    FitnessRegName = make_fitness_regname(FluName),
    FitnessSpec = {FitnessRegName,
                   {hums_fitness, start_link,
                    [ [{FluName}|Props] ]},
                   permanent, 5000, worker, []},
    MgrSpec = {make_mgr_supname(FluName),
               {hums_chain_manager1, start_link,
                [FluName, [], Props]},
               permanent, 5000, worker, []},

    {ok, {SupFlags, [ProjSpec, FitnessSpec, MgrSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================

make_mgr_supname(MgrName) when is_atom(MgrName) ->
    hums_chain_manager1:make_chmgr_regname(MgrName).

make_proj_supname(ProjName) when is_atom(ProjName) ->
    list_to_atom(atom_to_list(ProjName) ++ "_pstore").

make_fitness_regname(FluName) when is_atom(FluName) ->
    list_to_atom(atom_to_list(FluName) ++ "_fitness").

get_data_dir(FluName, Props) ->
    case proplists:get_value(data_dir, Props) of
        Path when is_list(Path) ->
            Path;
        undefined ->
            Dir = application:get_env(hums, flu_data_dir, "./data_dir"),
            filename:join(Dir, atom_to_list(FluName))
    end.
