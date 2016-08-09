-module(hums_client).

-export([write_projection/4,
         read_projection/3,
         read_projection/4,
         read_latest_projection/3,
         kick_projection_reaction/3,
         list_all_projections/3]).

write_projection(ProjStore, ProjType, Proj, _Timeout) ->
    try
        hums_projection_store:write(ProjStore, ProjType, Proj)
    catch
        _:_ ->
            {error, partition}
    end.

read_projection(ProjStore, ProjType, Epoch) ->
    try
        read_projection(ProjStore, ProjType, Epoch, infinity)
    catch
        _:_ ->
            {error, partition}
    end.

read_projection(ProjStore, ProjType, Epoch, _Timeout) ->
    try
        hums_projection_store:read(ProjStore, ProjType, Epoch)
    catch
        _:_ ->
            {error, partition}
    end.

read_latest_projection(ProjStore, ProjType, _Timeout) ->
    try
        hums_projection_store:read_latest_projection(ProjStore, ProjType)
    catch
        _:_ ->
            {error, partition}
    end.

kick_projection_reaction(ProjStore, Options, _Timeout) ->
    try
        hums_projection_store:kick_projection_reaction(ProjStore, Options)
    catch
        _:_ ->
            {error, partition}
    end.

list_all_projections(ProjStore, ProjType, _Timeout) ->
    try
        hums_projection_store:list_all_projections(ProjStore, ProjType)
    catch
        _:_ ->
            {error, partition}
    end.
