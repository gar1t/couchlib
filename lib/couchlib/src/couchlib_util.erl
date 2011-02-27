-module(couchlib_util).

-export([couch_lib_dir/0]).

%%---------------------------------------------------------------------------
%% @doc Looks for the Couch lib dir. Error if it can't be found. First
%% looks in the canonical app structure relative to the couch beam files
%% (i.e. EBIN_DIR/../priv/lib) and then looks in the location the CouchDB build
%% builds the shared library (i.e. COUCH_SRC/priv/.libs) (works on Linux, not
%% sure about other platforms).
%% ---------------------------------------------------------------------------
couch_lib_dir() ->
    {file, Mod} = code:is_loaded(couch_config),
    Ebin = filename:dirname(Mod),
    Installed = filename:join(Ebin, "../priv/lib"),
    case filelib:is_dir(Installed) of
        true -> Installed;
        false ->
            Dev = filename:join(Ebin, "priv/.libs"),
            case filelib:is_dir(Dev) of
                true -> Dev;
                false -> erlang:error(no_couch_lib)
            end
    end.
