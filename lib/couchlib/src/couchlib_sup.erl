-module(couchlib_sup).

-behavior(supervisor).

-export([start_link/0, start_service/1, stop_service/1]).

% Non-standard start functions.
-export([start_config_wrapper/0]).
-export([start_httpd_wrapper/0]).
-export([start_query_servers_wrapper/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%---------------------------------------------------------------------------
%% @doc Starts the couchlib supervisor.
%%---------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%---------------------------------------------------------------------------
%% @doc Starts a CouchDB service as specified by the service module. Some
%% services require special handling, which is implemented automatically here
%% based on the service type.
%% ---------------------------------------------------------------------------

start_service(couch_httpd) -> 
    start_spec({couch_httpd,
                {?MODULE, start_httpd_wrapper, []},
                permanent, 1000, worker, [couch_httpd]}),
    start_spec({couch_httpd_vhost,
                {couch_httpd_vhost, start_link, []},
                permanent, 1000, worker, [couch_httpd_vhost]});
start_service(couch_query_servers) ->
    start_spec({couch_query_servers,
                {?MODULE, start_query_servers_wrapper, []},
                permanent, 1000, worker, [couch_query_servers]});
start_service(couch_uuids) -> 
    % couch_uuids:start/0 is a link.
    start_spec({couch_uuids,
                {couch_uuids, start, []},
                permanent, brutal_kill, worker, [couch_uuids]});
start_service(Module) ->
    % We use a 1 second allowance for clean shutdown by default - some of the
    % couch procs use this.
    start_spec({Module, {Module, start_link, []},
                permanent, 1000, worker, [Module]}).

%%---------------------------------------------------------------------------
%% @doc Starts the specified spec.
%%---------------------------------------------------------------------------

start_spec({Id,_,_,_,_,_}=Spec) ->
    case supervisor:start_child(?SERVER, Spec) of
        {ok, Pid} ->
            {ok, Pid};
        {error, already_present} ->
            supervisor:restart_child(?SERVER, Id);
        {error, {already_started, Pid}} ->
            {error, {already_started, Pid}};
        Err -> 
            exit(Err)
    end.

%%---------------------------------------------------------------------------
%% @doc Stops a CouchDB service previously started with start_service/1.
%%---------------------------------------------------------------------------

stop_service(Module) ->
    supervisor:terminate_child(?SERVER, Module).

%%---------------------------------------------------------------------------
%% @doc Standard supervisor init/1.
%%
%% Any non-standard init is moved into module start_xxx_wrapper methods (see
%% below).
%%
%% The restart and shutdown specs are taken from CouchDB.
%%
%% TODO: shouldn't we use a one_for_one policy here?
%%---------------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_all, 10, 3600},
          [{couch_config, {?MODULE, start_config_wrapper, []},
            permanent, brutal_kill, worker, [couch_config]},
           {couch_drv, {couch_drv, start_link, []},
            permanent, brutal_kill, worker, [couch_drv]},
           {couch_db_update_event, {gen_event, start_link,
                                    [{local, couch_db_update}]},
            permanent, brutal_kill, worker, dynamic},
           {couch_task_status, {couch_task_status, start_link, []},
            permanent, brutal_kill, worker, [couch_task_status]},
           {couch_server, {couch_server, sup_start_link, []},
            permanent, 1000, worker, [couch_server]}]}}.

%%---------------------------------------------------------------------------
%% @doc Sets any missing config values to sensible defaults.
%%
%% This is a work around for any values that CouchDB assumes to be in config.
%% --------------------------------------------------------------------------

start_config_wrapper() ->
    % Start config process using an ini files in config.
    case application:get_env(ini) of
        {ok, IniFiles} ->
            {ok, Pid} = couch_config:start_link(IniFiles);
        undefined ->
            {ok, Pid} = couch_config:start_link([])
    end,

    % couchlib supports a handful of commonly used config params.
    apply_config(application:get_all_env()),

    % Modify database_dir = '.' to file:get_cwd() -> this works around a
    % bug in couch_server:all_databases/0 when database_dir is '.'.
    case couch_config:get("couchdb", "database_dir", ".") of
        "." -> 
            {ok, Cwd} = file:get_cwd(),
            couch_config:set("couchdb", "database_dir", Cwd);
        _ -> ok
    end,

    % No default value for max_dbs_open - use value from default.ini.
    set_missing_config("couchdb", "max_dbs_open", "100"),

    % No default value for view_index_dir, use same val as database_dir.
    set_missing_config("couchdb", "view_index_dir",
                       couch_config:get("couchdb", "database_dir", ".")),

    % Tell couch where the lib dir is (used by icu driver)
    set_missing_config("couchdb", "util_driver_dir",
                       couchlib_util:couch_lib_dir()),
    {ok, Pid}.

%%---------------------------------------------------------------------------
%% @doc Sets missing required httpd config values.
%% --------------------------------------------------------------------------

start_httpd_wrapper() ->
    % Default auth handler is required to run anything over http.
    set_missing_config("httpd", "authentication_handlers",
                       "{couch_httpd_auth, "
                       "default_authentication_handler}"),
    % Handler for root is required to avoid unhandled error.
    set_missing_config("httpd_global_handlers", "/",
                       "{couch_httpd_misc_handlers, handle_welcome_req, "
                       "<<\"Welcome\">>}"),
    % Futon requires an auth db value, though it can be blank.
    set_missing_config("couch_httpd_auth", "authentication_db", ""),
    couch_httpd:start_link().

%%---------------------------------------------------------------------------
%% @doc Starts the couch query servers, making sure that a "couchlib" native query
%% server is configured.
%% --------------------------------------------------------------------------

start_query_servers_wrapper() ->
    % Default auth handler is required to run anything over http.
    set_missing_config("native_query_servers", "couchlib",
                       "{couchlib_nqs, start_link, []}"),
    couch_query_servers:start_link().

%%---------------------------------------------------------------------------
%% Private functions
%%---------------------------------------------------------------------------

set_missing_config(S, K, Val) ->
    case couch_config:get(S, K) of
        undefined -> couch_config:set(S, K, Val);
        _ -> ok
    end.

apply_config(Config) -> lists:map(fun apply_setting/1, Config).

apply_setting({database_dir, Dir}) ->
    couch_config:set("couchdb", "database_dir", Dir);
apply_setting({delayed_commits, Flag}) ->
    couch_config:set("couchdb", "delayed_commits", boolean_to_string(Flag));
apply_setting(_) -> ok.

boolean_to_string(true) -> "true";
boolean_to_string(false) -> "false".
