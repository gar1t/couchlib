-module(couchlib_views).

-export([start/0, stop/0]).

-define(VIEW_SUPPORT, [couch_view, couch_task_status, couch_query_servers]).

start() ->
    lists:foreach(fun couchlib_sup:start_service/1, ?VIEW_SUPPORT).

stop() ->
    lists:foreach(fun couchlib_sup:stop_service/1,
                  lists:reverse(?VIEW_SUPPORT)).
