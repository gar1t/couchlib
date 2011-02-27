-module(couchlib_design).

-export([new/2]).

-define(LANGUAGE, <<"couchlib">>).

%% ----------------------------------------------------------------------------
%% @spec new(doc_name(), [options()]) -> {ok, doc()}
%%
%%   doc_name()      = string()
%%   option()        = {view, view()}
%%   view()          = {view_name(), function()}
%%   view_name()     = string()
%%
%% @doc Creates a new design document.
%% ----------------------------------------------------------------------------

new(DocName, Options) ->
    couchlib_doc:new("_design/" ++ DocName,
                [{<<"language">>, ?LANGUAGE},
                 {<<"views">>, views(Options, [])}]).

views([], Acc) -> Acc;
views([{view, {Name, MapFun}}|T], Acc) ->
    View = {to_bin(Name), [{<<"map">>, encode_fun(MapFun)}]},
    views(T, [View|Acc]);
views([{view, {Name, MapFun, ReduceFun}}|T], Acc) ->
    View = {to_bin(Name), [{<<"map">>, encode_fun(MapFun)},
                           {<<"reduce">>, encode_fun(ReduceFun)}]},
    views(T, [View|Acc]);
views([Other|_], _) -> erlang:exit({bad_option, Other}).

encode_fun(F) when is_function(F) ->
    term_to_binary(F);
encode_fun({M, F}) when is_atom(M) andalso is_atom(F) ->
    list_to_binary(io_lib:format("~p.", [{M, F}]));
encode_fun(Other) ->
    erlang:exit({bad_fun, Other}).

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L);
to_bin(Other) -> erlang:error({badarg, Other}).
