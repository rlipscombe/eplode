-module(eplode_http).
-export([start_link/0]).

-define(APPLICATION, eplode).

start_link() ->
    Port = 12121,
    Routes = [
              {'_', [
                     {"/", eplode_http_index, []},
                     {"/browse/:id", eplode_http_browse, []}
                    ]}],
    Dispatch = cowboy_router:compile(Routes),
    {ok, Pid} = cowboy:start_http(eplode_http, 10,
                                 [{port, Port}],
                                 [{env, [{dispatch, Dispatch}]}]),
    {ok, Pid}.
