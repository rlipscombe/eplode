-module(eplode).
-export([start/0]).

-define(APPLICATION, eplode).

start() ->
    {ok, _} = application:ensure_all_started(?APPLICATION).
