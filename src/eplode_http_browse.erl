-module(eplode_http_browse).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Type, Req, Opts) ->
    State = Opts,
    {ok, Req, State}.

handle(Req, State) ->
    {Id, _} = cowboy_req:binding(id, Req),
    % find the process registered with that name.
    Result = gen_server:call({via, eplode_via, Id}, hello),
    lager:notice("~p", [Result]),
    Body = Result,
    Headers = [{<<"content-type">>, <<"text/plain">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
