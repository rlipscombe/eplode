-module(eplode_http_browse).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Type, Req, Opts) ->
    State = Opts,
    {ok, Req, State}.

handle(Req, State) ->
    {Id, _} = cowboy_req:binding(id, Req),
    lager:notice("~p", [Id]),
    % @todo XSS?
    Body = Id,
    Headers = [{<<"content-type">>, <<"text/plain">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
