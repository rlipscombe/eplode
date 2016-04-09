-module(eplode_dtl_template).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Type, Req, Opts) ->
    State = Opts,
    {ok, Req, State}.

handle(Req, Template = State) ->
    to_html(Req, Template, [], State).

to_html(Req, Template, Props, State) ->
    {ok, Body} = Template:render(Props),
    Headers = [{<<"content-type">>, <<"text/html">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
