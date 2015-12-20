-module(eplode_udp_discovery).
-export([start_link/0]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% To discover empegs, send "?" on port 8300. The empeg will respond with some
%% player information.
-define(DISCOVERY_PORT, 8300).

%% How long before the first discovery packet, and how long between subsequent
%% ones? 5 seconds seems like a reasonable compromise between spotting a player
%% soon after it joins the network, and not spamming the network too much.
%% @todo Consider a variable interval, according to whether we've found a
%% player or not.
-define(INITIAL_DISCOVERY_DELAY_MS, 0).
-define(DISCOVERY_INTERVAL_MS, 5000).

%% We _could_ use {active, true}, but we're always expecting at least two
%% messages (our echo, and the response from a single player), so we use
%% {active, N}, reenabling active mode when we handle a {udp_passive, Socket}
%% message.
%%
%% We don't use {active, true}, or a particularly high value for N, because
%% that can lead to our message queue getting filled up.
-define(ACTIVE_N, 5).

-record(state, { socket }).

%%% @doc This module is responsible for discovering empeg car players on the
%%% local network, by using UDP discovery.
%%%
%%% @todo We notify some other process (how do they register with us?) about
%%% players we've seen. It's responsible for TTL expiring ones that have gone
%%% away.
%%%
%%% @todo I suspect that the registration requires a named process, and a
%%% specific supervision tree.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, Socket} = gen_udp:open(?DISCOVERY_PORT, [binary, {broadcast, true}, {active, false}]),
    ok = inet:setopts(Socket, [{active, ?ACTIVE_N}]),
    timer:send_after(?INITIAL_DISCOVERY_DELAY_MS, refresh),
    State = #state{ socket = Socket },
    {ok, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(refresh, #state{ socket = Socket } = State) ->
    Address = {255,255,255,255},
    ok = gen_udp:send(Socket, Address, ?DISCOVERY_PORT, <<"?">>),
    timer:send_after(?DISCOVERY_INTERVAL_MS, refresh),
    {noreply, State};
handle_info({udp, _Socket, _IP, _Port, <<"?">>}, State) ->
    % It's our (or someone else's) discovery packet. Ignore it.
    {noreply, State};
handle_info({udp, _Socket, IP, _Port, Data}, State) ->
    % It's (probably) a car player. A car player will send
    % <<"id=12345678\nname=empeg-car">>.
    handle_discovery_packet(IP, Data),
    {noreply, State};
handle_info({udp_passive, _Socket}, #state{ socket = Socket } = State) ->
    % {active, N} ran out; restart it.
    ok = inet:setopts(Socket, [{active, ?ACTIVE_N}]),
    {noreply, State};
handle_info(_Info, State) ->
    io:format("~p\n", [_Info]),
    {noreply, State}.

handle_discovery_packet(IP, Data) ->
    case parse_discovery_message(binary:split(Data, <<"\n">>, [global])) of
        {ok, {Name, Id}} -> io:format("~p: ~p (~p)\n", [IP, Name, Id]);
        _ -> ignore
    end.

parse_discovery_message(Lines) ->
    parse_discovery_message(Lines, {undefined, undefined}).

parse_discovery_message([], {Name, Id}) when Name =/= undefined, Id =/= undefined ->
    {ok, {Name, Id}};
parse_discovery_message([], _Acc) ->
    error;
parse_discovery_message([<<"name=", Name/binary>> | Rest], {_, Id}) ->
    parse_discovery_message(Rest, {Name, Id});
parse_discovery_message([<<"id=", Id/binary>> | Rest], {Name, _}) ->
    parse_discovery_message(Rest, {Name, Id});
parse_discovery_message([_ | Rest], Acc) ->
    parse_discovery_message(Rest, Acc).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
