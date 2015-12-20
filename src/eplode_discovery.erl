-module(eplode_discovery).
-export([start_link/0]).
-export([notify/1]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { players }).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

notify({IP, Name, Id}) ->
    gen_server:cast(?SERVER, {found, {IP, Name, Id}}).

init([]) ->
    % State is a dict :: player -> last_seen
    State = #state{ players = dict:new() },
    {ok, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({found, Player}, #state{ players = Players } = State) ->
    % @todo What if a player appears on more than one address / IP, etc.?
    Players2 = dict:store(Player, erlang:system_time(seconds), Players),
    {noreply, State#state{ players = Players2 }};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("~p\n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
