-module(eplode_via).
-export([start_link/0]).

% being a gen_server
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).

% being a 'via' module.
-export([register_name/2,
         unregister_name/1,
         whereis_name/1,
         send/2]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    State = no_state,
    {ok, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec register_name(Name :: term(), Pid :: pid()) -> yes | no.
register_name(Name, Pid) ->
    gen_server:call(?SERVER, {register_name, Name, Pid}).

-spec unregister_name(Name :: term()) -> term().
unregister_name(Name) ->
    gen_server:call(?SERVER, {unregister_name, Name}).

-spec whereis_name(Name :: term()) -> pid() | undefined.
whereis_name(Name) ->
    gen_server:call(?SERVER, {whereis_name, Name}).

-spec send(Name :: term(), Msg :: term()) -> pid().
send(Name, Msg) ->
    gen_server:call(?SERVER, {send, Name, Msg}).
