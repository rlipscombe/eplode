-module(eplode_playlists).
-export([new/0, put/3]).

-record(eplode_playlists, { inner :: dict:dict() }).
-define(eplode_playlists_(Term), #eplode_playlists{ inner = Term }).

new() ->
    #eplode_playlists{ inner = dict:new() }.

put(ParentFid, ChildFids, ?eplode_playlists_(Playlists)) ->
    ?eplode_playlists_(dict:store(ParentFid, ChildFids, Playlists)).
