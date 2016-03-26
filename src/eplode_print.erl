-module(eplode_print).
-export([model/1]).
-include("eplode_model.hrl").
-include("eplode_fids.hrl").

model(Model) ->
    Root = get_item(?FID_ROOTPLAYLIST, Model),
    print(?FID_ROOTPLAYLIST, Root, "", Model).

print(Fid, #{<<"title">> := Title, <<"type">> := <<"playlist">>}, Indent, Model) ->
    io:format("~s~.16B ~s\n", [Indent, Fid, Title]),
    ChildFids = get_children(Fid, Model),
    lists:foreach(
      fun(ChildFid) ->
              Child = get_item(ChildFid, Model),
              print(ChildFid, Child, ["  " | Indent], Model)
      end, ChildFids);
print(Fid, #{<<"tracknr">> := TrackNr, <<"artist">> := Artist, <<"title">> := Title} = _Item, Indent, _Model) ->
    io:format("~s~.16B ~s - ~s - ~s\n", [Indent, Fid, TrackNr, Artist, Title]);
print(Fid, #{<<"tracknr">> := TrackNr, <<"title">> := Title} = _Item, Indent, _Model) ->
    io:format("~s~.16B ~s - ~s\n", [Indent, Fid, TrackNr, Title]).

get_item(Fid, #eplode_model{ database = Database }) ->
    eplode_db:get(Fid, Database).

get_children(Fid, #eplode_model{ playlists = Playlists }) ->
    eplode_playlists:get(Fid, Playlists).
