-module(eplode_build).
-export([from_model/1]).
-include("eplode_model.hrl").

from_model(#eplode_model{ tags = Tags, database = Database, playlists = Playlists }) ->
    RawTags = build_tags(Tags),
    RawDatabase = build_database(Tags, Database),
    RawPlaylists = build_playlists(Playlists, Database),
    {RawTags, RawDatabase, RawPlaylists}.

build_tags(Tags) ->
    array:foldl(
      fun(_Index, Tag, <<>>) ->
              Tag;
         (_Index, Tag, Acc) ->
              <<Acc/binary, <<"\n">>/binary, Tag/binary>>
      end, <<>>, Tags).

build_database(Tags, Database) ->
    % tags are written in alphabetical order by tag name; this is not a hard
    % requirement, but it's what emplode does, so it makes it easier to compare
    % the values.
    % To make this easier, we'll build a list of the tag indexes, in alphabetical
    % order, along with the numbers.
    OrderedTags = sort_tags(Tags),
    eplode_db:foldl(
      fun(_Fid, Item, Acc) ->
              RawItem = build_item(Item, OrderedTags),
              <<Acc/binary, RawItem/binary, 16#FF>>
      end, <<>>, Database).

sort_tags(Tags) ->
    IndexValuePairs = array:to_orddict(Tags),
    SortFun = fun({_IndexA, ValueA}, {_IndexB, ValueB}) ->
                      ValueA =< ValueB
              end,
    SortedPairs = lists:sort(SortFun, IndexValuePairs),
    FilterFun = fun({_Index, <<>>}) -> false;
                   ({_Index, _Value}) -> true
                end,
    lists:filter(FilterFun, SortedPairs).

build_item(undefined, _OrderedTags) ->
    <<>>;
build_item(Item, OrderedTags) ->
    % {tagindex, taglen, value} ... 0xFF
    lists:foldl(
      fun({TagIndex, TagName}, Acc) ->
              case maps:get(TagName, Item, undefined) of
                  undefined ->
                      Acc;
                  TagValue ->
                      TagLength = byte_size(TagValue),
                      <<Acc/binary, TagIndex:8/unsigned, TagLength:8/unsigned, TagValue/binary>>
              end
      end, <<>>, OrderedTags).


build_playlists(Playlists, Database) ->
    % It's a simple concatenation of the child fids (written as little-endian
    % 32-bit integers) for each playlist. The playlists are in fid order,
    % starting with 0x100.
    % Playlists is a dictionary, Fid -> [ChildFid], so we need to iterate over
    % the database to find the playlists in fid order.
    eplode_db:foldl(
      fun(Fid, #{<<"type">> := <<"playlist">>}, Acc) ->
              ChildFids = eplode_playlists:get(Fid, Playlists),
              ChildFidsAsBin = << <<ChildFid:32/unsigned-little>> || ChildFid <- ChildFids>>,
              <<Acc/binary, ChildFidsAsBin/binary>>;
         (_Fid, _Item, Acc) ->
              Acc
      end, <<>>, Database).
