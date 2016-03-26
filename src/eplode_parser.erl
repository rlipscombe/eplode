-module(eplode_parser).
-export([parse/3]).
-include("eplode_model.hrl").

-spec parse(RawTags :: binary(), RawPlaylists :: binary(), RawDatabase :: binary()) -> #eplode_model{}.
parse(RawTags, RawPlaylists, RawDatabase) ->
    Tags = eplode_tags_parser:parse(RawTags),
    Database = eplode_database_parser:parse(RawDatabase, Tags),
    Playlists = eplode_playlists_parser:parse(RawPlaylists, Database),

    #eplode_model{ tags = Tags, database = Database, playlists = Playlists}.
