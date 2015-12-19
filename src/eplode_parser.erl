-module(eplode_parser).
-export([parse/3]).

parse(RawTags, RawPlaylists, RawDatabase) ->
    Tags = eplode_tags_parser:parse(RawTags),
    Database = eplode_database_parser:parse(RawDatabase, Tags),
    Playlists = eplode_playlists_parser:parse(RawPlaylists, Database),

    % @todo do we want a record for the return value?
    {Tags, Database, Playlists}.
