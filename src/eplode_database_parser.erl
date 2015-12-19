-module(eplode_database_parser).
-export([parse/2]).

%% @doc Parse the "/empeg/var/database" (or "database3") file.
%% We need to do this before we can understand the 'playlists' file. Because we
%% build the database records as tag => value maps, we need the parsed 'tags'
%% file.
parse(RawDatabase, Tags) ->
    parse_database(Tags, RawDatabase, first_fid(), new_record(), eplode_db:new()).

%% @doc FIDs start at 0x0 and are incremented by 0x10.
first_fid() -> 16#0.
next_fid(Fid) -> Fid + 16#10.

%% @doc We build the database records as tag => value maps.
new_record() -> maps:new().
update_record(Tag, Data, Record) -> maps:put(Tag, Data, Record).

%% @doc Each record is a list of {TagIndex, TagLength, TagData} tuples.
%% TagIndex marks the end of the record (and TagLength, TagData are omitted).
%%
%% The database ends when there are no more records to read.
%%
%% The database file begins with a single record with type=<<"illegal">>,
%% followed by 15 empty records. This means that the first valid record in the
%% file has FID 0x100. By convention, this refers to the root playlist.
%%
%% Empty records are stored as a single byte, value 255.
%%
%% Because the 'database' file is written in FID order and is not sparse, there
%% may be empty records anywhere in the file. An empty record is caused by a
%% track or playlist being deleted, where the FID has not been reused by a new
%% track.
%%
%% Note that when parsing the playlists list, you might find a reference to an
%% empty record. This is probably caused by a database rebuild on the player,
%% when it can't find the referenced track. The player's database rebuild logic
%% doesn't remove deleted or missing items from playlists (emplode does).
parse_database(_Tags, <<>>, Fid, Record, Database) ->
    % end of database
    eplode_db:put(Fid, Record, Database);
parse_database(Tags, <<Index:8, Rest/binary>>, Fid, Record, Database)
  when Index =:= 16#FF, map_size(Record) =/= 0 ->
    % end of record
    parse_database(Tags, Rest, next_fid(Fid), new_record(), eplode_db:put(Fid, Record, Database));
parse_database(Tags, <<Index:8, Rest/binary>>, Fid, Record, Database)
  when Index =:= 16#FF, map_size(Record) =:= 0 ->
    % empty record
    parse_database(Tags, Rest, next_fid(Fid), new_record(), Database);
parse_database(Tags, <<Index:8, Length:8, Data:Length/binary, Rest/binary>>, Fid, Record, Database) ->
    % tag => value entry
    Tag = array:get(Index, Tags),
    NewRecord = update_record(Tag, Data, Record),
    parse_database(Tags, Rest, Fid, NewRecord, Database).
