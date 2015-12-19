-module(eplode_db).
-export([new/0, put/3, get/2]).

%%% @doc The database (as in the 'database' or 'database3' file is stored as an array, indexed by FID >> 4.

new() ->
    array:new().

put(Fid, Record, Database) when map_size(Record) =/= 0 ->
    array:set(Fid bsr 4, Record, Database);
put(_Fid, Record, Database) when map_size(Record) =:= 0 ->
    % empty record; don't bother storing it.
    Database.

get(Fid, Database) ->
    array:get(Fid bsr 4, Database).
