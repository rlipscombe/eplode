-module(eplode_db).
-export([new/0, put/3, get/2]).
-export([foldl/3]).

%%% @doc The database (as in the 'database' or 'database3' file is stored as an array, indexed by FID >> 4.

-record(eplode_db, { inner :: array:array() }).
-define(eplode_db_(Term), #eplode_db{ inner = Term }).

new() ->
    #eplode_db{ inner = array:new() }.

put(Fid, Record, ?eplode_db_(Database)) when map_size(Record) =/= 0 ->
    ?eplode_db_(array:set(Fid bsr 4, Record, Database));
put(_Fid, Record, ?eplode_db_(Database)) when map_size(Record) =:= 0 ->
    % empty record; don't bother storing it.
    ?eplode_db_(Database).

get(Fid, Database) ->
    array:get(Fid bsr 4, Database).

foldl(Fun, Acc0, ?eplode_db_(Database)) ->
    array:foldl(Fun, Acc0, Database).
