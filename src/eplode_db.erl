-module(eplode_db).
-export([new/0, put/3, get/2]).
-export([foldl/3]).

%%% @doc The database (as in the 'database' or 'database3' file) is stored as
%%% an array, indexed by FID >> 4.
-record(eplode_db, { inner :: array:array() }).
-define(eplode_db_(Term), #eplode_db{ inner = Term }).

%%% @todo Why do we shift the FID? 'array' can be sparse, apparently, so is
%%% there any benefit?
-define(fid_to_index(Fid), Fid bsr 4).
-define(index_to_fid(Index), Index bsl 4).

new() ->
    #eplode_db{ inner = array:new() }.

put(Fid, Record, ?eplode_db_(Database)) when map_size(Record) =/= 0 ->
    ?eplode_db_(array:set(?fid_to_index(Fid), Record, Database));
put(_Fid, Record, ?eplode_db_(Database)) when map_size(Record) =:= 0 ->
    % empty record; don't bother storing it.
    ?eplode_db_(Database).

get(Fid, ?eplode_db_(Database)) ->
    array:get(?fid_to_index(Fid), Database).

foldl(Fun, Acc0, ?eplode_db_(Database)) ->
    array:foldl(
      fun(Index, Record, Acc) ->
              Fun(?index_to_fid(Index), Record, Acc)
      end, Acc0, Database).
