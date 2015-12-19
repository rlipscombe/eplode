-module(eplode_playlists_parser).
-export([parse/2]).

%% @doc Parse the "/empeg/var/playlists" file.
parse(RawPlaylists, Database) ->
    % We need to iterate over the 'database' file, *in order*.
    % For each entry with type="playlist", get the length value.
    % This is the size (in bytes) of the playlist in the 'playlists' file.
    % Each playlist is made up of little-endian FID values, which refer back to
    % the 'database' file.

    State0 = {RawPlaylists, [], Database},
    array:foldl(fun parse_playlist/3, State0, Database).

parse_playlist(Fid, #{<<"type">> := <<"playlist">>} = Record, {Raw, Playlists, Database} = _State) ->
    % Length is a binary string (all tag values are), in bytes.
    Length = binary_to_integer(maps:get(<<"length">>, Record)),

    % Logging:
    #{<<"title">> := Title} = Record,
    io:format("~.16B ~p is a playlist, length ~B bytes\n",
              [Fid bsl 4, Title, Length]),

    % Get those bytes from the 'playlists' data.
    <<PlaylistBytes:Length/binary, NextRaw/binary>> = Raw,

    % That's a sequence of FIDs, so...let's figure it out:
    dump_playlist(PlaylistBytes, Database),
    {NextRaw, Playlists, Database};
parse_playlist(_Fid, _Record, State) ->
    State.

dump_playlist(<<>>, _Database) ->
    ok;
dump_playlist(<<ChildFid:32/little, Rest/binary>>, Database) ->
    case eplode_db:get(ChildFid, Database) of
        undefined ->
            io:format("    ~.16B undefined\n", [ChildFid]);
        ChildRecord ->
            #{<<"title">> := Title} = ChildRecord,
            io:format("    ~.16B ~p\n", [ChildFid, Title])
    end,
    dump_playlist(Rest, Database).
