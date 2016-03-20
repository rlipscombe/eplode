-module(eplode_playlists_parser).
-export([parse/2]).

%% @doc Parse the "/empeg/var/playlists" file.
parse(RawPlaylists, Database) ->
    % We need to iterate over the 'database' file, *in order*.
    % For each entry with type="playlist", get the length value.
    % This is the size (in bytes) of the playlist in the 'playlists' file.
    % Each playlist is made up of little-endian FID values, which refer back to
    % the 'database' file.

    State1 = {RawPlaylists, eplode_playlists:new(), Database},
    State2 = eplode_db:foldl(fun parse_playlist/3, State1, Database),
    {_, Playlists, _} = State2,
    Playlists.

parse_playlist(Fid, #{<<"type">> := <<"playlist">>} = Record, {Raw, Playlists, Database} = _State) ->
    % Length is a binary string (all tag values are), in bytes.
    Length = binary_to_integer(maps:get(<<"length">>, Record)),

    % Logging:
    %#{<<"title">> := Title} = Record,
    %io:format("~.16B ~p is a playlist, length ~B bytes\n",
    %          [Fid bsl 4, Title, Length]),

    % Get those bytes from the 'playlists' data.
    <<PlaylistBytes:Length/binary, NextRaw/binary>> = Raw,
    Children = [ChildFid || <<ChildFid:32/little>> <= PlaylistBytes],
    {NextRaw, eplode_playlists:put(Fid, Children, Playlists), Database};
parse_playlist(_Fid, _Record, State) ->
    State.
