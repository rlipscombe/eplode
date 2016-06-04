#!/usr/bin/env escript

% "/home/roger/Music/Artists/N.W.A/Straight Outta Compton (20th Anniversary Edition)/01 - Straight Outta Compton.mp3"
main([Path]) ->
    % Open the file. We don't (yet) need to read the whole thing.
    {ok, File} = file:open(Path, [read, raw, binary]),

    % An ID3v2 tag header is 10 bytes.
    {ok, Header} = file:read(File, 10),

    % Parse the header.
    <<"ID3", VMaj:8/integer, VMin:8/integer,
        A:1/integer, B:1/integer, C:1/integer, D:1/integer, 0:4,
        SizeSS:4/binary>> = Header,

    % We only deal with v2.3.0 at the moment
    VMaj = 3,
    VMin = 0,

    Size = parse_synchsafe(SizeSS),

    A = 0,  % we don't deal with unsynchronization
    B = 0,  % we don't deal with extended headers
    C = 0,  % we don't deal with experimental stuff
    D = 0,  % we don't deal with footers, which are an ID3v2.4.0 thing, anyway.

    % The header is immediately (assuming A=B=C=D=0) followed by the frames.
    {ok, Frames} = file:read(File, Size),

    % Loop over the frames.
    next_frame_23(Frames),

    % A frame is a 4 character ID, followed by a sync-safe size (4 bytes), followed by flags (2 bytes)
    %io:format("~p\n", [Frames]),

    file:close(File).

parse_synchsafe(<<0:1, S1:7/integer,
                  0:1, S2:7/integer,
                  0:1, S3:7/integer,
                  0:1, S4:7/integer>>) ->
    <<Size:32/integer>> = <<0:4, S1:7/integer, S2:7/integer, S3:7/integer, S4:7/integer>>,
    Size.

% In ID3v2.3.0, frame sizes are *not* synchsafe.
next_frame_23(<<FrameID:4/binary, Size:32/integer,
             A:1/integer, B:1/integer, C:1/integer, 0:5/integer,
             I:1/integer, J:1/integer, K:1/integer, 0:5/integer,
             Rest/binary>>)
  when Size =/= 0, A =:= 0, B =:= 0, C =:= 0, I =:= 0, J =:= 0, K =:= 0 ->
    io:format("~s ", [FrameID]),
    io:format("~b ", [Size]),
    <<Data:Size/binary, Frames/binary>> = Rest,
    parse_frame_data(FrameID, Size, Data),
    next_frame_23(Frames);
next_frame_23(_) ->
    ok.

parse_frame_data(<<"TXXX">> = _FrameID, _Size, Data) ->
    io:format("~p\n", [Data]);
parse_frame_data(<<"T", _/binary>> = _FrameID, _Size, <<0:8, Latin1/binary>> = _Data) ->
    Text = unicode:characters_to_binary(Latin1, latin1, utf8),
    io:format("~ts\n", [Text]);
parse_frame_data(<<"T", _/binary>> = _FrameID, _Size, <<1:8, 16#ff, 16#fe, Unicode/binary>> = _Data) ->
%    {Encoding, Length} = unicode:bom_to_encoding(Unicode),
%
    Text = unicode:characters_to_binary(Unicode, {utf16, little}, utf8),
    io:format("~ts\n", [Text]);
parse_frame_data(_FrameID, _Size, Data) ->
    io:format("~p\n", [Data]).
