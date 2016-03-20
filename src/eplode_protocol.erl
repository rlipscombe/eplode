-module(eplode_protocol).
-export([connect/1]).
-export([readfid/4, statfid/2]).
-export([statfs/1]).
-export([build_packet/4]).

-include("eplode_protocol.hrl").

-define(PSOH, 16#02).
-define(MAX_PACKET_ID, 4294967295).  % 2^32-1

-define(INITIAL_TIMEOUT_MS, 2000).
-define(RECV_LEN, 0).   % set this to 1 to exercise the fragmentation reassembly.

connect(Address) ->
    Port = 8300,
    ConnectOpts = [binary, {nodelay, true}, {active, false}],
    ConnectTimeout = 30000,

    {ok, Socket} = gen_tcp:connect(Address, Port, ConnectOpts, ConnectTimeout),
    {ok, Socket}.

-type fid() :: non_neg_integer().

-spec readfid(inet:socket(), fid(), non_neg_integer(), non_neg_integer()) ->
    {ok, non_neg_integer()} | {error, eplode:status()}.
%% @doc transfer file data *from* the player.
readfid(Socket, Fid, Offset, Size) ->
    Req = <<Fid:32/unsigned-little,
            Offset:32/unsigned-little,
            Size:32/signed-little>>,
    make_call(Socket, ?OP_READFID, Req).

statfid(Socket, Fid) ->
    Req = <<Fid:32/unsigned-little>>,
    make_call(Socket, ?OP_STATFID, Req).

statfs(Socket) ->
    make_call(Socket, ?OP_STATFS, <<>>).

make_call(Socket, OpCode, Req) ->
    PacketId = new_packet_id(),
    Packet = build_packet(OpCode, ?OPTYPE_REQUEST, PacketId, Req),
    ok = gen_tcp:send(Socket, Packet),
    wait_response(Socket).

wait_response(Socket) ->
    wait_response(Socket, ?INITIAL_TIMEOUT_MS, <<>>).

wait_response(Socket, TimeoutMs, Buffer) ->
    % We expect zero or more progress reports, followed by the response. The
    % progress reports give us hints about extended timeouts, and we need to
    % deal with fragmentation.
    {ok, Bytes} = gen_tcp:recv(Socket, ?RECV_LEN, TimeoutMs),
    parse_message(Socket, <<Buffer/binary, Bytes/binary>>, TimeoutMs).

parse_message(Socket,
              <<?PSOH, Len:16/unsigned-little, OpCode:8, OpType:8,
                _PacketId:32/unsigned-little, Data:Len/binary, _CRC:16/unsigned-little, Rest/binary>>,
              _OldTimeoutMs) when OpType =:= ?OPTYPE_PROGRESS ->
    handle_progress_message(Socket, OpCode, Data, Rest);
parse_message(_Socket,
              <<?PSOH, Len:16/unsigned-little, OpCode:8, OpType:8,
                _PacketId:32/unsigned-little, Data:Len/binary, _CRC:16/unsigned-little, _Rest/binary>>,
              _OldTimeoutMs) when OpType =:= ?OPTYPE_RESPONSE ->
    handle_response_message(OpCode, Data);
parse_message(Socket,
              <<?PSOH, Len:16/unsigned-little, OpCode:8, OpType:8,
                _PacketId:32/unsigned-little, Data:Len/binary, _CRC:16/unsigned-little, Rest/binary>>,
              TimeoutMs) ->
    io:format("Unhandled: OpCode: ~B, OpType: ~B, Data: ~p\n", [OpCode, OpType, Data]),
    wait_response(Socket, TimeoutMs, Rest);
parse_message(Socket, Buffer, TimeoutMs) ->
    %io:format("~p", [Buffer]),
    % We don't have enough; go around again.
    wait_response(Socket, TimeoutMs, Buffer).

handle_progress_message(Socket, OpCode, Data, Rest) ->
    <<NewTimeoutSecs:32/unsigned-little,
      Stage:32/unsigned-little, StageMax:32/unsigned-little,
      Current:32/unsigned-little, Max:32/unsigned-little,
      _String/binary>> = Data,
    io:format("~B: ~B / ~B [~B / ~B]\n", [OpCode, Stage, StageMax, Current, Max]),
    io:format("NewTimeoutSecs: ~B\n", [NewTimeoutSecs]),
    wait_response(Socket, NewTimeoutSecs * 1000, Rest).

handle_response_message(?OP_READFID, Data) ->
    <<Status:32/unsigned-little,
      Fid:32/unsigned-little,
      Offset:32/unsigned-little,
      Size:32/signed-little,
      Bytes/binary>> = Data,
    handle_response(Status, {ok, Fid, Offset, Size, Bytes});
handle_response_message(?OP_STATFID, Data) ->
    <<Status:32/unsigned-little,
      _Fid:32/unsigned-little,
      Size:32/unsigned-little>> = Data,
    handle_response(Status, {ok, Size});
handle_response_message(?OP_STATFS, Data) ->
    <<Size0:32/unsigned-little, Space0:32/unsigned-little, BlockSize0:32/unsigned-little,
      Size1:32/unsigned-little, Space1:32/unsigned-little, BlockSize1:32/unsigned-little>> = Data,
    % Size and Space are in blocks. Multiply by BlockSize to get the value in bytes.
    {ok, [{Size0, Space0, BlockSize0}, {Size1, Space1, BlockSize1}]};
handle_response_message(OpCode, Data) ->
    io:format("Unhandled: ~2.16.0b: ~s\n", [OpCode, eplode_format:to_delimited_hex(" ", Data)]),
    error({unhandled_response_opcode, OpCode}).

handle_response(Status, Result) when Status =:= ?STATUS_OK ->
    Result;
handle_response(Status, _Result) ->
    {error, Status}.

build_packet(OpCode, OpType, PacketId, Data) ->
    Header = <<OpCode:8, OpType:8, PacketId:32/unsigned-little>>,
    % CRC16 starting from opcode...
    CRC = eplode_crc:crc16(<<Header/binary, Data/binary>>),
    DataSize = byte_size(Data),
    <<?PSOH, DataSize:16/unsigned-little, Header/binary, Data/binary, CRC:16/unsigned-little>>.

%% @doc Create a new packet ID.
%% The packet ID serves as a request correlation ID.
new_packet_id() ->
    % The packet ID does _not_ need to be strictly ascending, so we'll use the
    % RNG to create one.
    % Historically, the ID avoids serial control characters by taking a 28-bit
    % number, spreading it over 4 bytes and ensuring that the high bit is set
    % in each byte.
    % We don't need to do that for the TCP/IP protocol.
    rand:uniform(?MAX_PACKET_ID).
