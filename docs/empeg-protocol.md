# Empeg Car Communication Protocol

It's a bit crufty. Prefer HTTP or FTP over Hijack, except that the empeg car
protocol automatically distributes files between two hard disks.

Port 8300 is used for discovery (UDP), and for comms (TCP). There's also port
8301, which is used for "fast" comms, which (effectively) allows
`sendfile`-like semantics for sending files to the player.

## Connecting

While noodling around in the shell, we'll use `{active, true}`, but we might do
something different in real code.

    Address = {10,0,0,29}.
    Port = 8300.
    ConnectOpts = [binary, {nodelay, true}, {active, true}].
    ConnectTimeout = 30000.
    
    {ok, Socket} = gen_tcp:connect(Address, Port, ConnectOpts, ConnectTimeout).

Or:

    Address = {10,0,0,29}.
    {ok, Socket} = eplode_protocol:connect(Address).

    {ok, Len} = eplode_protocol:statfid(Socket, 7).
    eplode_protocol:readfid(Socket, 7, 0, Len).

## Packet format

- 1 byte: PSOH (0x02) -- sent before each packet, afaict, but not strictly part of the "packet"
- 2 bytes: payload length (not including header)
- 1 byte: opcode
- 1 byte: type: request=0, response=1, progress=2
- 4 bytes: packet ID
- N bytes: data
- CRC

## Get player type

To get the player type, we download FID 7 (`FID_PLAYERTYPE`):

    % We're going to send an OP_STATFID:
    Data = <<7:32/unsigned-little>>.    % FID 7
    DataSize = byte_size(Data).
    PacketId = 16#80810095.             % Arbitrary; see below.
    % OP_STATFID, OPTYPE_REQUEST, PacketId
    Header = <<16#07:8, 16#00:8, PacketId:32/unsigned-little>>.
    % CRC16 starting from opcode...
    CRC = eplode_crc:crc16(<<Header/binary, Data/binary>>).
    Packet = <<16#02, DataSize:16/unsigned-little, Header/binary, Data/binary, CRC:16/unsigned-little>>.
    ok = gen_tcp:send(Socket, Packet).

## Response

<<2,84,0,7,2,149,0,129,128,60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
                 0,0,0,52,216,17,2,16,1,18,2,175,128,128,128,60,233,27,2,88,
                 252,255,189,64,252,255,189,236,254,13,2,208,215,17,2,24,65,
                 29,2,136,252,255,189,120,218,34,2,108,252,255,189,92,252,255,
                 189,24,255,13,2,136,252,255,189,120,218,34,2,98,150>>

    02              PSOH
    54 00           DataSize
    07 02           OP_STATFID, OPTYPE_PROGRESS
    95 00 81 80     PacketId (correlates with above)
    
    3c 00 00 00     newtimeout
    00 00 00 00     stage
    00 00 00 00     stage_maximum
    00 00 00 00     current
    01 00 00 00     maximum

    string -- apparently contains garbage -- unused by emptool
    34 d8 11 02 10 01 12 02 af 80 80 80 3c e9 1b 02
    58 fc ff bd 40 fc ff bd ec fe 0d 02 d0 d7 11 02
    18 41 1d 02 88 fc ff bd 78 da 22 02 6c fc ff bd
    5c fc ff bd 18 ff 0d 02 88 fc ff bd 78 da 22 02
    62 96

<<2,12,0,7,1,149,0,129,128,0,0,0,0,7,0,0,0,12,0,0,0,124,124>>

    02              PSOH
    0c 00           DataSize
    07 01           OP_STATFID, OPTYPE_RESPONSE
    95 00 81 80     PacketId

    00 00 00 00     result - OK
    07 00 00 00     FID - 7
    0c 00 00 00     size - 12 bytes
    7c 7c

The only reason we got the size is so that we could (a) allocate enough memory
up front, and (b) report download progress. Download progress for a 12-byte
file is a moot point, but let's continue.

Then we issue an `OP_READFID` message for the data, which -- for the purposes
of player type -- returns a null-terminated ASCII string containing
"empeg-car-2".

## Request IDs

For, presumably, serial-connection-based reasons, the request ID guarantees
that certain bits are set high. Actually, there's some super weirdness in
emptool's `ping` implementation that attempts to ensure that various bits in
the request and in the CRC are set.

## External Links

- http://blog.differentpla.net/blog/2007/12/16/packet-format/
- http://blog.differentpla.net/blog/2007/12/16/an-empeg-reports-progress-of-op_statfid/
- http://blog.differentpla.net/blog/2007/12/16/an-empeg-reports-results-of-op_statfid/
- http://blog.differentpla.net/blog/2007/12/16/getting-the-player-type-of-an-empeg/
