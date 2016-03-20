% opcode (also shown in hex; makes Wireshark easier to read)
-define(OP_READFID, 5).
-define(OP_STATFID, 7).
-define(OP_FSCK,    10).    % 0a
-define(OP_STATFS,  11).    % 0b

-define(OPTYPE_REQUEST,  0).
-define(OPTYPE_RESPONSE, 1).
-define(OPTYPE_PROGRESS, 2).

-define(STATUS_OK, 0).
