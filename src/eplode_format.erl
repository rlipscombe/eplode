-module(eplode_format).
-export([to_hex/1]).
-export([to_delimited_hex/2]).

%% @doc Convert a binary value to a hex string.
%% Derived from http://stackoverflow.com/a/3771421/8446
to_hex(Bin) when is_binary(Bin) ->
    lists:flatten(to_hex_iolist(Bin)).

to_delimited_hex(Delimiter, Bin) when is_binary(Bin) ->
    lists:flatten(list_join(Delimiter, to_hex_iolist(Bin))).

to_hex_iolist(Bin) when is_binary(Bin) ->
    [io_lib:format("~2.16.0b",[X]) || <<X:8>> <= Bin].

%% @doc Join a list with a delimiter between each element.
list_join(Delimiter, List) ->
    [hd(List), [[Delimiter, X] || X <- tl(List)]].
