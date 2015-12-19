-module(eplode_tags_parser).
-export([parse/1]).

%% @doc Parse the "/empeg/var/tags" file.
parse(RawTags) ->
    % It's an ASCII text file, one tag per line, using "\n" as the line
    % separator.
    % If written by emplode, it's exactly 256 lines long. The trailing lines in
    % the file are empty (single "\n" characters).
    % We return it as an array, because it's relatively small and we need to
    % find items by index.
    % @todo Should we truncate the array?
    array:from_list(binary:split(RawTags, <<"\n">>, [global])).
