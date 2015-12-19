-module(eplode_http_client).
-export([get_tags/1, get_playlists/1, get_database/1]).

get_tags(BaseUrl) ->
    get_file(BaseUrl, "/empeg/var/tags").

get_playlists(BaseUrl) ->
    get_file(BaseUrl, "/empeg/var/playlists").

get_database(BaseUrl) ->
    get_file(BaseUrl, "/empeg/var/database3").

get_file(BaseUrl, Path) ->
    {ok, 200, _, Ref} = hackney:get([BaseUrl, Path]),
    {ok, Body} = hackney:body(Ref),
    Body.
