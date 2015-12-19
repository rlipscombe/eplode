Documented somewhere on my blog.

emplode treats the database as an acyclic digraph (because it _is_ one). In
Erlang, that'd be in the `digraph` module. I wonder if that's worth a quick
explore?

The empeg file structures are documented here:
http://blog.differentpla.net/blog/2005/02/13/empeg-file-structures/

So, in Erlang. Let's download the database files (by using hackney, because I
like it):

Hmmm. Can we use Hijack (HTTP) to push music?

    {ok, _} = application:ensure_all_started(hackney).

    BaseUrl = "http://10.0.0.29".

## Tags file

    {ok, 200, _, Ref} = hackney:get([BaseUrl, "/empeg/var/tags"]).
    {ok, Body} = hackney:body(Ref).

    Tags = binary:split(Body, <<"\n">>, [global]).

`Tags` is a binary, list of tag names (as referred to in `database3`); it's
one-per-line (`\n` separator), with a bunch of blank lines at the end, making
it 256 lines long. That's because the tag index in database3 is a single byte,
so this is a serialized array of those indexes.

I believe, but I'm not totally sure that the first tag name _must_ be `type`.
It's possible that `length` and `title` _must_ be the next two, but...?

## Database file

    {ok, 200, _, Ref} = hackney:get([BaseUrl, "/empeg/var/database3"]).
    {ok, Body} = hackney:body(Ref).

## Playlists file

    {ok, 200, _, Ref} = hackney:get([BaseUrl, "/empeg/var/playlists"]).
    {ok, Body} = hackney:body(Ref).

## Stuff

Can't do anything with the playlists data without knowing the lengths, so we
need to go through the database. Let's dump that first:

    BaseUrl = "http://10.0.0.29".

    RawTags = eplode_http_client:get_tags(BaseUrl).
    RawPlaylists = eplode_http_client:get_playlists(BaseUrl).
    RawDatabase = eplode_http_client:get_database(BaseUrl).

    {Tags, Database, Playlists} = eplode_parser:parse(RawTags, RawPlaylists, RawDatabase),
    ok.

    array:get(16#690 bsr 4, Database).

    eplode_dump:dump(RawTags, RawPlaylists, RawDatabase).

