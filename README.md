# eplode

emplode, sorta, in Erlang (probably)

## The story so far

Run the application in dev mode:

    make dev

Then, assuming your empeg is at 10.0.0.29, and has Hijack installed:

    BaseUrl = "http://10.0.0.29".

    RawTags = eplode_http_client:get_tags(BaseUrl).
    RawPlaylists = eplode_http_client:get_playlists(BaseUrl).
    RawDatabase = eplode_http_client:get_database(BaseUrl).

    {Tags, Database, Playlists} = eplode_parser:parse(RawTags, RawPlaylists, RawDatabase),
    ok.

## External Resources

- The original empeg home page: http://www.empeg.com/
- The (unofficial) Rio Car FAQ: http://www.riocar.org/modules.php?op=modload&name=FAQ&file=index
- The "Hijack" kernel: http://empeg-hijack.sourceforge.net/
- The unofficial empeg BBS: http://empegbbs.com/
