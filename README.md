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

    {Tags, Database, Playlists} = eplode_parser:parse(RawTags, RawPlaylists, RawDatabase).

Or you can play with the native empeg protocol:

    Address = {10,0,0,29}.
    {ok, C} = eplode_protocol:connect(Address).

    {ok, _, RawTags} = eplode_protocol:readfid(C, 2).
    {ok, _, RawDatabase} = eplode_protocol:readfid(C, 3).
    {ok, _, RawPlaylists} = eplode_protocol:readfid(C, 5).
    {Tags, Database, Playlists} = eplode_parser:parse(RawTags, RawPlaylists, RawDatabase).

## External Resources

- The original empeg home page: http://www.empeg.com/
- The (unofficial) Rio Car FAQ: http://www.riocar.org/modules.php?op=modload&name=FAQ&file=index
- The "Hijack" kernel: http://empeg-hijack.sourceforge.net/
- The unofficial empeg BBS: http://empegbbs.com/
