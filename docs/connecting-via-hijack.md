# Connecting via Hijack

The original empeg protocol is a bit odd. It's simpler to require that the
empeg be running a Hijack kernel, which provides FTP and HTTP access to the
filesystem.

http://empeg-hijack.sourceforge.net/

Step one, then, is downloading the empeg's existing database, which lives in
`/empeg/var`, in the `database`, `playlists` and `tags` files.

http://www.differentpla.net/content/2005/02/empeg-file-structures

Hijack provides directory listing functionality as well, which makes life
easier.

To do this manually, figure out the IP address of the empeg (it's under 'About'
on the main menu), then point a web browser at that IP address.

To do this from Erlang, I'm going to use the **hackney** HTTP client:

    Url = "http://10.0.0.21".
    {ok, Status, Headers, Ref} = hackney:get(Url).
    hackney:body(Ref).

## On connection timeouts

It's worth noting that it takes a short while for the empeg to respond
initially. This might be because it has to spin up the disk. We'll need to
figure out how to make Hackney be a bit more lenient about that (TODO).

## On closed connections

It appears that Hijack simply closes the connection when rendering directory
listings. This is fine, but hackney doesn't seem to like it. When you call
`hackney:body(Ref)`, it returns `{error, {closed, HTML}}`, which is a bit
sucky.

## On shotgun

This appears to require Erlang 17. Dammit. And it doesn't work: causes Hijack
to return "405 Method Not Allowed".

## To enable telnet on the empeg

    ftp 10.0.0.21
    cd /empeg/var
    get config.ini

...edit the config.ini file locally as follows:

    [hijack]
    ktelnetd_port=23

...then, back in FTP-land:

    quote site rw
    put config.ini
    quote site ro

...and then, reboot:

    quote site reboot

Then you can:

    telnet 10.0.0.21

