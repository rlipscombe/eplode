eplode
======

emplode, sorta, in Erlang.

Running It
----------

    make && _rel/eplode_release/bin/eplode_release console

Milestones
----------

- Connect to an empeg, via Hijack, on the local network.

What's the problem we're solving?
---------------------------------

It would be easy to fall into the trap of re-implementing emplode, or JEmplode,
or Rio Music Manager, which would require some kind of tag editing UI and some
kind of playlist management UI. However, that's probably not an optimal use of
my effort. Let's take a step back and look at the actual problem being solved
here:

**The music on my empeg is not up-to-date with the music I have bought
recently.**

What _should_ happen is this:

1. I buy music, usually in the form of a CD, but occasionally as an MP3
   download.
2. The CD needs to be ripped, probably to FLAC.
3. The tags need to be double-checked, and the file needs placing in the music
   library in the correct place.
4. For local playback, I use Clementine, which supports both FLAC and MP3
   files.
5. That music needs to make it to the empeg, which _does_ support FLAC, but MP3
   is probably a better option.

That implies that -- rather than a full-featured tag editing / music management
solution that talks to the empeg -- instead, I need something that simply takes
a folder containing a bunch of MP3 files and ensures that it's sent to the
empeg.

Now, in order for this to be as painless as possible, it suggests that this
should happen whenever the empeg appears on the local network.

So: how to get the music to the empeg?

Bear in mind that the empeg has a custom file layout and database, which means
that simply moving the files to the empeg won't work.

- It is possible to run rsync on the empeg, which implies that if I have a
  local tree that mirrors the expected empeg format, I could simply rsync that
  over.
- I could transfer files using FTP, and build the database locally.
- Or the empeg protocol.
- I don't think Hijack's HTTP protocol allows writing to the empeg.

Thus: I think the correct answer is to have:

1. A process that mirrors a folder full of MP3 files into an empeg-style
   layout.
2. Something that detects that the empeg has appeared on the local network.
3. Something that kicks off an rsync to the empeg.

I've already got something that detects that the empeg has appeared.

Open questions:

- What to do about multiple disks in the empeg?
- What to do if the music won't fit?
- How to kick off rsync?

My test empeg, 'toothgnip' only has one disk; it's a "SAMSUNG HM160HC", so
160GB. My live empeg, 'crowley' has a Transcend SSD in it. I believe it's a
64GB model.

So: how do we get rsync running on the empeg?

- Can we use Hijack to leave it running permanently? There are memory use implications to this.
- Can we use Hijack, somehow, to run it when connected to the LAN / AC power?
- Should we simply use Hijack, via telnet, or ftp site commands, to run it on demand?
- Can we, instead, use a custom init to kill the player and run it?

The player init cycles between bash and the player, when you quit the player
(either by pressing Ctrl+C over serial, or by using the quit option on the
player). Instead, could we have a custom init, and send it signals which cause
it to kill what it's doing and do something else?

That'd require, probably, some C coding. That's awkward.

How to run rsync, in server mode, without daemonizing it? I've got some notes
somewhere about that...

Of course, there's always mp3tofid, which does exactly what I'm describing, but
that's no fun.

There are a bunch of ID3 libraries for Erlang, which is a start. Or I could
write my own, which would be more fun.

Also: rsync. Run it as a port? Or write an rsync client in Erlang? That'd be
awkward, probably.

Alternatively, write a custom synchronisation protocol, and run the other end
on the empeg? Can I get Erlang up on the empeg? Will I need a cross compiler?

Note: I appear to have 12GB RAM in my mk2. I think the mk2a has 16GB.

