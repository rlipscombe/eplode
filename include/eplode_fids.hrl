% <<"3.00-alpha11\n\n">>
-define(FID_VERSION,            0).

% <<"hwrev : 07\n
%    serial: 80000384\n
%    build : 3994f8d3\n
%    id    : 0b038cd3-0dc19b07-ca3a73c9-c4e5a04a\n
%    ram   : 12288K\n
%    flash : 1024K\n
%    drives: 7\n
%    image : 20706d65\n">>
-define(FID_ID,                 1).

% /empeg/var/tags
-define(FID_TAGINDEX,           2).

% /empeg/var/{database,database3}
-define(FID_STATICDATABASE,     3).
-define(FID_DYNAMICDATABASE,    4).

% /empeg/var/playlists
-define(FID_PLAYLISTDATABASE,   5).

% /empeg/var/config.ini
-define(FID_CONFIGFILE,         6).

% <<"empeg-car-2", 0>>
-define(FID_PLAYERTYPE,         7).
-define(FID_VISUALLIST,         8).

-define(FID_ROOTPLAYLIST,  16#100).
