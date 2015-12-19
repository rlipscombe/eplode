# Serial Connection to empeg

I'm using a "Magic Control Technology Corp. MCT-232 Serial Port", which
enumerates as `/dev/ttyUSB1` (I looked in the dmesg output as I connected it).

You could use `minicom` (which requires messing around with configuration files
as root), but it turns out that `screen` works just as well:

    screen /dev/ttyUSB1 115200
