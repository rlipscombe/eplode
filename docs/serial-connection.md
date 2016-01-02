# Serial Connection to empeg

I'm using a "Magic Control Technology Corp. MCT-232 Serial Port", which
enumerates as `/dev/ttyUSB1` (I looked in the dmesg output as I connected it).

You can connect using `screen`:

    screen /dev/ttyUSB1 115200

...or `minicom`:

    minicom -b 115200 -o -D /dev/ttyUSB1
