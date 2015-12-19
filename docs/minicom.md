Habe a USB-serial adapter, it appears as `Bus 001 Device 080: ID 0711:0230
Magic Control Technology Corp. MCT-232 Serial Port` in `lsusb`. Using `dmesg`
to find that it's on `/dev/ttyUSB1`.

Connect serial cable, start minicom in setup mode.

    $ sudo minicom -s

(configure minicom to talk to /dev/ttyUSB1, 115200-8N1; no flow control; save as dfl)

Run minicom again:

    $ sudo minicom

Power up empeg / Rio Car.
