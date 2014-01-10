hemokit
=======

Haskell library and tool suite for the [Emotiv](http://emotiv.com) Epoc EEG, inspired by the [Emokit](https://github.com/openyou/emokit) code.

It currently only works on Linux and Windows - patches for other platforms are welcome, they should be trivial.


Download
--------

You can download [pre-built binaries here](https://github.com/nh2/hemokit/releases), or build it yourself via [`cabal install hemokit`](http://hackage.haskell.org/package/hemokit).


Library Features
----------------

* device discovery via [hidapi](https://github.com/vahokif/haskell-hidapi)
* decryption of the raw data (one-to-one port from from Emokit)
* convenient access to sensor values, gyro, qualities, battery, and raw data


Programs
--------

Hemokit comes with example programs to

* `hemokit-dump` print out the current EEG data
* `hemokit-mouse` move the cursor using the gyro

Note that we have to use `sudo` in most of the cases because the HIDAP-hidraw implementation reads directly from a device file.


hemokit-dump - Examples
-----------------------

*hemokit-dump* can print EEG data, format it as JSON, serve it via TCP or Websockets, and read from real devices and dump files.


* Output EEG *cumulative state* for an automatically found device:

  ```bash
  sudo hemokit-dump
  ```

* Select one of many connected EEGs by serial number:

  ```bash
  sudo hemokit-dump --serial SN...GM
  ```

* Output only the data the device sends (no cumulative state), and format the output as JSON:

  ```bash
  sudo hemokit-dump --mode packets --json
  ```

* Instead of from a real device, read data recorded to a file, and serve it via JSON over a TCP server on port `1234`:

  ```bash
  sudo cat /dev/hidraw1 > encrypted.dump  # Dump data to a file
  sudo hemokit-dump --from-file encrypted.dump --serial SN...GM --serve 0.0.0.0:1234 --json
  ```

  Here you **have** to specify the serial since HIDAPI is not used to obtain it automatically.
  
  If you prefer a Websockets server over a raw TCP server, use `ws://0.0.0.0:1234` instead.

* Output decrypted raw data to stdout:

  ```bash
  sudo hemokit-dump --mode raw
  ```

* Both print the data from the EEG **and** store the original data for later use:

  ```bash
  sudo cat /dev/hidraw1 | tee >(hemokit-dump --from-file - --serial SN...GM --json) > encrypted.dump
  ```

  We use `tee` and shell process substitution to duplicate the data stream, and tell *hemokit-dump* to read from `-` (stdin).
