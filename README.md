hemokit
=======

Haskell library for the Emotiv Epoc EEG, inspired by the [Emokit](https://github.com/openyou/emokit) code.

Hemokit currently only works on Linux (patches welcome).


Library Features
----------------

* device discovery via [hidapi](https://github.com/vahokif/haskell-hidapi)
* decryption of the raw data (one-to-one port from from Emokit)
* convenient access to sensor values, gyro, qualities, battery, and raw data


Programs
--------

Hemokit comes with example programs to

* print out the current EEG data
* move the cursor with the gyro

You must run them as root.
