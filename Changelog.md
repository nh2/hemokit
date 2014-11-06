0.6.4

  * Improved cross-platform builds (releasing only to upload new Windows and Linux binaries).

0.6.3

  * Hemokit can now be used as input for [OpenVibe](http://openvibe.inria.fr) using `--format sensorbytes`

0.6.0

  * Added support for serving via TCP sockets
  * Added `--format spaced` output for `--mode state` that simply prints values separated by spaces
  
  These changes make it easy to get raw data even in difficult programming environments, e.g. when the platform is Windows (piping is not easy) and the language is Matlab (JSON parsing is hard).

0.5.2

  * Added Windows support for `dump` (Windows executable attached, tested on Win 7 x64 and Win XP x32)

0.5

  * Improved `dump` utility (also attached to this release as a 64-bit Linux binary)
  * `Hemokit.Start` module for easily bootstrapping new applications
  * Conduit interface
