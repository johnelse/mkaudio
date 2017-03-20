mkaudio
=======

[![Build status](https://travis-ci.org/johnelse/mkaudio.png?branch=master)](https://travis-ci.org/johnelse/mkaudio)

CLI program for generating audio files

```
mkaudio(1)                      Mkaudio Manual                      mkaudio(1)



NAME
       mkaudio - mkaudio

SYNOPSIS
       mkaudio COMMAND ...

MORE HELP
       Use `mkaudio command --help' for help on a single command.
COMMANDS
       beat
           write an audio file containing a beat.

       help
           display help about mkaudio

       saw write an audio file containing a saw wave.

       sine
           write an audio file containing a sine wave.

       square
           write an audio file containing a square wave.

       white-noise
           write an audio file containing white noise.

OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       --version
           Show version information.
```
