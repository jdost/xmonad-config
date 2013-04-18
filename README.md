# XMonad Configuration

This is the current configuration I use across my machines for XMonad.  It is 
written in a modular way and uses the machine's hostname to differentiate various
per machine settings (such as workspace names, hooks, etc).  

## Requirements

Required is `xmonad` and `xmonad-contrib` (contrib is included in some distro
packages for xmonad).  `dzen2` and some `conky` (`conky-cli` works) are needed for
the status bars.  Additionally, `unclutter` and `xcompmgr` are two things I use to
improve the experience but will not break the experience if missing.

## Setup

There is an included `setup.sh` script that will try to setup the system with my
other configuration sets.  This will install the required packages for either
Ubuntu or Arch and symlink the needed files.  There is an `environment` file that
should be imported into `.xinitrc` to tell it things to start for xmonad, this is
specific to my system, tailor it to how you do it yourself.
