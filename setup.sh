#/bin/sh

if [ -z "$XDG_CONFIG_HOME" ]; then
   export XDG_CONFIG_HOME=$HOME/.config
fi

####################################################################################
# Linking {{{
linkIfNot() {
   if [ -e $1 ]; then
      if [ ! -e $2 ]; then
         echo "Linking " $1
         ln -s $PWD/$1 $2
      fi
   elif [ ! -e $2 ]; then
      echo "Linking " $1
      ln -s $PWD/$1 $2
   fi
}

link() {
   # Shell/Environment
   linkIfNot environment $HOME/.local/environment/xmonad
   linkIfNot "" $HOME/.xmonad
} # }}}
####################################################################################
# Install - Arch {{{
aurGet() {
   cd $HOME/.aur/
   if [ ! -d "$1" ]; then
      ABBR=${1:0:2}
      wget http://aur.archlinux.org/packages/$ABBR/$1/$1.tar.gz
      tar -xf "$1.tar.gz"
      rm "$1.tar.gz"
   fi
   cd "$1"
   if makepkg > /dev/null; then
      sudo pacman -U "$(ls -t --file-type | grep tar | head -1)"
   fi
}

run_pacman() {
   sudo pacman -Sy
   sudo pacman -S --needed xmonad xmonad-contrib
   sudo pacman -S --needed xorg-xsetroot
   sudo pacman -S --needed conky
   sudo pacman -S --needed dzen2 trayer
   sudo pacman -S --needed unclutter
   sudo pacman -S --needed xcompmgr
}

build_arch() {
   run_pacman
}

update_arch() {
   git pull
   run_pacman
   link
} # }}}
####################################################################################
# Install - Ubuntu {{{
run_apt() {
   sudo apt-get update
   sudo apt-get upgrade

   sudo apt-get install xmonad
   sudo apt-get install conky-cli dzen2
   sudo apt-get install unclutter xcompmgr
}

build_ubuntu() {
   run_apt
}

update_ubuntu() {
   git pull
   run_apt
} # }}}
####################################################################################

if [ -z "${1}" ]; then
   echo "Missing action. Syntax: ${0} [command]"
   echo "  Options:"
   echo "    init    -- installs associated programs and creates all symlinks"
   echo "    update  -- updates packages associated with repo, creates any new symlinks"
   echo "    link    -- create symlinks for files (will not overwrite existing files"
   echo ""
   exit 1
fi
case "${1}" in
   'init')
      type pacman > /dev/null  && build_arch
      type apt-get > /dev/null && build_ubuntu
      link
      ;;
   'update')
      type pacman > /dev/null && update_arch
      type apt-get > /dev/null && update_ubuntu
      link
      ;;
   'link')
      link
      ;;
esac
