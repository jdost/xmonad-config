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
   linkIfNot dmrc $HOME/.dmrc

   LAYOUT_PATH="lib/machines/$HOSTNAME.hs"
   if [ ! -e $LAYOUT_PATH ]; then
      LAYOUT_PATH="lib/machines/Default.hs"
   fi

   linkIfNot $LAYOUT_PATH $HOME/.xmonad/lib/CurrentMachine.hs
} # }}}
####################################################################################
# Install - Arch {{{
aurGet() {
   local END_DIR=$PWD
   cd $HOME/.aur/
   ABBR=${1:0:2}
   wget http://aur.archlinux.org/packages/$ABBR/$1/$1.tar.gz
   tar -xf "$1.tar.gz"
   rm "$1.tar.gz"
   cd "$1"
   makepkg -si
   cd $END_DIR
}

run_pacman() {
   sudo pacman -Sy
   sudo pacman -S --needed xmonad xmonad-contrib
   sudo pacman -S --needed xorg-xsetroot
   sudo pacman -S --needed conky
   sudo pacman -S --needed dzen2 trayer
   sudo pacman -S --needed unclutter
   sudo pacman -S --needed xcompmgr
   sudo pacman -S --needed dunst
   sudo pacman -S --needed mpc
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
      command -v pacman >/dev/null 2>&1  && build_arch
      command -v apt-get >/dev/null 2>&1 && build_ubuntu
      link
      ;;
   'update')
      command -v pacman >/dev/null 2>&1  && update_arch
      command -v apt-get >/dev/null 2>&1 && update_ubuntu
      link
      ;;
   'link')
      link
      ;;
esac
