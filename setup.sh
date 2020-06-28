#/bin/sh

set -euo pipefail

export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}

show_help() {
   cat <<-HELP
Setup script for xmonad configuration

USAGE: ${0} [command]

commands:
   init    -- Initialize system with expected packages and linked configs
   update  -- Updates state of local repo and fixes any drift issues
   link    -- Create missing links not already defined
HELP
}

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
   mkdir -p $XDG_CONFIG_HOME/supervisord/config.d/
   linkIfNot supervisor.d/urxvtd.conf $XDG_CONFIG_HOME/supervisord/config.d/urxvtd.conf
   linkIfNot supervisor.d/unclutter.conf $XDG_CONFIG_HOME/supervisord/config.d/unclutter.conf
   linkIfNot supervisor.d/polybar.conf $XDG_CONFIG_HOME/supervisord/config.d/statusbar.conf
}

install() {
   sudo pacman -Sy
   sudo pacman -S --needed xmonad xmonad-contrib
   sudo pacman -S --needed xorg-xsetroot xdotool
   #sudo pacman -S --needed conky
   #sudo pacman -S --needed dzen2 trayer
   #sudo pacman -S --needed polybar
   sudo pacman -S --needed unclutter
   #sudo pacman -S --needed mpc
}

update() {
   git pull
}

if [ -z "${1}" ]; then
   echo "Missing action. Syntax: ${0} [command]"
   echo "  Options:"
   echo "    init    -- installs associated programs and creates all symlinks"
   echo "    update  -- updates packages associated with repo, creates any new symlinks"
   echo "    link    -- create symlinks for files (will not overwrite existing files"
   echo ""
   exit 1
fi
case "${1:-}" in
   'init')
      install
      link
      ;;
   'update')
      update
      link
      ;;
   'link')
      link
      ;;
   *)
      show_help
      exit
      ;;
esac
