#!/usr/bin/env bash

# Reference: https://powerline.readthedocs.org/en/latest/installation/linux.html#fonts-installation

# Make sure only root can run the script.
if [[ `id -u` -ne 0 ]] ; then echo "Please run me as root." ; exit 1 ; fi

# Download latest versions.
sudo -u ${USERNAME} wget https://github.com/Lokaltog/powerline/raw/develop/font/PowerlineSymbols.otf
sudo -u ${USERNAME} wget https://github.com/Lokaltog/powerline/raw/develop/font/10-powerline-symbols.conf

# Move to X font path.
# TODO: get a path from `xset q`?
mv PowerlineSymbols.otf /usr/share/fonts/X11/misc/

# Update font cache.
fc-cache -vf /usr/share/fonts/X11/misc/

# Install the fontconfig file.
sudo -u ${USERNAME} mkdir -p ~/.config/fontconfig/conf.d/
sudo -u ${USERNAME} mv 10-powerline-symbols.conf ~/.config/fontconfig/conf.d/
