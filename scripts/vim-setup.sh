#!/usr/bin/env bash

# Some code derived from the installer by shoma2da.
# https://github.com/shoma2da/neobundle_installer

# Installation directory.
BUNDLE_DIR=~/.vim/bundle
INSTALL_DIR=$BUNDLE_DIR/neobundle.vim

if [ -e $INSTALL_DIR ]; then
	  echo "$INSTALL_DIR already exists!"
	    exit 1
	fi

	# Check if git is available.
	if type git; then
		  : # It's in place.
	  else
		    echo 'Please install git.'
			  exit 1;
		  fi

		  echo "Begin fetching NeoBundle..."
		  mkdir -p $BUNDLE_DIR
		  git clone https://github.com/Shougo/neobundle.vim $BUNDLE_DIR/neobundle.vim
		  echo "Done."

# Dirs that are needed by vim config.
mkdir -p ~/.vim/backups
mkdir -p ~/.vim/swaps
mkdir -p ~/.vim/undo
