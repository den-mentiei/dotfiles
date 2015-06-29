if [[ $COLORTERM == "gnome-terminal" ]] then
  export TERM=xterm-256color
fi

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"

plugins=(git command-not-found colored-man apt npm)

# User configuration

export PATH="/home/den/.rvm/gems/ruby-2.1.5/bin:/home/den/.rvm/gems/ruby-2.1.5@global/bin:/home/den/.rvm/rubies/ruby-2.1.5/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/den/.rvm/bin:/home/den/.rvm/bin"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8

export EDITOR='vim'
export VISUAL='vim'

# causes globs which do not match a file to result in an error without running the command
unsetopt nomatch

# aliases
alias servethis="python -c 'import SimpleHTTPServer; SimpleHTTPServer.test()'"
alias please="sudo $(fc -ln -1)"

# directory alises
hash -d d=~/dotfiles.git
