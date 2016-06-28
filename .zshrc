ZSH_CUSTOM=$HOME/.zsh_customs

if [[ $COLORTERM == "gnome-terminal" ]] then
  export TERM=xterm-256color
fi

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

ZSH_THEME="amuse-mod"

plugins=(git command-not-found colored-man apt node npm bower encode64 gem urltools nyan)

# User configuration

export PATH="/home/den/.rvm/gems/ruby-2.1.5/bin:/home/den/.rvm/gems/ruby-2.1.5@global/bin:/home/den/.rvm/rubies/ruby-2.1.5/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/den/.rvm/bin:/home/den/.rvm/bin"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8

export EDITOR='vim'
export VISUAL='gvim'

# causes globs which do not match a file to result in an error without running the command
unsetopt nomatch

# aliases
alias servethis="python -c 'import SimpleHTTPServer; SimpleHTTPServer.test()'"
alias please='sudo $(fc -ln -1)'

# OPAM configuration
. ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
