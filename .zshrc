export ZSH=~/.oh-my-zsh
ZSH_CUSTOM=$HOME/.zsh_customs

ZSH_THEME="amuse-mod"
 
plugins=(git command-not-found colored-man-pages encode64 urltools ssh-agent archlinux)

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8

# causes globs which do not match a file to result in an error without running the command
unsetopt nomatch

# aliases
alias please='sudo $(fc -ln -1)'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
