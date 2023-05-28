#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
. "$HOME/.cargo/env"

export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
