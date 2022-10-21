# No need to greet :)
set -U fish_greeting

# Append paths.
set -Ua PATH "~/.local/bin"
set -Ua fish_user_paths ~/.cargo/bin

# Want dem colours.
set -g TERM screen-256color

# All hail, unicode!
set -x LANG en_US.UTF-8

# Loads all my ssh keys in to the agent.
load_keys

if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Trying out the starship promt.
starship init fish | source
