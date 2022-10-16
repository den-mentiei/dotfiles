# No need to greet :)
set -U fish_greeting

if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Trying out the starship promt.
starship init fish | source
