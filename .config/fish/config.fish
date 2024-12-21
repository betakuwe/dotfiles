fish_vi_key_bindings
set -g fish_escape_delay_ms 10

source ~/.bash_aliases

if status is-interactive
    # Commands to run in interactive sessions can go here
    if not type -q hx
        alias hx=helix
    end
end
