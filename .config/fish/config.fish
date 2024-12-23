fish_vi_key_bindings
set -g fish_escape_delay_ms 10

source ~/.bash_aliases

if status is-interactive
    # Commands to run in interactive sessions can go here

    # helix on arch linux isn't hx, so set alias if hx isn't found
    if not type -q hx; and type -q helix
        alias hx=helix
    end

    # set fzf plugin to show hidden files and gitignore files
    set fzf_fd_opts --hidden --no-ignore
end
