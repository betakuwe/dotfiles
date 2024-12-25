if status is-interactive
    # Commands to run in interactive sessions can go here
    source ~/.bash_aliases

    # vi mode
    fish_vi_key_bindings

    # reduce delay to enter normal mode
    set fish_escape_delay_ms 10

    # set ls color theme to catppuccin latte
    if type --query vivid # requires vivid installed
        set --export LS_COLORS (vivid generate catppuccin-latte)
    end

    # helix on arch linux isn't hx, so set alias if hx isn't found
    if not type --query hx; and type --query helix
        alias hx=helix
    end

    # set fzf plugin to show hidden files and gitignore files
    set fzf_fd_opts --hidden --no-ignore
end
