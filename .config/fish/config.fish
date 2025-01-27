if status is-interactive
    # Commands to run in interactive sessions can go here

    # vi mode
    fish_vi_key_bindings

    # reduce delay to enter normal mode
    set fish_escape_delay_ms 10

    # set ls color theme to catppuccin latte
    if type --query vivid # requires vivid installed
        set --export --universal LS_COLORS (vivid generate catppuccin-latte)
    end

    # helix on arch linux isn't hx, so set alias if hx isn't found
    if not type --query hx; and type --query helix
        abbr --add hx helix
    end

    # set fzf plugin to show hidden files and gitignore files
    set fzf_fd_opts --hidden --no-ignore

    # use bat as man pager
    if type --query bat
        set --export --universal MANPAGER "sh -c 'col -bx | bat -l man -p'"
    end

    source /home/local/KLASS/benjamin.tan/.config/helix/hx.fish

    # activate npm on terminal start up
    if type --query nvm
        nvm --silent use latest
    end

    # abbreviations
    abbr --add l ls -C --classify
    abbr --add la ls --almost-all
    abbr --add ll ls --all -l --classify --human-readable
    abbr --add ls ls --color=auto
    abbr --add chgrp chgrp --preserve-root
    abbr --add chmod chmod --preserve-root
    abbr --add chown chown --preserve-root
    abbr --add zll zellij list-sessions
    abbr --add zla zellij attach
    abbr --add zld zellij delete-session
    abbr --add zljs zellij --session
    abbr --add cljd clj -M:cljd
    abbr --add info info --vi-keys
    abbr --add flutter fvm flutter
    abbr --add dart fvm dart
    abbr --add ffd fvm flutter devices
    abbr --add ffr fvm flutter run
    abbr --add dbrb fvm dart run build_runner build
    abbr --add dbrw fvm dart run build_runner watch
    abbr --add gs git status
    abbr --add gpr git pull --rebase
    abbr --add gss git stash
    abbr --add gsp git stash pop
    abbr --add gca git commit -am
    abbr --add gcm git commit -m
    abbr --add gco git checkout
    abbr --add gbl git branch --list
    abbr --add gmf git merge --ff-only
end
