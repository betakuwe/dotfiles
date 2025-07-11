#!/usr/bin/env sh

# don't use, more trouble than it's worth, just symlink manually

set -eu

DOT_CONFIG="$HOME/.config/doom"
stow --dotfiles --verbose --target="$DOT_CONFIG" --restow dot-config/doom

# Stow .gitconfig
stow --dotfiles --verbose --target="$HOME" --restow .
