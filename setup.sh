#!/usr/bin/env sh

set -eu

# Setup emacs
EMACS_CONFIG="$HOME/.emacs.d"
rm -rf "$EMACS_CONFIG"
git clone https://github.com/jamescherti/minimal-emacs.d.git "$EMACS_CONFIG"
stow --dotfiles --verbose --target="$EMACS_CONFIG" --restow dot-emacs.d

# Stow .gitconfig
stow --dotfiles --verbose --target="$HOME" --restow .
