#!/usr/bin/env sh

set -eu

# Setup emacs
EMACS_CONFIG="$HOME/.emacs.d"
PRELUDE_GIT_URL="https://github.com/bbatsov/prelude.git"
if [ -d $EMACS_CONFIG/.git ] && [ "$(git -C ~/.emacs.d/ config --get remote.origin.url)" = "$PRELUDE_GIT_URL" ]; then
    echo "Prelude git directory found at $EMACS_CONFIG"
else
    echo "Prelude git directory NOT found at $EMACS_CONFIG"
    rm -rf $EMACS_CONFIG
    git clone $PRELUDE_GIT_URL $EMACS_CONFIG
fi
stow --dotfiles --verbose --target="$EMACS_CONFIG" --restow dot-emacs.d

# Stow .gitconfig
stow --dotfiles --verbose --target="$HOME" --restow .
