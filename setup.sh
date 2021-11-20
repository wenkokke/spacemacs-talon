#!/bin/sh

EMACS_SRC=`realpath ./talon-integration`
TALON_SRC=`realpath ./spacemacs-talon`
TALON_DIR="$HOME/.talon/user/"
EMACS_DIR="$HOME/.emacs.d/private/local/"

ln -sf "$EMACS_SRC" "$EMACS_DIR"
ln -sf "$TALON_SRC" "$TALON_DIR"
