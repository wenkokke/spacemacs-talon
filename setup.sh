#!/bin/sh

EMACS_MONITOR=`realpath ./emacs-monitor`
SPACEMACS_TALON=`realpath ./spacemacs-talon`

TALON_DIR="$HOME/.talon/user"
EMACS_DIR="$HOME/.emacs.d/private/local"

echo "Linking emacs-monitor to $EMACS_DIR"
ln -sf "$EMACS_MONITOR" "$EMACS_DIR/"

echo "Linking emacs-monitor to $TALON_DIR"
ln -sf "$EMACS_MONITOR" "$TALON_DIR/"

echo "Linking spacemacs-talon to $TALON_DIR"
ln -sf "$SPACEMACS_TALON" "$TALON_DIR/"
