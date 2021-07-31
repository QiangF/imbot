#!/usr/bin/bash

active_window=$(xprop -root 32x '\t$0' _NET_ACTIVE_WINDOW | cut -f 2)
wm_class=$(xprop -id ${active_window} WM_CLASS | sed -e 's/.*= "\([^"]*\)".*/\1/')
fcitx5-remote -t
if [ "$wm_class" == 'emacs' ] ; then
    emacsclient --eval '(imbot--pre-command-check)'
    notify-send 'Update imbot state!'
fi
