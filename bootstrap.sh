#!/usr/bin/env bash

set -xe

die() {
    echo -e "$@" >&2
}

require() {
    which $1 >/dev/null || die "failed to find $1"
}

require git
require emacs

[[ -d ~/.emacs.d/ ]] && die "already a .emacs.d directory present."

git clone https://github.com/sugol-a/eggmacs ~/.emacs.d
nohup emacs&
