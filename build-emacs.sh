#!/bin/sh
set -x
set -e

SOURCES_ROOT="$HOME/src"
GIT_REPO="https://github.com/emacs-mirror/emacs"
PREFIX="$HOME/.local"

__git() {
    __cmd="$1"
    shift
    git "$__cmd" $*
}


__make() {
    if [ "$(uname)" != "Linux" ]
    then
        gmake "$@"
    else
        make "$@"
    fi
}

if [ "$(uname)" = "OpenBSD" ]
then
    AUTOCONF_VERSION=2.65
    CC=egcc
    MAKEINFO="/usr/local/bin/gmakeinfo"
    export AUTOCONF_VERSION MAKEINFO CC
fi


if [ ! -d "$SOURCES_ROOT" ]; then
    mkdir -p "$SOURCES_ROOT"
fi

cd "$SOURCES_ROOT"

if [ ! -d emacs ]; then
    __git clone "$GIT_REPO"
fi

cd emacs

__git fetch --depth=1 origin master
__git reset --hard origin/master

./autogen.sh

./configure \
	--prefix="${PREFIX:?no prefix}" \
	--with-x-toolkit=gtk3 \
	--with-sqlite3 \
	--with-imagemagick \
	--without-harfbuzz \
	--without-cairo \
	--without-libotf \
	--without-toolkit-scroll-bars \
	--enable-link-time-optimization

__make bootstrap
__make
__make install
