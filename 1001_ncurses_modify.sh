#!/bin/bash

# 1001_ncurses_modify.sh
# 2018-9-5 v1.01

set -e

##### settings #####
NCURSES_ASD_FILE=lem-ncurses.asd
NCURSES_ASD_BKUP=lem-ncurses_orig1001.asd
NCURSES_LISP_FILE=ncurses.lisp
NCURSES_LISP_BKUP=ncurses_orig1001.lisp

##### functions #####
function usage {
    echo "Usage: 1001_ncurses_modify.sh"
}

function do_check_file {
    if [ ! -f "$1" ]; then
        echo "File '$1' not found.  Aborting."; exit 1
    fi
}

function do_backup_file {
    if [ ! -f "$2" ]; then
        cp "$1" "$2"
    fi
}

function do_patch_to_asd_file {
    local patch_file="$1"
    local bak="bak5001"

    # add '(:file "cl-charms_patch")'
    if ! grep -q -e '(:file "cl-charms_patch")' $patch_file; then
        cp $patch_file $patch_file.$bak
        sed -e 's@\(:components (\)@\1(:file "cl-charms_patch")@' $patch_file.$bak > $patch_file
    fi

    # add '(:file "ncurses_patch")'
    if ! grep -q -e '(:file "ncurses_patch")' $patch_file; then
        cp $patch_file $patch_file.$bak
        sed -e 's@\((:file "ncurses")\)@\1(:file "ncurses_patch")@' $patch_file.$bak > $patch_file
    fi

    rm -f $patch_file.$bak
}

function do_patch_to_lisp_file {
    local patch_file="$1"
    local bak="bak5001"

    # comment out '(defstruct ncurses-view ... )'
    if grep -q -e '^(defstruct ncurses-view' $patch_file; then
        cp $patch_file $patch_file.$bak
        sed -e '/(defstruct ncurses-view/,/height)/ s@^\(.*\)$@;\1@g' $patch_file.$bak > $patch_file
    fi

    rm -f $patch_file.$bak
}

##### main #####

while [ "$#" -gt 0 ]; do
    case $1 in
        *) usage; exit 1;;
    esac
done

do_check_file  $NCURSES_ASD_FILE
do_check_file  $NCURSES_LISP_FILE
do_backup_file $NCURSES_ASD_FILE  $NCURSES_ASD_BKUP
do_backup_file $NCURSES_LISP_FILE $NCURSES_LISP_BKUP
do_patch_to_asd_file  $NCURSES_ASD_FILE
do_patch_to_lisp_file $NCURSES_LISP_FILE

echo "Files were modified successfully."

