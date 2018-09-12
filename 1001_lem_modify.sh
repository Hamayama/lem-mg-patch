#!/bin/bash

# 1001_lem_modify.sh
# 2018-9-12 v1.12

set -e

##### settings #####
LEMBASE_ASD_FILE=lem-base/lem-base.asd
LEMBASE_ASD_BKUP=lem-base/lem-base_orig1001.asd
LISPMODE_ASD_FILE=modes/lisp-mode/lem-lisp-mode.asd
LISPMODE_ASD_BKUP=modes/lisp-mode/lem-lisp-mode_orig1001.asd
NCURSES_ASD_FILE=frontends/ncurses/lem-ncurses.asd
NCURSES_ASD_BKUP=frontends/ncurses/lem-ncurses_orig1001.asd

##### functions #####
function usage {
    echo "Usage: 1001_lem_modify.sh"
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

function do_patch_to_lembase_asd_file {
    local patch_file="$1"
    local bak="bak5001"

    # add '(:file "buffer_patch")'
    if ! grep -q -e '(:file "buffer_patch")' $patch_file; then
        cp $patch_file $patch_file.$bak
        sed -e 's@\((:file "buffer")\)@\1(:file "buffer_patch")@' $patch_file.$bak > $patch_file
    fi

    rm -f $patch_file.$bak
}

function do_patch_to_lispmode_asd_file {
    local patch_file="$1"
    local bak="bak5001"

    # add '(:file "lisp-mode_patch")'
    if ! grep -q -e '(:file "lisp-mode_patch")' $patch_file; then
        cp $patch_file $patch_file.$bak
        sed -e 's@\((:file "lisp-mode")\)@\1(:file "lisp-mode_patch")@' $patch_file.$bak > $patch_file
    fi

    rm -f $patch_file.$bak
}

function do_patch_to_ncurses_asd_file {
    local patch_file="$1"
    local bak="bak5001"

    # add '(:file "cl-charms_patch")'
    if ! grep -q -e '(:file "cl-charms_patch")' $patch_file; then
        cp $patch_file $patch_file.$bak
        sed -e 's@\(:components (\)@\1(:file "cl-charms_patch")@' $patch_file.$bak > $patch_file
    fi

    # add '(:file "term_patch")'
    if ! grep -q -e '(:file "term_patch")' $patch_file; then
        cp $patch_file $patch_file.$bak
        sed -e 's@\((:file "term")\)@\1(:file "term_patch")@' $patch_file.$bak > $patch_file
    fi

    # add '(:file "ncurses_patch")'
    if ! grep -q -e '(:file "ncurses_patch")' $patch_file; then
        cp $patch_file $patch_file.$bak
        sed -e 's@\((:file "ncurses")\)@\1(:file "ncurses_patch")@' $patch_file.$bak > $patch_file
    fi

    rm -f $patch_file.$bak
}

##### main #####

while [ "$#" -gt 0 ]; do
    case $1 in
        *) usage; exit 1;;
    esac
done

do_check_file  $LEMBASE_ASD_FILE
do_check_file  $LISPMODE_ASD_FILE
do_check_file  $NCURSES_ASD_FILE
do_backup_file $LEMBASE_ASD_FILE  $LEMBASE_ASD_BKUP
do_backup_file $LISPMODE_ASD_FILE $LISPMODE_ASD_BKUP
do_backup_file $NCURSES_ASD_FILE  $NCURSES_ASD_BKUP
do_patch_to_lembase_asd_file  $LEMBASE_ASD_FILE
do_patch_to_lispmode_asd_file $LISPMODE_ASD_FILE
do_patch_to_ncurses_asd_file  $NCURSES_ASD_FILE

echo "Files were modified successfully."

