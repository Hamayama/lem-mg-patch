pause コピーします。
cd %~dp0

set LEM_PATH=%USERPROFILE%\.roswell\lisp\quicklisp\local-projects\cxxxr\lem

copy 1001_lem_modify.sh   "%LEM_PATH%"
copy buffer_patch.lisp    "%LEM_PATH%\lem-base"
copy lisp-mode_patch.lisp "%LEM_PATH%\modes\lisp-mode"
copy cl-charms_patch.lisp "%LEM_PATH%\frontends/ncurses"
copy term_patch.lisp      "%LEM_PATH%\frontends/ncurses"
copy ncurses_patch.lisp   "%LEM_PATH%\frontends/ncurses"

pause コピーしました。
