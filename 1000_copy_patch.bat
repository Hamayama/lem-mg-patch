pause コピーします。
cd %~dp0

set LEM_PATH=%USERPROFILE%\.roswell\lisp\quicklisp\local-projects\cxxxr\lem

copy 1001_ncurses_modify.sh "%LEM_PATH%\lem-frontend-ncurses"
copy cl-charms_patch.lisp   "%LEM_PATH%\lem-frontend-ncurses"
copy ncurses_patch.lisp     "%LEM_PATH%\lem-frontend-ncurses"

pause コピーしました。
