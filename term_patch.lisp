;; workaround for windows pdcurses

(in-package :lem.term)

#+win32
(progn

  ;; enable default color code (-1)
  (defun term-init ()
    #+(or (and ccl unix) (and lispworks unix))
    (lem-setlocale/cffi:setlocale lem-setlocale/cffi:+lc-all+ "")
    (if *tty-name*
        (term-init-tty *tty-name*)
        (charms/ll:initscr))
    (when (zerop (charms/ll:has-colors))
      (charms/ll:endwin)
      (write-line "Please execute TERM=xterm-256color and try again.")
      (return-from term-init nil))
    (charms/ll:start-color)

    ;; enable default color code (-1)
    (charms/ll:use-default-colors)

    (init-colors charms/ll:*colors*)
    (set-default-color nil nil)
    (charms/ll:noecho)
    (charms/ll:cbreak)
    (charms/ll:raw)
    (charms/ll:nonl)
    (charms/ll:refresh)
    (charms/ll:keypad charms/ll:*stdscr* 1)
    (setf charms/ll::*escdelay* 0)
    ;(charms/ll:curs-set 0)
    t)

  )

