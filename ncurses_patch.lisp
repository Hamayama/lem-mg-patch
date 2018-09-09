;; workaround for windows pdcurses

(in-package :lem-ncurses)

#+win32
(progn

  ;; debug log
  (defun dbg-log-format (fmt &rest args)
    (with-open-file (out "lemlog_ncurses0001.txt"
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :append)
      (fresh-line out)
      (apply #'format out fmt args)
      (terpri out)))

  ;; for resizing display
  (defkeycode "[resize]" #x222)
  (defvar *resizing* nil)

  ;; add 'window' slot
  (defstruct ncurses-view
    window
    scrwin
    modeline-scrwin
    x
    y
    width
    height)

  ;; add 'window' slot
  (defmethod lem-if:make-view
      ((implementation ncurses) window x y width height use-modeline)
    (flet ((newwin (nlines ncols begin-y begin-x main-screen)
             (declare (ignore main-screen))
             (let ((win (charms/ll:newwin nlines ncols begin-y begin-x)))
               (when use-modeline (charms/ll:keypad win 1))
               ;; (when main-screen
               ;;   (charms/ll:idlok win 1)
               ;;   (charms/ll:scrollok win 1))
               win)))
      (make-ncurses-view
       :window window
       :scrwin (newwin height width y x nil)
       :modeline-scrwin (when use-modeline (newwin 1 width (+ y height) x nil))
       :x x
       :y y
       :width width
       :height height)))

  ;; enable modifier keys
  (let ((resize-code (get-code "[resize]"))
        (abort-code (get-code "C-]"))
        (escape-code (get-code "escape"))
        (ctrl-key nil)
        (alt-key  nil)
        (esc-key  nil))
    (defun get-ch ()
      ;(dbg-log-format "get-ch")
      (charms/ll:PDC-save-key-modifiers 1)
      ;; workaround for display update problem (incomplete)
      ;(charms/ll:timeout 100)
      (let ((code          (charms/ll:getch))
            (modifier-keys (charms/ll:PDC-get-key-modifiers)))
        (setf ctrl-key (logtest modifier-keys charms/ll:PDC_KEY_MODIFIER_CONTROL))
        (setf alt-key  (logtest modifier-keys charms/ll:PDC_KEY_MODIFIER_ALT))
        ;(dbg-log-format "1 code=~D ctrl-key=~S alt-key=~S" code ctrl-key alt-key)
        (cond
         ;; ctrl key workaround
         (ctrl-key
          (cond
           ;; C-space / C-@
           ((= code #x040) (setf code 0))
           ;; C-down / C-up / C-left / C-right
           ((= code #x1e1) (setf code 525))
           ((= code #x1e0) (setf code 566))
           ((= code #x1bb) (setf code 545))
           ((= code #x1bc) (setf code 560))
           ;; C-Home / C-End / C-PageUp / C-PageDown
           ((= code #x1bf) (setf alt-key t) (setf code #x03c)) ; M-<
           ((= code #x1c0) (setf alt-key t) (setf code #x03e)) ; M->
           ((= code #x1bd) (setf code #o523)) ; PageUp
           ((= code #x1be) (setf code #o522)) ; PageDown
           ))
         ;; alt key workaround
         (alt-key
          (cond
           ;; M-0 - M-9
           ;((<= #x197 code #x1a0) (setf code (- code #x167)))
           ;; M-A - M-Z
           ((<= #x1a1 code #x1ba) (setf code (- code #x140)))
           ;; M-down / M-up / M-left / M-right
           ((= code #x1eb) (setf code #o402))
           ((= code #x1ea) (setf code #o403))
           ((= code #x1ed) (setf code #o404))
           ((= code #x1ec) (setf code #o405))
           ))
         ;; normal key workaround
         (t
          (cond
           ;; Home / End / PageUp / PageDown
           ;((= code #x106) (setf code #o406))
           ((= code #x166) (setf code #o550))
           ;((= code #x153) (setf code #o523))
           ;((= code #x152) (setf code #o522))
           )))
        ;(dbg-log-format "2 code=~D ctrl-key=~S alt-key=~S" code ctrl-key alt-key)
        code))
    (defun get-event ()
      (tagbody :start
        (return-from get-event
          (let ((code (get-ch)))
            (cond ((= code -1) (go :start))
                  ((= code resize-code)
                   (setf esc-key nil)
                   ;; for resizing display
                   ;(charms/ll:resizeterm 0 0)
                   ;(charms/ll:erase)
                   (setf *resizing* t)
                   :resize)
                  ((= code abort-code)
                   (setf esc-key nil)
                   :abort)
                  ((= code escape-code)
                   (setf esc-key nil)
                   (charms/ll:timeout 100)
                   (let ((code (prog1 (get-ch)
                                 (charms/ll:timeout -1))))
                     (cond ((= code -1)
                            (setf esc-key t)
                            (get-key-from-name "escape"))
                           ((= code #.(char-code #\[))
                            (if (= (prog1 (get-ch)
                                     (charms/ll:timeout -1))
                                   #.(char-code #\<))
                                ;;sgr(1006)
                                (uiop:symbol-call :lem-mouse-sgr1006 :parse-mouse-event)
                                (get-key-from-name "escape"))) ;; [tbd] unknown escape sequence
                           (t
                            (let ((key (get-key code)))
                              (make-key :meta t
                                        :sym (key-sym key)
                                        :ctrl (key-ctrl key)))))))
                  ((or alt-key esc-key)
                   (setf esc-key nil)
                   (let ((key (get-key code)))
                     (make-key :meta t
                               :sym (key-sym key)
                               :ctrl (key-ctrl key))))
                  (t
                   (setf esc-key nil)
                   (get-key code))))))))

  ;; workaround for exit problem
  (defun input-loop (editor-thread)
    (handler-case
        (loop
          (handler-case
              (progn
                (unless (bt:thread-alive-p editor-thread) (return))
                ;; workaround for exit problem
                (sleep 0.005)
                (let ((event (get-event)))
                  (if (eq event :abort)
                      (send-abort-event editor-thread nil)
                      (send-event event))))
            #+sbcl
            (sb-sys:interactive-interrupt (c)
              (declare (ignore c))
              (send-abort-event editor-thread t))))
      (exit-editor (c) (return-from input-loop c))))

  ;; for resizing display
  (defun resize-display ()
    (when *resizing*
      (setf *resizing* nil)
      (charms/ll:resizeterm 0 0)
      (charms/ll:erase)
      ;; remake minibuffer's scrwin (to avoid SEGV)
      (let ((minibuffer-view (window-view (lem::minibuffer-window))))
        (charms/ll:delwin (ncurses-view-scrwin minibuffer-view))
        (setf (ncurses-view-scrwin minibuffer-view)
              (charms/ll:newwin 1 charms/ll:*cols* (max (- charms/ll:*lines* 1) 0) 0)))))

  ;; for resizing display
  (defmethod lem-if:display-width ((implementation ncurses))
    ;(dbg-log-format "lem-if:display-width")
    (resize-display)
    (max 5 charms/ll:*cols*))

  ;; for resizing display
  (defmethod lem-if:display-height ((implementation ncurses))
    ;(dbg-log-format "lem-if:display-height")
    (resize-display)
    (max 3 charms/ll:*lines*))

  ;; no change
  (defmethod lem-if:clear ((implementation ncurses) view)
    ;(dbg-log-format "lem-if:clear")
    (charms/ll:clearok (ncurses-view-scrwin view) 1)
    (when (ncurses-view-modeline-scrwin view)
      (charms/ll:clearok (ncurses-view-modeline-scrwin view) 1)))

  ;; no change
  (defmethod lem-if:set-view-size ((implementation ncurses) view width height)
    ;(dbg-log-format "lem-if:set-view-size")
    (setf (ncurses-view-width view) width)
    (setf (ncurses-view-height view) height)
    (charms/ll:wresize (ncurses-view-scrwin view) height width)
    (when (ncurses-view-modeline-scrwin view)
      (charms/ll:mvwin (ncurses-view-modeline-scrwin view)
                       (+ (ncurses-view-y view) height)
                       (ncurses-view-x view))
      (charms/ll:wresize (ncurses-view-modeline-scrwin view)
                         (minibuffer-window-height)
                         width)))

  ;; no change
  (defmethod lem-if:set-view-pos ((implementation ncurses) view x y)
    ;(dbg-log-format "lem-if:set-view-pos")
    (setf (ncurses-view-x view) x)
    (setf (ncurses-view-y view) y)
    (charms/ll:mvwin (ncurses-view-scrwin view) y x)
    (when (ncurses-view-modeline-scrwin view)
      (charms/ll:mvwin (ncurses-view-modeline-scrwin view)
                       (+ y (ncurses-view-height view))
                       x)))

  ;; for dealing with surrogate pair characters
  (defun get-charcode-from-scrwin (view x y &key (modeline nil))
    (let* ((scrwin (if modeline (ncurses-view-modeline-scrwin view)
                                (ncurses-view-scrwin view)))
           (c-lead (logand (charms/ll:mvwinch scrwin y x)
                           charms/ll:A_CHARTEXT)))
      (cond
       ((and (<= #xd800 c-lead #xdbff) (< x (- (ncurses-view-width view) 1)))
        (let ((c-trail (logand (charms/ll:mvwinch scrwin y (+ x 1))
                               charms/ll:A_CHARTEXT)))
          (+ #x10000 (* (- c-lead #xd800) #x0400) (- c-trail #xdc00))))
       (t
        c-lead))))

  ;; for printing wide characters
  (defun get-pos-x (view x y &key (modeline nil))
    (let ((disp-x 0)
          (pos-x  0))
      (loop :while (< disp-x x)
            :for c-code := (get-charcode-from-scrwin view pos-x y :modeline modeline)
            :for c := (code-char c-code)
            :do ;(dbg-log-format "x=~D y=~D disp-x=~D pos-x=~D c-code=~X c=~S"
                ;                x y disp-x pos-x c-code c)
                (setf disp-x (lem-base:char-width c disp-x))
                (cond
                 ((gethash c lem-base:*char-replacement*)
                  (setf pos-x (lem-base:char-width c pos-x)))
                 (t
                  (incf pos-x))))
      ;(dbg-log-format "x=~D y=~D disp-x=~D pos-x=~D" x y disp-x pos-x)
      pos-x))

  ;; adjust line width by using zero-width-space character (#\u200b)
  (defun adjust-line (view x y &key (modeline nil))
    (let ((scrwin (if modeline (ncurses-view-modeline-scrwin view)
                               (ncurses-view-scrwin view)))
          (disp-width  (ncurses-view-width view))
          (disp-x      0)
          (pos-x       0)
          (last-disp-x 0)
          (last-pos-x  0))
      (loop :while (< disp-x disp-width)
            :for c-code := (get-charcode-from-scrwin view pos-x y :modeline modeline)
            :for c := (code-char c-code)
            :do (setf disp-x (lem-base:char-width c disp-x))
                (cond
                 ((gethash c lem-base:*char-replacement*)
                  (setf pos-x (lem-base:char-width c pos-x)))
                 (t
                  (incf pos-x)))
                (unless (or (char= c #\space) (char= c #\u200b))
                  (setf last-disp-x disp-x)
                  (setf last-pos-x  pos-x)))
      ;(dbg-log-format "disp-w=~D disp-x=~D pos-x=~D last-d-x=~D last-p-x=~D"
      ;                disp-width disp-x pos-x last-disp-x last-pos-x)
      (let ((str (concatenate 'string
                              (make-string (max (- disp-width last-disp-x) 0)
                                           :initial-element #\space)
                              (make-string (max (- disp-width pos-x) 0)
                                           :initial-element #\u200b))))
        (charms/ll:mvwaddstr scrwin y last-pos-x str))))

  ;; use get-pos-x and adjust-line
  (defmethod lem-if:print ((implementation ncurses) view x y string attribute)
    ;(dbg-log-format "lem-if:print")
    ;(dbg-log-format "x=~D y=~D view-x=~D view-y=~D view-w=~D view-h=~D"
    ;                x y (ncurses-view-x view) (ncurses-view-y view)
    ;                (ncurses-view-width view) (ncurses-view-height view))
    ;(dbg-log-format "x=~D y=~D pos-x=~D string=~S" x y (get-pos-x view x y) string)
    (let ((attr (attribute-to-bits attribute)))
      (charms/ll:wattron (ncurses-view-scrwin view) attr)
      ;(charms/ll:scrollok (ncurses-view-scrwin view) 0)
      (charms/ll:mvwaddstr (ncurses-view-scrwin view) y (get-pos-x view x y) string)
      ;(charms/ll:scrollok (ncurses-view-scrwin view) 1)
      (charms/ll:wattroff (ncurses-view-scrwin view) attr)
      (adjust-line view x y)))

  ;; no change
  (defmethod lem-if:print-modeline ((implementation ncurses) view x y string attribute)
    ;(dbg-log-format "lem-if:print-modeline")
    (let ((attr (attribute-to-bits attribute)))
      (charms/ll:wattron (ncurses-view-modeline-scrwin view) attr)
      (charms/ll:mvwaddstr (ncurses-view-modeline-scrwin view) y x string)
      (charms/ll:wattroff (ncurses-view-modeline-scrwin view) attr)))

  ;; use get-pos-x and adjust-line (incomplete)
  ;(defmethod lem-if:print-modeline ((implementation ncurses) view x y string attribute)
  ;  ;(dbg-log-format "lem-if:print-modeline")
  ;  (let ((attr (attribute-to-bits attribute)))
  ;    (charms/ll:wattron (ncurses-view-modeline-scrwin view) attr)
  ;    (charms/ll:mvwaddstr (ncurses-view-modeline-scrwin view) y (get-pos-x view x y :modeline t) string)
  ;    (adjust-line view x y :modeline t)
  ;    (charms/ll:wattroff (ncurses-view-modeline-scrwin view) attr)))

  ;; use get-pos-x and adjust-line
  (defmethod lem-if:clear-eol ((implementation ncurses) view x y)
    ;(dbg-log-format "lem-if:clear-eol")
    ;(dbg-log-format "x=~D y=~D view-x=~D view-y=~D view-w=~D view-h=~D"
    ;                x y (ncurses-view-x view) (ncurses-view-y view)
    ;                (ncurses-view-width view) (ncurses-view-height view))
    ;; workaround for line continuetion character missing (incomplete)
    (when (< x (ncurses-view-width view))
      (charms/ll:wmove (ncurses-view-scrwin view) y (get-pos-x view x y))
      (charms/ll:wclrtoeol (ncurses-view-scrwin view))
      (adjust-line view x y)))

  ;; use get-pos-x and adjust-line
  (defmethod lem-if:clear-eob ((implementation ncurses) view x y)
    ;(dbg-log-format "lem-if:clear-eob")
    ;(dbg-log-format "x=~D y=~D view-x=~D view-y=~D view-w=~D view-h=~D"
    ;                x y (ncurses-view-x view) (ncurses-view-y view)
    ;                (ncurses-view-width view) (ncurses-view-height view))
    ;; workaround for line continuetion character missing (incomplete)
    (cond
     ((< x (ncurses-view-width view))
      (charms/ll:wmove (ncurses-view-scrwin view) y (get-pos-x view x y))
      (charms/ll:wclrtobot (ncurses-view-scrwin view))
      (adjust-line view x y))
     ((< y (- (ncurses-view-height view) 1))
      (charms/ll:wmove (ncurses-view-scrwin view) (+ y 1) 0)
      (charms/ll:wclrtobot (ncurses-view-scrwin view)))))

  ;; check display height (to avoid SEGV)
  (defmethod lem-if:redraw-view-after ((implementation ncurses) view focus-window-p)
    ;(dbg-log-format "lem-if:redraw-view-after 1")
    (let ((attr (attribute-to-bits 'modeline)))
      (charms/ll:attron attr)
      (when (and (ncurses-view-modeline-scrwin view)
                 (< 0 (ncurses-view-x view)))
        (charms/ll:move (ncurses-view-y view) (1- (ncurses-view-x view)))
        (charms/ll:vline (char-code #\space) (1+ (ncurses-view-height view))))
      (charms/ll:attroff attr)
      (charms/ll:wnoutrefresh charms/ll:*stdscr*))
    ;(dbg-log-format "lem-if:redraw-view-after 2")
    ;; check display height (to avoid SEGV)
    (when (and (ncurses-view-modeline-scrwin view)
               (>= charms/ll:*lines* 2))
      (charms/ll:wnoutrefresh (ncurses-view-modeline-scrwin view)))
    ;(dbg-log-format "lem-if:redraw-view-after 3")
    (charms/ll:wnoutrefresh (ncurses-view-scrwin view))
    ;(dbg-log-format "lem-if:redraw-view-after 4")
    ;; workaround for cursor position problem
    (unless (minibuffer-window-active-p)
      (charms/ll:move lem::*cursor-y* lem::*cursor-x*)))

  ;; no change
  (defmethod lem-if:update-display ((implementation ncurses))
    ;(dbg-log-format "lem-if:update-display")
    (let ((scrwin (ncurses-view-scrwin (window-view (current-window)))))
      (if (lem::covered-with-floating-window-p (current-window) lem::*cursor-x* lem::*cursor-y*)
          (charms/ll:curs-set 0)
          (progn
            (charms/ll:curs-set 1)
            (charms/ll:wmove scrwin lem::*cursor-y* lem::*cursor-x*)))
      (charms/ll:wnoutrefresh scrwin)
      (charms/ll:doupdate)))

  )

