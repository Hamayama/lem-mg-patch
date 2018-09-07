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

  ;; add window slot
  (defstruct ncurses-view
    window
    scrwin
    modeline-scrwin
    x
    y
    width
    height)

  ;; add window slot
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

  ;; for dealing with surrogate pairs
  (defun get-charcode-from-scrwin (view y x)
    (let ((c-lead (logand (charms/ll:mvwinch (ncurses-view-scrwin view) y x)
                          charms/ll:A_CHARTEXT)))
      (cond
       ((and (<= #xd800 c-lead #xdbff) (< x (- charms/ll:*cols* 1)))
        (let ((c-trail (logand (charms/ll:mvwinch (ncurses-view-scrwin view) y (+ x 1))
                               charms/ll:A_CHARTEXT)))
          (+ #x10000 (* (- c-lead #xd800) #x0400) (- c-trail #xdc00))))
       (t
        c-lead))))

  ;; get-pos-x is used for printing wide characters
  (defun get-pos-x (view x y)
    (loop :with disp-x := 0 :while (< disp-x x)
          :with pos-x  := 0
          :for c-code := (get-charcode-from-scrwin view y pos-x)
          :for c := (code-char c-code)
          :do ;(dbg-log-format "x=~D y=~D disp-x=~D pos-x=~D c-code=~X c=~S"
              ;                x y disp-x pos-x c-code c)
              (setf disp-x (lem-base:char-width c disp-x))
              (cond
               ((gethash c lem-base:*char-replacement*)
                (setf pos-x (lem-base:char-width c pos-x)))
               (t
                (incf pos-x)))
          :finally ;(dbg-log-format "x=~D y=~D pos-x=~D" x y pos-x)
                   (return pos-x)))

  ;; use get-pos-x
  (defmethod lem-if:print ((implementation ncurses) view x y string attribute)
    ;(dbg-log-format "x=~D y=~D pos-x=~D string=~S" x y (get-pos-x view x y) string)
    (let ((attr (attribute-to-bits attribute)))
      (charms/ll:wattron (ncurses-view-scrwin view) attr)
      ;(charms/ll:scrollok (ncurses-view-scrwin view) 0)
      (charms/ll:mvwaddstr (ncurses-view-scrwin view) y (get-pos-x view x y) string)
      ;(charms/ll:scrollok (ncurses-view-scrwin view) 1)
      (charms/ll:wattroff (ncurses-view-scrwin view) attr)))

  ;; use get-pos-x
  ;(defmethod lem-if:print-modeline ((implementation ncurses) view x y string attribute)
  ;  (let ((attr (attribute-to-bits attribute)))
  ;    (charms/ll:wattron (ncurses-view-modeline-scrwin view) attr)
  ;    (charms/ll:mvwaddstr (ncurses-view-modeline-scrwin view) y (get-pos-x view x y) string)
  ;    (charms/ll:wattroff (ncurses-view-modeline-scrwin view) attr)))

  ;; use get-pos-x
  (defmethod lem-if:clear-eol ((implementation ncurses) view x y)
    ;(dbg-log-format "lem-if:clear-eol")
    ;; workaround for a line continuetion character missing
    (when (< x charms/ll:*cols*)
      (charms/ll:wmove (ncurses-view-scrwin view) y (get-pos-x view x y))
      (charms/ll:wclrtoeol (ncurses-view-scrwin view))))

  ;; use get-pos-x
  (defmethod lem-if:clear-eob ((implementation ncurses) view x y)
    ;(dbg-log-format "lem-if:clear-eob")
    (charms/ll:wmove (ncurses-view-scrwin view) y (get-pos-x view x y))
    (charms/ll:wclrtobot (ncurses-view-scrwin view)))

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
            (setf *resizing* nil)
            (cond ((= code -1) (go :start))
                  ((= code resize-code)
                   (setf esc-key nil)
                   ;; for resizing display
                   (charms/ll:resizeterm 0 0)
                   (charms/ll:erase)
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
                (sleep 0.001)
                (let ((event (get-event)))
                  (if (eq event :abort)
                      (send-abort-event editor-thread nil)
                      (send-event event))))
            #+sbcl
            (sb-sys:interactive-interrupt (c)
              (declare (ignore c))
              (send-abort-event editor-thread t))))
      (exit-editor (c) (return-from input-loop c))))

  ;; for resizing display (avoid SEGV)
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
    (when (and (ncurses-view-modeline-scrwin view)
               (>= charms/ll:*lines* 2))
      ;; remake modeline's scrwin (avoid SEGV)
      ;(when *resizing*
      ;  (charms/ll:delwin (ncurses-view-modeline-scrwin view))
      ;  (setf (ncurses-view-modeline-scrwin view)
      ;        (charms/ll:newwin 1 charms/ll:*cols* (max (- charms/ll:*lines* 2) 0) 0)))
      (charms/ll:wnoutrefresh (ncurses-view-modeline-scrwin view)))
    ;(dbg-log-format "lem-if:redraw-view-after 3")
    ;(dbg-log-format "width=~D height=~D *resizing*=~S"
    ;                (ncurses-view-width view) (ncurses-view-height view) *resizing*)
    ;; remake minibuffer's scrwin (avoid SEGV)
    (when (and *resizing*
               (minibuffer-window-p (ncurses-view-window view)))
      (charms/ll:delwin (ncurses-view-scrwin view))
      (setf (ncurses-view-scrwin view)
            (charms/ll:newwin 1 charms/ll:*cols* (max (- charms/ll:*lines* 1) 0) 0)))
    (charms/ll:wnoutrefresh (ncurses-view-scrwin view))
    ;(dbg-log-format "lem-if:redraw-view-after 4")
    )

  )

