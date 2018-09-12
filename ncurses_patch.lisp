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

  ;; for mouse
  (defkeycode "[mouse]" #x21b)
  (defvar *dragging-window* ())

  ;; no change
  (defstruct ncurses-view
    scrwin
    modeline-scrwin
    x
    y
    width
    height)

  ;; use only stdscr
  (defmethod lem-if:make-view
      ((implementation ncurses) window x y width height use-modeline)
    ;(flet ((newwin (nlines ncols begin-y begin-x main-screen)
    ;         (declare (ignore main-screen))
    ;         (let ((win (charms/ll:newwin nlines ncols begin-y begin-x)))
    ;           (when use-modeline (charms/ll:keypad win 1))
    ;           ;; (when main-screen
    ;           ;;   (charms/ll:idlok win 1)
    ;           ;;   (charms/ll:scrollok win 1))
    ;           win)))
    ;  (make-ncurses-view
    ;   :scrwin (newwin height width y x nil)
    ;   :modeline-scrwin (when use-modeline (newwin 1 width (+ y height) x nil))
    ;   :x x
    ;   :y y
    ;   :width width
    ;   :height height)))
    (make-ncurses-view
     :scrwin charms/ll:*stdscr*
     :modeline-scrwin (if use-modeline charms/ll:*stdscr* nil)
     :x x
     :y y
     :width width
     :height height))

  ;; for mouse
  (defun mouse-get-window-rect (window)
    (values (lem:window-x window)
            (lem:window-y window)
            (lem:window-width window)
            (lem:window-height window)))
  (defun mouse-move-to-cursor (window x y)
    (lem:move-point (lem:current-point) (lem::window-view-point window))
    (lem:move-to-next-virtual-line (lem:current-point) y)
    (lem:move-to-virtual-line-column (lem:current-point) x))
  (defun mouse-event-proc (bstate x1 y1)
    (lambda ()
      ;(dbg-log-format "bstate=~X x1=~D y1=~D" bstate x1 y1)
      (cond
       ;; button1 down
       ((logtest bstate (logior charms/ll:BUTTON1_PRESSED
                                charms/ll:BUTTON1_CLICKED
                                charms/ll:BUTTON1_DOUBLE_CLICKED
                                charms/ll:BUTTON1_TRIPLE_CLICKED))
        (let ((press (logtest bstate charms/ll:BUTTON1_PRESSED)))
          (find-if
           (lambda(o)
             (multiple-value-bind (x y w h) (mouse-get-window-rect o)
               ;(dbg-log-format "x=~D y=~D w=~D h=~D x1=~D y1=~D" x y w h x1 y1)
               (cond
                ;; vertical dragging window
                ((and press (= y1 (- y 1)) (<= x x1 (+ x w -1)))
                 (setf *dragging-window* (list o 'y))
                 t)
                ;; horizontal dragging window
                ((and press (= x1 (- x 1)) (<= y y1 (+ y h -2)))
                 (setf *dragging-window* (list o 'x))
                 t)
                ;; move cursor
                ((and (<= x x1 (+ x w -1)) (<= y y1 (+ y h -2)))
                 (lem:send-event
                  (lambda ()
                    (setf (lem:current-window) o)
                    (mouse-move-to-cursor o (- x1 x) (- y1 y))
                    (lem:redraw-display)))
                 t)
                (t nil))))
           (lem:window-list))))
       ;; button1 up
       ((logtest bstate charms/ll:BUTTON1_RELEASED)
        (let ((o (first *dragging-window*)))
          (when (windowp o)
            (multiple-value-bind (x y w h) (mouse-get-window-rect o)
              (setf (lem:current-window) o)
              (cond
               ;; vertical dragging window
               ((eq (second *dragging-window*) 'x)
                (let ((vx (- (- (lem:window-x o) 1) x1)))
                  ;; this check is incomplete for 3 or more divisions
                  (when (and (>= x1       5)
                             (>= (+ w vx) 5))
                    (lem:grow-window-horizontally vx)
                    ;; workaround for display update problem (incomplete)
                    (force-refresh-display charms/ll:*cols* (- charms/ll:*lines* 1))
                    (lem:redraw-display))))
               ;; horizontal dragging window
               (t
                (let ((vy (- (- (lem:window-y o) 1) y1)))
                  ;; this check is incomplete for 3 or more divisions
                  (when (and (>= y1       3)
                             (>= (+ h vy) 3))
                    (lem:grow-window vy)
                    (lem:redraw-display))))
               )))
          (when o
            (setf *dragging-window*
                  (list nil (list x1 y1) *dragging-window*)))))
       ;; wheel up
       ((logtest bstate charms/ll:BUTTON4_PRESSED)
        (lem:scroll-up 3)
        (lem:redraw-display))
       ;; wheel down
       ((logtest bstate charms/ll:BUTTON5_PRESSED)
        (lem:scroll-down 3)
        (lem:redraw-display))
       )))

  ;; dealing with utf-16 surrogate pair characters
  (defun get-key (code)
    (let ((char #\null))
      (cond
       ((<= #xd800 code #xdbff)
        (let* ((c-trail (charms/ll:getch))
               (c-code  (+ #x10000 (* (- code #xd800) #x0400) (- c-trail #xdc00))))
          ;(dbg-log-format "code=~X c-trail=~X c-code=~X" code c-trail c-code)
          (setf char (code-char c-code))))
       (t
        (setf char (code-char code))))
      (char-to-key char)))

  ;; enable modifier keys
  (let ((resize-code (get-code "[resize]"))
        (mouse-code (get-code "[mouse]"))
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
        ;(dbg-log-format "1 code=~X ctrl-key=~S alt-key=~S" code ctrl-key alt-key)
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
           ;; M-[
           ((= code #x1f1)
            (charms/ll:timeout 100)
            (let ((code1 (charms/ll:getch)))
              (cond
               ;; drop mouse escape sequence
               ((= code1 #x03c) ; <
                (loop :for code2 := (charms/ll:getch)
                      :until (or (= code2 -1)
                                 (= code2 #x04d)   ; M
                                 (= code2 #x06d))) ; m
                (setf code -1)))
              (charms/ll:timeout -1)))
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
        ;(dbg-log-format "2 code=~X ctrl-key=~S alt-key=~S" code ctrl-key alt-key)
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
                  ((= code mouse-code)
                   ;; for mouse
                   (multiple-value-bind (bstate x y z id)
                       (charms/ll:getmouse)
                     ;(dbg-log-format "bstate=~X x=~D y=~D z=~D id=~D"
                     ;                bstate x y z id)
                     (mouse-event-proc bstate x y)))
                  ((= code abort-code)
                   (setf esc-key nil)
                   :abort)
                  ((= code escape-code)
                   ;(setf esc-key nil)
                   ;(charms/ll:timeout 100)
                   ;(let ((code (prog1 (get-ch)
                   ;              (charms/ll:timeout -1))))
                   ;  (cond ((= code -1)
                   ;         (setf esc-key t)
                   ;         (get-key-from-name "escape"))
                   ;        ((= code #.(char-code #\[))
                   ;         (if (= (prog1 (get-ch)
                   ;                  (charms/ll:timeout -1))
                   ;                #.(char-code #\<))
                   ;             ;;sgr(1006)
                   ;             (uiop:symbol-call :lem-mouse-sgr1006 :parse-mouse-event)
                   ;             (get-key-from-name "escape"))) ;; [tbd] unknown escape sequence
                   ;        (t
                   ;         (let ((key (get-key code)))
                   ;           (make-key :meta t
                   ;                     :sym (key-sym key)
                   ;                     :ctrl (key-ctrl key)))))))
                   (setf esc-key t)
                   (get-key-from-name "escape"))
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
                (let ((event (get-event)))
                  (if (eq event :abort)
                      (send-abort-event editor-thread nil)
                      (send-event event)))
                ;; workaround for exit problem
                ;; workaround for display update problem (incomplete)
                (sleep 0.0001))
            #+sbcl
            (sb-sys:interactive-interrupt (c)
              (declare (ignore c))
              (send-abort-event editor-thread t))))
      (exit-editor (c) (return-from input-loop c))))

  ;; workaround for exit problem
  (defmethod lem-if:invoke ((implementation ncurses) function)
    (let ((result nil)
          (input-thread (bt:current-thread)))
      (unwind-protect
          (progn
            (when (lem.term:term-init)
              (let ((editor-thread
                      (funcall function
                               nil
                               (lambda (report)
                                 (bt:interrupt-thread
                                  input-thread
                                  (lambda () (error 'exit-editor :value report)))))))
                (setf result (input-loop editor-thread))
                ;; workaround for exit problem
                ;; (to avoid 'compilation unit aborted caught 1 fatal ERROR condition')
                (bt:join-thread editor-thread)
                )))
        (lem.term:term-finalize))
      (when (and (typep result 'exit-editor)
                 (exit-editor-value result))
        (format t "~&~A~%" (exit-editor-value result)))))

  ;; workaround for display update problem (incomplete)
  (defun force-refresh-display (width height)
    (loop :for y1 :from 0 :below height
          :with str := (make-string width :initial-element #\.)
          :do ;(dbg-log-format "y1=~D" y1)
              (charms/ll:mvwaddstr charms/ll:*stdscr* y1 0 str))
    (charms/ll:refresh)
    (sleep 0.1))

  ;; for resizing display
  (defun resize-display ()
    (when *resizing*
      (setf *resizing* nil)
      (charms/ll:resizeterm 0 0)
      (charms/ll:erase)
      ;; workaround for display update problem (incomplete)
      (force-refresh-display charms/ll:*cols* charms/ll:*lines*)))

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

  ;; use only stdscr
  (defmethod lem-if:delete-view ((implementation ncurses) view)
    ;(dbg-log-format "lem-if:delete-view")
    ;(charms/ll:delwin (ncurses-view-scrwin view))
    ;(when (ncurses-view-modeline-scrwin view)
    ;  (charms/ll:delwin (ncurses-view-modeline-scrwin view))))
    )

  ;; use only stdscr
  (defmethod lem-if:clear ((implementation ncurses) view)
    ;(dbg-log-format "lem-if:clear")
    ;(charms/ll:clearok (ncurses-view-scrwin view) 1)
    ;(when (ncurses-view-modeline-scrwin view)
    ;  (charms/ll:clearok (ncurses-view-modeline-scrwin view) 1)))
    (loop :for y1 :from 0 :below (+ (ncurses-view-height view)
                                    (if (ncurses-view-modeline-scrwin view) 1 0))
          :with str := (make-string (ncurses-view-width view) :initial-element #\space)
          :do (charms/ll:mvwaddstr (ncurses-view-scrwin view)
                                   (get-pos-y view 0 y1)
                                   (get-pos-x view 0 y1)
                                   str)))

  ;; use only stdscr
  (defmethod lem-if:set-view-size ((implementation ncurses) view width height)
    ;(dbg-log-format "lem-if:set-view-size")
    (setf (ncurses-view-width view) width)
    (setf (ncurses-view-height view) height)
    ;(charms/ll:wresize (ncurses-view-scrwin view) height width)
    ;(when (ncurses-view-modeline-scrwin view)
    ;  (charms/ll:mvwin (ncurses-view-modeline-scrwin view)
    ;                   (+ (ncurses-view-y view) height)
    ;                   (ncurses-view-x view))
    ;  (charms/ll:wresize (ncurses-view-modeline-scrwin view)
    ;                     (minibuffer-window-height)
    ;                     width)))
    )

  ;; use only stdscr
  (defmethod lem-if:set-view-pos ((implementation ncurses) view x y)
    ;(dbg-log-format "lem-if:set-view-pos")
    (setf (ncurses-view-x view) x)
    (setf (ncurses-view-y view) y)
    ;(charms/ll:mvwin (ncurses-view-scrwin view) y x)
    ;(when (ncurses-view-modeline-scrwin view)
    ;  (charms/ll:mvwin (ncurses-view-modeline-scrwin view)
    ;                   (+ y (ncurses-view-height view))
    ;                   x)))
    )

  ;; dealing with utf-16 surrogate pair characters
  (defun get-charcode-from-scrwin (view x y)
    (let ((c-lead (logand (charms/ll:mvwinch (ncurses-view-scrwin view) y x)
                          charms/ll:A_CHARTEXT)))
      (cond
       ((and (<= #xd800 c-lead #xdbff) (< x (- (ncurses-view-width view) 1)))
        (let ((c-trail (logand (charms/ll:mvwinch (ncurses-view-scrwin view) y (+ x 1))
                               charms/ll:A_CHARTEXT)))
          (+ #x10000 (* (- c-lead #xd800) #x0400) (- c-trail #xdc00))))
       (t
        c-lead))))

  ;; get pos-x/y for printing wide characters
  (defun get-pos-x (view x y &key (modeline nil) (cursor nil))
    (unless (or (= lem.term:*pos-adjust-mode* 1)
                (and (= lem.term:*pos-adjust-mode* 2) (not cursor)))
      (return-from get-pos-x (+ x (ncurses-view-x view))))
    (let* ((start-x (ncurses-view-x view))
           (disp-x0 (+ x start-x))
           (disp-x  start-x)
           (pos-x   start-x)
           (pos-y   (get-pos-y view x y :modeline modeline)))
      (loop :while (< disp-x disp-x0)
            :for c-code := (get-charcode-from-scrwin view pos-x pos-y)
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
  (defun get-pos-y (view x y &key (modeline nil))
    (+ y (ncurses-view-y view) (if modeline (ncurses-view-height view) 0)))

  ;; adjust line width by using zero-width-space character (#\u200b)
  (defun adjust-line (view x y &key (modeline nil))
    (unless (or (= lem.term:*pos-adjust-mode* 1)
                (= lem.term:*pos-adjust-mode* 2))
      (return-from adjust-line))
    (let* ((start-x    (ncurses-view-x view))
           (disp-width (ncurses-view-width view))
           (disp-x0    (+ disp-width start-x))
           (pos-x      (get-pos-x view disp-width y :modeline modeline))
           (pos-y      (get-pos-y view disp-width y :modeline modeline)))
      ;(dbg-log-format "disp-w=~D disp-x0=~D pos-x=~D pos-y=~D"
      ;                disp-width disp-x0 pos-x pos-y)
      (when (> disp-x0 pos-x)
        (charms/ll:mvwaddstr (ncurses-view-scrwin view) pos-y pos-x
                             (make-string (- disp-x0 pos-x)
                                          :initial-element #\u200b)))))

  ;; workaround for display problem of utf-16
  ;; surrogate pair characters (incomplete)
  (defun remake-string (string)
    (let ((clist    '())
          (splitted nil))
      (loop :for c :across string
            :for c-code := (char-code c)
            :do (cond 
                 ((>= c-code #x10000)
                  (multiple-value-bind (q r) (floor (- c-code #x10000) #x0400)
                    (push (code-char (+ q #xd800)) clist)
                    (push (code-char (+ r #xdc00)) clist)
                    (setf splitted t)))
                 (t
                  (push c clist))))
      ;(dbg-log-format "clist=~S" (reverse clist))
      (if splitted (concatenate 'string (reverse clist)) string)))

  ;; clip string to fit inside of view
  (defun clip-string (view x y string)
    (let ((disp-width (ncurses-view-width view))
          (str-len    (length string)))
      (cond
       ((>= x disp-width)
        "")
       ((> (+ x str-len) disp-width)
        (subseq string 0 (- disp-width x)))
       (t
        string))))

  ;; use get-pos-x/y and adjust-line
  (defmethod lem-if:print ((implementation ncurses) view x y string attribute)
    ;(dbg-log-format "lem-if:print")
    ;(dbg-log-format "x=~D y=~D view-x=~D view-y=~D view-w=~D view-h=~D"
    ;                x y (ncurses-view-x view) (ncurses-view-y view)
    ;                (ncurses-view-width view) (ncurses-view-height view))
    ;(dbg-log-format "x=~D y=~D pos-x=~D pos-y=~D string=~S"
    ;                x y (get-pos-x view x y) (get-pos-y view x y) string)
    (let ((attr (attribute-to-bits attribute)))
      (charms/ll:wattron (ncurses-view-scrwin view) attr)
      ;(charms/ll:scrollok (ncurses-view-scrwin view) 0)
      (charms/ll:mvwaddstr (ncurses-view-scrwin view)
                           (get-pos-y view x y)
                           (get-pos-x view x y)
                           (clip-string view x y (remake-string string)))
      ;(charms/ll:scrollok (ncurses-view-scrwin view) 1)
      (charms/ll:wattroff (ncurses-view-scrwin view) attr)
      (adjust-line view x y)))

  ;; use get-pos-x/y and adjust-line
  (defmethod lem-if:print-modeline ((implementation ncurses) view x y string attribute)
    ;(dbg-log-format "lem-if:print-modeline")
    (let ((attr (attribute-to-bits attribute)))
      (charms/ll:wattron (ncurses-view-modeline-scrwin view) attr)
      (charms/ll:mvwaddstr (ncurses-view-modeline-scrwin view)
                           (get-pos-y view x y :modeline t)
                           (get-pos-x view x y :modeline t)
                           (clip-string view x y (remake-string string)))
      (charms/ll:wattroff (ncurses-view-modeline-scrwin view) attr)
      (adjust-line view x y :modeline t)))

  ;; use get-pos-x/y and adjust-line
  (defmethod lem-if:clear-eol ((implementation ncurses) view x y)
    ;(dbg-log-format "lem-if:clear-eol")
    ;(dbg-log-format "x=~D y=~D view-x=~D view-y=~D view-w=~D view-h=~D"
    ;                x y (ncurses-view-x view) (ncurses-view-y view)
    ;                (ncurses-view-width view) (ncurses-view-height view))
    ;(charms/ll:wmove (ncurses-view-scrwin view) y x)
    ;(charms/ll:wclrtoeol (ncurses-view-scrwin view)))
    (charms/ll:mvwaddstr (ncurses-view-scrwin view)
                         (get-pos-y view x y)
                         (get-pos-x view x y)
                         (make-string (max (- (ncurses-view-width view) x) 0)
                                      :initial-element #\space))
    (adjust-line view x y))

  ;; use get-pos-x/y and adjust-line
  (defmethod lem-if:clear-eob ((implementation ncurses) view x y)
    ;(dbg-log-format "lem-if:clear-eob")
    ;(dbg-log-format "x=~D y=~D view-x=~D view-y=~D view-w=~D view-h=~D"
    ;                x y (ncurses-view-x view) (ncurses-view-y view)
    ;                (ncurses-view-width view) (ncurses-view-height view))
    ;(charms/ll:wmove (ncurses-view-scrwin view) y x)
    ;(charms/ll:wclrtobot (ncurses-view-scrwin view)))
    (lem-if:clear-eol implementation view x y)
    (loop :for y1 :from (+ y 1) :below (ncurses-view-height view)
          :with str := (make-string (ncurses-view-width view) :initial-element #\space)
          :do (charms/ll:mvwaddstr (ncurses-view-scrwin view)
                                   (get-pos-y view 0 y1)
                                   (get-pos-x view 0 y1)
                                   str)))

  ;; use only stdscr
  (defmethod lem-if:redraw-view-after ((implementation ncurses) view focus-window-p)
    ;(dbg-log-format "lem-if:redraw-view-after")
    (let ((attr (attribute-to-bits 'modeline)))
      (charms/ll:attron attr)
      (when (and (ncurses-view-modeline-scrwin view)
                 (< 0 (ncurses-view-x view)))
        (loop :for y1 :from 0 :below (+ (ncurses-view-height view) 1)
              :do (charms/ll:mvwaddch (ncurses-view-scrwin view)
                                      (+ (ncurses-view-y view) y1)
                                      (- (ncurses-view-x view) 1)
                                      (char-code #\space))))
      (charms/ll:attroff attr))
    (charms/ll:wnoutrefresh (ncurses-view-scrwin view)))

  ;; adjust cursor position
  (defmethod lem-if:update-display ((implementation ncurses))
    ;(dbg-log-format "lem-if:update-display")
    (let* ((view   (window-view (current-window)))
           (scrwin (ncurses-view-scrwin view)))
      (if (lem::covered-with-floating-window-p (current-window) lem::*cursor-x* lem::*cursor-y*)
          (charms/ll:curs-set 0)
          (progn
            (charms/ll:curs-set 1)
            (charms/ll:wmove scrwin
                             (get-pos-y view lem::*cursor-x* lem::*cursor-y*)
                             (get-pos-x view lem::*cursor-x* lem::*cursor-y* :cursor t))))
      (charms/ll:wnoutrefresh scrwin)
      (charms/ll:doupdate)))

  )

