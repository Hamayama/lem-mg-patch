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

  ;; pos-x is used for printing characters
  (defun get-pos-x (view x y)
    (let* ((screen (lem::window-screen (ncurses-view-window view)))
           (str    (or (car (aref (lem::screen-lines screen) y)) ""))
           (pos-x  0))
      ;(dbg-log-format "x=~D y=~D str=~S" x y str)
      (loop :with w := 0 :while (< w x)
            :for c :across str
            :do (setf w (lem-base:char-width c w))
                (cond
                 ((gethash c lem-base:*char-replacement*)
                  (setf pos-x (lem-base:char-width c pos-x)))
                 (t
                  (incf pos-x))))
      pos-x))

  ;; use get-pos-x
  (defmethod lem-if:print ((implementation ncurses) view x y string attribute)
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
    (charms/ll:wmove (ncurses-view-scrwin view) y (get-pos-x view x y))
    (charms/ll:wclrtoeol (ncurses-view-scrwin view)))

  ;; use get-pos-x
  (defmethod lem-if:clear-eob ((implementation ncurses) view x y)
    (charms/ll:wmove (ncurses-view-scrwin view) y (get-pos-x view x y))
    (charms/ll:wclrtobot (ncurses-view-scrwin view)))

  ;; enable modifier keys
  (let ((resize-code (get-code "[resize]"))
        (abort-code (get-code "C-]"))
        (escape-code (get-code "escape"))
        (ctrl-key nil)
        (alt-key  nil))
    (defun get-ch ()
      (charms/ll:PDC-save-key-modifiers 1)
      (let ((code          (charms/ll:getch))
            (modifier-keys (charms/ll:PDC-get-key-modifiers)))
        (setf ctrl-key (logtest modifier-keys charms/ll:PDC_KEY_MODIFIER_CONTROL))
        (setf alt-key  (logtest modifier-keys charms/ll:PDC_KEY_MODIFIER_ALT))
        ;(dbg-log-format "code=~D ctrl-key=~S alt-key=~S" code ctrl-key alt-key)
        ;; ctrl key workaround
        (when ctrl-key
          (cond
           ;; C-space / C-@
           ((= code 64) (setf code 0))
           ;; C-down / C-up / C-left / C-right
           ((= code #x1e1) (setf code 525))
           ((= code #x1e0) (setf code 566))
           ((= code #x1bb) (setf code 545))
           ((= code #x1bc) (setf code 560))
           ))
        ;; alt key workaround
        (when alt-key
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
        code))
    (defun get-event ()
      (tagbody :start
        (return-from get-event
          (let ((code (get-ch)))
            (cond ((= code -1) (go :start))
                  ((= code resize-code) :resize)
                  ((= code abort-code) :abort)
                  ((= code escape-code)
                   (charms/ll:timeout 100)
                   (let ((code (prog1 (get-ch)
                                 (charms/ll:timeout -1))))
                     (cond ((= code -1)
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
                  (alt-key
                   (let ((key (get-key code)))
                     (make-key :meta t
                               :sym (key-sym key)
                               :ctrl (key-ctrl key))))
                  (t
                   (get-key code))))))))

  )

