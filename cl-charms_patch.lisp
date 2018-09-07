;; workaround for windows pdcurses

(in-package :cl-charms/low-level)

#+win32
(progn

  (export '(getch color-pair resizeterm *escdelay*))

  ;; add missing definitions
  (defun getch () (wgetch *STDSCR*))
  (defun color-pair (n) (logand (ash n 24) #xff000000))
  (setf (fdefinition 'resizeterm) #'resize-term)
  (defvar *escdelay-dummy* 0)
  (define-symbol-macro *escdelay* *escdelay-dummy*)

  ;; enable modifier keys
  (define-exported-constant PDC_KEY_MODIFIER_SHIFT   1)
  (define-exported-constant PDC_KEY_MODIFIER_CONTROL 2)
  (define-exported-constant PDC_KEY_MODIFIER_ALT     4)
  (define-exported-constant PDC_KEY_MODIFIER_NUMLOCK 8)
  (define-exported-cfuns ("PDC_get_key_modifiers") :unsigned-long)
  (define-exported-cfuns ("PDC_save_key_modifiers") :int (flag bool))
  (define-exported-cfuns ("PDC_return_key_modifiers") :int (flag bool))

  ;; for extracting a character code
  (define-exported-constant A_CHARTEXT #x0000ffff)

  )

