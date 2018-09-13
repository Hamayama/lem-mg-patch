;; workaround for windows

(in-package :lem-lisp-mode)

#+win32
(progn

  ;; quit slime to exit lem normally (incomplete)
  (add-hook *exit-editor-hook*
            (lambda ()
              (ignore-errors
               (slime-quit))))

  )

