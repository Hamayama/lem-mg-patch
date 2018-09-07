;; workaround for windows

(in-package :lem-base)

#+win32
(progn

  ;; set default buffer encoding to utf-8
  (defmethod initialize-instance :after ((buffer buffer) &rest initargs)
    (setf (buffer-encoding buffer) (encoding :utf-8 :lf)))

  )

