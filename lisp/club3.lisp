(eval-when (:compile-toplevel :load-toplevel :execute)
  #+clozure
  (declaim (optimize (compilation-speed 0)
                     (debug 0)
                     (safety 0)
                     (space 0)
                     speed)))

(defpackage :club3
  (:use :cl)
  (:export :generate))

(in-package :club3)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *version* 20090823) ;last real change
  (defparameter *copyright* "Copyright © 2009 Dorai Sitaram")
  (defparameter *website*
    ;for details, please see
    "https://github.com/ds26gte/club3")
  (defvar *club-name* "&#x2663;&#x2663;&#x2663;")
  (defvar *num-trials* 400)
  )

(eval-when (:compile-toplevel)
  (load (merge-pathnames "dotopo" *compile-file-pathname*)))

(eval-when (:compile-toplevel)
  (do-in-topological-order
      (lambda (f)
        (unless (eq f 'club3)
          (compile-file (merge-pathnames (string-downcase (symbol-name f))
                                         *compile-file-pathname*))))
    ;
    (club3 ;depends on
      utils lineups members history schedules profiles)
    (utils
      implspecific dateprocs)
    ;(history roles)
    ;(members roles)
    (lineups
      roles)
    (schedules
      pickassignment printschedule)
    (pickassignment
      demerits)))

;(setq *read-default-float-format* 'long-float)

(load (merge-pathnames "utils" *load-pathname*))
(load (merge-pathnames "lineups" *load-pathname*))
(load (merge-pathnames "members" *load-pathname*))
(load (merge-pathnames "history" *load-pathname*))
(load (merge-pathnames "schedules" *load-pathname*))
(load (merge-pathnames "profiles" *load-pathname*))

(load "club3rc")
(read-history)

(defparameter *this-package* *package*)

(defun generate (x)
  (let ((*package* *this-package*))
    (ecase x
      (:schedules (make-schedules))
      (:profiles (make-profiles)))))
