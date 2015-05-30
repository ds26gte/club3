;last change 2009-03-18

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *lineups*)
  (defvar *calendar*))

(load (merge-pathnames "roles" *load-pathname*))

(defun create-lineup (l &optional default)
  (destructuring-bind (type-of-meeting lineup) l
    (check-lineup-for-roles lineup)
    (setf (gethash type-of-meeting *lineups*) lineup)
    (when (and default (not (eq type-of-meeting :default)))
      (setf (gethash :default *lineups*) lineup))))

(defmacro lineups (l &rest ll)
  `(progn (setq *lineups* (make-hash-table)
                *roles* '())
          (create-lineup ',l t)
          (mapc #'create-lineup ',ll)))

(defun create-calendar-entry (arg)
  (let ((date arg) (type-of-meeting :default))
    (when (consp arg)
      (setq date (car arg) type-of-meeting (cadr arg)))
    (push (list (undot-yyyy-mm-dd date) type-of-meeting) *calendar*)))

(defmacro calendar (&rest args)
  `(progn (setq *calendar* '())
          (mapc #'create-calendar-entry ',args)
          (setq *calendar* (nreverse *calendar*))))

(defun number-of-trials (n)
  (setq *num-trials* n))
