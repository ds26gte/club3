;last change 2009-04-03

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *roles*)
  (defvar *easy-roles*)
  (defvar *intermediate-roles*)
  (defvar *hard-roles*)
  (defvar *role-names*))

(defun check-lineup-for-roles (lineup)
  (dolist (role lineup)
    (pushnew role *roles*))
  (setq *roles* (nreverse *roles*)))

(defmacro role-names (&rest args)
  `(progn (setq *role-names* (make-hash-table))
          (mapc #'create-role-name ',args)))

(defun create-role-name (arg)
  (setf (gethash (car arg) *role-names*) (cadr arg)))

(defun role-full-name (r)
  (or (gethash r *role-names*)
      (string-capitalize (symbol-name r))))

(defmacro role-difficulty (&rest args)
  `(progn
    (assert (and (boundp '*roles*) (listp *roles*)) ()
      "LINEUPS must be called before ROLE-DIFFICULTY")
    (setq *easy-roles* *roles*
          *intermediate-roles* '()
          *hard-roles* '())
    (mapc #'create-role-difficulty ',args)))

(defun create-role-difficulty (arg)
  (destructuring-bind (r d) arg
    (ecase d
      (:easy (setq *intermediate-roles* (remove r *intermediate-roles*)
                   *hard-roles* (remove r *hard-roles*))
       (pushnew r *easy-roles*))
      (:intermediate (setq *easy-roles* (remove r *easy-roles*)
                           *hard-roles* (remove r *hard-roles*))
       (pushnew r *intermediate-roles*))
      (:hard (setq *easy-roles* (remove r *easy-roles*)
                   *intermediate-roles* (remove r *intermediate-roles*))
       (pushnew r *hard-roles*)))))
