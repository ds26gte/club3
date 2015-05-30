;last change 2015-05-30

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *members*))

(defvar *capitalized-names*)

(defun club-name (s)
  (setq *club-name* s))

(defstruct cmember*
  name
  expertise
  (eagerness 2.0)
  (availability '(always))
  (availablep #'always)
  (rolecount (make-hash-table))
  (history (make-hash-table))
  (phistory (make-hash-table)))

(defun create-member (m)
  (setf (symbol-value m) (make-cmember* :name m))
  (pushnew m *members*))

(defmacro members (&rest mm)
  `(progn (setq *members* '())
          (mapc #'create-member ',mm)
          (setq *members* (nreverse *members*))))

(defun create-capitalized-name (nn)
  (setf (gethash (car nn) *capitalized-names*) (cadr nn)))

(defmacro full-names (&rest nn)
  `(progn (setq *capitalized-names* (make-hash-table))
          (mapc #'create-capitalized-name ',nn)))

(defun full-name (x)
  (or (gethash x *capitalized-names*)
      (string-capitalize (symbol-name x))))

(defvar *non-members* '())

(defun not-a-member (m)
  (unless (member m *non-members*)
    (push m *non-members*)
    (warn "~a is not a member" m)))

(defun create-availability-info (arg)
  (destructuring-bind (m p) arg
    (cond ((not (member m *members*))
           (not-a-member m))
          (t (setq m (symbol-value m))
             (setf (cmember*-availability m) p
                   (cmember*-availablep m)
                   (eval `(lambda ()
                           ,(canonicalize-dates-in-expression p))))))))

(defmacro availability (&rest args)
  `(progn
    (assert (and (boundp '*members*) (listp *members*)) ()
      "MEMBERS must be called before AVAILABILITY")
    (mapc #'create-availability-info ',args)))

(defun update-expertise (m r action)
  (if (consp r)
    (if (eq (car r) '-)
      (dolist (r1 (cdr r))
        (update-expertise m r1 :delete))
      (dolist (r1 r)
        (update-expertise m r1 action)))
    (case r
      (:easy (dolist (r *easy-roles*)
               (update-expertise m r action)))
      (:intermediate (dolist (r *easy-roles*)
                       (update-expertise m r action))
                     (dolist (r *intermediate-roles*)
                       (update-expertise m r action)))
      (:hard (dolist (r *easy-roles*)
               (update-expertise m r action))
             (dolist (r *intermediate-roles*)
               (update-expertise m r action))
             (dolist (r *hard-roles*)
               (update-expertise m r action)))
      (:all (dolist (r *roles*)
              (update-expertise m r action)))
      (t (case action
           (:add (pushnew r (cmember*-expertise m)))
           (:delete (setf (cmember*-expertise m)
                          (delete r (cmember*-expertise m)))))))))

(defun create-expertise (arg)
  (destructuring-bind (m &rest rr) arg
    (cond ((not (member m *members*))
           (not-a-member m))
          (t (update-expertise (symbol-value m) rr :add)))))

(defmacro expertise (&rest args)
  `(progn
    (assert (and (boundp '*members*) (listp *members*)) ()
      "MEMBERS must be called before EXPERTISE")
    '(dolist (m *members*)
      (let ((m-s (symbol-value m)))
        (dolist (r *easy-roles*)
          (pushnew r (cmember*-expertise m-s)))))
    (mapc #'create-expertise ',args)))

(defun create-eagerness (arg)
  (destructuring-bind (m e) arg
    (cond ((not (member m *members*))
           (not-a-member m))
          (t
           (setf (cmember*-eagerness (symbol-value m)) (* 1.0 e))))))

(defmacro eagerness (&rest args)
  `(progn
    (assert (and (boundp '*members*) (listp *members*)) ()
      "MEMBERS must be called before EAGERNESS")
    (mapc #'create-eagerness ',args)))
