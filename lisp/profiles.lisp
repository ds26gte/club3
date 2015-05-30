;last change 2009-03-27

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *club-name*))

(defun verbal-eagerness (n)
  (case n
    (0 "Inactive")
    (1 "Taking it slow")
    (2 "Normal")
    (3 "Fast Track")
    (t n)))

(defun log-member-data-1 (m)
  ;disabling printing of assignable roles and eagerness
  (let ((mem (symbol-value m))
        roles-allowed)
    (with-open-file (o (format nil "profiles/~a.txt" m) :direction :output
                       :if-exists :supersede)
      (format o "~%Name: ~a~%~%" (full-name m))
      (format o "Number of speeches (?): ~a~%~%"
              (gethash 'speaker
                       (cmember*-rolecount mem) 0))
      (setq roles-allowed (cmember*-expertise mem))
      (format o "Assignable roles: ~a~%~%" (cmember*-expertise mem))
      (format o "Availability: ~a~%~%" (humanize-dates-in-expression
                                         (cmember*-availability mem)))
      ;(format o "Eagerness level: ~a (~a)~%~%"
      ;eagerness (verbal-eagerness (cmember*-eagerness mem)))
      (format o "Last time performing role: ~%")
      (let ((h (cmember*-phistory mem)))
        (dolist (r *roles*)
          (let ((role-date (gethash r h 0)))
            (unless (and (not (member r roles-allowed)) (= role-date 0))
              (format o "    ~10@a: ~a~%"  r (human-date role-date))))))
      (terpri o))))

(defun make-profiles ()
  (let ((*print-case* :downcase)
        (title (format nil "Scheduling Profiles of the Members of ~a"
                       *club-name*)))
    ;most Lisps won't automatically create the enclosing directories of an
    ;output file; Clozure does
    #-clozure
    (os-execute "if test ! -d profiles; then mkdir profiles; fi")
    (with-open-file (o (format nil "profiles/index.html") :direction :output
                       :if-exists :supersede)
      (html-title title o)
      (format o "<body style=\"margin-top: 2cm; margin-left: 2cm; margin-bottom: 2cm\">~%")
      (format o "<h3>~a</h3>~%" title)
      (dolist (m *members*)
        (log-member-data-1 m)
        (format o "<a href=\"~a.txt\">~a</a><br class=doublespaced>~%" m
                (full-name m)))
      (format o "</body>~%</html>~%"))))
