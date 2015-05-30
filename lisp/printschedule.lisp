;last change 2009-03-27

(defun multiply-instanced-role-p (r lineup)
  (/= (position r lineup)
      (position r lineup :from-end t)))

(defun nth-assoc (k al n)
  (cond (n (decf n))
        (t (setq n 0)))
  (loop
    (let ((c (or (assoc k al) (return nil))))
      (when (= n 0) (return c))
      (decf n)
      (setq al (cdr (member c al))))))

(defun print-meetings (meetings)
  (let* ((date1 (meeting*-date (car meetings)))
         (dateN (meeting*-date (car (last meetings))))
         (html-file-name (format nil "~a-~a" date1 dateN)))
    (with-open-file (o (format nil "~a.html" html-file-name) :direction :output
                       :if-exists :supersede)
      (setq date1 (human-date date1)
            dateN (human-date dateN))
      (html-title html-file-name o)
      (format o "<body style=\"margin-top: 2cm; margin-bottom: 2cm; ")
      (format o "margin-left: 2cm; margin-right:2cm\">~%")
      (format o "<h2 style=\"font-family: sans-serif\" align=center>~
                 ~a Schedule</h2>~%" *club-name*)
      (format o "<h3 style=\"font-family: sans-serif\" align=center> ~a &mdash; ~a</h3>~%"
              date1 dateN)
      (format o "<div align=center>~%<table cellpadding=2 width=\"80%\">~%")
      ;
      (format o "<tr class=meetingdate>~%")
      (format o "<td align=right>Meeting Date:&nbsp;</td>~%")
      (dolist (m meetings)
        (format o "<td align=center>~a</td>~%" (month-and-day (meeting*-date m))))
      (format o "</tr>~%")
      ;
      (let ((multiply-instanced-roles (make-hash-table))
            (superset-lineup (or (gethash :superset *lineups*)
                                 (gethash :default *lineups*))))
        (dotimes (i (length superset-lineup))
          (let ((role (elt superset-lineup i))
                (suffix nil))
            (when (multiply-instanced-role-p role superset-lineup)
              (setq suffix (incf (gethash role multiply-instanced-roles 0))))
            (format o "<tr")
            (when (oddp i)
              (format o " class=oddrow"))
            (format o ">~%")
            (format o "<td align=left class=rolename>~
                       &nbsp;~a~a</td>~%"
                    (role-full-name role)
                    (if suffix (format nil " #~a" suffix) ""))
            (dolist (m meetings)
              (let ((s (meeting*-schedule m))
                    (mtype (format nil "<small>~a</small>"
                                   (meeting*-type m))))
                (format o "<td align=center>~a</td>~%"
                        (let ((person (cadr (nth-assoc role s suffix))))
                          (cond (person (full-name person))
                                ((or (= i 0) suffix) mtype)
                                (t ""))))))
            (format o "</tr>~%"))))
      (format o "</table>~%</div>~%</body>~%</html>~%"))
    (os-execute
     (format nil "cp -p ~a.html new-schedules.html" html-file-name))))
