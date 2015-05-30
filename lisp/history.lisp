;last change 2015-05-30

(defun read-history-from-schedule (date cdate schedule-file)
  (with-open-file (i schedule-file )
    (loop
      (let* ((role (or (read i nil) (return)))
             (member-name (read i nil)))
        (cond ((not (boundp member-name))
               (not-a-member member-name))
              ((not (member role *roles*))
               (warn "~a not a valid role" role))
              (t (let* ((m (symbol-value member-name))
                        (member-history (cmember*-history m))
                        (member-phistory (cmember*-phistory m))
                        (member-role-last (gethash role member-history 0)))
                   (when (> cdate member-role-last)
                     (setf (gethash role member-history) cdate
                           (gethash role member-phistory) date))
                   (incf (gethash role (cmember*-rolecount m) 0)))))))))

(defun read-history ()
  (os-execute "ls past-schedules/*.txt > .tmp 2> /dev/null")
  (with-open-file (i ".tmp")
    (loop
      (let* ((schedule-file (or (read-line i nil) (return)))
             (date (subseq schedule-file 0
                           (position #\. schedule-file :from-end t)))
             (slash (position #\/ date :from-end t)))
        (when slash
          (setq date (subseq date (+ slash 1))))
        (setq date (read-from-string date))
        (read-history-from-schedule
         date
         (encode-yyyymmdd-time date)
         schedule-file))))
  (when (fboundp 'read-history-hook)
    (funcall 'read-history-hook)))
