;last change 2009-03-27

(defstruct meeting* date type schedule)

(load (merge-pathnames "pickassignment" *load-pathname*))
(load (merge-pathnames "printschedule" *load-pathname*))

(defun schedule (cdate type-of-meeting)
  (let ((lineup (gethash type-of-meeting *lineups*))
        (*date* cdate))
    (declare (special *date*))
    (when lineup
      (get-a-good-assignment lineup))))

(defun file-schedule (date schedule)
  (let ((*print-case* :downcase)
        (schedule-file (format nil "~a.txt" date)))
    (with-open-file (o schedule-file
                       :direction :output
                       :if-exists :supersede)
      (dolist (i schedule)
        (destructuring-bind (role person) i
          (format o "~a ~a~%" role person))))
    schedule-file))

(defun make-schedules ()
  (let ((meetings '()))
    (dolist (calendar-entry *calendar*)
      (destructuring-bind (date type-of-meeting) calendar-entry
        ;date looks like yyyymmdd and is for easy printing and
        ;filenaming;
        ;cdate (canonicalized date) is #days since 8 years ago and is
        ;used for calculations
        (let* ((cdate (encode-yyyymmdd-time date))
               (schedule (schedule cdate type-of-meeting)))
          (pushnew (make-meeting* :date date
                                 :type type-of-meeting
                                 :schedule schedule)
                   meetings)
          (let ((sfile (file-schedule date schedule)))
            (read-history-from-schedule date cdate sfile)))))
    (print-meetings (nreverse meetings))))
