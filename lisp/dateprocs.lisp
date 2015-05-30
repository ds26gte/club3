;last change 2009-03-18

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *date*))

(defvar *zero-year*
  ;choose it to be 8 years ago
  #.(multiple-value-bind (s m h d mo y)
      (decode-universal-time (get-universal-time) 5)
      (declare (ignore s m h d mo))
      (- y 8)))

(defun every-week () t)

(defun always () t)

(defun on (&rest dates)
  (member *date* dates))

(defun between (date1 date2)
  (<= date1 *date*  date2))

(defun after (date)
  (> *date* date))

(defun from (date)
  (>= *date* date))

(defun before (date)
  (< *date* date))

(defun until (date)
  (if (eq date :further-notice) t
    (<= *date* date)))

(defun encode-yyyymmdd-time (yyyymmdd)
  (declare (type fixnum yyyymmdd))
  ;return number of days since the beginning of *zero-year*, which is 8 years ago
  ;(an arbitrary but fixed point in the past for each Lisp session).
  ;Returns 0 if yyyymmdd is before *zero-year*.
  ;Don't worry about leap years
  (multiple-value-bind (yyyy mmdd) (floor yyyymmdd 10000)
    (declare (type fixnum yyyy mmdd))
    (if (< yyyy 2000) 0
      (multiple-value-bind (mm dd) (floor mmdd 100)
        (declare (type fixnum mm dd))
        (+ (* 365 (- yyyy 2000))
           (ecase mm
             ;(month  days-in-year-before-month-started)
             (1 0)
             (2 31)
             (3 #.(+ 31 28))
             (4 #.(+ 31 28 31))
             (5 #.(+ 31 28 31 30))
             (6 #.(+ 31 28 31 30 31))
             (7 #.(+ 31 28 31 30 31 30))
             (8 #.(+ 31 28 31 30 31 30 31))
             (9 #.(+ 31 28 31 30 31 30 31 31))
             (10 #.(+ 31 28 31 30 31 30 31 31 30))
             (11 #.(+ 31 28 31 30 31 30 31 31 30 31))
             (12 #.(+ 31 28 31 30 31 30 31 31 30 31 30)))
           dd)))))

(defun num-to-month (mm)
  (declare (type fixnum mm))
  (ecase mm
    (1 "Jan") (2 "Feb") (3 "March") (4 "April") (5 "May") (6 "June")
    (7 "July") (8 "Aug") (9 "Sept") (10 "Oct") (11 "Nov") (12 "Dec")))

(defun human-date (yyyymmdd)
  (declare (type fixnum yyyymmdd))
  (if (= yyyymmdd 0) "?"
    (multiple-value-bind (yyyy mmdd) (floor yyyymmdd 10000)
      (multiple-value-bind (mm dd) (floor mmdd 100)
        (format nil "~a ~a, ~a"
                (num-to-month mm)
                dd
                yyyy)))))

(defun undot-yyyy-mm-dd (yyyy-mm-dd)
  (declare (type symbol yyyy-mm-dd))
  (let ((n (read-from-string (remove #\. (symbol-name yyyy-mm-dd)))))
    (if (numberp n) n yyyy-mm-dd)))

(defun canonicalize-dates-in-expression (e)
  (typecase e
    (cons (cons (canonicalize-dates-in-expression (car e))
                (canonicalize-dates-in-expression (cdr e))))
    (symbol (let ((e-s (symbol-name e)))
              (if (not (position #\. e-s)) e
                (let ((n (read-from-string (remove #\. e-s))))
                  (if (numberp n) (encode-yyyymmdd-time n)
                    e)))))
    (t e)))

(defun humanize-dates-in-expression (e)
  ;used only when printing availability profile
  (typecase e
    (cons (cons (humanize-dates-in-expression (car e))
                (humanize-dates-in-expression (cdr e))))
    (symbol (let ((e-s (symbol-name e)))
              (if (not (position #\. e-s)) e
                (let ((n (read-from-string (remove #\. e-s))))
                  (if (numberp n)
                    (concatenate 'string "{" (human-date n) "}")
                    e)))))
    (t e)))

(defun month-and-day (yyyymmdd)
  (declare (type fixnum yyyymmdd))
  (multiple-value-bind (yyyy mmdd) (floor yyyymmdd 10000)
    (declare (ignore yyyy))
    (multiple-value-bind (mm dd) (floor mmdd 100)
      (format nil "~a ~a" (num-to-month mm) dd))))

(defun diff-date (now then)
  (declare (type fixnum now then))
  ;return diff in weeks
  (/ (- now then) 7.0))
