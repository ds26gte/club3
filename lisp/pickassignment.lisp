;last change 2009-05-06

(defvar *assignments-already-tried*)
(defvar *num-assignments-already-tried*)
(defvar *max-num-assignments*)

(load (merge-pathnames "demerits" *load-pathname*))

(defun random-e (m)
  (declare (type fixnum m))
  ;returns an int i in [0, m-1] with
  ;  prob(i) = 2^(m-1-i) / (2^m - 1).
  ;I.e. prob(0) is a tad more than 1/2, and probability halves
  ;for each succeeding int until prob(m-1) = 1/(2^m - 1).
  ;E.g. if m=3,
  ;  prob(0) = 4/7; prob(1) = 2/7; prob(2) = 1/7.
  ;This is essentially the geometrical pmf (1/2)^n except that the discrete
  ;random variable range is [0, m-1], not [0, ∞), so the probabilities
  ;in [0, m-1] have to be hiked up slightly to make the overall sum = 1
  (let* ((threshold (expt 2 (- m 1)))
         (r (random (- (* 2 threshold) 1)))
         (i 0))
    (loop
      (when (< r threshold) (return i))
      (incf i)
      (decf r threshold)
      (setq threshold (/ threshold 2)))))

(defun set-equal (s1 s2)
  (declare (type list s1 s2))
  ;since s1 and s2 are same size, just one-way diff is enough
  (null (set-difference s1 s2 :test #'equal)))

(defun suggest-assignment (lineup)
  (declare (type list lineup))
  (let ((lineup-len (length lineup))
        (max-num-assignments 1))
    (loop
      (when (and *max-num-assignments*
                 (>= *num-assignments-already-tried* *max-num-assignments*))
        (return nil))
      (let ((s (make-list lineup-len)))
        (dotimes (i lineup-len)
          (let* ((role (elt lineup i))
                 (eligible-members
                  (sort (remove-if
                         (lambda (m)
                           (let ((m-s (symbol-value m)))
                             (or (= (cmember*-eagerness m-s) 0)
                                 (not (member role (cmember*-expertise m-s)))
                                 (not (funcall (cmember*-availablep m-s))))))
                         *members*)
                        (lambda (m1 m2)
                          (< (person-demerits m1 role)
                             (person-demerits m2 role)))))
                 (num-eligible-members (length eligible-members))
                 (j (random-e num-eligible-members)))
            (unless *max-num-assignments*
              (setq max-num-assignments
                    (* max-num-assignments num-eligible-members)))
            (loop
              (let ((person (elt eligible-members j)))
                (unless (member person s :test #'eq)
                  (setf (elt s i) person)
                  (return))
                (setq j (mod (1+ j) num-eligible-members))))))
        (unless *max-num-assignments*
          (setq *max-num-assignments* max-num-assignments))
        (setq s (mapcar #'list lineup s))
        (unless (member s *assignments-already-tried* :test #'set-equal)
          (push s *assignments-already-tried*)
          (incf *num-assignments-already-tried*)
          (return s))))))

(defun get-a-good-assignment (lineup)
  (declare (type list lineup))
  (let ((best-assignment nil)
        best-assignment-demerits
        (*assignments-already-tried* '())
        (*num-assignments-already-tried* 0)
        (updates 0)
        num-of-tries-till-best
        (*max-num-assignments* nil) ;successive calls to suggest-assignment share this variable
        )
    (dotimes (i *num-trials*)
      (let ((new-assignment (suggest-assignment lineup)))
        (when new-assignment
          (let ((new-assignment-demerits (assignment-demerits new-assignment)))
            (when (or (null best-assignment)
                      (< new-assignment-demerits best-assignment-demerits))
              (setq num-of-tries-till-best i)
              (incf updates)
              (setq best-assignment new-assignment
                    best-assignment-demerits new-assignment-demerits))))))
    (format t "Max tries: ~a; successful try: ~a: updates needed: ~a~%"
            *num-trials* num-of-tries-till-best updates)
    best-assignment))
