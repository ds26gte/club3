;last change 2009-03-27

(defun weeks-since (roles history)
  (declare (type list roles)
           (type hash-table history))
  (let ((n 0))
    (dolist (role roles)
      (let ((n2 (gethash role history 0)))
        (when (> n2 n) (setq n n2))))
    (diff-date *date* n)))

(defun decay (x &key (halflife 1.0) (phase 0.0))
  (declare (type float x halflife phase))
  ;demerit decays exponentially as x increases.
  ;Large phase => initial demerit high.
  ;Large halflife => demerit decays slower
  (handler-case
      (exp (* #.(- (log 2.0))
              (/ (- x phase) halflife)))
    ;clisp's EXP aborts outside [-87,88]
    (floating-point-underflow () 0.0)
    (floating-point-overflow () #.(exp 88.0))))

(defun person-demerits (person this-role)
  (declare (type symbol person this-role))
  (let* ((person-s (symbol-value person))
         (history (cmember*-history person-s))
         (eagerness (cmember*-eagerness person-s))
         (disinclinedness (- eagerness 3.0))
         (weeks-since-easy-role (weeks-since *easy-roles* history))
         (weeks-since-intermediate-role
           (weeks-since *intermediate-roles* history))
         (weeks-since-hard-role (weeks-since *hard-roles* history))
         (weeks-since-this-role (weeks-since (list this-role) history))
         (weeks-since-any-role (min weeks-since-easy-role
                                    weeks-since-intermediate-role
                                    weeks-since-hard-role)))
    (+
      ;the more eager get less demerit
      (decay eagerness :halflife .2 :phase 2.0)
      (decay weeks-since-easy-role :halflife 5.0 :phase 1.0)
      ;more demerit if current role is intermediate/hard for those that are
      ;uneager and/or have recently done another intermediate/hard role
      (decay weeks-since-intermediate-role :halflife 5.0 :phase
             (+ disinclinedness
                (cond ((member this-role *intermediate-roles*) 3)
                      ((member this-role *hard-roles*) 3)
                      (t 0))))
      (decay weeks-since-hard-role :halflife 5.0 :phase
             (+ disinclinedness
                (cond ((member this-role *intermediate-roles*) 3)
                      ((member this-role *hard-roles*) 5)
                      (t 0))))
      ;huge demerit if current role was already done recently
      (decay weeks-since-this-role :halflife 5.0 :phase 5.0)
      ;any recent participation contributes only small demerit
      (decay weeks-since-any-role :halflife 5.0 :phase -2.0))))

(defun assignment-demerits (assignment)
  (declare (type list assignment))
  (let ((acc 0.0))
    (dolist (rassignment assignment)
      (incf acc (person-demerits (cadr rassignment)
                                 (car rassignment))))
    acc))
