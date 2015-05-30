; last change 2009-08-23

;Note: It is not necessary to put all the initialization information
;into this one file.  You may parcel it out into different files and
;have club3rc.lisp use Lisp's LOAD to slurp in everything.
;club3rc.lisp is a Lisp file, and you may use full Lisp to arrange its
;contents however you like

(club-name "The Articulators")

(load (merge-pathnames "memberlist" *load-pathname*))

(lineups (:default
           (tm word joke poet speaker speaker speaker ge eval eval eval topics))
         (:skills-night
           ;Skills Night has one extra role, the Skillsmaster
          (tm skills word joke poet speaker speaker speaker ge eval eval eval topics))
         (:topics-tuesday
           ;Topics Tuesday has only two speakers and two evaluators, to
           ;make time for more topics
          (tm word joke poet speaker speaker ge eval eval topics))
         (:superset
           ;all the roles that should be listed in the leftmost column
           ;of the schedule table
          (tm skills word joke poet speaker speaker speaker ge eval eval eval topics)))

(role-difficulty (speaker :intermediate)
                 ;
                 (tm :hard)
                 (skills :hard)
                 (eval :hard)
                 (ge :hard)
                 (topics :hard))

(role-names (tm "Toastmaster")
            (skills "Skillsmaster")
            (word "Wordmaster")
            (joke "Jokemaster")
            (ge "General Evaluator")
            (eval "Evaluator")
            (topics "Topic Master"))


(defun standard-member-evolution ()
  (dolist (mem *members*)
    (let* ((m (symbol-value mem))
           (n-speech (gethash 'speaker (cmember*-rolecount m) 0))
           (n-joke (gethash 'joke (cmember*-rolecount m) 0))
           (n-word (gethash 'word (cmember*-rolecount m) 0))
           (n-poet (gethash 'poet (cmember*-rolecount m) 0))
           )
      (update-expertise m 'joke :add)
      (update-expertise m 'word :add)
      (when (and (> n-joke 0) (> n-word 0))
        (update-expertise m 'poet :add))
      (when (> n-poet 0)
        (update-expertise m 'speaker :add))
      (when (>= n-speech 3)
        (mapc (lambda (r) (update-expertise m r :add))
              '(eval)))
      (when (>= n-speech 5)
        (mapc (lambda (r) (update-expertise m r :add))
              '(tm topics)))
      (when (>= n-speech 7)
        (update-expertise m :all :add)))))

(defun read-history-hook ()
  (standard-member-evolution)
  (expertise
   (andre speaker)
   ;(andrewc poet speaker)
   (andrews speaker)
   (baha (- eval tm ge topics skills))
   (christian speaker)
   (gordon :all)
   (johns :all)
   (judyk (- speaker topics))
   (kumaran :all)
   (laura speaker)
   (lauryn (- speaker))
   (lenny speaker)
   (marcia poet)
   (prasad speaker)))

(eagerness (judyk 1)
           )

(load (merge-pathnames "availability" *load-pathname*))

(load (merge-pathnames "calendar" *load-pathname*))
