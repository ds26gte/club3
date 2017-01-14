;last change 2017-01-13

(defun os-execute (s)
  (or #+abcl (ext:run-shell-command s)
      #+clisp (ext:shell s)
      #+clozure (ccl::os-command s)
      #+cmu (ext:run-program "sh" (list "-c" s) :output t)
      #+ecl (si:system s)
      #+mkcl (mkcl:system s)
      #+sbcl (sb-ext:run-program "sh" (list "-c" s) :search t :output t)))
