tags: *.lisp
	ctags *.lisp

clozure: club3.dx64fsl

abcl: club3.abcl

sbcl: club3.fasl

cmucl: club3.sse2f

club3.abcl: *.lisp
	echo '(compile-file "club3")' | abcl

club3.sse2f: *.lisp
	lisp -eval '(compile-file "club3")' -eval '(quit)'

club3.dx64fsl: *.lisp
	ccl -e '(compile-file "club3")' -e '(ccl:quit)'

club3.fasl: *.lisp
	sbcl --eval '(compile-file "club3")' --eval '(quit)'

clean:
	rm -f *.abcl *.*fsl *.fasl *.lib *.sse2f
