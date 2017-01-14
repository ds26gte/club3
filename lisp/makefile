# last modified 2017-01-13

abcl:
	echo '(compile-file "club3")' | abcl

clozure:
	ccl -e '(compile-file "club3")' -e '(ccl:quit)'

cmucl:
	lisp -eval '(compile-file "club3")' -eval '(quit)'

ecl:
	ecl --compile club3.lisp

mkcl:
	mkcl -compile club3.lisp

sbcl:
	sbcl --eval '(compile-file "club3")' --quit

clean:
	rm -f *.abcl *.fas *.*fsl *.fasl *.lib *.sse2f
