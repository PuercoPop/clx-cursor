.PHONY: clean

all: html pdf info

clean:
	rm -rf include
	rm -f  *.pdf *.html *.info
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr
	rm -f include-stamp

clx-cursor.texinfo:


include-stamp: ../*.lisp ../*.asd
	sbcl \
	--eval '(let ((asdf:*central-registry* (cons #p"../" (cons #p"/home/michael/webspace/sb-texinfo/" asdf:*central-registry*)))) (require :sb-texinfo) (require :clx-cursor))' \
	--eval '(sb-texinfo:generate-includes "include/" (list (find-package :clx-cursor)) :base-package :clx-cursor)' \
	--eval '(quit)'
	touch include-stamp

%.html: clx-cursor.texinfo style.css include-stamp
	makeinfo --html --no-split --css-include=style.css $<

%.pdf: clx-cursor.texinfo include-stamp
	texi2dvi -p $<

%.info: clx-cursor.texinfo include-stamp
	makeinfo --no-split $<

html: clx-cursor.html

pdf: clx-cursor.pdf

info: clx-cursor.info
