.PHONY: web

web:
	rm -rf web
	mkdir web
	make -C doc html pdf
	cp doc/*.html doc/*.pdf web/
	cp doc/images web/ -r
	cp web/clx-cursor.html web/index.html

pages: web
	git checkout gh-pages
	cp web/* . -r
	git commit -a -c master
	rm -rf web/
	git checkout -f master
