

%.html: %.md
	markdown $< | perl -p -e 's#[.]md#.html#' > $@


all: getting-started.html getting-started-with-clojure.html

getting-started.html: getting-started.md

getting-started-with-clojure.html: getting-started-with-clojure.md


clean: 
	- rm *html
