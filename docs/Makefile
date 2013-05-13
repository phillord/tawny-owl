

%.html: %.md
	markdown $< | perl -p -e 's#[.]md#.html#' > $@


all: getting-started.html getting-started-with-clojure.html \
     adding-restrictions.html importing.html

getting-started.html: getting-started.md

getting-started-with-clojure.html: getting-started-with-clojure.md

adding-restrictions.html: adding-restrictions.md

importing.html: importing.md

clean:
	- rm *html
