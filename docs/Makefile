

%.html: %.md
	markdown $< | perl -p -e 's#[.]md#.html#' > $@


all: getting-started.html getting-started-with-clojure.html \
     adding-restrictions.html

getting-started.html: getting-started.md

getting-started-with-clojure.html: getting-started-with-clojure.md

adding-restrictions.html: adding-restrictions.md

clean:
	- rm *html
