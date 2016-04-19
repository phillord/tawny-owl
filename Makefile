
install:
	cask install

gen-src: install
	cask exec emacs --debug --script script/build.el -- gen-src

clean-src:
	cask exec emacs --script script/build.el -- clean-src

html: gen-src
	cask exec emacs --script script/build.el -- gen-html

test:
	lein test

travis:
	$(MAKE) html | grep --invert-match "newer than byte-compiled file"

.PHONY: test
