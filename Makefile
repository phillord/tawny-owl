install:
	cask install

gen-src: install
	cask exec emacs --debug --script script/build.el -- gen-src

clean-src:
	cask exec emacs --script script/build.el -- clean-src

html: gen-src
	cask exec emacs --script script/build.el -- gen-html

commit-test: travis
	lein test :commit

travis:
	$(MAKE) html 2>&1 | grep --invert-match "newer than byte-compiled file"

.PHONY: test
