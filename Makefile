CASK?=cask

-include makefile-local

ifdef EMACS
EMACS_ENV=EMACS=$(EMACS)
endif

install:
	$(EMACS_ENV) $(CASK) install

## "cask emacs" is currently only on my fork!
gen-src: install
	$(EMACS_ENV) $(CASK) emacs --debug --script script/build.el -- gen-src

clean-src:
	$(EMACS_ENV) $(CASK) emacs --script script/build.el -- clean-src

html: gen-src
	$(EMACS_ENV) $(CASK) emacs --script script/build.el -- gen-html

commit-test: travis
	lein test :commit

travis:
	$(EMACS_ENV) $(MAKE) html 2>&1 | grep --invert-match "newer than byte-compiled file"

.PHONY: test travis commit-test clean-src html
