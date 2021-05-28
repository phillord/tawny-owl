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

commit-test:
	lein test :commit

travis:
	$(EMACS_ENV) $(MAKE) html 2>&1 | grep --invert-match "newer than byte-compiled file"

DOCKER_TAG=openjdk-11-lein
PROFILE=default
test-cp:
	docker run -it --rm --name docker-cp -v $(PWD):/usr/src/app -w /usr/src/app --entrypoint=/bin/bash  clojure:$(DOCKER_TAG) ./test-by-cp $(PROFILE)

multi-test-cp:
	$(MAKE) test-cp DOCKER_TAG=openjdk-11-lein PROFILE=default
	$(MAKE) test-cp DOCKER_TAG=openjdk-11-lein PROFILE=1.9
	$(MAKE) test-cp DOCKER_TAG=openjdk-8-lein PROFILE=default
	$(MAKE) test-cp DOCKER_TAG=openjdk-8-lein PROFILE=1.9


test-git:
	docker run -it --rm --name docker-cp -v $(PWD):/usr/src/app -w /usr/src/app --entrypoint=/bin/bash  clojure:$(DOCKER_TAG) ./test-by-git $(PROFILE)

multi-test-git:
	$(MAKE) test-git DOCKER_TAG=openjdk-11-lein PROFILE=default
	$(MAKE) test-git DOCKER_TAG=openjdk-11-lein PROFILE=1.9
	$(MAKE) test-git DOCKER_TAG=openjdk-8-lein PROFILE=default
	$(MAKE) test-git DOCKER_TAG=openjdk-8-lein PROFILE=1.9


.PHONY: test travis commit-test clean-src html
