.PHONY: build clean install test

clean :
	@cask clean-elc

build : clean install
	@cask build

install :
	@cask install

test : clean
	@cask build \
	&& cask emacs -batch -q --load=tests/gtde-oo-tests.el -f ert-run-tests-batch-and-exit
