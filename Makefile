help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

chibi:
	mkdir -p local/src
	cd local/src && (git clone --depth=1 https://github.com/ashinn/chibi-scheme/ || git pull)
	cd local/src/chibi-scheme/ && make
	cd local/src/chibi-scheme/ && sudo make install

init: chibi  ## Compile chibi from source and install it!

check:   ## Run tests
	chibi-scheme finger-tree-test.scm
	chibi-scheme iset-test.scm
	chibi-scheme ideque-test.scm
