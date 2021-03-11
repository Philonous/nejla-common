ifeq ($(origin stack_args),undefined)
export stack_args := --colour=auto
endif

ifeq ($(origin stack_yaml),undefined)
export stack_yaml := stack.lts17.yaml
endif

stack := stack --stack-yaml $(stack_yaml)

ifeq (${TERM},dumb)
export stack_build_args := ${stack_build_args} --colour=never
endif

.PHONY: build
build:
	rm -f *.cabal
	rm -f stack.yaml.lock
	$(stack) build --install-ghc \
	      	 --test --no-run-tests \
	      	 ${stack_args} \
	      	 ${stack_build_args} \
	      	 --haddock --no-haddock-deps --haddock-hyperlink-source
	mkdir -p dist/
	rm -rf dist/doc
	cp -fr $(shell $(stack) path ${stack_args} --dist-dir)/doc/html/nejla-common dist/doc
	cp -f resources/badge-documentation.svg dist/

.PHONY: doc
doc: build
	xdg-open dist/doc/index.html


.PHONY: clean
clean:
	$(stack) clean
	rm -rf *.cabal
	rm -f stack.yaml.lock
	rm -rf .stack-work

.PHONY: test
test: build
