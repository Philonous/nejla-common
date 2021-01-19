ifeq ($(origin stack_args),undefined)
export stack_args := --docker --docker-stack-exe download --no-nix --colour=auto
endif

ifeq (${TERM},dumb)
export stack_build_args := ${stack_build_args} --colour=never
endif


.PHONY: build
build:
	rm -f *.cabal
	rm -f stack.yaml.lock
	stack build --install-ghc \
	      --test --no-run-tests \
	      ${stack_args} \
	      ${stack_build_args} \
	      --haddock --no-haddock-deps
	mkdir -p dist/
	cp -r $(shell stack path --dist-dir)/doc/html/nejla-common dist/doc

.PHONY: doc
doc:
	stack build ${stack_args} \
	      ${stack_build_args} \
	      --haddock --no-haddock-deps


.PHONY: clean
clean:
	stack clean
	rm -rf *.cabal
	rm -f stack.yaml.lock
	rm -rf .stack-work

.PHONY: test
test: build
