ifeq (${TERM},dumb)
stack_args += --colour=never
endif

ifeq ($(origin stack_build_on),undefined)
stack_build_on := host
endif

# Build on docker, not host
ifeq ($(stack_build_on), docker)
stack_args += --work-dir .stack-work.docker --docker --no-nix --docker-stack-exe download --install-ghc
endif

ifeq ($(origin resolver),undefined)
resolver := lts19
endif

stack_yaml = stack.$(resolver).yaml
stack = stack --stack-yaml $(stack_yaml)

srcfiles := $(shell find src -type f)
test-srcfiles := $(shell find tests -type f)

.PHONY: all
all: dist

dist: dist-$(resolver)
	ln -sfT dist-$(resolver) dist

dist-$(resolver): $(srcfiles) $(test-srcfiles) package.yaml stack.yaml
	rm -f *.cabal
	rm -f stack.*.yaml.lock
	$(stack) build --install-ghc \
	      	 --test --no-run-tests \
	      	 ${stack_args} \
	      	 ${stack_build_args} \
	      	 --haddock --no-haddock-deps --haddock-hyperlink-source
	mkdir -p $@
	rm -rf $@/doc
	cp -fr $(shell $(stack) path ${stack_args} --dist-dir)/doc/html/nejla-common $@/doc
	cp -f resources/badge-documentation.svg $@/

# In CI, we only want to build against a single lts at a time
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

tests := $(addprefix dist-$(resolver)/tests/, $(shell stack ide targets --stdout | grep ':test:' | cut -d ':' -f3))

.PHONY: check
check: test

testimage:
	docker-compose -f docker-compose.tests.yaml build
	touch testimage

.PHONY: test
test: override stack_build_on:=docker
test: $(tests) testimage
	docker-compose -f docker-compose.tests.yaml run --rm test
	docker-compose -f docker-compose.tests.yaml down -v

.PHONY: test-down
test-down:
	docker-compose -f docker-compose.tests.yaml down -v

# Only run tests for the current resolver for now
$(tests): override stack_args := --work-dir .stack-work.docker --docker --no-nix --docker-stack-exe download --install-ghc
$(tests): dist-$(resolver)/tests/%: $(srcfiles) $(test-srcfiles) package.yaml stack.yaml
# Rebuild, since we always want to use docker
	rm -f *.cabal
	rm -f stack.*.yaml.lock
	stack build $(stack_args) \
	      	 --test --no-run-tests
	mkdir -p dist-$(resolver)/tests
	cp "$(shell stack $(stack_args) path --dist-dir)/build/$(notdir $@)/$(notdir $@)" dist-$(resolver)/tests/
	ln -sfT dist-$(resolver) dist




.PHONY: clean
clean:
	$(stack) clean
	rm -rf *.cabal
	rm -f stack*.yaml.lock
	rm -rf .stack-work*
	rm -rf dist-*
	rm -rf dist
