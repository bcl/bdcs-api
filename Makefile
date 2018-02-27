default: all

all: bdcs-api-server

sandbox:
	cabal sandbox init

bdcs-api-server: sandbox
	cabal update
	# happy appears to be required for warp(?) and won't be installed by --dependencies-only
	[ -x .cabal-sandbox/bin/happy ] || cabal install happy
	cabal install --dependencies-only
	cabal configure
	cabal build

clean:
	cabal clean

hlint: sandbox
	cabal update
	[ -x .cabal-sandbox/bin/happy ] || cabal install happy
	[ -x .cabal-sandbox/bin/hlint ] || cabal install hlint
	.cabal-sandbox/bin/hlint .

tests: sandbox
	cabal update
	# happy appears to be required for warp(?) and won't be installed by --dependencies-only
	[ -x .cabal-sandbox/bin/happy ] || cabal install happy
	cabal install --dependencies-only --enable-tests
	cabal configure --enable-tests --enable-coverage
	cabal build
	cabal test --show-details=always

build-and-test: Dockerfile.build
	sudo docker build -t welder/bdcs-api-build-img -f $< .
	sudo docker run --rm --security-opt label=disable -v `pwd`:/bdcs-api/ welder/bdcs-api-build-img

bdcs-api-img: build-and-test
	sudo docker build -t welder/bdcs-api-img .

ci: build-and-test

ci_after_success:
	sudo docker run --rm --security-opt label=disable -v `pwd`:/bdcs-api/ \
	    --env "TRAVIS=$$TRAVIS" --env "TRAVIS_JOB_ID=$$TRAVIS_JOB_ID" --entrypoint /usr/bin/make welder/bdcs-api coveralls

coveralls: sandbox
	[ -x .cabal-sandbox/bin/hpc-coveralls ] || cabal update && cabal install hpc-coveralls
	.cabal-sandbox/bin/hpc-coveralls --display-report spec

.PHONY: sandbox bdcs-api-server clean tests hlint ci ci_after_success coveralls
