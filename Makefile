all: test bin/psycho

bin/psycho: main.hs Psycho/*.hs
	ghc --make "$<" -o "$@"

.PHONY: test

test:
	#runhaskell -Wall test/all.hs
