all: test bin/psycho

bin/psycho: main.hs Psycho/*.hs
	ghc --make "$<" -o "$@"

clean:
	rm bin/psycho *.o *.hi Psycho/*.o Psycho/*.hi

.PHONY: test

test:
	#runhaskell -Wall test/all.hs
