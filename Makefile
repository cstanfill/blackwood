HC=ghc
TARGETS=dealer

SRCDIR=./src
BRIDGEDIR=$(SRCDIR)
SRCDIRS=$(BRIDGEDIR)
HFLAGS=-i$(SRCDIRS)

dealer: bin/dealer
	cp bin/dealer cgi-bin/dealer

clean: 
	rm -rf bin/* cgi-bin/*
	rm -f src/*.hi src/Blackwood/*.hi
	rm -f src/*.o src/Blackwood/*.o

bin/%: src/%.hs
	$(HC) $(HFLAGS) -o $@ $<

