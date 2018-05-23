FASLS=swat.sch.slfasl binarize.sch.slfasl

.PHONY: default test clean

default: $(FASLS)

test: $(FASLS)
	( cd test ; $(MAKE) test )
	( cd demo ; $(MAKE) test )

clean:
	rm -f $(FASLS) *~
	( cd test ; $(MAKE) clean )
	( cd demo ; $(MAKE) clean )

%.sch.slfasl: %.sch
	compile-larceny $<
	rm -f a.out
