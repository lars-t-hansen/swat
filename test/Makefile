# Run the SpiderMonkey shell
JS=~/bin/jsshell --wasm-gc

# Compile .swat to .js and other files
SWAT=../swat
BINARIZE=../binarize

# Tests that should pass
OBJS=fib.js while.js loop.js multi-modules.js global.js syntax.js bool.js \
	unop.js binop.js misc.js conversion.js class.js evalexpr.js wabbit.js \
	number.js lex.js anyref.js string.js vector.js

# Tests that should fail
FAILOBJS=binop-mismatch.fail.js

.PHONY: test test-success test-failure clean

test: test-success test-failure

test-success: $(OBJS)
	@for i in $(OBJS); do echo $(JS) $$i; $(JS) $$i; done

test-failure: $(FAILOBJS)

clean:
	rm -f $(OBJS) $(FAILOBJS) *~

$(OBJS) $(FAILOBJS): Makefile

%.js : %.swat
	$(SWAT) --js $<

%.fail.js : %.swat
	$(SWAT) --fail $<
	touch $@

%.wasm : %.metawasm.js
	$(JS) $< > TEMP
	$(BINARIZE) TEMP $@
	@rm -f TEMP

%.bytes.js : %.metabytes.js
	$(JS) $< > $@
