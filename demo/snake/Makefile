# Run the SpiderMonkey shell
JS=~/bin/jsshell --wasm-gc

# Compile .swat to .js and other files
SWAT=../../swat
BINARIZE=../../binarize

COMPILER=../../swat.sch.slfasl

.PHONY: default test snake

default: snake

test: snake

snake: snake.js snake.wasm

snake.js snake.metawasm.js: snake.swat Makefile $(COMPILER)
	$(SWAT) --js+wasm snake.swat

clean:
	rm -f snake.metawasm.js *~

%.wasm : %.metawasm.js
	$(JS) $< > TEMP
	$(BINARIZE) TEMP $@
	@rm -f TEMP
