# Swat - a programming language targeting WebAssembly

Swat is an evolving Scheme-syntaxed, statically typed, class-based
language meant as a test bed for WebAssembly's co-evolving object and
GC support.

See [MANUAL.md](MANUAL.md) for a short tutorial and a language reference.

See test programs in [test/](test) and demos in [demo/](demo) for larger examples.

## Prerequisites

The Swat compiler is written in r7rs Scheme and currently requires you
to install the `larceny` Scheme implementation, http://larcenists.org.

You also need a very recent build of the SpiderMonkey JS shell; see
[MANUAL.md](MANUAL.md) if you don't know how to obtain this.
