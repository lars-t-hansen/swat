For flat descriptors:

* Move anyref-to-whatever downcasts to incorporate explicit null checks
  and be systematic so that anyref is a different thing.
* Move the anyref-to-class downcasts to use flat descriptor tables.
* Get rid of the JS descriptor tables; we should no longer need them.
* Merge back to main

General hygiene:

* Add maybenull / nonnull to names as appropriate to expose assumptions

