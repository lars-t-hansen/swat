For flat descriptors:

* Make anyref-to-whatever downcasts be systematic so that anyref is a
  different thing.  Downcasts are now downcast from nullable anyref to
  some nullable thing (string, array, object) with subsequent tests.
   - string should be done
   - object is easy
   - vector is a little tricky because we have no (Vector *) type
* Move the anyref-to-class downcasts to use flat descriptor tables for stage 2
* For vectors, the anyref-to-vector downcast should use the vector descriptor
  for stage 2
* Need to rewrite the 'is' tests too, but trickier since the anyref unboxing
  is conditional.  But it's possible to use null as oob value?
* Get rid of the JS descriptor tables; we should no longer need them.
  This means:
   * The generated constructor code just passes a literal int to `new`,
   * The type in the object changes from TO.Object to TO.int32,
     and the code that reads the desc just reads _desc_, not _desc_.addr
   * The entire "desc" substructure disappears along with the objects in it
* Clean up the comments about the vtable layout.
* Merge back to main.

Bugfixes:

(nothing)

General hygiene:

* expand-expr should and functions in general should take (cx env expr) not (cx expr env)