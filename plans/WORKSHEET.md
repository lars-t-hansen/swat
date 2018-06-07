For flat descriptors:

* Make anyref-to-whatever downcasts be systematic so that anyref is a
  different thing.
* Move the anyref-to-class downcasts to use flat descriptor tables.
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