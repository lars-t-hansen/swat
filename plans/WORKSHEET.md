For flat descriptors:

* First fix classes:
  - Anyref-to-class: handle null systematically?
  - Remove 'desc' altogether if we can, just expand the constant in-line
  - Clean up prose about descriptors

* Then fix arrays:
  - instead of a numeric _desc_ that is a cookie, use an address that points
    to a descriptor
  - we should have logic that unboxes from anyref to any vector, and then use
    descriptor test to test actual type (loading vector type from the descriptor)
  - descriptor has vector id (denoting type), and length
  - remove length field from the Vector type
  - expose null tests maybe?
  - document all this

* Make anyref-to-whatever downcasts be systematic so that anyref is a
  different thing.  Downcasts are now downcast from nullable anyref to
  some nullable thing (string, array, object) with subsequent tests.
   - string should be done
   - object is easy
   - vector is a little tricky because we have no (Vector *) type
* For vectors, the anyref-to-vector downcast should use the vector descriptor
  for stage 2
* Clean up the comments about the vtable layout.
* Merge back to main.

Bugfixes:

(nothing)

General hygiene:

* expand-expr should and functions in general should take (cx env expr) not (cx expr env)