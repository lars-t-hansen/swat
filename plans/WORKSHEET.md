For flat descriptors:

* Fix classes
  - render-unbox-maybenull-anyref-as-object should go away and be replaced by some
    combination of null testing and render-maybe-unbox-nonnull-anyref-as-object?
  - Remove 'desc' altogether if we can, just expand the constant in-line
  - Remove 'desc' altogether if we can, just expand the constant in-line; we'll
    need this to generate struct.new anyway.  But in that case we can use an
    indirection cell for the address, since the wasm output is postprocessed;
    for text this is different.  So maybe await this?

* Fix arrays
  - instead of a numeric _desc_ that is a cookie, use an address that points
    to a descriptor
  - we should have logic that unboxes from anyref to any vector, and then use
    descriptor test to test actual type (loading vector type from the descriptor)
  - descriptor has vector id (denoting type), and length
  - remove length field from the Vector type
  - expose null tests maybe?

* Fix documentation


Week plan:

* Get this out of the way asap, it's dragging on for too long
* Implement --wizard
* Demo swat with gc object feature, ideally even in Firefox
