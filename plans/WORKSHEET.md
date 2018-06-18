Bugfixes/changes in SpiderMonkey:

* Fields should always be called _0, _1, etc, even in non-wizard mode.  Right
  now they are _1, _2 in wizard mode and their original names in non-wizard mode.
  Both are wrong.

For flat descriptors:

* Fix classes:
  - render-unbox-maybenull-anyref-as-object should go away and be replaced by some
    combination of null testing and render-maybe-unbox-nonnull-anyref-as-object?

* Fix arrays:
  - instead of a numeric _desc_ that is a cookie, use an address that points
    to a descriptor
  - descriptor has vector id (denoting type), and vector length
  - remove length field from the Vector type
  - expose null tests maybe?

* Fix strings:
  - ???
