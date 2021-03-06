# swat v1 

## Stress points

* Downcasts at virtual invocation - ideally we'd avoid it entirely
* Downcast bytecode size in general - more diverse instructions would help
* Our descriptor objects are in flat memory but this is certainly not ideal;
  if an object escapes from one wasm instance and flows into another, the
  type info will be utterly bizarre and very strange things can happen

Let's move away from flat memory if we can; we'll need ref globals before we
can implement this, and there needs to be some sort of init to create the
descriptors:

(type $Obj_hdr (struct
                 (field $_cls_id  i32)
                 (field $_cls_len i32)
		 (field $_Obj_cls_id i32)))

(type $A_hdr (struct
                 (field $_cls_id  i32)
                 (field $_cls_len i32)
		 (field $_Obj_cls_id i32)
		 (field $_A_cls_id i32)
		 (field $_A_virt_f i32)))


Virtual access is still by constant known field number.  But subclass testing
is a little tricky.

Actually this does not work at all with constant field names, I think?
We can handle the supertype table but not the virtual table, because
the length of the supertype table in a dispatch is not known (this is
the point of virtual dispatch).

We could do this if we had arrays though.

So hold off on this until we have subtype support in the engine, and then we can
use this exclusively for the vtable, or maybe for rtti + vtable.


## Missing language features and implementation bugs

* Wizard mode
  * Pointer stores
  * Pointer globals
* Type checks at the call-in boundary + defined semantics for visible unexported types
* Virtual function cleanup
  * Missing handling of the "default" case
  * Idiosyncratic "closed" syntax, defvirtual + defmethod would be less weird and "open" and
    forward-looking and also remove the trampolines we need now for the downcast
* Exportable classes
  * They should show up as factory functions M.make.Cls(), and with access to fields thru the std TypedObject mechanism
* Some way of accessing host objects, so that we don't have to go to JS for DOM access.
  * Ad-hoc / limited is OK for now, anything's better than what we have
  * See FUTURE.md for a discussion
* Very high value missing language operators
  * <number>->string, string-><number> for number types
  * eq? on compatible reference types (probably not anyref)
* Our vectors should be TO vectors.  The way to construct those are by taking the basic
  type, say, TO.int32, and applying an array constructor: TO.int32.array(5), this yields
  a constructor for arrays of length 5.  We'll need to cache those.

## Quality etc

### Required

* More documentation, esp about how to use for web development
* More positive test code
  - ???
* More negative test code
  - broken downcasts
    - class -> subclass
    - anyref -> class
    - anyref -> vector
    - anyref -> string
  - static type failures 
    - lots & lots of cases
* Less brittle compiler / better error messages by reading phrase-at-a-time so that
  we at least have a starting line number

### Desirable

* An "include" feature (both top-level, which aids JS, and top-level in the module, which aids swat)
  with a reasonable include path search algorithm
* note, right now JS performs a null check on obj reference, does this move into wasm somehow?
* can we create a general runtime facility?
* more demos
* Fix the L.#x10 bug if possible, maybe by finessing it to L.x10

## Demo ideas

* The lack of reference-type globals is a real hardship
* Really want something that deals with the DOM
* maybe port Life / Mandelbrot / PSON from AssemblyScript?  Life / Mandel are
  not obvious because they are very array-oriented, but clearly doable.
* Obviously we can do a self-compiler, but it's a big project and not very interesting

# swat later, or opportunistically

## Globals of reference type

Currently not supported because of missing support in SpiderMonkey.
We could hack around it but it's just as easy to wait until support is
available.

## Host types

I envision something like `(defhost TypeName (predicate Fn))` where
TypeName is the name of the type and `Fn` names a function from anyref
to i32 that determines whether an object is of the given type.  This
can be used to improve type checking around host types.

There might be more operators but a predicate is a good start.

## Vector operations

* (vector E0 E1 ...) constructs a vector of type T common to the E
* vector-copy, with optional start and end
* vector-append

## Lists

* Immutable
* Type syntax (List T)
* Constructor (list E0 E1 ...), (cons E0 E1) where E0 is T and E1 is (List T)
* cons, car, cdr, ...
* list->vector, vector->list, list-copy, list-ref, list-head, list-tail, reverse, append
* operators with function arguments: map, for-each
  * For initial cut, require either reference to global function or *literal* lambda expression, which cannot be used in other contexts

## Tuples / multiple values

* Immutable fixed-size records with integer-named fields
* Type syntax (Values T0 T1 ...) maybe, or perhaps just (T0 T1 ...), tricky
* Constructor (values T0 T1 ...)
* Accessors *0 *1 etc, as for fields
* Destructuring tuples with let-values and let*-values

## Sequences

* (seq T a b ...) creates a sequence from a b ...:
   If T = (List U) then a list of U
   If T = (Vector U) then a vector of U
   If T = String then a String, though no real reason to do this

## Boxes

* Sum type containers, speculative
* (box v) -> Box
* (unbox e ((x i32) ...) ((y f32) ...) (else ...))
* (box-seq (List Box) e0 ...) boxes everything, this is == (list e0 ...) in Scheme
* autoboxing should be a thing but is tricky to get right, consider passing an i32 to
  something taking Box, this is easy, we box.  But then consider passing a Box value
  to something taking Box.  Now we really do not want to box.  But does this hold up?

## Symbols

* Because symbols are a good idea
* Nice if these can interop with JS symbols

## Multi-module + meaningful type import and export

A good first step would just be multi-module without importing and exporting
types, so that useful library functionality can be written in swat.

## Enums

## Less bizarre trap operator

We want this to be written `???` and we want it to have a type that is
compatible with other types so that we don't have to provide an
explicit type.

## Return statement

'nuff said.

# Auto widening

At least i32 -> f64, i32 -> i64, f32 -> f64, null -> any reference type

# Multi-arity ops for better ergonomics

* High value:  + - (including generating 'neg') * < <= > >= = <u <=u > >u =u max min bitand bitor bitxor
* In Scheme, (/ x) == (/ 1 x) and / is multi-arity; might preserve that here.
* In Scheme, quotient and remainder are not multi-arity.
* Might preserve quotient and remainder as names.
* Might allow / on FP numbers, or yield FP results even from int operands?
* eqv? should translate to eq? or = depending on types of operands?

# Ad-hoc polymorphism

No reason not to, except language complexity.
