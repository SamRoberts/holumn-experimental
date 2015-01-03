Experimental project, working name "Holumn"

Implementing flexible storage format in Haskell. Original motivation was do to a columnar file format, but it has sinced turned into a file format with
generic schema re-writing capabilities.

At the moment this project doesn't work at all, and is just a bunch of brainstormings. Once I have some useful subset of functionality working, that will be version 0.2.

In the fullness of time, I imagine this library will work as follows (for writing, analogous process for reading):

  1. The user writes a value to a holumn file with some schema using a programming language specific API (haskell Map String Long)
  2. The programming language specific API maps the language specific type of the value to a type from a standard library of holumn types (Map(Utf8String,64BitUInt))
  3. The standard library of holumn types maps this type to a simplified Type based on a handful of primitives and combinators (List(Prod(List(Prim(0,7)), Prim(0,2^64 - 1))))
  4. Holumn code maps the simplified Type to a C-like type which makes all the lengths and sum tags explicit, and candidates for rewriting (Struct(Length, Array(Struct(Struct(Length, Array(Val(0,7))), Val(0, 2^64-1))))). Also (only?) generates readers and writers).
  5. Holumn code uses file schema to rewrite the type. (Struct(Length, Array(Length), Array(Val(0,7)), Array(Val(0, 2^64-1)))) Also (only?) rewrites readers and writers.
  6. Holumn uses rewritten writer to write original value to file

Goal for version 0.2:
  1. Simplified Type defined
  2. C-like Repr, reader, and writer defined, including multiple streams, even if it's not what we will eventually end up with
  3. Transformations limited to distributing array over it's sub type, turning array of blah into structs of more primitive arrays
  4. No other transformations on writers
  5. Transformations on readers limited to selectively reading fields
  6. Very limited standard library
  7. Very basic haskell API, even if it's not what we eventually end up with
