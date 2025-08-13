nested_access
=============

Introduction
------------

This library contains Erlang functions to access and update deeply nested data
structures.

Lets assume a deeply nested data structure like this:
```erlang
Data = {1,
        2,
        {
           a,
           b,
           #{a => 42, b => 0},
           d
        },
        4}.
```

To retrieve the value `42` from `Data`,
we would typically use lookup functions or pattern matching as shown below:

```erlang
> D2 = element(3, Data).
> M3 = element(3, D2).
> maps:get(a, M3).
42

> {_, _, D2, _} = Data.
> {_, _, M3, _} = D2.
> #{a := Val} = M3.
> Val.
42
```

With the current library this can be simplified to a "path" based lookup:
```erlang
> na:get(Data, [3, 3, a]).
42
```

Replacing, updating and removing content also becomes similarly terse.
This comes with the added benefit that we no longer need to
explicitly re-build the data structure to propagate the change
up along the path (see *Example 2 - 4*).


Paths
-----

All functions in the `na` module, take a "path" as input,
e.g. `[3, 3, a]` as in the example above. This path is used to locate the
desired leaf element or branch, in a nested data structure.

The path is a list, typically containing a mix of index and key values,
needed to traverse the elements of a nested data structure.

The `na` functions support traversal of any
`proplist()`, `map()`, `tuple()` and `record()`, found in a nested data
structure, using keys or indexes (as appropriate for the type).

Note that the `na` module relies on Erlang runtime type checks to detect
the types of elements.
This detection is somewhat limited and coarse-grained, so an `accessor()`
can be supplied as a path element.
This allows traversal of unsupported types and custom traversal of
supported types.

An `accessor()` consist of one or more funs, that specify
how to access and/or update a specific piece of data. See the `na` docs
for details.

In practice it should be uncommon to have to use accessors, as most nested
data structures are built from maps, proplists, tuples and records - lists,
sets, dicts, arrays and the like are typically located at the end of a branch,
so will normally only show up as the destination of a path, rather than
being something that needs to be traversed.

Note that any `list()` is assumed to be a `proplist()`.
Use the `na_lists` module to create accessors to do "normal" index based
list access.

**Note**: It may seem somewhat odd to treat all lists as proplists,
but this is done because type checks can't distinguish between the two,
so the most common type is used as the default.
When dealing with paths you're more likely to traverse a
`[{Key, Value}, ...]` style lists (i.e. a proplist) rather than a "regular"
`[V1, V2, ...]` style list, so `Key` based lookup is more useful than
index based lookup.


Example 1 - na:get()
--------------------

Example of accessing a `proplist()` where each entry contains a `map()`:

```erlang
> Users = [{john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
           {mary, #{name => "Mary", age => 29, languages => ["Elixir", "F#", "Clojure"]}}].

> na:get(Users, [john, age]).
27
> na:get(Users, [john, languages, na_lists:get_at(1)]).
"Erlang"
```
Note the use of `na_lists:get_at(1)` to get an `accessor()` to access the
`list()` data.

--------------------------------------------------------------------------------

The data structure above is fairly shallow, but we can still see that each path
entry replaces one line of lookup code, so a call like:

```erlang
na:get(Users, [john, languages, na_lists:get_at(1)])
```
replaces a multi-line lookup like:
```erlang
User = proplists:get_value(john, Users),
#{languages := Lang} = User,
lists:nth(1, Lang)
```


Example 2 - na:replace()
------------------------

With the following record definitions:
```erlang
-record(level3, {a, b, c}).
-record(level2, {a, b, c = #level3{}}).
-record(level1, {a, b = #level2{}, c}).
```

Where `Rec` is set as `Rec = #level1{}` and we want to do *"Rec.b.c.a = 42"*,
we can either do:
```erlang
na:replace(Rec, [#level1.b, #level2.c, #level3.a], 42)
```

or the much more verbose:

```erlang
%% "unpack" data structure
RL2 = Rec#level1.b,
RL3 = RL2#level2.c,

%% insert 42 and rebuild data structure
UL3 = RL3#level3{a = 42},
UL2 = RL2#level2{c = UL3},
Rec#level1{b = UL2}
```

**Reminder**: Erlang implements records as tuples.
The syntax `#<record name>.<field name>` is used to get the tuple index of a
record field. The index is generated at compile time, so there no runtime cost.

**Note**: for a debugging one-off, if might be simpler to just supply the
indexes directly.
```erlang
na:replace(Rec, [3, 4, 2], 42)
```


Example 3 - na:remove()
-----------------------

`na:remove()` removes a specific leaf or branch from a data structure, in the
example below the `age` field is removed from `mary`:

```erlang
> Users = [{john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
           {mary, #{name => "Mary", age => 29, languages => ["Elixir", "F#", "Clojure"]}}].

> na:remove(Users, [mary, age]).
[{john, #{name => "John", age => 27, languages => ["Erlang","Ruby","Elixir"]}},
 {mary, #{name => "Mary",            languages => ["Elixir","F#","Clojure"]}}]
```

**Note**: be aware that `na:remove()` can't be applied to all data types,
see the `na` module documentation for details.


Example 4 - na:modify()
-----------------------

`na:modify()` is a more flexible version of `na:replace()`, as the last argument
is a `fun()`, rather than a simple replacement value.
This allows for arbitrary transformations of the data referenced by the path.

Assuming the following data structure:
```erlang
Users = [{john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
         {mary, #{name => "Mary", age => 29, languages => ["Elixir", "F#", "Clojure"]}}].
```

Then the following (and more) can be done:

--------------------------------------------------------------------------------

Add `"C"` to the to the programming languages known by `john`:
```erlang
> na:modify(Users, [john, languages], fun(L) -> ["C" | L] end).
[{john,#{name => "John",age => 27, languages => ["C", "Erlang", "Ruby", "Elixir"]}},
 {mary,#{name => "Mary",age => 29, languages => ["Elixir", "F#", "Clojure"]}}]
```

Sort the `languages` of `mary`:
```erlang
> na:modify(Users, [mary, languages], fun(L) -> lists:sort(L) end).
[{john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
 {mary, #{name => "Mary", age => 29, languages => ["Clojure", "Elixir", "F#"]}}]
```

`na:modify()` can also be used to remove elements, though `na:remove()` would
generally be more convenient:
```erlang
> na:modify(Users, [mary], fun(M) -> maps:remove(age, M) end).
[{john, #{name => "John", age => 27, languages => ["Erlang","Ruby","Elixir"]}},
 {mary, #{name => "Mary",            languages => ["Elixir","F#","Clojure"]}}]
```

--------------------------------------------------------------------------------

**Note**: it's recommended to use `na:replace()` and `na:remove()` rather
than `na:modify()` when possible. This will be slightly terser and make the
intent more obvious. It should also be slightly faster, as no fun needs to be
created or invoked.


Example 5 - combining calls
---------------------------

In some cases data needs to be read from one place and written to another.
To do this `na:get()` and `na:replace()` / `na:modify()` can be combined,
as in the example below:
```erlang
> M = #{c => #{scores => [1, 5, 3, 2]}, d => #{avg => undefined}}.

> Scores = na:get(M, [c, scores]).
> Avg = lists:sum(Scores) / length(Scores).

> na:replace(M, [d, avg], Avg).
#{c => #{scores => [1, 5, 3, 2]}, d => #{avg => 2.75}}
```


Usage Recommendations
---------------------

The `na` functions are designed to act on select parts, of a
deeply nested data structure.

If the whole data structure needs to be "walked",
then it's best to write custom code to do this, to ensure that each tree node
is only visited once. Iterating over a number of mostly identical paths,
using `na` functions, will result in repeated visits to the same tree nodes.

It's preferable to act on "larger" elements (e.g. a list of test scores as in
the previous example), as this minimises the cost of the `na` function call.

--------------------------------------------------------------------------------

While `na` handles proplists more correctly than many Erlang/OTP functions
(used to manipulate proplists) it still assumes that proplists
are essentially maps, causing some limitations:
* Only the first matching key-val gets accessed, even if the same key occurs
  more than once.
* Only `{Key, Val}` tuples and atoms (treated as `{Atom, true}`) are
  matched against when accessing the proplist. Other elements are ignored
  but preserved.

As documented in the `na_proplists_helper` docs, proplists can also have
other tuples in their lists.

`[{Key1}, {Key2, 1, 2}]` is e.g. a valid `proplist()`.

**Note**: a custom `accessor()` would need to be supplied,
to pick between multiple key matches in a proplist or
between multiple values in a tuple.

--------------------------------------------------------------------------------

It should be noted that the `na` functions usually come with a notable overhead,
especially when compared to using traditional lookup/update code run
directly inside of the current function
(see the [Running the Benchmarks](#running-the-benchmarks)
section for details).
This may limit the viability of `na` functions in performance critical code.

**Note**: performance should be a non-issue (except in very special cases)
when using `na` functions for debugging, runtime exploration or in test suites.


Design choices
--------------

This library aims to be easy to use from the Erlang shell - this is why
the `na` functions don't rely on include files, macros,
parse transforms or similar Erlang compile time features.

The `na` API is intentionally kept simple, to make it easy to learn.

The `na.erl` implementation aims to be as performant as possible.
This is also why the functions in `na.erl` tend to avoid abstractions e.g.
helper functions, as this would add to the already notably overhead of the
`na` functions.


Working with the repo
---------------------

An optional `Makefile` is included to cut down on `rebar3` command verbosity.

The `Makefile` also acts as an informal reference, for useful rebar
commands.


Running the Benchmarks
----------------------

The `benchmark` directory contains several benchmark modules.
These can be run to see the performance difference between
`na` functions vs various forms of traditional access code.

To compile the `benchmark` files and start an interactive Erlang shell, do:
```
make bcompile
make bshell
```
To run the individual benchmark modules, use:
```erlang
na_benchmark_get:run().
na_benchmark_replace:run().
```
Note that there are also `run/1` and `run/2` variants, to control how the tests
are run - see the benchmark modules for details.

In regards to the benchmarks:

* `na_benchmark_get.erl` benchmarks `na:get()` i.e. the performance of
  accessing, an element within a nested data structure.
* `na_benchmark_replace.erl` benchmarks `na:replace()` i.e. the performance of
  accessing an element and then propagating the change up through the data
  structure.
* `na:modify()` and `na:remove()` don't have benchmarks, but should perform
   similar to `na:replace()` as they mostly work the same way.

Note that the benchmarks generally represent a worst case scenario for the
`na` functions:

* The traditional code is normally run in the local scope.
* In comparison the `na` functions do one recursive `na` function call per
  path element.
  Each function call also has to do a number of type checks, to determine what
  kind of access to do, at each path step.
* Also note that the traditional code can often make more assumptions, about the
  the data that it is working on, so may end up being able to access/rebuild
  data in a more efficient way.

The benchmarks result below are from running on a
*2019 MacBook Pro 16", 2.6GHz 6 Core Intel i7, with macOS Sonoma 14.7,
using Erlang/OTP 28.0.2 (with JIT), where the benchmark `run/1` functions where used
with `Dups = 100` (default is 10)*:

| test                           | `na:get()`               | `na:replace()`             |
| :----------------------------- | :----------------------- | :------------------------- |
| Mixed nested types             | 2.3 times slower         | 1.2 times slower.          |
| Mixed (only accessors in path) | 2.8 times slower         | -                          |
| Nested lists                   | 2.4 to 13.6 times slower | 1.1 to 20.6 times slower   |
| Nested records/tuples          | 8.2 to 10.3 times slower | 27.8 to 36.0 times slower  |
| Nested maps                    | 3.1 to 3.2 times slower  | x4.2 faster to x3.2 slower |
| Nested proplists               | 1.02 times slower        | 1.02 times slower          |


Using Erlang/OTP 28.0, compiled **without** JIT support, yields somewhat different numbers:

| test                           | `na:get()`              | `na:replace()`            |
| :----------------------------- | :---------------------- | :------------------------ |
| Mixed nested types             | 2.6 times slower        | 1.8 times slower.         |
| Mixed (only accessors in path) | 3.6 times slower        | -                         |
| Nested lists                   | 2.2 to 8.7 times slower | 1.3 to 17.1 times slower  |
| Nested records/tuples          | 8.2 times slower        | 24.7 to 24.8 times slower |
| Nested maps                    | 2.3 to 3.6 times slower | 2.2 to 3.5 times slower   |
| Nested proplists               | 1.4 times slower        | 1.6 times slower          |


While the specific benchmark results should be taken with a grain of salt,
as they depend on the specifics of individual tests,
there are are few general take-aways:

* `na` functions will most likely be several times slower than traditional code.
* lists, records, tuples and maps are generally accessed via BIFs or similar
  low level operators, that take very little time, so the `na` function call and
  type check overhead tends to be very noticable.
* Using `accessor()` path elements (rather than regular indexes and keys),
  as done in `na_benchmark_get:get_mixed_accessor_only()` will add additional
  slowdown, due to the added fun() call overhead.
* Note that proplist access mostly uses regular Erlang functions - this makes
  the `na` overhead less noticeable.


History
-------

This project was initially inspired by the Elixir (non-macro) access functions,
like `Kernel.get_in/2` and `Kernel.update_in/3`.

The initial Erlang API mimicked the Elixir API. This changed over time,
reflecting various differences between the languages and the implementations
(the Erlang implementation is not based on the Elixir one).

Certain features like being able to return multiple elements in
`Kernel.get_in/2` or returning `nil` on failed lookup (rather than crash),
where never considered for the Erlang version. Supporting these would have
made the main use-case ("access single element at path") slower,
due to the extra code that would have had to be executed.


Future ideas
------------

Parse transforms could be used to support Lisp/Elixir style macros.

This would allow for a function call like:
```erlang
Result = nap:get(Data, [{tuple, 2}, {list, N}, {map, "foo"}]).
```
to be replaced by a series of local lookup calls, i.e. something like:
```erlang
Temp1  = element(2, Data),
Temp2  = lists:nth(N, Temp1),
Result = maps:get("foo", Temp2),
```

This allows for Erlang "native" performance, but comes at the cost
of having to know the types and path elements at compile time.
This means that the types (`tuple`, `list` and `map` in the example above)
must be literals. This also holds true for the list elements.
This is so that the parse transform can inspect
the code (at compile time) to pick the appropriate replacement code.

Note that the lookup key/index can still be a variable - see the use of
`N` in the example above.

Also note that a large number of types could easily be supported, as
types are user supplied, rather than detected at runtime.

A unique module name (e.g. `nap`) should be used, to make it easier to identify
what function calls should be parse transformed.


Elvis
-----

This project also contains an `elvis.config` file that is used by Elvis
(an Erlang linter).

The config is configured to run as many Elvis rules as possible, while also
removing the need to use Elvis exceptions to mute undesirable warnings.
This is achieved by omitting specific Elvis rules, that are prone to false
positives.

For more on Elvis see:
* https://github.com/inaka/elvis
* https://github.com/inaka/elvis_core
* https://github.com/project-fifo/rebar3_lint


Wrap Up
-------

Thanks goes out to [Erlang Solutions](https://www.erlang-solutions.com/)
for letting me worked on random projects (like this one),
while being on the "bench" between consultancy projects.

This project has also given me the time to delve into
[ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code).
These allow for the fancy formatting and progress-bar animations, as seen in
the shell, when running the benchmarks.