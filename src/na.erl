-module(na).

%% Note: function clause order in this module matters.
%%       Ordering the Path type check clauses by tuple -> map -> proplist
%%       (i.e. by how common they likely are) gives a small performance
%%       improvement in na_benchmark_get.erl and na_benchmark_replace.erl for
%%       tuple accesses.
%%       This is because the function clauses end up being checked in order,
%%       so checking for a tuple earlier, makes the tuple handling slightly
%%       faster.


-moduledoc """
Functions used to access and modify nested data structures.

See the main [README](readme.html) for a basic introduction on `na` usage.

All functions are on the format `xxx(Data, Path, ...)`.

* `Data` is a nested data structure.
* `Path` is assumed to point to an existing leaf or branch in `Data`.

Note: using an invalid path will result in a crash.


Edge cases related to unusual Data
----------------------------------

Most `proplist()` usage assumes that keys are unique, but note that it's valid
for proplists to have multiple entries with the same key.
In such cases the `na` functions will always pick the first match.
Supply a custom `accessor()` to circumvent this behaviour.

While rather unlikely, `Data` may use funs (or tuples of funs) as keys, e.g. in
a `map()` or `proplist()`. Using such a key in a `Path`, will likely result in
it being treated as an `accessor()` element, which in turn will result in
crashes and other oddities.

**Note**: an `accessor()` uses one or more funs, this is for two reasons:
* The use of funs make them easy to distinguish from "normal" `Path` elements.
* Using funs rather than something like a callback module, avoids the need to
  write/compile/load a callback module. This is helpful when working
  interactively in an Erlang shell.


Type support
------------

The `Path` in the `na` functions handles `proplist()`, `map()`, `tuple()` and
`record()` data, using runtime type checks, as already discussed in the main
[README](readme.html).
Any other types must be handled using accessors.

The `na_lists` module adds support for `list()` data, by having functions that
return the appropriate accessors.

It's recommended to study the `na_lists.erl` module if you want to create
another `na_<type>.erl` module.

It's recommended that a `na_<type>.erl` module implements the `na` behaviour,
as this ensures that function names and return types are consistent with
other `na_<type>.erl` modules like `na_lists.erl`.

**Note**: read the `na_proplists_helper` module docs, for more info on how
proplists are handled.
""".

%% -----------------------------------------------------------------------------

-export([get/2,
         replace/3,
         modify/3,
         remove/2
        ]).

-ignore_xref([get/2,
              replace/3,
              modify/3,
              remove/2
             ]).

-export_type([
              key_or_index/0,

              modify_fun/0,

              %% accessor() types
              get_at/0,
              update_at/0,
              remove_at/0,
              %% fun types used by accessor()
              rebuild_at/0,

              %% exported to make this type show up in the generated docs,
              %% as this type is only mentioned in doc text and not used in any spec
              accessor/0
             ]).

%% suppress "unused accessor()" warning
-compile([nowarn_unused_type]).

%% -----------------------------------------------------------------------------

-doc """
Key or index as used by `proplist()`, `map()`, `tuple()` and `record()`, to
access a specific element of these data types.

**Note**: `key_or_index()` should be defined as any `term()` that is not an
`accessor()`. The current type is a bit to permissive, due to the type system
not being expressive enough.
""".
-type key_or_index() :: term().


-doc """
Fun or tuple of funs, supplied in place of a `key_or_index()` (in a path),
to access a data type not supported by `key_or_index()`. It can also be used to
allow for custom access to a supported data type.
""".
-type accessor() :: get_at() | update_at() | remove_at().


-doc
"""
See the `na:modify()` function for usage.
""".
-type modify_fun() :: fun((Value :: term()) -> term()).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% functions to implement for a na_<type>.erl module

-callback get_at(term())    -> get_at().
-callback update_at(term()) -> update_at().
-callback remove_at(term()) -> remove_at().

-optional_callbacks([remove_at/1]).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


-doc """
An `accessor()`.

A fun that gets a specific element from `Data`.

`fun(Data) -> lists:nth(3, Data) end` would e.g. get the third element from a list.

**Note**: this fun should crash if a lookup fails or is invalid.
""".
-type get_at() :: fun((Data :: term()) -> term()).


-doc """
An `accessor()`.

Combines `get_at()` and `rebuild_at()`. See the `rebuild_at()` docs for details.
""".
-type update_at() :: {get_at(), rebuild_at()}.


-doc """
An `accessor()`.

A fun that removes a specific element from `Data`.

`fun(Data) -> tl(Data) end` would e.g. remove the first element from a list.

**Note**: this fun should crash if removal is an invalid operation.
""".
-type remove_at() :: fun((Data :: term()) -> term()).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-doc """
Part of the `update_at()` accessors.

This is a fun() used by the `na:replace()`, `na:modify()` and `na:remove()`
functions, to propagate the change up the path, after reaching the element
referenced by `Path`.

`rebuild_at(UpdatedValue, Data)` updates `Data` at the current tree level with
`UpdatedValue`, to reflect the change done in the lower path level.

Note that the `update_at()` accessor uses `get_at()` and `rebuild_at()`
in pairs - one does the access (goes one step down the path), while the other
updates the same data, with the change done at the lower levels of the path.

Example - if `get_at()` is:

    fun(Data) -> maps:get(abc, Data) end

then `rebuild_at()` would be something like:

    fun(UpdatedValue, Data) -> maps:put(abc, UpdatedValue, Data) end


The `na_lists:update_at(N)` accessor creator function, is another example.
It passes the same index `N`, to both its `get_at()` and `rebuild_at()`,
so that both can act on the same data.
""".
-type rebuild_at() :: fun((UpdatedValue :: term(), Data :: term()) -> term()).


%% -----------------------------------------------------------------------------

-spec get(Data :: term(), Path :: [key_or_index() | get_at()]) -> term().

-doc """
Access a nested data structure using a path of keys, indexes and `get_at()`
accessors.
""".

get(Data, []) ->
    Data;

%% look for get_at() accessor first, so that it can override the default type based handling
get(Data, [LookupFun | R]) when is_function(LookupFun) ->
    D2 = LookupFun(Data),
    get(D2, R);

%% tuple / record
get(Data, [Index | R]) when is_tuple(Data) ->
    D2 = element(Index, Data),
    get(D2, R);

%% map
get(Data, [Key | R]) when is_map(Data) ->
    #{Key := D2} = Data, % use `:=` rather than `=>` as Key is assumed to exist
    get(D2, R);

%% proplist
get(Data, [Key | R]) when is_list(Data) ->
    D2 = na_proplists_helper:get_first_proplist_value(Key, Data),
    get(D2, R).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-spec remove(Data :: T,
             Path :: [key_or_index() | update_at() | remove_at()])
            -> T when T :: term().

-doc """
Returns a updated `Data`, without the element previously located at `Path`.

Example:
```
> Data = #{a => #{a => 1, b => 2},
>          b => #{a => 3, b => 4}}.

> na:remove(Data, [b, a]).
#{a => #{a => 1, b => 2},
  b => #{b => 4}}
```

The `Path` must reference an element (in a type) from which it makes sense to
remove elements.

Removal makes sense from `proplist()` and `map()`, as these are collections that
can change in size.

Trying to remove an element from a fixed size type - like `tuple()` or
`record()`, will result in a crash.
For the rare cases where it makes sense to remove elements from these types,
then either use a custom `remove_at()` accessor or functions like
`na:replace()` and `na:modify()` instead.

**Note** when using accessors in `Path `- use `update_at()`, unless the accessor
is the final `Path` element, in which case `remove_at()` must be used.

Example:
```
> Data = [[420], [440], [460]].
>
> na:remove(Data, [na_lists:update_at(2), na_lists:remove_at(1)]).
[[420], [], [460]]
```
""".

%% remove_at()
%% look for accessor() first so that it can override the default type based handling
remove(Data, [RemoveFun]) when is_function(RemoveFun) ->
    RemoveFun(Data);

%% update_at()
%% look for accessor() first so that it can override the default type based handling
remove(Data, [{LookupFun, RebuildFun} | R]) when is_function(LookupFun) andalso
                                                 is_function(RebuildFun) ->
    Val = LookupFun(Data),
    Val2 = remove(Val, R),
    RebuildFun(Val2, Data);

%% tuple / record
remove(Tuple, [Index | R]) when is_tuple(Tuple) ->
    Val = element(Index, Tuple),
    Val2 = remove(Val, R),
    NewTuple = setelement(Index, Tuple, Val2),
    NewTuple;

%% map
remove(Map, [Key]) when is_map(Map) ->
    #{Key := _} = Map, % crash if caller tries to remove a non-existent map entry
    NewMap = maps:remove(Key, Map),
    NewMap;
remove(Map, [Key | R]) when is_map(Map) ->
    #{Key := Val} = Map, % use `:=` rather than `=>` as Key is assumed to exist
    Val2 = remove(Val, R),

    %% path is assumed to reference a existing entry, so use `:=` to ensure that no
    %% new element gets added in the map()
    NewMap = Map#{Key := Val2},
    NewMap;

%% proplist
remove(Proplist, [Key]) when is_list(Proplist) ->
    NewProplist = na_proplists_helper:rm_first_proplist_match(Key, Proplist),
    NewProplist;
remove(Proplist, [Key | R]) when is_list(Proplist) ->
    Val = na_proplists_helper:get_first_proplist_value(Key, Proplist),
    Val2 = remove(Val, R),
    NewProp = na_proplists_helper:replace_on_first_proplist_match({Key, Val2}, Proplist),
    NewProp.

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-spec modify(Data :: T,
             Path :: [key_or_index() | update_at()],
             ModifyFun :: modify_fun())
            -> T when T :: term().

-doc """
Transform an element (using `ModifyFun`) in a deeply nested data structure.

The `ModifyFun` fun takes the element referenced by `Path` as input.
The result will then be used to replace the original element.

Example of editing marys `languages`, using `ModifyFun`.
In this case `"Clojure"` is removed:
```
> Users = [{john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
>          {mary, #{name => "Mary", age => 29, languages => ["Elixir", "F#", "Clojure"]}}].

> na:modify(Users, [mary, languages], fun(L) -> L -- ["Clojure"] end).
[{john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
 {mary, #{name => "Mary", age => 29, languages => ["Elixir", "F#"]}}]
```
""".

modify(Data, [], ModifyFun) ->
    ModifyFun(Data);

%% look for accessor() first so that it can override the default type based handling
modify(Data, [{LookupFun, RebuildFun} | R], ModifyFun) when is_function(LookupFun) andalso
                                                            is_function(RebuildFun) ->
    Val  = LookupFun(Data),
    Val2 = modify(Val, R, ModifyFun),
    RebuildFun(Val2, Data);

%% tuple / record
modify(Tuple, [Index | R], ModifyFun) when is_tuple(Tuple) ->
    Val  = element(Index, Tuple),
    Val2 = modify(Val, R, ModifyFun),
    setelement(Index, Tuple, Val2);

%% map
modify(Map, [Key | R], ModifyFun) when is_map(Map) ->
    #{Key := Val} = Map, % use `:=` rather than `=>` as Key is assumed to exist
    Val2 = modify(Val, R, ModifyFun),
    %% path is assumed to reference a existing entry, so use `:=` to ensure that no
    %% new element gets added in the map()
    Map#{Key := Val2};

%% proplist
modify(Proplist, [Key | R], ModifyFun) when is_list(Proplist) ->
    Val  = na_proplists_helper:get_first_proplist_value(Key, Proplist),
    Val2 = modify(Val, R, ModifyFun),
    na_proplists_helper:replace_on_first_proplist_match({Key, Val2}, Proplist).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-spec replace(Data :: T,
              Path :: [key_or_index() | update_at()],
              Replacement :: term())
             -> T when T :: term().

-doc """
Replace an element in a deeply nested data structure with `Replacement`.

Example of updating a `proplist()` wrapped inside of two tuples:
```
> Data = {a, {[{a, 1}, {b, 2}], abc}, 42, 56}.

> na:replace(Data, [2, 1, b], 4000).
{a, {[{a, 1},{b, 4000}], abc}, 42, 56}
```
""".

replace(_Data, [], Replacement) ->
    Replacement;

%% look for accessor() first so that it can override the default type based handling
replace(Data, [{LookupFun, RebuildFun} | R], Replacement) when is_function(LookupFun) andalso
                                                               is_function(RebuildFun) ->
    Val  = LookupFun(Data),
    Val2 = replace(Val, R, Replacement),
    RebuildFun(Val2, Data);

%% tuple / record
replace(Tuple, [Index | R], Replacement) when is_tuple(Tuple) ->
    Val  = element(Index, Tuple),
    Val2 = replace(Val, R, Replacement),
    setelement(Index, Tuple, Val2);

%% map
replace(Map, [Key | R], Replacement) when is_map(Map) ->
    #{Key := Val} = Map, % use `:=` rather than `=>` as Key is assumed to exist
    Val2 = replace(Val, R, Replacement),
    %% path is assumed to reference a existing entry, so use `:=` to ensure that no
    %% new element gets added in the map()
    Map#{Key := Val2};

%% proplist
replace(Proplist, [Key | R], Replacement) when is_list(Proplist) ->
    Val  = na_proplists_helper:get_first_proplist_value(Key, Proplist),
    Val2 = replace(Val, R, Replacement),
    na_proplists_helper:replace_on_first_proplist_match({Key, Val2}, Proplist).
