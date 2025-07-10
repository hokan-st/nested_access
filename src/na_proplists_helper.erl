-module(na_proplists_helper).

-moduledoc """
Helper functions used to work with `proplist()` data.

This module is mainly used internally.
It's included in the documentation, to give more information on how
`proplist()` data is handled.
These helper functions may also be useful outside of the current library.


Working with proplists
----------------------

The Erlang `proplist()` type is more complex than one may naively assume:

* It's a list that can contains both `atom()` and `tuple()`.
* The tuple is usually of the format `{Key, Val}` but shorter tuples like
  `{Key}` and longer variants like `{Key, V1, ...}` are also valid.
* The atom is treated as a shorthand for `{Key :: atom(), true}`.

Additionally:
* The same key can occur multiple times in a `proplist()`.
  This is usually used to associate multiple values with the same key or to
  allow for "overriding" of a previous key-value pairing.
  Such duplicates typically occur, when the proplist data comes from multiple
  config files and/or command line options.
* **Note** that the Erlang `proplists` module is rather permissive
  and will accept any `[term()]` as a  `proplist()`. When dealing with such
  input any non-proplist entry will be preserved, but will be otherwise ignored.
  The `na_proplists_helper` functions work the same way.
* The Erlang docs mention that keys are compared using `=:=`.
  This is also the case in this helper module.
  **Note**: pattern matching in function heads compares this way as well.

The functions normally used to manage `proplist()` data are flawed in various
ways:

* `proplists:delete()` removes all matches.
  There is currently (in Erlang/OTP 27) no `proplists` function to remove
  individual matches.
* The `proplists` module also lacks a replacer function - to update proplist
  elements in-place.\
  While it's often sufficient to remove an entry, and then append a new (updated)
  entry to the head of the proplist, this is not always enough, as proplist
  ordering can matter.
* `lists` functions like `keydelete()` and `keyreplace()` can be used with
  proplists, but will only work with tuples, i.e. `[{Key, ...}]` style proplists.
* Operators like `--` can remove the first occurrence of a specific element
  (atom or tuple). This is of limited use, as the full element
  (not just the key) must be known.

This is why the helper functions below are needed.

**Note** this helper module is geared towards proplists that are used as maps,
i.e. keys are assumed to be unique and the list is assumed to store key-val entries.

This is why the helper functions all act on the "first" element.

Also note that arity 1 and 3+ tuples will be ignored, though they are still
preserved, just like any other non-proplist element found in the list.

Note that all helper functions assume that the specified key can be found
in the proplist. The functions will crash on lookup failure.
""".

-export([
         rm_first_proplist_match/2,
         replace_on_first_proplist_match/2,
         get_first_proplist_value/2
        ]).

%% -----------------------------------------------------------------------------

-spec rm_first_proplist_match(Key :: term(), Proplist :: proplists:proplist()) ->
          proplists:proplist().

-doc """
Remove the first `Key` atom or `{Key, Val}` tuple from `Proplist`.
""".

%% match on atom()
rm_first_proplist_match(Key, [Key | Proplist]) when is_atom(Key) ->
    Proplist;
%% match on tuple()
rm_first_proplist_match(Key, [{Key, _} | Proplist]) ->
    Proplist;
%% no match / not a proplist() element
rm_first_proplist_match(Key, [KV | Proplist]) ->
    [KV | rm_first_proplist_match(Key, Proplist)].

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-spec replace_on_first_proplist_match(KV :: {Key :: term(), Val :: term()},
                                      Proplist :: proplists:proplist())
                                     -> proplists:proplist().

-doc """
Find the first match for `Key` in `Proplist` and replace it with `KV`.\
Note: `KV` is a `{Key, Val}` tuple.

The new entry will be `KV`, as supplied by the function arguments.\
`KV` is entered as is, so something like `{foo, true}` will no be rewritten into `foo`.
""".

%% match on atom()
replace_on_first_proplist_match({Key, _} = KV, [Key | Proplist]) when is_atom(Key) ->
    [KV | Proplist];
%% match on tuple()
replace_on_first_proplist_match({Key, _} = KV, [{Key, _} | Proplist]) ->
    [KV | Proplist];
%% no match / not a proplist() element
replace_on_first_proplist_match(KV, [KV2 | Proplist]) ->
    [KV2 | replace_on_first_proplist_match(KV, Proplist)].

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-spec get_first_proplist_value(Key :: term(), Proplist :: proplists:proplist()) -> term().

-doc """
Find the first match for `Key` in `Proplist`, and return its value.
""".

%% match on atom()
get_first_proplist_value(Key, [Key | _Proplist]) when is_atom(Key) ->
    true;
%% match on tuple()
get_first_proplist_value(Key, [{Key, Val} | _Proplist]) ->
    Val;
%% no match / not a proplist() element
get_first_proplist_value(Key, [_ | Proplist]) ->
    get_first_proplist_value(Key, Proplist).
