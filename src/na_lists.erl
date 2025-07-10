-module(na_lists).

-moduledoc """
This module supplies index based accessors for `list()` data.
The index is 1 based, as with other Erlang list functions.

Note that this module conforms to the `na` behaviour, for the naming of its
functions and their type specs.
""".

-behaviour(na).

-export([
         get_at/1,
         update_at/1,
         remove_at/1
        ]).

-ignore_xref([
              get_at/1,
              update_at/1,
              remove_at/1
             ]).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

-spec get_at(N :: pos_integer()) -> na:get_at().

-doc """
Access list element `N`.
""".
get_at(N) ->
    fun(Data) -> lists:nth(N, Data) end.

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-spec update_at(N :: pos_integer()) -> na:update_at().

-doc """
Access list element `N`.
""".
update_at(N) ->
    {get_at(N), rebuild_at(N)}.

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-spec remove_at(N :: pos_integer()) -> na:remove_at().

-doc """
Access list element `N`.
""".
remove_at(N) ->
    fun(Data) -> rm_nth(N, Data) end.


%% remove element number N from list
rm_nth(1, [_ | R]) ->
    R;
rm_nth(N, [E | R]) ->
    [E | rm_nth(N - 1, R)].

%% -----------------------------------------------------------------------------
%% Internal
%% -----------------------------------------------------------------------------

-spec rebuild_at(N :: pos_integer()) -> na:rebuild_at().

rebuild_at(N) ->
    fun(NewVal, Data) -> replace_nth(Data, N, NewVal) end.

%% replace list() element as pos N with NewVal
replace_nth([_ | R], 1, NewVal) ->
    [NewVal | R];
replace_nth([E | R], N, NewVal) ->
    [E | replace_nth(R, N - 1, NewVal)].

%% -----------------------------------------------------------------------------
