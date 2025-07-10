-module(na_test).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).
-compile(nowarn_export_all).

%% -------------------------------------------------------------------------------------------------

-record(level3, {a = 5, b = 6, c = 7}).
-record(level2, {a = 3, b = 4, c = #level3{}}).
-record(level1, {a = 1, b = #level2{}, c = 2}).

get_test() ->
    Users = [
             {john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
             {mary, #{name => "Mary", age => 29, languages => ["Elixir", "F#", "Clojure"]}}
            ],

    %% empty path - i.e. get all
    ?assertEqual(Users, na:get(Users, [])),

    %% access nested data using built-in keys
    ?assertEqual(27, na:get(Users, [john, age])),
    %% access nested data using built-in keys and lookup fun
    ?assertEqual("Erlang", na:get(Users, [john, languages, na_lists:get_at(1)])),

    %% access a nested record
    Rec = #level1{},
    ?assertEqual(5, na:get(Rec, [#level1.b, #level2.c, #level3.a])),

    %% get bottom right in "array"
    L = [[1, 2, 3],
         [4, 5, 6],
         [7, 8, 9]],
    ?assertEqual(9, na:get(L, [na_lists:get_at(3), na_lists:get_at(3)])).

%% -------------------------------------------------------------------------------------------------

remove_test() ->
    Users = [
             {john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
             {mary, #{name => "Mary", age => 29, languages => ["Elixir", "F#", "Clojure"]}}
            ],

    %% empty path - implies lookup of nothing, which is an error as lookups are assumed to
    %% always be successful
    ?assertError(_, na:remove(Users, [])),

    %% do a "normal" nested lookup
    Expected1 = [
                 {john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
                 {mary, #{name => "Mary", languages => ["Elixir", "F#", "Clojure"]}}
                ],
    ?assertEqual(Expected1, na:remove(Users, [mary, age])),

    %% test using a `na:remove_at()` as the last path key
    Expected2 = [
                 {john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
                 {mary, #{name => "Mary", age => 29, languages => ["Elixir", "Clojure"]}}
                ],
    ?assertEqual(Expected2,
                 na:remove(Users, [mary, languages, na_lists:remove_at(2)])),

    %% test using `na:update_at()` as non-last path key
    Data3     = [#{a => 42}, #{a => 44}, #{a => 46}],
    Expected3 = [#{a => 42}, #{},        #{a => 46}],
    ?assertEqual(Expected3, na:remove(Data3, [na_lists:update_at(2), a])),

    %% test using `na:remove_at()` in final and `na:update_at()` in non-final path keys
    Data4     = [[42], [44], [46]],
    Expected4 = [[42], [],   [46]],
    ?assertEqual(Expected4,
                 na:remove(Data4, [na_lists:update_at(2), na_lists:remove_at(1)])
                ),

    %% access proplist inside of proplist
    Data5     = [{a, [{a, 1}, {b, 2}]},
                 {b, [{a, 3}, {b, 4}]}],
    Expected5 = [{a, [{a, 1}, {b, 2}]},
                 {b, [{b, 4}]}],
    ?assertEqual(Expected5, na:remove(Data5, [b, a])),

    %% access map inside map
    Data6     = #{a => #{a => 1, b => 2},
                  b => #{a => 3, b => 4}},
    Expected6 = #{a => #{a => 1, b => 2},
                  b => #{b => 4}},
    ?assertEqual(Expected6, na:remove(Data6, [b, a])),

    Data7     = {12, 34, #{a => 2, b => 4}},
    Expected7 = {12, 34, #{a => 2}},
    %% try to access element that can't be removed
    ?assertError(_, na:remove(Data7, [2])),
    %% test tuple taversal
    ?assertEqual(Expected7, na:remove(Data7, [3, b])).

%% -------------------------------------------------------------------------------------------------

%% test with `key_or_index()` path values
%% update with a `modify_fun()`
modify_test() ->
    Users = [
             {john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
             {mary, #{name => "Mary", age => 29, languages => ["Elixir", "F#", "Clojure"]}}
            ],

    U2 = na:modify(Users, [mary, languages], fun(L) -> L -- ["Clojure"] end),
    U3 = na:modify(U2,    [john, age],       fun(_) -> 31 end),

    Expected = [
                {john, #{name => "John", age => 31, languages => ["Erlang", "Ruby", "Elixir"]}},
                {mary, #{name => "Mary", age => 29, languages => ["Elixir", "F#"]}}
               ],
    ?assertEqual(Expected, U3).

%% test with a `na:update_at()` path element
%% update both with a `modify_fun()`
modify_using_access_funs_test() ->
    Users = [
             {john, ["John", 27, ["Erlang", "Ruby", "Elixir"]]},
             {mary, ["Mary", 29, ["Elixir", "F#", "Clojure"]]}
            ],

    U2 = na:modify(Users, [mary, na_lists:update_at(3)], fun(L) -> L -- ["Clojure"] end),
    U3 = na:modify(U2,    [john, na_lists:update_at(2)], fun(_) -> 31 end),

    Expected = [
                {john, ["John", 31, ["Erlang", "Ruby", "Elixir"]]},
                {mary, ["Mary", 29, ["Elixir", "F#"]]}
               ],
    ?assertEqual(Expected, U3).

modify_using_proplists_test() ->
    %% test with proplist() containing atoms, verify that only the first 'b' is updated
    %% Note: 1st path step acts on map()
    Data     = #{a => [a, b, c, b],       b => 42, c => 56},
    Expected = #{a => [a, {b, 42}, c, b], b => 42, c => 56},
    ?assertEqual(Expected, na:modify(Data, [a, b], fun(_) -> 42 end)),

    %% test with proplist() containing atoms, verify that only the first 'b' is updated
    %% Note: 1st and 2nd path steps act on tuple()
    Data2     = {a, {[a, b, c, b]},       42, 56},
    Expected2 = {a, {[a, {b, 42}, c, b]}, 42, 56},
    ?assertEqual(Expected2, na:modify(Data2, [2, 1, b], fun(_) -> 42 end)).

%% --------------------------------------------------------------------------------------------------

%% test with `key_or_index()` path values
%% update with a replacement value
replace_test() ->
    Users = [
             {john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
             {mary, #{name => "Mary", age => 29, languages => ["Elixir", "F#", "Clojure"]}}
            ],

    U2 = na:replace(Users, [mary, languages], ["Erlang"]),
    U3 = na:replace(U2,    [john, age],       31),

    Expected = [
                {john, #{name => "John", age => 31, languages => ["Erlang", "Ruby", "Elixir"]}},
                {mary, #{name => "Mary", age => 29, languages => ["Erlang"]}}
               ],
    ?assertEqual(Expected, U3).

%% test with a `na:update_at()` path value
%% update with a replacement value
replace_using_access_funs_test() ->
    Users = [
             {john, ["John", 27, ["Erlang", "Ruby", "Elixir"]]},
             {mary, ["Mary", 29, ["Elixir", "F#", "Clojure"]]}
            ],

    U2 = na:replace(Users, [mary, na_lists:update_at(3)], ["Erlang"]),
    U3 = na:replace(U2,    [john, na_lists:update_at(2)], 31),

    Expected = [
                {john, ["John", 31, ["Erlang", "Ruby", "Elixir"]]},
                {mary, ["Mary", 29, ["Erlang"]]}
               ],
    ?assertEqual(Expected, U3).

replace_using_proplists_test() ->
    %% test with proplist() containing atoms, verify that only the first 'b' is updated
    %% Note: 1st path step acts on map()
    Data     = #{a => [a, b, c, b],       b => 42, c => 56},
    Expected = #{a => [a, {b, 42}, c, b], b => 42, c => 56},
    ?assertEqual(Expected, na:replace(Data, [a, b], 42)),

    %% test with proplist() containing atoms, verify that only the first 'b' is updated
    %% Note: 1st and 2nd path steps act on tuple()
    Data2     = {a, {[a, b, c, b]},       42, 56},
    Expected2 = {a, {[a, {b, 42}, c, b]}, 42, 56},
    ?assertEqual(Expected2, na:replace(Data2, [2, 1, b], 42)).
