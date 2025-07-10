-module(na_benchmark_replace).

-include("na_benchmark_ansi_escape_codes.hrl").

-export([
         run/0,
         run/1,
         run/2
        ]).

-define(DUPS, 10).
-define(ITERATIONS, 500_000).

%% -----------------------------------------------------------------------------
%% Compare time usage of replace() vs more traditional accessor functions
%% and pattern matches.

%% Each of the test actions in Tests is run `Dups` times.
%% These test actions are run in random order, this is to minimise the
%% impact of GC and other BEAM VM activities.
%%
%% Each action calls a test function that runs `Iterations` number of times.
%% The actual iterations are run explicitly, to minimise the cost of the
%% looping overhead.

run() ->
    run(?DUPS).

run(Dups) ->
    run(Dups, ?ITERATIONS).

run(Dups, Iterations) ->
    %% sanity check benchmark code
    replace_mixed_sanity_check(),
    replace_lists_sanity_check_1(),
    replace_lists_sanity_check_2(),
    replace_lists_sanity_check_3(),
    replace_rec_sanity_check_1(),
    replace_rec_sanity_check_2(),
    replace_rec_sanity_check_3(),
    replace_rec_sanity_check_4(),
    replace_map_sanity_check_1(),
    replace_map_sanity_check_2(),
    replace_map_sanity_check_3(),
    replace_proplist_sanity_check_1(),
    replace_proplist_sanity_check_2(),

    Tests = [
             {replace_mixed,                fun() -> replace_mixed(Iterations) end},
             {manual_in_mixed,              fun() -> manual_in_mixed(Iterations) end},
             {replace_lists,                fun() -> replace_lists(Iterations) end},
             {pattern_match_lists,          fun() -> pattern_match_lists(Iterations) end},
             {split_lists,                  fun() -> split_lists(Iterations) end},
             {replace_rec,                  fun() -> replace_rec(Iterations) end},
             {rec_field_in_rec,             fun() -> rec_field_in_rec(Iterations) end},
             {tuple_build_in_rec,           fun() -> tuple_build_in_rec(Iterations) end},
             {setelement_in_rec,            fun() -> setelement_in_rec(Iterations) end},
             {replace_maps,                 fun() -> replace_maps(Iterations) end},
             {pattern_match_and_build_maps, fun() -> pattern_match_and_build_maps(Iterations) end},
             {maps_get_put_maps,            fun() -> maps_get_put_maps(Iterations) end},
             {replace_proplists,            fun() -> replace_proplists(Iterations) end},
             {get_value_and_keyreplace_proplists,
              fun() -> get_value_and_keyreplace_proplists(Iterations) end}

            ],
    Actions = lists:flatten(lists:duplicate(Dups, Tests)),
    ShuffledActions = na_benchmark_helper:shuffle(Actions),

    na_benchmark_helper:print_info(Dups, Iterations),
    Res = na_benchmark_helper:run_actions(ShuffledActions),
    PageWidth = na_benchmark_helper:page_width(),

    T1 = na_benchmark_helper:get_avrg(replace_mixed, Res),
    T2 = na_benchmark_helper:get_avrg(manual_in_mixed, Res),
    H1 = string:pad("Mixed data structure - nested record/tuple, proplist, map, list", PageWidth),
    io:format(?BOLD ++ ?BG_TITLE ++ H1 ++ ?RESET ++ "~n", []),
    io:format("replace():                                  ~p µs~n", [round(T1)]),
    io:format("normal local access ops:                    ~p µs~n", [round(T2)]),
    io:format("replace()/normal:                           ~.3f~n", [T1 / T2]),
    io:format("~n", []),

    T3 = na_benchmark_helper:get_avrg(replace_lists, Res),
    T4 = na_benchmark_helper:get_avrg(pattern_match_lists, Res),
    T5 = na_benchmark_helper:get_avrg(split_lists, Res),
    H2 = string:pad("Nested lists", PageWidth),
    io:format(?BOLD ++ ?BG_TITLE ++ H2 ++ ?RESET ++ "~n", []),
    io:format("replace():                                  ~p µs~n", [round(T3)]),
    io:format("local pattern match and build:              ~p µs~n", [round(T4)]),
    io:format("local lists:split() and build:              ~p µs~n", [round(T5)]),
    io:format("replace()/local pattern match and build:    ~.3f~n", [T3 / T4]),
    io:format("replace()/local lists:split() and build:    ~.3f~n", [T3 / T5]),
    io:format("~n", []),

    T6 = na_benchmark_helper:get_avrg(replace_rec, Res),
    T7 = na_benchmark_helper:get_avrg(rec_field_in_rec, Res),
    T8 = na_benchmark_helper:get_avrg(tuple_build_in_rec, Res),
    T9 = na_benchmark_helper:get_avrg(setelement_in_rec, Res),
    H3 = string:pad("Nested record/tuple", PageWidth),
    io:format(?BOLD ++ ?BG_TITLE ++ H3 ++ ?RESET ++ "~n", []),
    io:format("replace():                                  ~p µs~n", [round(T6)]),
    io:format("local R#rec{field = ...}:                   ~p µs~n", [round(T7)]),
    io:format("local tuple pattern match and build:        ~p µs~n", [round(T8)]),
    io:format("local setelement():                         ~p µs~n", [round(T9)]),
    io:format("replace()/R#rec{field = ...}:               ~.3f~n", [T6 / T7]),
    io:format("replace()/tuple pattern match and build:    ~.3f~n", [T6 / T8]),
    io:format("replace()/setelement():                     ~.3f~n", [T6 / T9]),
    io:format("~n", []),

    T10 = na_benchmark_helper:get_avrg(replace_maps, Res),
    T11 = na_benchmark_helper:get_avrg(pattern_match_and_build_maps, Res),
    T12 = na_benchmark_helper:get_avrg(maps_get_put_maps, Res),
    H4 = string:pad("Nested maps", PageWidth),
    io:format(?BOLD ++ ?BG_TITLE ++ H4 ++ ?RESET ++ "~n", []),
    io:format("replace():                                  ~p µs~n", [round(T10)]),
    io:format("local pattern and build match:              ~p µs~n", [round(T11)]),
    io:format("local maps:get() & put():                   ~p µs~n", [round(T12)]),
    io:format("replace()/local pattern match and build:    ~.3f~n", [T10 / T11]),
    io:format("replace()/local maps:get() & put():         ~.3f~n", [T10 / T12]),
    io:format("~n", []),

    T13 = na_benchmark_helper:get_avrg(replace_proplists, Res),
    T14 = na_benchmark_helper:get_avrg(get_value_and_keyreplace_proplists, Res),
    H5 = string:pad("Nested proplists", PageWidth),
    io:format(?BOLD ++ ?BG_TITLE ++ H5 ++ ?RESET ++ "~n", []),
    io:format("replace():                                  ~p µs~n", [round(T13)]),
    io:format("local get_value() & keyreplace():           ~p µs~n", [round(T14)]),
    io:format("replace()/local get_value() & keyreplace(): ~.3f~n", [T13 / T14]),
    io:format("~n", []).

%% --------------------------------------------------------------------------------------------------

-record(level3, {a = 5, b = 6, c = 7}).
-record(level2, {a = 3, b = 4, c = #level3{}}).
-record(level1, {a = 1, b = #level2{}, c = 2}).

-record(rec, {a, b, c, users}).

%% --------------------------------------------------------------------------------------------------
%% Use nested record(), proplist(), map(), list() access

mixed_data() ->
    Users = [
             {john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
             {mary, #{name => "Mary", age => 29, languages => ["Elixir", "F#", "Clojure"]}}
            ],
    #rec{users = Users}.


replace_mixed(Iterations) ->
    replace_mixed(Iterations, mixed_data()).

replace_mixed(0, _) ->
    ok;
replace_mixed(Iterations, Rec) ->
    _R1 = na:replace(Rec, [#rec.users, mary, age], 42),
    _R2 = na:replace(Rec, [#rec.users, mary, languages, na_lists:update_at(1)], "Erlang"),
    replace_mixed(Iterations - 1, Rec).


%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(REPLACE_1(RecA),
        begin
            UsersA = RecA#rec.users,
            UserA1 = proplists:get_value(mary, UsersA),

            %% rm old user from proplists
            UsersA2 = UsersA -- [{mary, UserA1}],
            %% update and re-add user as new entry
            %% ordering is not preserved, as this isn't usually necessary
            UsersA3 = [{mary, UserA1#{age => 42}} | UsersA2],

            RecA#rec{users = UsersA3}
        end
       ).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(REPLACE_2(RecB),
        begin
            UsersB = RecB#rec.users,
            UserB1 = proplists:get_value(mary, UsersB),

            #{languages := LangB} = UserB1,
            [_ | LangBR] = LangB,
            NewLangB = ["Erlang" | LangBR],

            %% rm old user from proplists
            UsersB2 = UsersB -- [{mary, UserB1}],
            %% update and re-add user as new entry
            %% ordering is not preserved, as this isn't usually necessary
            UsersB3 = [{mary, UserB1#{languages => NewLangB}} | UsersB2],

            RecB#rec{users = UsersB3}
        end
       ).

manual_in_mixed(Iterations) ->
    manual_in_mixed(Iterations, mixed_data()).

manual_in_mixed(0, _) ->
    ok;
manual_in_mixed(Iterations, Rec) ->
    _R1 = ?REPLACE_1(Rec),
    _R2 = ?REPLACE_2(Rec),
    manual_in_mixed(Iterations - 1, Rec).


%% Check correctness of calls separatly to not impact benchmark.
%% Note: that 'marry' and 'john' change places in the proplists.
replace_mixed_sanity_check() ->
    Rec = mixed_data(),

    Users1 = [
              {john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
              {mary, #{name => "Mary", age => 42, languages => ["Elixir", "F#", "Clojure"]}}
            ],
    ExpectedRec1 = #rec{users = Users1},

    Users2 = [
              {john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}},
              {mary, #{name => "Mary", age => 29, languages => ["Erlang", "F#", "Clojure"]}}
             ],
    ExpectedRec2 = #rec{users = Users2},

    ExpectedRec1 = na:replace(Rec, [#rec.users, mary, age], 42),
    ExpectedRec2 = na:replace(Rec, [#rec.users, mary, languages, na_lists:update_at(1)], "Erlang"),

    Users3 = [
              {mary, #{name => "Mary", age => 42, languages => ["Elixir", "F#", "Clojure"]}},
              {john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}}
             ],
    ExpectedRec3 = #rec{users = Users3},

    Users4 = [
              {mary, #{name => "Mary", age => 29, languages => ["Erlang", "F#", "Clojure"]}},
              {john, #{name => "John", age => 27, languages => ["Erlang", "Ruby", "Elixir"]}}
             ],
    ExpectedRec4 = #rec{users = Users4},

    ExpectedRec3 = ?REPLACE_1(Rec),
    ExpectedRec4 = ?REPLACE_2(Rec).

%% --------------------------------------------------------------------------------------------------

list_data() ->
    [[1, 2, 3],
     [4, 5, 6],
     [7, 8, 9]].


replace_lists(Iterations) ->
    replace_lists(Iterations, list_data()).

replace_lists(0, _) ->
    ok;
replace_lists(Iterations, L) ->
    _R = na:replace(L, [na_lists:update_at(2), na_lists:update_at(2)], 42),
    replace_lists(Iterations - 1, L).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(REPLACE_LISTS_1(L),
        begin
            [R1, R2, R3] = L,
            [C1, _, C3] = R2,
            [R1, [C1, 42, C3], R3]
        end
       ).

pattern_match_lists(Iterations) ->
    pattern_match_lists(Iterations, list_data()).

pattern_match_lists(0, _) ->
    ok;
pattern_match_lists(Iterations, L) ->
    _R = ?REPLACE_LISTS_1(L),
    pattern_match_lists(Iterations - 1, L).

%% reasonably generic code that can take any valid 2D index (I,J).
%% Each N value in split(N ...) is N = I - 1 and N = J - 1.
%%
%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(REPLACE_LISTS_2(L),
        begin
            {R1, [R2 | R3]} = lists:split(1, L),
            {C1, [_ | C3]}  = lists:split(1, R2),
            R1 ++
                [
                 C1 ++ [42 | C3]
                | R3]
        end
       ).

split_lists(Iterations) ->
    split_lists(Iterations, list_data()).

split_lists(0, _) ->
    ok;
split_lists(Iterations, L) ->
    _R = ?REPLACE_LISTS_2(L),
    split_lists(Iterations - 1, L).


list_expected() ->
    [[1, 2, 3],
     [4, 42, 6],
     [7, 8, 9]].

replace_lists_sanity_check_1() ->
    L = list_data(),
    Expected = list_expected(),
    Expected = na:replace(L, [na_lists:update_at(2), na_lists:update_at(2)], 42).

replace_lists_sanity_check_2() ->
    L = list_data(),
    Expected = list_expected(),
    Expected = ?REPLACE_LISTS_1(L).

replace_lists_sanity_check_3() ->
    L = list_data(),
    Expected = list_expected(),
    Expected = ?REPLACE_LISTS_2(L).

%% --------------------------------------------------------------------------------------------------

rec_data() ->
    #level1{}.

replace_rec(Iterations) ->
    replace_rec(Iterations, rec_data()).

replace_rec(0, _) ->
    ok;
replace_rec(Iterations, Rec) ->
    _R = na:replace(Rec, [#level1.b, #level2.c, #level3.a], 42),
    replace_rec(Iterations - 1, Rec).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(REPLACE_REC_1(Rec),
        begin
            RL2 = Rec#level1.b,
            RL3 = RL2#level2.c,

            UL3 = RL3#level3{a = 42},
            UL2 = RL2#level2{c = UL3},
            Rec#level1{b = UL2}
        end
       ).

rec_field_in_rec(Iterations) ->
    rec_field_in_rec(Iterations, rec_data()).

rec_field_in_rec(0, _) ->
    ok;
rec_field_in_rec(Iterations, Rec) ->
    _R = ?REPLACE_REC_1(Rec),
    rec_field_in_rec(Iterations - 1, Rec).


%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(REPLACE_REC_2(Rec),
        begin
            {T1r, T1a, T1b, T1c} = Rec,
            {T2r, T2a, T2b, T2c} = T1b,
            {T3r, _, T3b, T3c} = T2c,

            U3 = {T3r, 42, T3b, T3c},
            U2 = {T2r, T2a, T2b, U3},
            {T1r, T1a, U2, T1c}
        end
       ).

tuple_build_in_rec(Iterations) ->
    tuple_build_in_rec(Iterations, rec_data()).

tuple_build_in_rec(0, _) ->
    ok;
tuple_build_in_rec(Iterations, Rec) ->
    _R = ?REPLACE_REC_2(Rec),
    tuple_build_in_rec(Iterations - 1, Rec).


%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(REPLACE_REC_3(Rec),
        begin
            T2 = element(3, Rec),
            T3 = element(4, T2),

            U3 = setelement(2, T3, 42),
            U2 = setelement(4, T2, U3),
            setelement(3, Rec, U2)
        end
       ).

setelement_in_rec(Iterations) ->
    setelement_in_rec(Iterations, rec_data()).

setelement_in_rec(0, _) ->
    ok;
setelement_in_rec(Iterations, Rec) ->
    _R = ?REPLACE_REC_3(Rec),
    setelement_in_rec(Iterations - 1, Rec).


rec_expected() ->
    L3 = #level3{a = 42},
    L2 = #level2{c = L3},
    #level1{b = L2}.

replace_rec_sanity_check_1() ->
    Rec = rec_data(),
    Expected = rec_expected(),
    Expected = na:replace(Rec, [#level1.b, #level2.c, #level3.a], 42).

replace_rec_sanity_check_2() ->
    Rec = rec_data(),
    Expected = rec_expected(),
    Expected = ?REPLACE_REC_1(Rec).

replace_rec_sanity_check_3() ->
    Rec = rec_data(),
    Expected = rec_expected(),
    Expected = ?REPLACE_REC_2(Rec).

replace_rec_sanity_check_4() ->
    Rec = rec_data(),
    Expected = rec_expected(),
    Expected = ?REPLACE_REC_3(Rec).

%% --------------------------------------------------------------------------------------------------

map_data() ->
    #{
      john => #{name => "John", age => 27,
                languages => #{1 => "Erlang", 2 => "Ruby", 3 => "Elixir"}},
      mary => #{name => "Mary", age => 29,
                languages => #{1 => "Elixir", 2 => "F#", 3 => "Clojure"}}
     }.

replace_maps(Iterations) ->
    replace_maps(Iterations, map_data()).

replace_maps(0, _) ->
    ok;
replace_maps(Iterations, Users) ->
    _R1 = na:replace(Users, [john, age], 42),
    _R2 = na:replace(Users, [mary, languages, 1], "Erlang"),
    replace_maps(Iterations - 1, Users).


%% WARNING - these two macros introduces new variable names, where the macros are introduced.
%%           macros are used to avoid code duplication, while avoiding function call overhead.

-define(REPLACE_MAP_1(Users),
        begin
            #{john := User1} = Users,
            User1Updated = User1#{age := 42},
            Users#{john := User1Updated}
        end
       ).

-define(REPLACE_MAP_2(Users),
        begin
            #{mary := User2} = Users,
            #{languages := Lang2} = User2,
            Lang2Updated = Lang2#{1 := "Erlang"},
            User2Updated = User2#{languages := Lang2Updated},
            Users#{mary => User2Updated}
        end
       ).

pattern_match_and_build_maps(Iteration) ->
    pattern_match_and_build_maps(Iteration, map_data()).

pattern_match_and_build_maps(0, _) ->
    ok;
pattern_match_and_build_maps(Iterations, Users) ->
    _R1 = ?REPLACE_MAP_1(Users),
    _R2 = ?REPLACE_MAP_2(Users),
    pattern_match_and_build_maps(Iterations - 1, Users).


%% WARNING - these two macros introduces new variable names, where the macros are introduced.
%%           macros are used to avoid code duplication, while avoiding function call overhead.

-define(REPLACE_MAP_3(Users),
        begin
            User1 = maps:get(john, Users),
            User1Updated = maps:put(age, 42, User1),
            maps:put(john, User1Updated, Users)
        end
       ).

-define(REPLACE_MAP_4(Users),
        begin
            User2 = maps:get(mary, Users),
            Lang2 = maps:get(languages, User2),
            Lang2Updated = maps:put(1, "Erlang", Lang2),
            User2Updated = maps:put(languages, Lang2Updated, User2),
            maps:put(mary, User2Updated, Users)
        end
       ).

maps_get_put_maps(Iterations) ->
    maps_get_put_maps(Iterations, map_data()).

maps_get_put_maps(0, _) ->
    ok;
maps_get_put_maps(Iterations, Users) ->
    _R1 = ?REPLACE_MAP_3(Users),
    _R2 = ?REPLACE_MAP_4(Users),
    maps_get_put_maps(Iterations - 1, Users).


map_expected1() ->
    #{
      john => #{name => "John", age => 42,
                languages => #{1 => "Erlang", 2 => "Ruby", 3 => "Elixir"}},
      mary => #{name => "Mary", age => 29,
                languages => #{1 => "Elixir", 2 => "F#", 3 => "Clojure"}}
     }.

map_expected2() ->
    #{
      john => #{name => "John", age => 27,
                languages => #{1 => "Erlang", 2 => "Ruby", 3 => "Elixir"}},
      mary => #{name => "Mary", age => 29,
                languages => #{1 => "Erlang", 2 => "F#", 3 => "Clojure"}}
     }.

replace_map_sanity_check_1() ->
    Users = map_data(),
    Expected1 = map_expected1(),
    Expected1 = na:replace(Users, [john, age], 42),
    Expected2 = map_expected2(),
    Expected2 = na:replace(Users, [mary, languages, 1], "Erlang").

replace_map_sanity_check_2() ->
    Users = map_data(),
    Expected1 = map_expected1(),
    Expected1 = ?REPLACE_MAP_1(Users),
    Expected2 = map_expected2(),
    Expected2 = ?REPLACE_MAP_2(Users).

replace_map_sanity_check_3() ->
    Users = map_data(),
    Expected1 = map_expected1(),
    Expected1 = ?REPLACE_MAP_3(Users),
    Expected2 = map_expected2(),
    Expected2 = ?REPLACE_MAP_4(Users).

%% --------------------------------------------------------------------------------------------------

proplist_data() ->
    [
     {john, [{name, "John"},
             {age, 27},
             {languages, [{1, "Erlang"}, {2, "Ruby"}, {3, "Elixir"}]}
            ]},
     {mary, [{name, "Mary"},
             {age, 29},
             {languages, [{1, "Elixir"}, {2, "F#"}, {3, "Clojure"}]}
            ]}
    ].

replace_proplists(Iterations) ->
    replace_proplists(Iterations, proplist_data()).

replace_proplists(0, _) ->
    ok;
replace_proplists(Iterations, Users) ->
    _R1 = na:replace(Users, [john, age], 42),
    _R2 = na:replace(Users, [mary, languages, 1], "Erlang"),
    replace_proplists(Iterations - 1, Users).


%% WARNING - these two macros introduces new variable names, where the macros are introduced.
%%           macros are used to avoid code duplication, while avoiding function call overhead.

-define(REPLACE_PROPLISTS_1(Users),
        begin
            User1 = proplists:get_value(john, Users),
            User1Updated = lists:keyreplace(age, 1, User1, {age, 42}),
            lists:keyreplace(john, 1, Users, {john, User1Updated})
        end
       ).

-define(REPLACE_PROPLISTS_2(Users),
        begin
            User2 = proplists:get_value(mary, Users),
            Lang2 = proplists:get_value(languages, User2),
            Lang2Updated = lists:keyreplace(1, 1, Lang2, {1, "Erlang"}),
            User2Updated = lists:keyreplace(languages, 1, User2, {languages, Lang2Updated}),
            lists:keyreplace(mary, 1, Users, {mary, User2Updated})
        end
       ).

get_value_and_keyreplace_proplists(Iterations) ->
    get_value_and_keyreplace_proplists(Iterations, proplist_data()).

get_value_and_keyreplace_proplists(0, _) ->
    ok;
get_value_and_keyreplace_proplists(Iterations, Users) ->
    %% code below assumes that proplist() only uses {Key, Val} entries - i.e. not atom()
    %% like na:replace() it only replaces the 1st proplist() entry that matches

    _Users1Updated = ?REPLACE_PROPLISTS_1(Users),
    _Users2Updated = ?REPLACE_PROPLISTS_2(Users),
    get_value_and_keyreplace_proplists(Iterations - 1, Users).


proplist_expected1() ->
    [
     {john, [{name, "John"},
             {age, 42},
             {languages, [{1, "Erlang"}, {2, "Ruby"}, {3, "Elixir"}]}
            ]},
     {mary, [{name, "Mary"},
             {age, 29},
             {languages, [{1, "Elixir"}, {2, "F#"}, {3, "Clojure"}]}
            ]}
    ].

proplist_expected2() ->
    [
     {john, [{name, "John"},
             {age, 27},
             {languages, [{1, "Erlang"}, {2, "Ruby"}, {3, "Elixir"}]}
            ]},
     {mary, [{name, "Mary"},
             {age, 29},
             {languages, [{1, "Erlang"}, {2, "F#"}, {3, "Clojure"}]}
            ]}
    ].

replace_proplist_sanity_check_1() ->
    Users = proplist_data(),
    Expected1 = proplist_expected1(),
    Expected1 = na:replace(Users, [john, age], 42),
    Expected2 = proplist_expected2(),
    Expected2 = na:replace(Users, [mary, languages, 1], "Erlang").

replace_proplist_sanity_check_2() ->
    Users = proplist_data(),
    Expected1 = proplist_expected1(),
    Expected1 = ?REPLACE_PROPLISTS_1(Users),
    Expected2 = proplist_expected2(),
    Expected2 = ?REPLACE_PROPLISTS_2(Users).
