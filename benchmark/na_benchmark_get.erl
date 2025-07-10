-module(na_benchmark_get).

-include("na_benchmark_ansi_escape_codes.hrl").

-export([
         run/0,
         run/1,
         run/2
        ]).

-define(DUPS, 10).
-define(ITERATIONS, 1_000_000).

%% -----------------------------------------------------------------------------
%% Compare time usage of get() vs more traditional accessor functions
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
    get_mixed_sanity_check(),
    get_rec_sanity_check(),
    get_lists_sanity_check(),
    get_maps_sanity_check(),
    get_proplists_sanity_check(),

    Tests = [
             {get_mixed,                  fun() -> get_mixed(Iterations) end},
             {get_mixed_accessor_only,    fun() -> get_mixed_accessor_only(Iterations) end},
             {manual_mixed,               fun() -> manual_mixed(Iterations) end},
             {get_lists,                  fun() -> get_lists(Iterations) end},
             {nth_lists,                  fun() -> nth_lists(Iterations) end},
             {pattern_match_lists,        fun() -> pattern_match_lists(Iterations) end},
             {hd_tl_lists,                fun() -> hd_tl_lists(Iterations) end},
             {get_rec,                    fun() -> get_rec(Iterations) end},
             {rec_field_in_rec,           fun() -> rec_field_in_rec(Iterations) end},
             {tuple_pattern_match_in_rec, fun() -> tuple_pattern_match_in_rec(Iterations) end},
             {element_in_rec,             fun() -> element_in_rec(Iterations) end},
             {get_maps,                   fun() -> get_maps(Iterations) end},
             {pattern_match_maps,         fun() -> pattern_match_maps(Iterations) end},
             {maps_get_maps,              fun() -> maps_get_maps(Iterations) end},
             {get_proplists,              fun() -> get_proplists(Iterations) end},
             {get_value_proplists,        fun() -> get_value_proplists(Iterations) end}
            ],
    Actions = lists:flatten(lists:duplicate(Dups, Tests)),
    ShuffledActions = na_benchmark_helper:shuffle(Actions),

    na_benchmark_helper:print_info(Dups, Iterations),
    Res = na_benchmark_helper:run_actions(ShuffledActions),
    PageWidth = na_benchmark_helper:page_width(),

    T1 = na_benchmark_helper:get_avrg(get_mixed, Res),
    T2 = na_benchmark_helper:get_avrg(manual_mixed, Res),
    H1 = string:pad("Mixed data structure - nested record/tuple, proplist, map, list", PageWidth),
    io:format(?BOLD ++ ?BG_TITLE ++ H1 ++ ?RESET ++ "~n", []),
    io:format("get():                             ~p µs~n", [round(T1)]),
    io:format("normal local access ops:           ~p µs~n", [round(T2)]),
    io:format("get()/normal:                      ~.3f~n", [T1 / T2]),
    io:format("~n", []),

    T1b = na_benchmark_helper:get_avrg(get_mixed_accessor_only, Res),
    H2 = string:pad("Mixed data structure - nested record/tuple, proplist, map, list", PageWidth),
    io:format(?BOLD ++ ?BG_TITLE ++ H2 ++ ?RESET ++ "~n", []),
    io:format("get() using only accessors:        ~p µs~n", [round(T1b)]),
    io:format("normal local access ops:           ~p µs~n", [round(T2)]),
    io:format("get()/normal:                      ~.3f~n", [T1b / T2]),
    io:format("~n", []),

    T3  = na_benchmark_helper:get_avrg(get_lists, Res),
    T4  = na_benchmark_helper:get_avrg(nth_lists, Res),
    T4b = na_benchmark_helper:get_avrg(pattern_match_lists, Res),
    T4c = na_benchmark_helper:get_avrg(hd_tl_lists, Res),
    H3 = string:pad("Nested lists", PageWidth),
    io:format(?BOLD ++ ?BG_TITLE ++ H3 ++ ?RESET ++ "~n", []),
    io:format("get():                             ~p µs~n", [round(T3)]),
    io:format("local lists:nth():                 ~p µs~n", [round(T4)]),
    io:format("local pattern match:               ~p µs~n", [round(T4b)]),
    io:format("local hd() and tl():               ~p µs~n", [round(T4c)]),
    io:format("get()/nth():                       ~.3f~n", [T3 / T4]),
    io:format("get()/local pattern match:         ~.3f~n", [T3 / T4b]),
    io:format("get()/local hd() and tl():         ~.3f~n", [T3 / T4c]),
    io:format("~n", []),

    T5 = na_benchmark_helper:get_avrg(get_rec, Res),
    T6 = na_benchmark_helper:get_avrg(rec_field_in_rec, Res),
    T7 = na_benchmark_helper:get_avrg(tuple_pattern_match_in_rec, Res),
    T8 = na_benchmark_helper:get_avrg(element_in_rec, Res),
    H4 = string:pad("Nested record / tuple", PageWidth),
    io:format(?BOLD ++ ?BG_TITLE ++ H4 ++ ?RESET ++ "~n", []),
    io:format("get():                             ~p µs~n", [round(T5)]),
    io:format("local R#rec.field:                 ~p µs~n", [round(T6)]),
    io:format("local tuple pattern match:         ~p µs~n", [round(T7)]),
    io:format("local element():                   ~p µs~n", [round(T8)]),
    io:format("get()/R#rec.field:                 ~.3f~n", [T5 / T6]),
    io:format("get()/tuple pattern match:         ~.3f~n", [T5 / T7]),
    io:format("get()/element():                   ~.3f~n", [T5 / T8]),
    io:format("~n", []),

    T12 = na_benchmark_helper:get_avrg(get_maps, Res),
    T13 = na_benchmark_helper:get_avrg(pattern_match_maps, Res),
    T14 = na_benchmark_helper:get_avrg(maps_get_maps, Res),
    H5 = string:pad("Nested maps", PageWidth),
    io:format(?BOLD ++ ?BG_TITLE ++ H5 ++ ?RESET  ++ "~n", []),
    io:format("get():                             ~p µs~n", [round(T12)]),
    io:format("local pattern match:               ~p µs~n", [round(T13)]),
    io:format("local maps:get():                  ~p µs~n", [round(T14)]),
    io:format("get()/local pattern match:         ~.3f~n", [T12 / T13]),
    io:format("get()/local maps:get():            ~.3f~n", [T12 / T14]),
    io:format("~n", []),

    T10 = na_benchmark_helper:get_avrg(get_proplists, Res),
    T11 = na_benchmark_helper:get_avrg(get_value_proplists, Res),
    H6 = string:pad("Nested proplists", PageWidth),
    io:format(?BOLD ++ ?BG_TITLE ++ H6 ++ ?RESET ++ "~n", []),
    io:format("get():                             ~p µs~n", [round(T10)]),
    io:format("local proplists:get_value():       ~p µs~n", [round(T11)]),
    io:format("get()/local proplists:get_value(): ~.3f~n", [T10 / T11]),
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


get_mixed(Iterations) ->
    get_mixed(Iterations, mixed_data()).

get_mixed(0, _) ->
    ok;
get_mixed(Iterations, Rec) ->
    _R1 = na:get(Rec, [#rec.users, john, age]),
    _R2 = na:get(Rec, [#rec.users, john, languages, na_lists:get_at(1)]),
    get_mixed(Iterations - 1, Rec).


%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_ACESSOR_ONLY_1(Rec),
        na:get(Rec, [fun(D) -> element(#rec.users, D) end,
                     fun(P) -> na_proplists_helper:get_first_proplist_value(john, P) end,
                     fun(M) -> #{age := V} = M, V end
                    ])
       ).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_ACESSOR_ONLY_2(Rec),
        na:get(Rec, [fun(D) -> element(#rec.users, D) end,
                     fun(P) -> na_proplists_helper:get_first_proplist_value(john, P) end,
                     fun(M) -> #{languages := Lang} = M, Lang end,
                     na_lists:get_at(1)
                    ])
       ).


%% A test to see if matching quicker in na:get() helps performance.
%% Note: this ends up being slower, presumably due to the fun() creation and calling overhead.
get_mixed_accessor_only(Iterations) ->
    get_mixed_accessor_only(Iterations, mixed_data()).

get_mixed_accessor_only(0, _) ->
    ok;
get_mixed_accessor_only(Iterations, Rec) ->
    _R1 = ?GET_ACESSOR_ONLY_1(Rec),
    _R2 = ?GET_ACESSOR_ONLY_2(Rec),
    get_mixed_accessor_only(Iterations - 1, Rec).


%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_1(Rec),
        begin
            Users = Rec#rec.users,
            User1 = proplists:get_value(john, Users),
            #{age := Age} = User1,
            Age
        end
       ).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_2(Rec),
        begin
            Users2 = Rec#rec.users,
            User2 = proplists:get_value(john, Users2),
            #{languages := Lang} = User2,
            lists:nth(1, Lang)
        end
       ).

manual_mixed(Iterations) ->
    manual_mixed(Iterations, mixed_data()).

manual_mixed(0, _) ->
    ok;
manual_mixed(Iterations, Rec) ->
    _R1 = ?GET_1(Rec),
    _R2 = ?GET_2(Rec),
    manual_mixed(Iterations - 1, Rec).


get_mixed_sanity_check() ->
    Rec = mixed_data(),
    Expected1 = 27,
    Expected2 = "Erlang",

    Expected1 = na:get(Rec, [#rec.users, john, age]),
    Expected2 = na:get(Rec, [#rec.users, john, languages, na_lists:get_at(1)]),

    Expected1 = ?GET_ACESSOR_ONLY_1(Rec),
    Expected2 = ?GET_ACESSOR_ONLY_2(Rec),

    Expected1 = ?GET_1(Rec),
    Expected2 = ?GET_2(Rec).

%% --------------------------------------------------------------------------------------------------

rec_data() ->
    #level1{}.

get_rec(Iterations) ->
    get_rec(Iterations, rec_data()).

get_rec(0, _) ->
    ok;
get_rec(Iterations, Rec) ->
    5 = na:get(Rec, [#level1.b, #level2.c, #level3.a]),
    get_rec(Iterations - 1, Rec).


-define(GET_REC_1(Rec),
        (((Rec#level1.b)#level2.c)#level3.a)
       ).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_REC_2(Rec),
        begin
            {_, _, T2, _} = Rec,
            {_, _, _, T3} = T2,
            {_, E, _, _} = T3,
            E
        end
       ).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_REC_3(Rec),
        begin
            T2 = element(3, Rec),
            T3 = element(4, T2),
            element(2, T3)
        end
       ).


rec_field_in_rec(Iterations) ->
    rec_field_in_rec(Iterations, rec_data()).

rec_field_in_rec(0, _) ->
    ok;
rec_field_in_rec(Iterations, Rec) ->
    _R = ?GET_REC_1(Rec),
    rec_field_in_rec(Iterations - 1, Rec).


tuple_pattern_match_in_rec(Iterations) ->
    tuple_pattern_match_in_rec(Iterations, rec_data()).

tuple_pattern_match_in_rec(0, _) ->
    ok;
tuple_pattern_match_in_rec(Iterations, Rec) ->
    _R = ?GET_REC_2(Rec),
    tuple_pattern_match_in_rec(Iterations - 1, Rec).


element_in_rec(Iterations) ->
    element_in_rec(Iterations, rec_data()).

element_in_rec(0, _) ->
    ok;
element_in_rec(Iterations, Rec) ->
    _R = ?GET_REC_3(Rec),
    element_in_rec(Iterations - 1, Rec).


get_rec_sanity_check() ->
    Rec = rec_data(),
    Expected = 5,

    Expected = na:get(Rec, [#level1.b, #level2.c, #level3.a]),
    Expected = ?GET_REC_1(Rec),
    Expected = ?GET_REC_2(Rec),
    Expected = ?GET_REC_3(Rec).

%% --------------------------------------------------------------------------------------------------

list_data() ->
    [[1, 2, 3],
     [4, 5, 6],
     [7, 8, 9]].


get_lists(Iterations) ->
    get_lists(Iterations, list_data()).

get_lists(0, _) ->
    ok;
get_lists(Iterations, L) ->
    9 = na:get(L, [na_lists:get_at(3), na_lists:get_at(3)]),
    get_lists(Iterations - 1, L).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_LISTS_1(L),
        begin
            R3 = lists:nth(3, L),
            lists:nth(3, R3)
        end
       ).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_LISTS_2(L),
        begin
            [_, _, R3] = L,
            [_, _, C3] = R3,
            C3
        end
       ).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_LISTS_3(L),
        begin
            R3 = hd(tl(tl(L))),
            hd(tl(tl(R3)))
        end
       ).

nth_lists(Iterations) ->
    nth_lists(Iterations, list_data()).

nth_lists(0, _) ->
    ok;
nth_lists(Iterations, L) ->
    _R = ?GET_LISTS_1(L),
    nth_lists(Iterations - 1, L).


pattern_match_lists(Iterations) ->
    pattern_match_lists(Iterations, list_data()).

pattern_match_lists(0, _) ->
    ok;
pattern_match_lists(Iterations, L) ->
    _R = ?GET_LISTS_2(L),
    pattern_match_lists(Iterations - 1, L).


hd_tl_lists(Iterations) ->
    hd_tl_lists(Iterations, list_data()).

hd_tl_lists(0, _) ->
    ok;
hd_tl_lists(Iterations, L) ->
    _R = ?GET_LISTS_3(L),
    hd_tl_lists(Iterations - 1, L).


get_lists_sanity_check() ->
    L = list_data(),
    Expected = 9,

    Expected = na:get(L, [na_lists:get_at(3), na_lists:get_at(3)]),
    Expected = ?GET_LISTS_1(L),
    Expected = ?GET_LISTS_2(L),
    Expected = ?GET_LISTS_3(L).

%% --------------------------------------------------------------------------------------------------

map_data() ->
    #{
      john => #{name => "John", age => 27,
                languages => #{1 => "Erlang", 2 => "Ruby", 3 => "Elixir"}},
      mary => #{name => "Mary", age => 29,
                languages => #{1 => "Elixir", 2 => "F#", 3 => "Clojure"}}
     }.

get_maps(Iterations) ->
    get_maps(Iterations, map_data()).

get_maps(0, _) ->
    ok;
get_maps(Iterations, Users) ->
    27 = na:get(Users, [john, age]),
    "Erlang" = na:get(Users, [john, languages, 1]),
    get_maps(Iterations - 1, Users).


%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_MAPS_1_A(Users),
       begin
           #{john := User1} = Users,
           #{age := Age} = User1,
           Age
       end
       ).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_MAPS_2_A(Users),
       begin
           #{john := User2} = Users,
           #{languages := Lang} = User2,
           #{1 := E} = Lang,
           E
       end
       ).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_MAPS_1_B(Users),
       begin
           User1 = maps:get(john, Users),
           maps:get(age, User1)
       end
       ).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_MAPS_2_B(Users),
       begin
           User2 = maps:get(john, Users),
           Lang = maps:get(languages, User2),
           maps:get(1, Lang)
       end
       ).


pattern_match_maps(Iteration) ->
    pattern_match_maps(Iteration, map_data()).

pattern_match_maps(0, _) ->
    ok;
pattern_match_maps(Iterations, Users) ->
    _R1 = ?GET_MAPS_1_A(Users),
    _R2 = ?GET_MAPS_2_A(Users),
    pattern_match_maps(Iterations - 1, Users).


maps_get_maps(Iterations) ->
    maps_get_maps(Iterations, map_data()).

maps_get_maps(0, _) ->
    ok;
maps_get_maps(Iterations, Users) ->
    _R1 = ?GET_MAPS_1_B(Users),
    _R2 = ?GET_MAPS_2_B(Users),
    maps_get_maps(Iterations - 1, Users).


get_maps_sanity_check() ->
    Users = map_data(),

    Expected1 = 27,
    Expected2 = "Erlang",

    Expected1 = na:get(Users, [john, age]),
    Expected2 = na:get(Users, [john, languages, 1]),

    Expected1 = ?GET_MAPS_1_A(Users),
    Expected2 = ?GET_MAPS_2_A(Users),

    Expected1 = ?GET_MAPS_1_B(Users),
    Expected2 = ?GET_MAPS_2_B(Users).

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

get_proplists(Iterations) ->
    get_proplists(Iterations, proplist_data()).

get_proplists(0, _) ->
    ok;
get_proplists(Iterations, Users) ->
    27 = na:get(Users, [john, age]),
    "Erlang" = na:get(Users, [john, languages, 1]),
    get_proplists(Iterations - 1, Users).


%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_PROPLISTS_1(Users),
        begin
            User1 = proplists:get_value(john, Users),
            proplists:get_value(age, User1)
        end
       ).

%% WARNING - introduces new variable names, where the macro is used.
%%           The macro is used to avoid code duplication, while avoiding function call overhead.
-define(GET_PROPLISTS_2(Users),
        begin
            User2 = proplists:get_value(john, Users),
            Lang = proplists:get_value(languages, User2),
            proplists:get_value(1, Lang)
        end
       ).


get_value_proplists(Iterations) ->
    get_value_proplists(Iterations, proplist_data()).

get_value_proplists(0, _) ->
    ok;
get_value_proplists(Iterations, Users) ->
    _R1 = ?GET_PROPLISTS_1(Users),
    _R2 = ?GET_PROPLISTS_2(Users),
    get_value_proplists(Iterations - 1, Users).


get_proplists_sanity_check() ->
    Users = proplist_data(),

    Expected1 = 27,
    Expected2 = "Erlang",

    Expected1 = na:get(Users, [john, age]),
    Expected2 = na:get(Users, [john, languages, 1]),

    Expected1 = ?GET_PROPLISTS_1(Users),
    Expected2 = ?GET_PROPLISTS_2(Users).
