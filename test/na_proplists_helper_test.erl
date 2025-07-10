-module(na_proplists_helper_test).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).
-compile(nowarn_export_all).

rm_first_proplist_match_test() ->
    %% rm first {b, ...} tuple entry, don't rm second/duplicate {b, ...} entry
    ?assertEqual([{a, 1}, {c, 3}, {b, 4}],
                 na_proplists_helper:rm_first_proplist_match(b, [{a, 1}, {b, 2}, {c, 3}, {b, 4}])),
    %% rm first 'b' atom() entry, don't rm second/duplicate 'b' entry
    ?assertEqual([{a, 1}, {c, 3}, b],
                 na_proplists_helper:rm_first_proplist_match(b, [{a, 1}, b, {c, 3}, b])),
    %% ignore but preserve non property() entry `42` in the proplist()
    ?assertEqual([{a, 1}, 42, {c, 3}, b],
                 na_proplists_helper:rm_first_proplist_match(b, [{a, 1}, 42, b, {c, 3}, b])),
    %% ignore but preserve non property() entry `{a, 1, 2, 3}` in the proplist()
    ?assertEqual([{a, 1, 2, 3}, {c, 3}, b],
                 na_proplists_helper:rm_first_proplist_match(b, [{a, 1, 2, 3}, b, {c, 3}, b])),
    %% no match found -> error()
    ?assertError(_, na_proplists_helper:rm_first_proplist_match(d, [{a, 1}, 42, b, {c, 3}, b])),
    %% no match found (proplist() is empty) -> error()
    ?assertError(_, na_proplists_helper:rm_first_proplist_match(d, [])).


replace_on_first_proplist_match_test() ->
    %% replace {b, 2} with {b, 42}
    ?assertEqual([{a, 1}, {b, 42}, {c, 3}],
                 na_proplists_helper:replace_on_first_proplist_match({b, 42},
                                                                     [{a, 1}, {b, 2}, {c, 3}])
                ),
    %% replace 'b' with {b, 42}
    ?assertEqual([{a, 1}, {b, 42}, {c, 3}],
                 na_proplists_helper:replace_on_first_proplist_match({b, 42},
                                                                     [{a, 1}, b, {c, 3}])
                ),
    %% replace only the first 'b' match
    ?assertEqual([{a, 1}, {b, 42}, {c, 3}, b],
                 na_proplists_helper:replace_on_first_proplist_match({b, 42},
                                                                     [{a, 1}, b, {c, 3}, b])
                ),
    %% no match found -> error()
    ?assertError(_, na_proplists_helper:replace_on_first_proplist_match({b, 42}, [{a, 1}, {c, 3}])),
    %% no match found (in empty proplist()) -> error()
    ?assertError(_, na_proplists_helper:replace_on_first_proplist_match({b, 42}, [])).


get_first_proplist_value_test() ->
    %% find first 'b' entry, it will get the default 'true' value
    ?assertEqual(true,
                 na_proplists_helper:get_first_proplist_value(b, [{a, 1}, b, {c, 3}, {b, 42}])),
    %% find first 'b' entry
    ?assertEqual(42, na_proplists_helper:get_first_proplist_value(b, [{a, 1}, {b, 42}, {c, 3}, b])),
    %% nomatch found -> error()
    ?assertError(_, na_proplists_helper:get_first_proplist_value(d, [{a, 1}, {b, 42}, {c, 3}, b])),
    %% no match found (in empty proplist()) -> error()
    ?assertError(_, na_proplists_helper:get_first_proplist_value(d, [])).


