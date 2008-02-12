-module(erlydtl_runtime).

-compile(export_all).

find_value(Key, L) when is_list(L) ->
    proplists:get_value(Key, L);
find_value(Key, {GBSize, GBData}) ->
    case gb_trees:lookup(Key, {GBSize, GBData}) of
        {value, Val} ->
            Val;
        _ ->
            undefined
    end;
find_value(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Val} ->
            Val;
        _ ->
            undefined
    end.

fetch_value(Key, Data) ->
    case find_value(Key, Data) of
        undefined ->
            throw({undefined_variable, Key});
        Val ->
            Val
    end.

are_equal([Arg1], Arg2) when is_list(Arg1) ->
    are_equal(Arg1, Arg2);
are_equal(Arg1, [Arg2]) when is_list(Arg1) ->
    are_equal(Arg1, Arg2);
are_equal(Arg1, Arg2) when is_binary(Arg1) ->
    are_equal(binary_to_list(Arg1), Arg2);
are_equal(Arg1, Arg2) when is_binary(Arg2) ->
    are_equal(Arg1, binary_to_list(Arg2));
are_equal(Arg1, Arg2) ->
    Arg1 =:= Arg2.

is_false("") ->
    true;
is_false(false) ->
    true;
is_false(undefined) ->
    true;
is_false("0") ->
    true;
is_false(<<"0">>) ->
    true;
is_false(<<>>) ->
    true;
is_false(_) ->
    false.
