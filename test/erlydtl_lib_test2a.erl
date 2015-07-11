-module(erlydtl_lib_test2a).
%% test multiple behaviors (alternative spelling)
-behavior(erlydtl_lib_test1).
-behavior(erlydtl_library).

-export([version/0, inventory/1, reverse/1]).

version() -> 1.

inventory(filters) -> [reverse];
inventory(tags) -> [].

reverse(String) when is_list(String) ->
    lists:reverse(String);
reverse(String) when is_binary(String) ->
    reverse(binary_to_list(String)).
