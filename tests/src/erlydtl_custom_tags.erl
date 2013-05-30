-module(erlydtl_custom_tags).

-export([custom1/1, custom2/2, custom3/2, custom4/1]).

custom1(_TagVars = []) ->
    <<"b1">>.

custom2([], _RenderOptions = [{locale, ru}, {foo, bar}]) ->
    <<"b2">>.

custom3([], _RenderOptions = [{locale, ru}]) ->
    <<"b3">>.

custom4(TagVars) ->
    Lst = [binary_to_list(X) || X <- TagVars],
    string:join(Lst, "|").
