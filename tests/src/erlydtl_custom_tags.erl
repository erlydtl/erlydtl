-module(erlydtl_custom_tags).

-export([custom1/1, custom2/2, custom3/2]).

custom1(_TagVars = []) ->
    <<"b1">>.

custom2([], _CustomTagsContext = ctx) ->
    <<"b2">>.

custom3([], _RenderOptions = [{locale, ru}]) ->
    <<"b3">>.

