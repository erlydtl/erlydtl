-module(erlydtl_custom_tags_lib).
-behaviour(erlydtl_library).

-export([version/0, inventory/1, customtag2_var/2]).

version() -> 1.

inventory(filters) -> [];
inventory(tags) -> [customtag2_var].

customtag2_var(_V,_R) -> [{name, <<"b1">>}, {count, 11}].
