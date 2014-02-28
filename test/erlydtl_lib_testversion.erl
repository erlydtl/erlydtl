-module(erlydtl_lib_testversion).
-behaviour(erlydtl_library).

-export([version/0, inventory/1]).

version() -> invalid.

inventory(filters) -> [];
inventory(tags) -> [].
