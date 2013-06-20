-module(erlydtl_extension_test).

-export([scan/1, parse/1]).
-include("erlydtl_ext.hrl").

%% look for a foo identifer followed by a #
scan(#scanner_state{ template="#" ++ T, 
		     scanned=[{identifier, Loc, "oof"}|Scanned], 
		     pos={L,C} }=S) ->
    %% return new state with the hash dropped, and the foo identifer replaced with bar
    {ok, S#scanner_state{ template=T,
			  scanned=[{identifier, Loc, "rab"}|Scanned],
			  pos={L, C+1} }};
scan(#scanner_state{ template="#" ++ _T, pos={L, C} }) ->
    %% give error when # not follows foo
    {error, {L,?MODULE,lists:concat(["Unexpected '#' in code at column ", C])}};
scan(_) -> 
    %% for anything else, fallback to the error message from erlydtl_scanner..
    undefined.

parse(State) ->
    io:format("extension:parse got: ~p~n", [State]),
    %code:load_file(erlydtl_extension_testparser),
    Res=erlydtl_extension_testparser:resume(State),
    io:format("extension result: ~p~n", [Res]),
    Res.
