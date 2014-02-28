-module(erlydtl_test_extension).

-export([scan/1, parse/1, compile_ast/2]).
-include("erlydtl_ext.hrl").

%% look for a foo identifer followed by a #
scan(#scanner_state{ template="#" ++ T, 
		     scanned=[{identifier, Loc, foo}|Scanned],
		     pos={L,C} }=S) ->
    %% return new state with the hash dropped, and the foo identifer replaced with bar
    {ok, S#scanner_state{ template=T,
			  scanned=[{identifier, Loc, "rab"}|Scanned],
			  pos={L, C+1} }};
scan(#scanner_state{ template="#" ++ _T, pos=Pos }) ->
    %% give error when # not follows foo
    {error, {Pos,erlydtl_scanner,{illegal_char, $#}}};
scan(_) -> 
    %% for anything else, fallback to the error message from erlydtl_scanner..
    undefined.

parse(State) ->
    erlydtl_extension_testparser:resume(State).

%% {{ varA or varB }} is equivalent to {% if varA %}{{ varA }}{% else %}{{ varB }}{% endif %}
compile_ast({value_or, {Value1, Value2}}, TreeWalker) ->
    {{V1_Ast, V1_Info}, TW1} = erlydtl_beam_compiler:value_ast(Value1, false, false, TreeWalker),
    {{V2_Ast, V2_Info}, TW2} = erlydtl_beam_compiler:value_ast(Value2, false, false, TW1),
    {{erl_syntax:case_expr(V1_Ast,
                           [erl_syntax:clause([erl_syntax:atom(undefined)], none, [V2_Ast]),
                            erl_syntax:clause([erl_syntax:underscore()], none, [V1_Ast])
                           ]), erlydtl_compiler_utils:merge_info(V1_Info, V2_Info)}, TW2}.
