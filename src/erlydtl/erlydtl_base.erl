%%%-------------------------------------------------------------------
%%% File:      erlydtl_base.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon
%%% @doc  
%%% ErlyDTL AST tools (tree builder and parse transformations)
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-11-17 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlydtl_base).
-author('rsaccon@gmail.com').

-record(dtl, {
    buffer = [], 
    args = [], 
    doc_root = [], 
    ext = [], 
    ns = [],
    var = [], 
    props = []}).
	
%% API
-export([parse/1, 
    build_tree/3, 
    build_tree/4, 
    build_tree/5, 
    parse_transform/6, 
    parse_transform/3, 
    parse_transform/2, 
    parse_transform/1, 
    rel_dir/2, 
    new_var/2]).


build_tree(H, T, Ext) ->
    build_tree2(H, T, #dtl{ext = Ext}).
    
build_tree(H, T, DocRoot, Ext) ->
    build_tree2(H, T, #dtl{doc_root = DocRot, ext = Ext}).
    
build_tree(H, T, Args, Ext, Var) ->
    build_tree2(H, T, #dtl{args = Args, ext = Ext, var = Var}).
    

%%====================================================================
%% API
%%====================================================================
parse(File) ->
    case file:read_file(File) of
        {ok, B} ->
            case erlydtl_scanner:scan(binary_to_list(B)) of
                {ok, Tokens} ->
                    erlydtl_parser:parse(Tokens);
                Err ->
                    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, File ++ " Scanner failure:"]),
                    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, Err]),
                    Err
            end;
        Err ->
            io:format("TRACE ~p:~p ~p: ~p~n",[?MODULE, ?LINE, "File read error with:", File]),
            Err   
    end.
    

parse_transform({var, Line, Val}, Var, Val) when is_atom(Var) ->
    io:format("TRACE ~p:~p var_parse_transform: ~p~n",[?MODULE, ?LINE, Val]),
    {var, Line, Var}.
    

parse_transform({block, _, Name, [nil, Block]}, #dtl{buffer = Buffer} = Dtl) ->
	case lists:keysearch(Name, 3, Buffer) of
		false -> 
            parse_transform(Block, Dtl);
		{value, {_, _, _, [H | T]}} -> 
		    {_, Buffer1, Args1, Props1} = build_tree(H, T, Dtl),
            parse_transform(lists:reverse(Buffer1), Dtl#dtl{args = Args1, props = Props1})
 	end;
parse_transform(Other, #dtl{args = Args}) ->    
    {Other, Args};                
parse_transform({var, _Line, Var}, Args) ->
    Var2 = list_to_atom(tl(atom_to_list(Var))),
    binary_string(proplists:get_value(Var2, Args));      
parse_transform(Other, _) ->    
    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, other]),
    Other.
        

parse_transform({block, _Line , _Name, [nil, T]}) ->
	parse_transform(T); 
parse_transform({var, L, Val}) ->
    io:format("TRACE ~p:~p var_parse_transform: ~p~n",[?MODULE, ?LINE, Val]),    
    erl_syntax:variable(Val);
parse_transform(Other) -> 
    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, other]),   
    Other.   	


rel_dir(Dir, DocRoot) when Dir =:= DocRoot ->
    DocRoot;
rel_dir(Dir, DocRoot) ->
    RelFile = string:substr(Dir, length(DocRoot)+2),
    filename:join([DocRoot, RelFile]).


new_var(List, Acc) ->
    Var = list_to_atom(lists:concat(["A", Acc])),
    case lists:member(Var, List) of
        false ->
            Var;
        _ ->
            new_var(List, Acc + 1)
    end.
        

%%====================================================================
%% Internal functions
%%====================================================================
build_tree(nil, [{extends, Line, Name}], #dtl{buffer = Buffer, doc_root = DocRoot, ext = Ext} = Dtl) ->
    case parse(filename:join([RelDir, Name])) of
        {ok, ParentAst} ->
		    [H|T] = ParentAst,
			{_, Buffer1, Args1, _} = build_tree(H, T, DocRoot, Ext),			 
			{Buffer2, Args3} = lists:foldl(fun(X, {AccBuffer, AccArgs}) ->  
                    {Buffer3, Args4} = parse_transform(X, Dtl#dtl{args = AccArgs, ns = [], var = []}),           
                    {[Buffer3 | AccBuffer], Args4}
                end, 
                {[], Args1}, 
                Buffer1),		   
		    {inherited, lists:reverse(lists:flatten([Buffer2])), lists:flatten(Args3), []};
	    {error, _} ->
		     {error, {extends, Line, "file not found"}}		
    end;
    
build_tree(nil, [{var, Line, Var}], #dtl{buffer = Buffer, args = Args, props = Props}) ->      
    {regular, [erl_syntax:variable(Var) | Buffer], Args, Prop};

build_tree(nil, [{var, Line, Ns, Var}], #dtl{buffer = Buffer, args = Args, var = Ns, props = Props}) ->     
    Var1 = lists:concat([Ns, ".", Var]),
    Props1 = [list_to_atom(Var1) | Props],
    {regular, [erl_syntax:variable(Var1) | Buffer], Args, Props1};
 
build_tree(nil, [{var, _, Var}], #dtl{buffer = Buffer, args = Args, props = Props}) ->       	
    case lists:member(Var, Args) of
        true ->
            {regular, [erl_syntax:variable(Var) | Buffer], Args, Rec};
        _ ->
            {regular, [erl_syntax:variable(Var) | Buffer], [Var | Args], Rec} 
    end;    
 
build_tree(nil, [{tag, _, TagName, TagArgs}], #dtl{buffer = Buffer, args = Args, ext = Ext, props = Props}) ->
    Buffer1 = handle_tag(TagName, TagArgs, Buffer, default, Ext),    
    {regular, lists:flatten([Buffer1, Buffer]), Args, Props};
    
build_tree2(nil, [{for, _, It, Var, [HFor | TFor]}], #dtl{buffer = Buffer, props = Props} = Dtl) ->
    {Buffer1, Args1} = handle_for(It, Var, HFor, TFor, Dtl), 
    {regular, lists:flatten([Buffer1, Buffer]), Args1, Props};     

build_tree2(nil, [Token], #dtl{buffer = Buffer, args = Args, props = Props}) ->
    io:format("TRACE ~p:~p other1-Token: ~p~n",[?MODULE, ?LINE, Token]),
    {regular, [Token | Buffer], Args, Props}; 

build_tree2([H | T], [{var, _, Var}], #dtl{buffer = Buffer} = Dtl) ->
    build_tree2(H, T, Dtl#dtl{buffer = [erl_syntax:variable(Var) | Buffer]);
 
build_tree2([H | T], [{var, _, Ns, Var}], #dtl{buffer = Buffer, ns = Ns, props = Props} = Dtl) ->
    Var1 = lists:concat([Ns, ".", Var]),
    Dtl1 = Dtl#dtl{
        buffer = [erl_syntax:variable(Var1) | Buffer], 
        props = [list_to_atom(Var1) | Props]},
    build_tree2(H, T, Dtl);
  
build_tree2([H | T], [{var, _, Var}], #dtl{buffer = Buffer, args = Args} = Dtl) ->           		
    Dtl1 = case lists:member(Var, Args) of
        true ->
            Dtl#dtl{buffer = [erl_syntax:variable(Var) | Buffer];
        _ ->
            Dtl#dtl{
                buffer = [erl_syntax:variable(Var) | Buffer],
                args = [Var | Args]}
    end,
    build_tree2(H, T, Dtl1);
    
build_tree2([H | T], [{tag, _Line, TagName, TagArgs}], #dtl{buffer = Buffer, ext = Ext} = Dtl) ->	
    Buffer2 = handle_tag(TagName, TagArgs, Buffer, default, Ext),
    build_tree2(H, T, Dtl#dtl{buffer = Buffer2});
 
build_tree2([H | T], [{for, _, It, Var, [HFor | TFor]}], Dtl) ->
    {Buffer1, Args1} = handle_for(It, Var, HFor, TFor, Dtl),
    build_tree2(H, T, Dtl#dtl{buffer = lists:flatten([Buffer1, Buffer]), args = Args1});
        	
build_tree2([H | T], [Token], #dtl{buffer = Buffer} = Dtl) ->
    io:format("TRACE ~p:~p other2-Token: ~p~n",[?MODULE, ?LINE, Token]),
    build_tree2(H, T, Dtl#dtl{buffer = [Token | Buffer]}).
    

handle_for(It, Var, HFor, TFor, #dtl{buffer = Buffer, args = Args, ext = Ext, props = Props}) ->
    {_, List1, Args1, Props1} = build_tree(HFor, TFor, Args, Ext, It),  
    Args2 = case lists:member(Var, Args1) of
        true ->
            Args1;
        _ ->
            [Var | Args1]
	end,  
	ItAST = erl_syntax:variable(It),
    Buffer1 = case Props1 of
        [] ->
            BodyAST = erl_syntax:generator(ItAST, erl_syntax:variable(Var)),  
            erl_syntax:list_comp(erl_syntax:list(List1), [BodyAST]);
        _ ->                                 
            FunBodyAST = lists:foldl(fun(X, Acc) -> 
                    [_,Prop] = string:tokens(tl(atom_to_list(X)), "."),
                    A = erl_syntax:variable(X),
                    B = erl_syntax:application(erl_syntax:atom(proplists), 
                        erl_syntax:atom(get_value), [erl_syntax:atom(Prop), ItAST]),
                    [erl_syntax:match_expr(A, B) | Acc]
                 end,
                 [erl_syntax:list(List1)],
                 Props1),
            FunClauseAST = erl_syntax:clause([ItAST], none, FunBodyAST),
            erl_syntax:application(erl_syntax:atom(lists), 
                erl_syntax:atom(map),
                [erl_syntax:fun_expr([FunClauseAST]), erl_syntax:variable(Var)])
    end,   
    {Buffer1, Args1}.
    
           	        	
handle_tag(TagName, TagArgs, Acc0, default, Ext) ->
    case parse(filename:join([erlydtl_deps:get_base_dir(), "priv", "tags", atom_to_list(TagName) ++ Ext])) of
        {ok, ParentAst} ->
		    [H|T]=ParentAst,
			{_, List, _, _} = build_tree(H, T, Ext),
			lists:foldl(fun(X, Acc) -> 
			        [parse_transform(X, TagArgs) | Acc]			        
			    end, 
			    Acc0,
			    lists:reverse(List));
		{error, Msg} ->
    	    Acc0
    end.
  
    
binary_string(String) ->
    erl_syntax:binary([erl_syntax:binary_field(erl_syntax:integer(X)) || X <- String]).