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
    var = [], 
    props = [],
    preset = []}).
	
%% API
-export([parse/1, 
    build_tree/4, 
    build_tree/5, 
    build_tree/6,  
    parse_transform/1, 
    parse_transform/2, 
    rel_dir/2, 
    new_var/2,
    binary_string/1]).


build_tree(H, T, Ext, Preset) ->
    Dtl = #dtl{ext = Ext, preset = Preset},
    build_tree2(H, T, Dtl).
    
build_tree(H, T, DocRoot, Ext, Preset) ->
    Dtl = #dtl{doc_root = DocRoot, ext = Ext, preset = Preset},
    build_tree2(H, T, Dtl).
    
build_tree(H, T, Args, Ext, Var, Preset) ->
    Dtl = #dtl{args = Args, ext = Ext, var = Var, preset = Preset},
    build_tree2(H, T, Dtl).
    

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
                    Err
            end;
        _ ->
            {error, "reading " ++ File ++ " failed"} 
    end.

                                              
parse_transform({block, _Line , _Name, [nil, T]}) ->
	parse_transform(T); 
parse_transform(Other) ->   
    Other.   	

parse_transform({tree, variable, _, Var}, Args) ->
    Key = list_to_atom(tl(atom_to_list(Var))),
    binary_string(proplists:get_value(Key, Args));      
parse_transform(Other, _) ->    
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


binary_string(String) ->
    erl_syntax:binary([erl_syntax:binary_field(erl_syntax:integer(X)) || X <- String]).
    % erl_syntax:string(String).  %% less verbose for debugging    


%%====================================================================
%% Internal functions
%%====================================================================
build_tree2(nil, [{extends, Line, Name}], Dtl) ->
    #dtl{buffer = Buffer, doc_root = DocRoot, ext = Ext, preset = Preset} = Dtl,
    case parse(filename:join([DocRoot, Name])) of
        {ok, [AstH | AstT]} ->
			{_, BaseBuffer, BaseArgs, _} = build_tree(AstH, AstT, DocRoot, Ext, Preset),			 
			Out = lists:foldl(fun(X, {AccBuffer, AccArgs, Status}) ->   
                    case X of
                        {block, Line1, BlockName, _} ->
                            case lists:keysearch(BlockName, 3, Buffer) of
                        		{value, {block, _, BlockName, [H | T]}} ->
                        		    {_, Buffer2, Args2, _} = build_tree(H, T, Ext, Preset),
                        		    Buffer3 = [lists:reverse(Buffer2), AccBuffer],
                        		    Args3 = [Args2, AccArgs],
                        		    {lists:flatten(Buffer3), lists:flatten(Args3), Status};
                        		_ ->
                            	    {AccBuffer, AccArgs, {block, Line1, Name}}                        		     
                            end;
                        _ ->
                            {[X | AccBuffer], AccArgs, Status}
                    end
                end, 
                {[], BaseArgs, ok}, 
                BaseBuffer),   
            case Out of
                {Buffer1, Args1, ok} ->       		   
		            Buffer2 = lists:reverse(lists:flatten([Buffer1])),
		            {inherited, Buffer2, lists:flatten(Args1), []};
		        {_, _, Err} ->
		            {error, Err}
		    end;
	    {error, _} ->
		     {error, {extends, Line, "file not found"}}		
    end;
    
build_tree2(nil, [{var, _, Var}], #dtl{var = Var} = Dtl) ->    
    #dtl{buffer = Buffer, args = Args, props = Props} = Dtl,  
    {regular, [erl_syntax:variable(Var) | Buffer], Args, Props};

build_tree2(nil, [{var, _, Ns, Var}], #dtl{var = Ns} = Dtl) ->
    #dtl{buffer = Buffer, args = Args, props = Props} = Dtl,     
    Var1 = lists:concat([Ns, ".", Var]),
    Props1 = [list_to_atom(Var1) | Props],
    {regular, [erl_syntax:variable(Var1) | Buffer], Args, Props1};
  
    
build_tree2(nil, [{var, _, Var}], Dtl) ->
    #dtl{buffer = Buffer, args = Args, props = Props, preset = Preset} = Dtl,       	
    case lists:member(Var, Args) of
        true ->
            {regular, [erl_syntax:variable(Var) | Buffer], Args, Props};
        _ ->
            Key = list_to_atom(tl(atom_to_list(Var))),
            case proplists:get_value(Key, Preset) of
                undefined ->
                    {regular, [erl_syntax:variable(Var) | Buffer], [Var | Args], Props};
                Val ->
                    {regular, [binary_string(Val) | Buffer], Args, Props}
            end
    end;     
 
build_tree2(nil, [{tag, Line, TagName, TagArgs}], Dtl) ->
    #dtl{buffer = Buffer, args = Args, ext = Ext, props = Props, preset = Preset} = Dtl,
    case handle_tag(TagName, Line, TagArgs, Buffer, Ext, Preset) of
        {ok, Buffer1} ->
            {regular, lists:flatten([Buffer1, Buffer]), Args, Props};
        Err ->
            Err
    end;
    
build_tree2(nil, [{for, _, It, Var, [HFor | TFor]}], Dtl) ->
    #dtl{buffer = Buffer, props = Props} = Dtl,
    {Buffer1, Args1} = handle_for(It, Var, HFor, TFor, Dtl), 
    {regular, lists:flatten([Buffer1, Buffer]), Args1, Props};     

build_tree2(nil, [Token], #dtl{buffer = Buffer, args = Args, props = Props}) ->
    {regular, [Token | Buffer], Args, Props}; 
  
build_tree2([H | T], [{var, _, Var}], #dtl{var = Var} = Dtl) ->
    #dtl{buffer = Buffer} = Dtl,
    build_tree2(H, T, Dtl#dtl{buffer = [erl_syntax:variable(Var) | Buffer]});
 
build_tree2([H | T], [{var, _, Ns, Var}], #dtl{var = Ns} = Dtl) ->
    #dtl{buffer = Buffer, props = Props} = Dtl,
    Var1 = lists:concat([Ns, ".", Var]),
    Dtl1 = Dtl#dtl{
        buffer = [erl_syntax:variable(Var1) | Buffer], 
        props = [list_to_atom(Var1) | Props]},
    build_tree2(H, T, Dtl1);


build_tree2([H | T], [{var, _, Var}], Dtl) ->
    #dtl{buffer = Buffer, args = Args, preset = Preset} = Dtl,           		
    Dtl1 = case lists:member(Var, Args) of
        true ->
            Dtl#dtl{buffer = [erl_syntax:variable(Var) | Buffer]};
        _ ->
            Key = list_to_atom(tl(atom_to_list(Var))),
            case proplists:get_value(Key, Preset) of
                undefined ->
                    Dtl#dtl{
                        buffer = [erl_syntax:variable(Var) | Buffer],
                        args = [Var | Args]};
                Val ->
                    Dtl#dtl{buffer = [binary_string(Val) | Buffer]}
            end
    end,
    build_tree2(H, T, Dtl1);
        
    
build_tree2([H | T], [{tag, Line, TagName, TagArgs}], Dtl) ->
    #dtl{buffer = Buffer, ext = Ext, preset = Preset} = Dtl,
    case handle_tag(TagName, Line, TagArgs, Buffer, Ext, Preset) of
        {ok, Buffer1} ->          
            build_tree2(H, T, Dtl#dtl{buffer = Buffer1});
        Err ->
            Err
    end;
 
build_tree2([H | T], [{for, _, It, Var, [HFor | TFor]}], #dtl{buffer = Buffer} = Dtl) ->
    {Buffer1, Args1} = handle_for(It, Var, HFor, TFor, Dtl),
    build_tree2(H, T, Dtl#dtl{buffer = lists:flatten([Buffer1, Buffer]), args = Args1});
        	
build_tree2([H | T], [Token], #dtl{buffer = Buffer} = Dtl) ->
    build_tree2(H, T, Dtl#dtl{buffer = [Token | Buffer]}).
    

handle_for(It, Var, HFor, TFor, Dtl) ->
    #dtl{args = Args, ext = Ext, preset = Preset} = Dtl,
    {_, List1, Args1, Props1} = build_tree(HFor, TFor, Args, Ext, It, Preset),    
	ItAST = erl_syntax:variable(It),
	Key = list_to_atom(tl(atom_to_list(Var))),
    case Props1 of
        [] ->
            case proplists:get_value(Key, Preset) of
                undefined ->
                    BodyAST = erl_syntax:generator(ItAST, erl_syntax:variable(Var)),  
                    List2 = erl_syntax:list_comp(erl_syntax:list(List1), [BodyAST]),
                    case lists:member(Var, Args1) of
                        true ->
                            {List2, Args1};
                        _ ->
                            {List2, [Var | Args1]}
                	end;                   
                Vals ->
                    List2 = lists:map(fun (X) -> 
                            lists:map(fun ({tree, variable, _, It1}) when It1 =:= It ->
                                    binary_string(X);
                                (Other) ->
                                    Other
                                end, List1)
                        end, Vals),
                    {lists:flatten(List2), Args1}
            end;
        _ ->
            case proplists:get_value(Key, Preset) of
                undefined ->
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
                    List2 = erl_syntax:application(erl_syntax:atom(lists), 
                        erl_syntax:atom(map),
                        [erl_syntax:fun_expr([FunClauseAST]), erl_syntax:variable(Var)]),
                    case lists:member(Var, Args1) of
                        true ->
                            {List2, Args1};
                        _ ->
                            {List2, [Var | Args1]}
                    end;
                Vals ->                    
                    Ns = tl(atom_to_list(It)),
                    List2 = lists:map(fun (X) -> 
                            lists:map(fun ({tree, variable, _, It1} = Node) ->
                                    case string:tokens(tl(atom_to_list(It1)), ".") of
                                        [Ns, Key1 | _] ->
                                            binary_string(proplists:get_value(list_to_atom(Key1), X));
                                        _ ->
                                            Node
                                    end;
                                (Other) ->
                                    Other
                                end, List1)
                        end, Vals),
                    {lists:flatten(List2), Args1}
            end
    end.
     
           	        	
handle_tag(TagName, Line, TagArgs, Acc0, Ext, Preset) ->
    case parse(filename:join([erlydtl_deps:get_base_dir(), "priv", "tags", atom_to_list(TagName) ++ Ext])) of
        {ok, ParentAst} ->
		    [H|T]=ParentAst,
			{_, List, _, _} = build_tree(H, T, Ext, Preset),
			List1 = lists:foldl(fun(X, Acc) -> 
			        [parse_transform(X, TagArgs) | Acc]			        
			    end, 
			    Acc0,
			    lists:reverse(List)),
			{ok, List1};
		_ ->
    	    {error, {TagName, Line, "loading tag source template failed"}}
    end.