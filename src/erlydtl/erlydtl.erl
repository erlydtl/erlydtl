%%%-------------------------------------------------------------------
%%% File:      erlydtl_api.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon
%%% @doc  
%%% API for compiling ErlyDTL templeates
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
-module(erlydtl).
-author('rsaccon@gmail.com').

%% API
-export([compile/3, compile/4]).

%%--------------------------------------------------------------------
%% @spec (File:string(), ModuleName:string(), DocRoot:string()) -> 
%%     {Ok::atom, Ast::tuple() | {Error::atom(), Msg:string()}
%% @doc compiles a template to a beam file
%% @end 
%%--------------------------------------------------------------------
compile(File, ModuleName, DocRoot) ->
    compile(File, ModuleName, DocRoot, "render").
    

%%--------------------------------------------------------------------
%% @spec (File:string(), ModuleName:string(), DocRoot:string(), FunctionName:atom()) -> 
%%     {Ok::atom, Ast::tuple() | {Error::atom(), Msg:string()}
%% @doc compiles a template to a beam file
%% @end 
%%--------------------------------------------------------------------
compile(File, ModuleName, DocRoot, FunctionName) ->   
    case parse(File) of
        {ok, Ast} ->
			RelDir = rel_dir(filename:dirname(File), DocRoot),
            compile_reload_ast(Ast, ModuleName, FunctionName, RelDir);
        {error, Msg} = Err ->
            io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, File ++ " Parser failure:"]),
            io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, Msg]),
            Err
	end.


%%====================================================================
%% Internal functions
%%====================================================================

rel_dir(Dir, DocRoot) when Dir =:= DocRoot ->
    DocRoot;
rel_dir(Dir, DocRoot) ->
    RelFile = string:substr(Dir, length(DocRoot)+2),
    filename:join([DocRoot, RelFile]).


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
	        io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, "File read error"]),
	        Err   
	end.
	

compile_reload_ast([H | T], ModuleName, FunctionName, RelDir) ->
    {List, Args} = case transl(H, T, [], [], RelDir) of
	    {regular, List0, Args0} ->
		    {[inplace_block(X) ||  X <- List0], Args0};
		{inherited, List0, Arg0} ->
			{List0, Arg0}
	end,    	           
    Args2 = lists:reverse([{var, 1, Val} || {Val, _} <- Args]),  
    Cons = list_fold(lists:reverse(List)),                           
    Ast2 = {function, 1, list_to_atom(FunctionName), length(Args2),
        [{clause, 1, Args2, [], [Cons]}]},
    Ac = erlydtl_tools:create_module(Ast2 , ModuleName),    
    case compile:forms(Ac) of
        {ok, Module, Bin} ->
            case erlydtl_tools:reload(Module, Bin) of
                ok ->
                    erlydtl_tools:write_beam(Module, Bin, "ebin");
                _ -> 
                    {error, "reload failed"}
            end;            
        _ ->
           {error, "compilation failed"}
    end.


list_fold([E]) ->
    E;      
list_fold([E1, E2]) ->
    {cons, 1, E2, E1};           
list_fold([E1, E2 | Tail]) ->
    lists:foldl(fun(X, T) -> 
        {cons, 1, X, T}
    end, {cons, 1, E2, E1}, Tail).                       


transl(nil, [{extends, _, Name}], Out, Args, RelDir) -> 
    case parse(filename:join([RelDir, Name])) of
        {ok, ParentAst} ->
		    [H|T]=ParentAst,
			{_, List, Args1} = transl(H, T, [], [], RelDir),			 
			{List3, Args3} = lists:foldl(fun(X, {List2, Args2}) -> 
                {List4, Args4} = replace_block(X, Out, Args2),            
                {[List4 | List2], Args4}
            end, {[], Args1}, List),		   
		    {inherited, lists:reverse(lists:flatten([List3])), lists:flatten(Args3)};
	    {error, Msg} ->
	         io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, Msg]),
	         io:format("TRACE ~p:~p Parent Parser failure: ~p~n",[?MODULE, ?LINE, Name]),
		     {regular, Out, Args}			
    end;
	
transl(nil, [{var, L, Val}], Out, Args, _) ->
     io:format("TRACE ~p:~p nil ~p~n",[?MODULE, ?LINE, {Val, Args}]),
    case lists:keysearch(Val, 2, Args) of
        false ->
            Key = list_to_atom(lists:concat(["A", length(Args) + 1])),
            {regular, [{var, L, Key} | Out], [{Key, Val} | Args]}; 
        {value, {Key, _}} ->   
            {regular, [{var, L, Key} | Out], Args}
    end;

transl(nil, [Token], Out, Args, _) ->
    {regular, [Token | Out], Args}; 
	
transl([H | T], [{var, L, Val}], Out, Args, DocRoot) ->
    case lists:keysearch(Val, 2, Args) of
        false ->
                io:format("TRACE ~p:~p normal_not_found ~p~n",[?MODULE, ?LINE, {Val, Args}]),              
            Key = list_to_atom(lists:concat(["A", length(Args) + 1])),
            transl(H, T, [{var, L, Key} | Out], [{Key, Val} | Args], DocRoot);
        {value, {Key, _}} ->  
                   io:format("TRACE ~p:~p normal_found ~p~n",[?MODULE, ?LINE, {Val, Key, Args}]),
            transl(H, T, [{var, L, Key} | Out], Args, DocRoot)
	end;	 
	
transl([H | T], [Token], Out, Args, DocRoot) ->       
    transl(H, T, [Token | Out], Args, DocRoot).


replace_block({block, Name, [nil, Val]}, List, Args) ->
	case lists:keysearch(Name, 2, List) of
		false -> 
			{Val, Args};
		{value, {_, _, [H | T]}} ->  
		    {_, List2, Args2} = transl(H, T, [], Args, undefined),
		    {lists:reverse(List2), Args2} 
 	end;
replace_block(Other, _, Args) ->	
	{Other, Args}.
    
	
inplace_block({block, _, [nil, Str]}) ->
	Str;
inplace_block(Other) ->	
	Other.