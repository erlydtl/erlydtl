%%%-------------------------------------------------------------------
%%% File:      erlydtl_server.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon
%%% @doc  
%%% Server for compiling ErlyDTL templeates
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
-module(erlydtl_server).
-author('rsaccon@gmail.com').

-behaviour(gen_server).
	
%% API
-export([start_link/0, compile/1, compile/3, compile/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    

%%--------------------------------------------------------------------
%% @spec (File:string()) -> 
%%     {Ok::atom, Ast::tuple() | {Error::atom(), Msg:string()}
%% @doc compiles a template to a beam file
%% @end 
%%--------------------------------------------------------------------
compile(File) ->
    compile(File, todo, todo).
        
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
    gen_server:call(?MODULE, {compile, File, ModuleName, DocRoot, FunctionName}).
        

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call({compile, File, ModuleName, DocRoot, FunctionName}, _From, State) ->
    Reply = case parse(File) of
        {ok, Ast} ->
		    RelDir = rel_dir(filename:dirname(File), DocRoot),
		    Ext = filename:extension(File),
            compile(Ast, ModuleName, FunctionName, RelDir, Ext);
        {error, Msg} = Err ->
            io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, File ++ " Parser failure:"]),
            io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, Msg]),
            Err
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.	


%%====================================================================
%% Internal functions
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
	

compile([H | T], ModuleName, FunctionName, RelDir, Ext) ->
    {List, Args} = case build_tree(H, T, [], [], RelDir, Ext, []) of
	    {regular, List0, Args0} ->
		    {[parse_transform(X) ||  X <- List0], Args0};
		{inherited, List0, Arg0} ->
			{List0, Arg0}
	end,    
	{Args1, Body} = case Args of 
	    []  ->
	        {[], [erl_syntax:list(List)]};
	    _ ->
	        Var = erl_syntax:variable(new_var(Args, 0)),
	        Body0 = lists:foldl(fun(X, Acc) -> 
	                X2 = list_to_atom(tl(atom_to_list(X))),
        	        A = erl_syntax:variable(X),
        	        B = erl_syntax:application(erl_syntax:atom(proplists), 
    	                erl_syntax:atom(get_value), [erl_syntax:atom(X2), Var]),
        	        [erl_syntax:match_expr(A, B) | Acc]
        	    end,
        	    [erl_syntax:list(List)],
        	    Args),
        	{[Var], Body0}
	end, 
	Clause = erl_syntax:clause(Args1, none, Body),
	Func = erl_syntax:function(erl_syntax:atom(FunctionName), [Clause]),
	[Mod, Cmp] = [erl_syntax:attribute(erl_syntax:atom(X), [erl_syntax:atom(Y)]) ||
	    {X, Y} <- [{"module", ModuleName}, {"compile", "export_all"}]],
    Forms = [erl_syntax:revert(X) || X <- [Mod, Cmp, Func]],
io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, Forms]),
    case compile:forms(Forms) of
        {ok, Module, Bin} ->
            erlydtl_tools:write_beam(Module, Bin, "ebin"),
            erlydtl_tools:reload(Module, Bin);
        _ ->
            {error, "compilation failed"}
    end.    
    

build_tree(nil, [{extends, _Line, Name}], Out, Args, RelDir, Ext, _) -> 
    case parse(filename:join([RelDir, Name])) of
        {ok, ParentAst} ->
		    [H|T]=ParentAst,
			{_, List, Args1} = build_tree(H, T, [], [], RelDir, Ext, []),			 
			{List3, Args3} = lists:foldl(fun(X, {List2, Args2}) -> 
                {List4, Args4} = parse_transform(X, Out, Args2, Ext, []),            
                {[List4 | List2], Args4}
            end, {[], Args1}, List),		   
		    {inherited, lists:reverse(lists:flatten([List3])), lists:flatten(Args3)};
	    {error, Msg} ->
	         io:format("TRACE ~p:~p Parent Parser failure: ~p~n",[?MODULE, ?LINE, Name]),
		     {regular, Out, Args}			
    end;
    
build_tree(nil, [{var, Line, Val}], Out, Args, DocRoot, Ext, Val) ->
    {regular, [erl_syntax:variable(Val) | Out], Args};
	
build_tree(nil, [{var, Line, Var}], Out, Args, _, _, IgnoreVar)  ->
    case lists:member(Var, Args) of
        true ->
            {regular, [{var, Line, Var} | Out], Args};
        _ ->
            {regular, [{var, Line, Var} | Out], [Var | Args]} 
    end;    
    
build_tree(nil, [{tag, _Line, [TagName | TagArgs]}], Out, Args, _, Ext, IgnoreVar) ->
    Out2 = load_tag(TagName, TagArgs, Out, default, Ext, IgnoreVar),    
    {regular, Out2, Args};
    
build_tree(nil, [{for, _Line, Iterator, Var, [HFor | TFor]}], Out, Args, _, Ext, _) -> 
    {_, List1, Args1} = build_tree(HFor, TFor, [], Args, undefined, Ext, Iterator),  
    Args2 = case lists:member(Var, Args1) of
        true ->
            Args1;
        _ ->
            [Var | Args1]
	end,     
    Body = erl_syntax:generator(erl_syntax:variable(Iterator), erl_syntax:variable(Var)),  
    Out1 = erl_syntax:list_comp(erl_syntax:list(List1), [Body]),
    {regular, Out1, Args2};   
 
build_tree(nil, [{string, Val}], Out, Args, _, _, _) ->
     {regular, [binary_string(Val) | Out], Args}; 
    
build_tree(nil, [Token], Out, Args, _, _, _) ->
    {regular, [Token | Out], Args}; 

build_tree([H | T], [{var, Line, Var}], Out, Args, DocRoot, Ext, Var) ->
    build_tree(H, T, [erl_syntax:variable(Var) | Out], Args, DocRoot, Ext, Var) ;
    		
build_tree([H | T], [{var, Line, Var}], Out, Args, DocRoot, Ext, IgnoreVar) ->
    case lists:member(Var, Args) of
        true ->
            build_tree(H, T, [{var, Line, Var} | Out], Args, DocRoot, Ext, IgnoreVar);
        _ ->
            build_tree(H, T, [{var, Line, Var} | Out], [Var | Args], DocRoot, Ext, IgnoreVar)
    end;    
	
build_tree([H | T], [{tag, _Line, [TagName | TagArgs]}], Out, Args, DocRoot, Ext, IgnoreVar) ->
    Out2 = load_tag(TagName, TagArgs, Out, default, Ext, IgnoreVar),
    build_tree(H, T, Out2, Args, DocRoot, Ext, IgnoreVar);
 
build_tree([H | T], [{for, _Line, Iterator, Var, [HFor | TFor]}], Out, Args, DocRoot, Ext, IgnoreVar) -> 	
    {_, List1, Args1} = build_tree(HFor, TFor, [], Args, undefined, Ext, Iterator),  
    Args2 = case lists:member(Var, Args1) of
        true ->
            Args1;
        _ ->
            [Var | Args1]
	end,
    Body = erl_syntax:generator(erl_syntax:variable(Iterator), erl_syntax:variable(Var)),  
    Out1 = erl_syntax:list_comp(erl_syntax:list(List1), [Body]),
    build_tree(H, T, lists:flatten([Out1, Out]), Args2, DocRoot, Ext, IgnoreVar);
    	
build_tree([H | T], [{string, Val}], Out, Args, DocRoot, Ext, IgnoreVar) ->      
    build_tree(H, T, [binary_string(Val) | Out], Args, DocRoot, Ext, IgnoreVar);
        	
build_tree([H | T], [Token], Out, Args, DocRoot, Ext, IgnoreVar) ->      
    build_tree(H, T, [Token | Out], Args, DocRoot, Ext, IgnoreVar).


parse_transform({block, _Line, Name, [nil, Val]}, List, Args, Ext, IgnoreVar) ->
	case lists:keysearch(Name, 3, List) of
		false -> 
            parse_transform(Val, List, Args, Ext, IgnoreVar);
		{value, {_, _, _, [H | T]}} ->  
		    {_, List2, Args2} = build_tree(H, T, [], Args, undefined, Ext, IgnoreVar),
            parse_transform(lists:reverse(List2), List, Args2, Ext, IgnoreVar)
 	end;
parse_transform({string, Val}, _, Args, _, _) ->    
    {binary_string(Val), Args};
parse_transform(Other, _What, Args, _, _) ->    
    {Other, Args}.

    
parse_transform({var, Line, Val}, Var, Val) when is_atom(Var) ->
    {var, Line, Var}.
    
            
parse_transform({var, Line, Val}, Args) ->
    {value, {_, Value}} = lists:keysearch(Val, 1, Args),
    binary_string(Value);      
parse_transform(Other, _) ->    
    Other.
        

parse_transform({block, _Line , _Name, [nil, T]}) ->
	parse_transform(T);
parse_transform({string, Val}) ->
    binary_string(Val); 
parse_transform({var, L, Val}) ->
    erl_syntax:variable(Val);
parse_transform(Other) ->    
    Other.   	

   	        	
load_tag(TagName, TagArgs, Acc0, default, Ext, IgnoreVar) ->
    case parse(filename:join([erlydtl_deps:get_base_dir(), "priv", "tags", atom_to_list(TagName) ++ Ext])) of
        {ok, ParentAst} ->
		    [H|T]=ParentAst,
			{_, List, Args1} = build_tree(H, T, [], [], undefined, Ext, IgnoreVar),
			Args2 = [{Var, Val} || {{Var, _}, Val} <- lists:zip(Args1, TagArgs)], 			
			lists:foldl(fun(X, Acc) -> 
			        [parse_transform(X, Args2) | Acc]			        
			    end, 
			    Acc0,
			    lists:reverse(List));
		{error, Msg} ->
    	    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, Msg]),
    	    Acc0
    end.
  
    
binary_string(String) ->
    erl_syntax:string(String).
%    erl_syntax:binary([erl_syntax:binary_field(erl_syntax:integer(X)) || X <- String]).


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