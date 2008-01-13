%%%-------------------------------------------------------------------
%%% File:      erlydtl_server.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc  
%%% Server for compiling ErlyDTL templeates
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
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
%%% @since 2007-11-17 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(erlydtl_server).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

-behaviour(gen_server).
	
%% API
-export([start_link/0, compile/1, compile/3, compile/4, compile/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    reload = true}).


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
%% @spec (File::string()) -> 
%%     {Ok::atom, Ast::tuple() | {Error::atom(), Msg::string()}
%% @doc compiles a template to a beam file
%% @end 
%%--------------------------------------------------------------------
compile(File) ->
    compile(File, todo, todo).
        
%%--------------------------------------------------------------------
%% @spec (File::string(), DocRoot::string(), Mod::string()) -> 
%%     {Ok::atom, Ast::tuple() | {Error::atom(), Msg:string()}
%% @doc compiles a template to a beam file
%% @end 
%%--------------------------------------------------------------------
compile(File, DocRoot, Mod) ->
    compile(File, DocRoot, Mod, "render").
    

%%--------------------------------------------------------------------
%% @spec (File::string(), DocRoot::string(), Mod::string(), VarsCallback::tuple()) -> 
%%     {Ok::atom, Ast::tuple() | {Error::atom(), Msg:string()}
%% @doc compiles a template to a beam file
%% @end 
%%--------------------------------------------------------------------
compile(File, DocRoot, Mod, {_, _} = VarsCallback) ->
    compile(File, DocRoot, Mod, "render", VarsCallback);
compile(File, DocRoot, Mod, Func) ->   
    gen_server:call(?MODULE, {compile, File, DocRoot, Mod, Func, []}).
        
%%--------------------------------------------------------------------
%% @spec (File::string(), DocRoot::string(), Mod::string(), Func::atom(),
%%         {VarsCbMod::atom(), VarsCbFunc::atom()}) -> 
%%     {Ok::atom, Ast::tuple() | {Error::atom(), Msg:string()}
%% @doc compiles a template to a beam file
%% @end 
%%--------------------------------------------------------------------            
compile(File, DocRoot, Mod, Func, {VarsCbMod, VarsCbFunc}) ->   
    case catch VarsCbMod:VarsCbFunc(list_to_atom(Mod)) of
        Vars when is_list(Vars) ->
            gen_server:call(?MODULE, {compile, File, DocRoot, Mod, Func, Vars});
        _ -> 
            gen_server:call(?MODULE, {compile, File, DocRoot, Mod, Func, []})
    end.
    
        
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
handle_call({compile, _File, _DocRoot, _Mod, _Func, _Vars}, _From, State) ->
    Reply = not_implemented,
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