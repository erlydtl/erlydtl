%%%-------------------------------------------------------------------
%%% File:      erlydtl_demo.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon
%%% @doc  
%%%
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
-module(erlydtl_demo).
-author('rsaccon@gmail.com').

%% API
-export([compile/0, compile/1, compile/2, render/0, render/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc  compiles the templates to beam files
%% @end 
%%--------------------------------------------------------------------
compile() ->
    DocRoot = filename:join([filename:dirname(code:which(?MODULE)),"..", "demo", "templates"]),
    filelib:fold_files(DocRoot,
        "\.html$|\.css$",
        true,
        fun(Path, _Acc) ->
            Module = filename:rootname(filename:basename(Path)),
            erlydtl_server:compile(Path, DocRoot, Module)
        end,
        []).


%%--------------------------------------------------------------------
%% @spec (string()) -> any()
%% @doc 
%% compiles the template to beam files
%% @end 
%%--------------------------------------------------------------------        
compile("var" = Name) ->
    compile(Name, ".html");

compile("extends" = Name) ->
    compile(Name, ".html"); 
          
compile("comment" = Name) ->
    compile(Name, ".html");
               
compile("for" = Name) ->
    compile(Name, ".html");
 
compile("for_records" = Name) ->
    compile(Name, ".html");
                                
compile("htmltags" = Name) ->
    compile(Name, ".html");
                    
compile("csstags" = Name) ->
     compile(Name, ".css");
      
compile(Name) ->
    io:format("No such template: ~p~n",[Name]).
               
               
%%--------------------------------------------------------------------
%% @spec (string(), string()) -> any()
%% @doc 
%% compiles the template to beam files
%% @end 
%%--------------------------------------------------------------------       
compile(Name, Ext) ->
    DocRoot = filename:join([filename:dirname(code:which(?MODULE)),"..", "demo", "templates"]),
    Module = "test_" ++ Name,
    Path = filename:join([DocRoot, Module ++ Ext]),
    erlydtl_server:compile(Path, DocRoot, Module).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc renders template to a file
%% @end 
%%--------------------------------------------------------------------
render() ->
    render("var"),
    render("extends"),
    render("comment"),
    render("for"),
    render("for_records"),
    render("htmltags"),
    render("csstags").
        

%%--------------------------------------------------------------------
%% @spec (string()) -> ok()
%% @doc renders template to a file
%% @end 
%%--------------------------------------------------------------------
render("var" = Name) ->
    render(Name, ".html", [{var1, "foostring1"}, {var2, "foostring2"}]);
 
render("extends" = Name) ->
    render(Name, ".html", [{base_var, "base-barstring"}, {test_var, "test-barstring"}]);
        
render("comment" = Name) ->
    render(Name, ".html");

render("for" = Name) ->
    render(Name, ".html", [{fruit_list, ["apple", "banana", "coconut"]}]);
            
render("for_records" = Name) ->
    Link1 = [{name, "Amazon"}, {url, "http://amazon.com"}],
    Link2 = [{name, "Google"}, {url, "http://google.com"}],
    Link3 = [{name, "Microsoft"}, {url, "http://microsoft.com"}],
    render(Name, ".html", [{link_list, [Link1, Link2, Link3]}]);
                
render("htmltags" = Name) ->
    render(Name, ".html");
    
render("csstags" = Name) ->
    render(Name, ".css");
    
render(Name) ->
    io:format("No such template: ~p~n",[Name]).  
                
                
%%--------------------------------------------------------------------
%% @spec (atom(), string()) -> any()
%% @doc renders template to a file
%% @end 
%%--------------------------------------------------------------------
render(Name, Ext) ->
    OutDir = filename:join([filename:dirname(code:which(?MODULE)),"..", "demo", "out"]),
    render2(OutDir, list_to_atom("test_" ++ Name), Ext).   
    

%%--------------------------------------------------------------------
%% @spec (atom(), string(), string()) -> any()
%% @doc renders template to a file
%% @end 
%%--------------------------------------------------------------------
render(Name, Ext, Args) ->
    OutDir = filename:join([filename:dirname(code:which(?MODULE)),"..", "demo", "out"]),
    render2(OutDir, list_to_atom("test_" ++ Name), Ext, Args).
            

              
%%====================================================================
%% Internal functions
%%====================================================================

render2(OutDir, Module, Ext, Arg) ->
    case catch apply(Module, render, [Arg]) of
        {'EXIT', Reason} -> 
            io:format("TRACE ~p:~p ~p: rendering failure: ~n",[?MODULE, ?LINE, Reason]);
        Val -> 
            case file:open(filename:join([OutDir, lists:concat([Module, Ext])]), [write]) of
        		{ok, IoDev} ->
        		    file:write(IoDev, Val),
        		    file:close(IoDev);        		    
        		_ ->
        		    io:format("TRACE ~p:~p ~p: file write failure~n",[?MODULE, ?LINE, Module])
        	end
    end.
    
render2(OutDir, Module, Ext) ->
    case catch Module:render() of
        {'EXIT', Reason} -> 
            io:format("TRACE ~p:~p ~p: rendering failure: ~n",[?MODULE, ?LINE, Reason]);
        Val -> 
            case file:open(filename:join([OutDir, lists:concat([Module, Ext])]), [write]) of
        		{ok, IoDev} ->
        		    file:write(IoDev, Val),
        		    file:close(IoDev),
        		    io:format("TRACE ~p:~p ~p: success~n",[?MODULE, ?LINE, Module]);
        		_ ->
        		    io:format("TRACE ~p:~p ~p: file write failure~n",[?MODULE, ?LINE, Module])
        	end
    end.
