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
-export([compile_all/0, compile/1, compile/2, render_all/0, render/1, preset/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc  compiles the templates to beam files
%% @end 
%%--------------------------------------------------------------------
compile_all() ->
    DocRoot = filename:join([filename:dirname(code:which(?MODULE)),"..", "demo", "templates"]),
    filelib:fold_files(DocRoot,
        "\.html$|\.css$",
        true,
        fun(Path, _Acc) ->
            Module = filename:rootname(filename:basename(Path)),
            % case erlydtl_server:compile(Path, DocRoot, Module, {?MODULE, preset}) of
            case erlydtl_compiler:compile(Path, DocRoot, Module) of
                ok ->
                    io:format("compile success: ~p~n",[Module]);
                _ ->
                    io:format("compile failure: ~p~n",[Module])
            end
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

compile("var_preset" = Name) ->
    compile(Name, ".html");
     
compile("for_preset" = Name) ->
    compile(Name, ".html");     

compile("for_records_preset" = Name) ->
    compile(Name, ".html");
          
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
    % case erlydtl_server:compile(Path, DocRoot, Module, {?MODULE, preset}) of
    case erlydtl_compiler:compile(Path, DocRoot, Module) of
        ok ->
            io:format("compile success: ~p~n",[Module]);
        _ ->
            io:format("compile failure: ~p~n",[Module])
    end.


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc renders template to a file
%% @end 
%%--------------------------------------------------------------------
render_all() ->
    render("var"),
    render("extends"),
    render("comment"),
    render("for"),
    render("for_records"),
    render("htmltags"),
    render("csstags"),
    render("var_preset"),
    render("for_preset"),
    render("for_records_preset").
        

%%--------------------------------------------------------------------
%% @spec (string()) -> ok()
%% @doc renders template to a file
%% @end 
%%--------------------------------------------------------------------
render("var" = Name) ->
    render(Name, ".html", [{var1, "foostring1"}, {var2, "foostring2"}, {var_not_used, "foostring3"}]);
 
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
  
render("var_preset" = Name) ->
    render(Name, ".html", [{var1, "foostring1"}, {var2, "foostring2"}]);
 
render("for_preset" = Name) ->
    render(Name, ".html");
            
render("for_records_preset" = Name) ->
    Link1 = [{name, "Canon"}, {url, "http://canon.com"}],
    Link2 = [{name, "Leica"}, {url, "http://leica.com"}],
    Link3 = [{name, "Nikon"}, {url, "http://nikon.com"}],
    render(Name, ".html", [{photo_links, [Link1, Link2, Link3]}]);
        
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
            

%%--------------------------------------------------------------------
%% @spec (atom()) -> proplist()
%% @doc returns template preset variables
%% @end 
%%--------------------------------------------------------------------
preset(test_extends) ->
    [{preset_base_var, "preset-base-barstring"}];

preset(test_var_preset) ->
    [{preset_var1, "preset-var1"}, {preset_var2, "preset-var2"}];
    
preset(test_for_preset) ->
    [{fruit_list, ["preset-apple", "preset-banana", "preset-coconut"]}];
           
preset(test_for_records_preset) ->
    Link1 = [{name, "Amazon (preset)"}, {url, "http://amazon.com"}],
    Link2 = [{name, "Google (preset)"}, {url, "http://google.com"}],
    Link3 = [{name, "Microsoft (preset)"}, {url, "http://microsoft.com"}],
    [{software_links, [Link1, Link2, Link3]}].

              
%%====================================================================
%% Internal functions
%%====================================================================

render2(OutDir, Module, Ext, Arg) ->
    case catch apply(Module, render, [Arg]) of
        {ok, Val, Warnings} -> 
            write_file(OutDir, Module, Ext, Val, Warnings);
        {error, Err, Warnings} ->
            io:format("TRACE ~p:~p Errors: ~p~n",[?MODULE, ?LINE, Err]),
            io:format("TRACE ~p:~p Warnings: ~p~n",[?MODULE, ?LINE, Warnings]);
        {'EXIT', Reason} -> 
            io:format("TRACE ~p:~p ~p: render failure: ~n",[?MODULE, ?LINE, Reason]);
        Val -> %% only temporarly
            write_file(OutDir, Module, Ext, Val, [])
    end.
    
    
render2(OutDir, Module, Ext) ->
    case catch Module:render() of      
        {ok, Val, Warnings} -> 
            write_file(OutDir, Module, Ext, Val, Warnings);
        {error, Err, Warnings} ->
            io:format("TRACE ~p:~p Errors: ~p~n",[?MODULE, ?LINE, Err]),
            io:format("TRACE ~p:~p Warnings: ~p~n",[?MODULE, ?LINE, Warnings]);
        {'EXIT', Reason} -> 
            io:format("TRACE ~p:~p ~p: render failure: ~n",[?MODULE, ?LINE, Reason]);
        Val -> %% only temporarly
            write_file(OutDir, Module, Ext, Val, [])
    end.


write_file(OutDir, Module, Ext, Val, Warnings) ->
    case file:open(filename:join([OutDir, lists:concat([Module, Ext])]), [write]) of
		{ok, IoDev} ->
		    file:write(IoDev, Val),
		    file:close(IoDev),
		    case Warnings of
		        [] ->
		            io:format("render success: ~p~n",[Module]);    
		        _ -> 
		            io:format("render success: ~p - Warnings: ~p~n",[Module, Warnings])
		    end;
		_ ->
		    io:format("render failure: ~p~n",[Module])
	end.