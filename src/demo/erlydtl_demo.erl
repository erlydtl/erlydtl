%%%-------------------------------------------------------------------
%%% File:      erlydtl_demo.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc  
%%% Demo application and tests
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
-module(erlydtl_demo).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

%% API
-export([create_parser/0, 
    compile_all/0, 
    compile/1, 
    compile/3, 
    render_all/0, 
    render/1, 
    render/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec () -> ok | {error, Reason::string()}
%% @doc creates the parser source code and compiles it 
%% @end 
%%--------------------------------------------------------------------
create_parser() ->
    case erlydtl:create_parser() of
        ok ->
            io:format("parser creation success ~n");
        {error, Reason} ->
            io:format("parser creation failure: ~p~n",[Reason])
    end.
            

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc  compiles all templates to beam files
%% @end 
%%--------------------------------------------------------------------
compile_all() ->
    compile("autoescape"),
    compile("comment"),
    compile("extends"),
    compile("filters"),
    compile("for"),
    compile("for_preset"),    
    compile("for_list"),
    compile("for_list_preset"),
    compile("for_records"),
    compile("for_records_preset"),
    compile("for_tuple"),
    compile("if"),
    compile("if_preset"),         
    compile("ifequal"),  
    compile("ifequal_preset"), 
    compile("ifnotequal"),  
    compile("ifnotequal_preset"),            
    compile("include"),
    compile("var"),
    compile("var_preset"),
    compile("var_error"),
    compile("custom_tag"),
    compile("custom_tag_error"),
    compile("custom_call").


%%--------------------------------------------------------------------
%% @spec (string()) -> any()
%% @doc 
%% compiles the template to beam files
%% @end 
%%--------------------------------------------------------------------        
compile("var" = Name) ->
    compile(Name, ".html", []);
    
compile("var_preset" = Name) ->
    Vars = [{preset_var1, "preset-var1"}, {preset_var2, "preset-var2"}],
    compile(Name, ".html", Vars);

compile("var_error" = Name) ->
    compile(Name, ".html", []);
        
compile("extends" = Name) ->
    compile(Name, ".html", []);

compile("include" = Name) ->
    compile(Name, ".html", []);

compile("autoescape" = Name) ->
    compile(Name, ".html", []);

compile("if" = Name) ->
    compile(Name, ".html", []);   
    
compile("if_preset" = Name) ->
    Vars = [{var1, "something"}],
    compile(Name, ".html", Vars);    
          
compile("ifequal" = Name) ->
    compile(Name, ".html", []);        
          
compile("ifequal_preset" = Name) ->
    Vars = [{var1, "foo"}, {var2, "foo"}],
    compile(Name, ".html", Vars);
 
compile("ifnotequal" = Name) ->
    compile(Name, ".html", []);        

compile("ifnotequal_preset" = Name) ->
    Vars = [{var1, "foo"}, {var2, "foo"}],
    compile(Name, ".html", Vars);
                      
compile("filters" = Name) ->
    compile(Name, ".html", []);

compile("comment" = Name) ->
    compile(Name, ".html", []);
               
compile("for" = Name) ->
    compile(Name, ".html", []);
 
compile("for_records" = Name) ->
    compile(Name, ".html", []);
    
compile("for_list" = Name) ->
    compile(Name, ".html", []);

compile("for_tuple" = Name) ->
    compile(Name, ".html", []);
 
compile("for_list_preset" = Name) ->
    Vars = [{fruit_list, [["apple", "apples"], ["banana", "bananas"], ["coconut", "coconuts"]]}],
    compile(Name, ".html", Vars);
   
compile("for_preset" = Name) ->
    Vars = [{fruit_list, ["preset-apple", "preset-banana", "preset-coconut"]}],
    compile(Name, ".html", Vars);     

compile("for_records_preset" = Name) ->
    Link1 = [{name, "Amazon (preset)"}, {url, "http://amazon.com"}],
    Link2 = [{name, "Google (preset)"}, {url, "http://google.com"}],
    Link3 = [{name, "Microsoft (preset)"}, {url, "http://microsoft.com"}],
    Var = [{software_links, [Link1, Link2, Link3]}],
    compile(Name, ".html", Var);
    
compile("custom_tag" = Name) ->
    compile(Name, ".html", []);

compile("custom_tag_error" = Name) ->
    compile(Name, ".html", []);
                  
compile("custom_call" = Name) ->
    compile(Name, ".html", []);
        
compile(Name) ->
    io:format("No such template: ~p~n",[Name]).
               
               
%%--------------------------------------------------------------------
%% @spec (string(), string()) -> any()
%% @doc 
%% compiles the template to beam files
%% @end 
%%--------------------------------------------------------------------       
compile(Name, Ext, Vars) ->
    DocRoot = filename:join([filename:dirname(code:which(?MODULE)),"..", "demo", "templates"]),
    Module = "test_" ++ Name,
    File = filename:join([DocRoot, Module ++ Ext]),
    case erlydtl_compiler:compile(File, Module, [{vars, Vars}, {force_recompile, true}]) of
        ok ->
            io:format("compile success: ~p~n",[Module]);
        {error, Reason} ->
            case string:str(File, "error") of
                0 ->
                    io:format("compile failure: ~p ~p~n", [Module, Reason]);
                _ ->
                    Explanation = "==>> this is ok, we are testing an error !",
                    io:format("compile failure: ~p ~p  ~s~n",[Module, Reason, Explanation])
            end
    end.

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc renders template to a file
%% @end 
%%--------------------------------------------------------------------
render_all() ->
    render("autoescape"),
    render("comment"),
    render("extends"),
    render("filters"),
    render("for"),
    render("for_preset"),    
    render("for_list"),
    render("for_tuple"),
    render("for_list_preset"),
    render("for_records"),
    render("for_records_preset"),
    render("if"),
    render("if_preset"),
    render("ifequal"),
    render("ifequal_preset"),
    render("ifnotequal"),
    render("ifnotequal_preset"),        
    render("include"),
    render("var"),
    render("var_preset"),
    render("var_error"),
    render("custom_tag"),
    render("custom_call").
        

%%--------------------------------------------------------------------
%% @spec (string()) -> ok()
%% @doc renders template to a file
%% @end 
%%--------------------------------------------------------------------
render("autoescape" = Name) ->
    render(Name, [{var1, "<b>bold</b>"}]);
  
render("comment" = Name) ->
    render(Name, []);
          
render("extends" = Name) ->
    render(Name, [{base_var, "base-barstring"}, {test_var, "test-barstring"}]);
    
render("filters" = Name) ->
    render(Name, [{'list', ["eins", "zwei", "drei"]}]);

render("include" = Name) ->
    render(Name, [{var1, "foostring1"}, {var2, "foostring2"}]);
 
render("if" = Name) ->
    render(Name, [{var1, "something"}]);   
    
render("if_preset" = Name) ->
    render(Name, []);
    
render("ifequal" = Name) ->
    render(Name, [{var1, "foo"}, {var2, "foo"}, {var3, "bar"}]);    

render("ifequal_preset" = Name) ->
    render(Name, [{var3, "bar"}]);
            
render("ifnotequal" = Name) ->
    render(Name, [{var1, "foo"}, {var2, "foo"}, {var3, "bar"}]);    

render("ifnotequal_preset" = Name) ->
    render(Name, [{var3, "bar"}]);
                    
render("for" = Name) ->
    render(Name, [{fruit_list, ["apple", "banana", "coconut"]}]);
    
render("for_preset" = Name) ->
    render(Name, []);
            
render("for_list" = Name) ->
    %render(Name, [{fruit_list, [["apple", "apples"], ["banana", "bananas"], ["coconut", "coconuts"]]}]);
    render(Name, [{fruit_list, [["apple", "apples", "$1"], ["banana", "bananas", "$2"], ["coconut", "coconuts", "$500"]]}]);

render("for_tuple" = Name) ->
    %render(Name, [{fruit_list, [{"apple", "apples", "$1"}, {"banana", "bananas", "$2"}, {"coconut", "coconuts", "$500"}]}]);
    render(Name, [{fruit_list, [{"apple", "apples"}, {"banana", "bananas"}, {"coconut", "coconuts"}]}]);

render("for_list_preset" = Name) ->
    render(Name, []);
        
render("for_records" = Name) ->
    Link1 = [{name, "Amazon"}, {url, "http://amazon.com"}],
    Link2 = [{name, "Google"}, {url, "http://google.com"}],
    Link3 = [{name, "Microsoft"}, {url, "http://microsoft.com"}],
    render(Name, [{link_list, [Link1, Link2, Link3]}]);
             
render("for_records_preset" = Name) ->
    Link1 = [{name, "Canon"}, {url, "http://canon.com"}],
    Link2 = [{name, "Leica"}, {url, "http://leica.com"}],
    Link3 = [{name, "Nikon"}, {url, "http://nikon.com"}],
    render(Name, [{photo_links, [Link1, Link2, Link3]}]);

render("var" = Name) ->
    render(Name, [{var1, "foostring1"}, {var2, "foostring2"}, {var_not_used, "foostring3"}]);

render("var_preset" = Name) ->
    render(Name, [{var1, "foostring1"}, {var2, "foostring2"}]);

render("var_error" = Name) ->
    render(Name, [{var1, "foostring1"}]);
        
render("custom_tag" = Name) ->
    render(Name, []);
                
render("custom_call" = Name) ->
    render(Name, [{var1, "something"}]);
                    
render(Name) ->
    io:format("No such template: ~p~n",[Name]).  
                
                
%%--------------------------------------------------------------------
%% @spec (atom(), string(), string()) -> any()
%% @doc renders template to a file
%% @end 
%%--------------------------------------------------------------------
render(Name, Args) ->
    OutDir = filename:join([filename:dirname(code:which(?MODULE)),"..", "demo", "out"]),
    Module = list_to_atom("test_" ++ Name),
    case catch Module:render(Args) of
        {ok, Val} -> 
            {File, _} = Module:source(),
            case file:open(filename:join([OutDir, filename:basename(File)]), [write]) of
                {ok, IoDev} ->
                    file:write(IoDev, Val),
                    file:close(IoDev),
                    io:format("render success: ~p~n",[Module]);    
                _ ->
                    io:format("file writing failure: ~p~n",[Module])
            end;
        {error, Reason} ->
            case string:str(Name, "error") of
                0 ->
                    io:format("render failure: ~p ~p~n",[Module, Reason]);
                _ ->
                    Explanation = "==>> this is ok, we are testing an error !",
                    io:format("render failure: ~p ~p  ~s~n",[Module, Reason, Explanation])
            end
    end.    
            
          
%%====================================================================
%% Internal functions
%%====================================================================
   
