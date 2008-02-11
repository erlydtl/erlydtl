%%%-------------------------------------------------------------------
%%% File:      erlydtl_tests.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc       ErlyDTS test suite
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
%%% @since 2008-02-11 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlydtl_tests).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').


%% API
-export([setup/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec (Name::string()) -> {CompileStatus::atom(), PresetVars::List, RenderStatus::atom(), RenderVars::List}
%% @doc
%% @end 
%%--------------------------------------------------------------------
setup("autoescape") ->
    CompileVars = [],
    RenderVars = [{var1, "<b>bold</b>"}],
    {ok, CompileVars, ok, RenderVars};  
setup("comment") ->
    CompileVars = [],
    RenderVars =[],
    {ok, CompileVars, ok, RenderVars};   
setup("extends") ->
    CompileVars = [],
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, CompileVars, ok, RenderVars};
setup("filters") ->
    CompileVars = [],
    RenderVars = [{'list', ["eins", "zwei", "drei"]}],
    {ok, CompileVars, ok, RenderVars};
setup("for") ->
    CompileVars = [],
    RenderVars = [{fruit_list, ["apple", "banana", "coconut"]}],
    {ok, CompileVars, ok, RenderVars};
setup("for_list") ->
    CompileVars = [],
    RenderVars = [{fruit_list, [["apple", "apples", "$1"], ["banana", "bananas", "$2"], ["coconut", "coconuts", "$500"]]}],
    {ok, CompileVars, ok, RenderVars};
setup("for_tuple") ->
    CompileVars = [],
    RenderVars = [{fruit_list, [{"apple", "apples"}, {"banana", "bananas"}, {"coconut", "coconuts"}]}],
    {ok, CompileVars, ok, RenderVars};
setup("for_list_preset") ->
    CompileVars = [{fruit_list, [["apple", "apples"], ["banana", "bananas"], ["coconut", "coconuts"]]}],
    RenderVars = [],
    {ok, CompileVars, ok, RenderVars};
setup("for_preset") ->
    CompileVars = [{fruit_list, ["preset-apple", "preset-banana", "preset-coconut"]}],
    RenderVars = [],
    {ok, CompileVars, ok, RenderVars};
setup("for_records") ->
    CompileVars = [],
    Link1 = [{name, "Amazon"}, {url, "http://amazon.com"}],
    Link2 = [{name, "Google"}, {url, "http://google.com"}],
    Link3 = [{name, "Microsoft"}, {url, "http://microsoft.com"}],
    RenderVars = [{link_list, [Link1, Link2, Link3]}],
    {ok, CompileVars, ok, RenderVars};  
setup("for_records_preset") ->
    Link1a = [{name, "Amazon (preset)"}, {url, "http://amazon.com"}],
    Link2a = [{name, "Google (preset)"}, {url, "http://google.com"}],
    Link3a = [{name, "Microsoft (preset)"}, {url, "http://microsoft.com"}],
    CompileVars = [{software_links, [Link1a, Link2a, Link3a]}], 
    Link1b = [{name, "Canon"}, {url, "http://canon.com"}],
    Link2b = [{name, "Leica"}, {url, "http://leica.com"}],
    Link3b = [{name, "Nikon"}, {url, "http://nikon.com"}],
    RenderVars = [{photo_links, [Link1b, Link2b, Link3b]}],
    {ok, CompileVars, ok, RenderVars};
setup("include") ->
    CompileVars = [],
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}],
    {ok, CompileVars, ok, RenderVars};
setup("if") ->
    CompileVars = [],
    RenderVars = [{var1, "something"}],
    {ok, CompileVars, ok, RenderVars}; 
setup("if_preset") ->
    CompileVars = [{var1, "something"}],
    RenderVars = [],
    {ok, CompileVars, ok, RenderVars};   
setup("ifequal") ->
    CompileVars = [],
    RenderVars = [{var1, "foo"}, {var2, "foo"}, {var3, "bar"}],
    {ok, CompileVars, ok, RenderVars};      
setup("ifequal_preset") ->
    CompileVars = [{var1, "foo"}, {var2, "foo"}],
    RenderVars = [{var3, "bar"}],
    {ok, CompileVars, ok, RenderVars};   
setup("ifnotequal") ->
    CompileVars = [],
    RenderVars = [{var1, "foo"}, {var2, "foo"}, {var3, "bar"}],
    {ok, CompileVars, ok, RenderVars};        
setup("ifnotequal_preset") ->
    CompileVars = [{var1, "foo"}, {var2, "foo"}],
    RenderVars = [],
    {ok, CompileVars, ok, RenderVars}; 
setup("var") ->
    CompileVars = [],
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}, {var_not_used, "foostring3"}],
    {ok, CompileVars, ok, RenderVars};
setup("var_preset") ->
    CompileVars = [{preset_var1, "preset-var1"}, {preset_var2, "preset-var2"}],
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}],
    {ok, CompileVars, ok, RenderVars}; 
setup("var_error") ->
    CompileVars = [],
    RenderVars = [{var1, "foostring1"}],   
    {ok, CompileVars, error, RenderVars};        
%% Custom tag
setup("custom_tag") ->
    CompileVars  = [],
    RenderVars = [],
    {ok, CompileVars, ok, RenderVars};  
%% Custom tag      
setup("custom_tag_error") ->
    CompileVars  = [],
    RenderVars = [],
    {error, CompileVars, ignore, RenderVars};
%% Custom tag        
setup("custom_call") ->
    CompileVars  = [],
    RenderVars = [{var1, "something"}],
    {ok, CompileVars, ok, RenderVars};        
setup(_) ->
    undefined.
    










