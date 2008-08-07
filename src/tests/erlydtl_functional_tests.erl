%%%-------------------------------------------------------------------
%%% File:      erlydtl_tests.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc       ErlyDTL test suite
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
-module(erlydtl_functional_tests).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').


%% API
-export([run_tests/0, run_test/1]).

%% @spec (Name::string()) -> {CompileStatus::atom(), PresetVars::list(), 
%%     RenderStatus::atom(), RenderVars::list()} | skip
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
    RenderVars = [
        {date_var1, {1975,7,24}},
        {datetime_var1, {{1975,7,24}, {7,13,1}}},
        {'list', ["eins", "zwei", "drei"]}
    ],
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
setup("now") ->
    CompileVars = [],
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
setup("cycle") ->
    CompileVars = [],
    RenderVars = [{test, [integer_to_list(X) || X <- lists:seq(1, 20)]},
                  {a, "Apple"}, {b, "Banana"}, {c, "Cherry"}],
    {ok, CompileVars, ok, RenderVars};
%%--------------------------------------------------------------------       
%% Custom tags
%%--------------------------------------------------------------------
setup("custom_tag") ->
    CompileVars  = [],
    RenderVars = [],
    {ok, CompileVars, ok, RenderVars};        
setup("custom_tag_error") ->
    CompileVars  = [],
    RenderVars = [],
    {error, CompileVars, skip, RenderVars};        
setup("custom_call") ->
    CompileVars  = [],
    RenderVars = [{var1, "something"}],
    {ok, CompileVars, ok, RenderVars};    
%%--------------------------------------------------------------------       
%% Files to ignore:
%%-------------------------------------------------------------------- 
setup("base") ->
    %% example base template used in the extends tag
    skip; 
setup("include.html") ->
    %% example plain text file used in include tag 
    skip;      
setup(_) ->
    undefined.
    

run_tests() ->    
    io:format("Running functional tests...~n"),
    case fold_tests() of
        {N, []}->
            Msg = lists:concat(["All ", N, " functional tests passed~n~n"]),
            io:format(Msg),
            {ok, Msg};
        {_, Errs} ->
            io:format("Errors: ~p~n~n",[Errs]),
            failed
    end.


run_test(Name) ->
    test_compile_render(filename:join([templates_docroot(), Name])).


%%====================================================================
%% Internal functions
%%====================================================================

fold_tests() ->
    filelib:fold_files(templates_docroot(), "^[^\.].+", false,
        fun
            (File, {AccCount, AccErrs}) ->
                case test_compile_render(File) of
                    ok -> 
                        {AccCount + 1, AccErrs};
                    {error, Reason} -> 
                        {AccCount + 1, [{File, Reason} | AccErrs]}
                end
        end, {0, []}). 


test_compile_render(File) ->  
    Name = filename:rootname(filename:basename(File)),
    Module = "example_" ++ Name,
    case setup(Name) of
        {CompileStatus, CompileVars, RenderStatus, RenderVars} ->
            Options = [
                {vars, CompileVars}, 
                {force_recompile, true}],
            io:format(" Template: ~p, ... compiling ... ", [Name]),
            case erlydtl:compile(File, Module, Options) of
                ok ->
                    case CompileStatus of
                        ok -> test_render(File, list_to_atom(Module), RenderStatus, RenderVars);
                        _ -> {error, "compiling should have failed :" ++ File}
                    end;
                {error, Err} ->
                    case CompileStatus of
                        error ->
                            io:format("~n"),  
                            ok;
                        _ ->
                            io:format("~nCompile errror: ~p~n",[Err]), 
                            Err
                    end
            end;
        skip ->
            ok;
        _ ->
            {error, "no 'setup' clause defined for this test"}
    end.


test_render(File, Module, RenderStatus, Vars) ->   
    case catch Module:render(Vars) of
        {ok, Data} ->
            io:format("rendering~n"), 
            case RenderStatus of
                ok ->
                    {File, _} = Module:source(),
                    OutFile = filename:join([templates_outdir(), filename:basename(File)]),
                    case file:open(OutFile, [write]) of
                        {ok, IoDev} ->
                            file:write(IoDev, Data),
                            file:close(IoDev),
                            ok;    
                        Err ->
                            Err
                    end;
                _ ->
                    {error, "rendering should have failed :" ++ File}
            end;
        {'EXIT', _} ->
            io:format("~n"),
            {error, "failed invoking render method:" ++ Module};
        Err ->
            io:format("~n"),
            case RenderStatus of
                error ->  ok;
                _ -> Err
            end
    end.   


templates_docroot() ->
    filename:join([erlydtl_deps:get_base_dir(), "examples", "docroot"]).

templates_outdir() ->   
    filename:join([erlydtl_deps:get_base_dir(), "examples", "rendered_output"]).
