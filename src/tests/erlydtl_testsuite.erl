%%%-------------------------------------------------------------------
%%% File:      erlydtl_testsuite.erl
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
%%% @since 2007-12-14 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlydtl_testsuite).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').



%% API
-export([run/0, run/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec 
%% @doc
%% @end 
%%--------------------------------------------------------------------

run() ->    
    case maybe_create_parser() of
        ok ->
            case fold_tests("^test_.*", false) of
                {N, []}->
                    Msg = lists:concat(["All ", N, " regression tests passed"]),
                    {ok, Msg};
                {_, Errs} -> 
                    {error, Errs}
            end;
        Err ->
            Err
    end.
    
    
run(Name) ->
    case maybe_create_parser() of
        ok ->
            case fold_tests("^test_" ++ Name ++ "$", true) of
                {0, _} -> {error, "Test not found: " ++ Name ++ ".js"};
                {1, []} -> {ok, "Regression test passed"};
                {1, Errs} -> {error, Errs};
                {_, _} -> {error, "Testsuite requires different filename for each test"}
            end;
        Err ->
            Err
    end.
    
    
%%====================================================================
%% Internal functions
%%====================================================================

maybe_create_parser() ->
    case filelib:is_regular(erlydtl:parser_src()) of
        ok ->
            ok; 
        _ ->
            erlydtl:create_parser()   
    end.
    

fold_tests(RegExp, Verbose) ->
    filelib:fold_files(templates_docroot(), RegExp, true, 
        fun
            (File, {AccCount, AccErrs}) ->
                case test_compile_render(File, Verbose) of
                    ok -> 
                        {AccCount + 1, AccErrs};
                    {error, Reason} -> 
                        {AccCount + 1, [{File, Reason} | AccErrs]}
                end
        end, {0, []}). 
        

test_compile_render(File, Verbose) ->   
    Module = filename:rootname(filename:basename(File)),
	[$t, $e, $s, $t, $_ | Name] = Module,
	case catch erlydtl_tests:setup(Name) of
	    {CompileStatus, CompileVars, RenderStatus, RenderVars} ->
	        Options = [
	            {vars, CompileVars}, 
	            {force_recompile, true},
	            {verbose, Verbose}],
            case erlydtl_compiler:compile(File, Module, Options) of
                ok ->
                    case CompileStatus of
                        ok -> test_render(File, list_to_atom(Module), RenderStatus, RenderVars);
                        _ -> {error, "compiling should have failed :" ++ File}
                    end;
                Err ->
                    case CompileStatus of
                        error ->  ok;
                        _ -> Err
                    end
            end;
        _ ->
            {error, "no test clause for found in erlydtl_test.erl"}
    end.
 
 
test_render(File, Module, RenderStatus, Vars) ->   
    case catch Module:render(Vars) of
        {ok, Data} -> 
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
            {error, "failed invoking render method:" ++ Module};
        Err ->
            case RenderStatus of
                error ->  ok;
                _ -> Err
            end
    end.   
        
        
templates_docroot() ->
    {file, Ebin} = code:is_loaded(?MODULE),
    filename:join([filename:dirname(filename:dirname(Ebin)), "tests", "templates"]).
    
templates_outdir() ->   
    {file, Ebin} = code:is_loaded(?MODULE),
    filename:join([filename:dirname(filename:dirname(Ebin)), "tests", "out"]).