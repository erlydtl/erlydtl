%%%-------------------------------------------------------------------
%%% File:      rebar_tsd.erl
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2013 Andreas Stenius
%%% @doc
%%% TSD compiler plugin for rebar.
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2013 Andreas Stenius
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
%%% @since 2013-11-05 by Andreas Stenius
%%%-------------------------------------------------------------------
-module(rebar_tsd).
-export([compile/2]).

%% for internal use only
-export([info/2]).

%% rebar.hrl
-define(FAIL, rebar_utils:abort()).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar_log:log(error, Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).
%% end rebar.hrl


%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    rebar_base_compiler:run(Config,
                            [],
                            "src", ".tsd", "ebin", ".beam",
                            fun compile_tsd/3).


%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
    ?CONSOLE(
       "Build Template Scanner Definition (*.tsd) sources.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n",
       [
        undefined
       ]).

compile_tsd(Source, Target, Config) ->
    try erlydtl_tsd_compiler:compile(Source, [{out_dir, ebin}]) of
        {ok, _, Target} -> ok;
        {error, Error} ->
            ?DEBUG("compile ~p -> ~p ~n  fail: ~P~n", [Source, Target, Error, 10]),
            rebar_base_compiler:error_tuple(Config, Source, [{Source, [Error]}], [], [])
    catch
        throw:Error ->
            ?DEBUG("compile ~p -> ~p ~n  throw: ~P~n", [Source, Target, Error, 10]),
            rebar_base_compiler:error_tuple(Config, Source, [{Source, [Error]}], [], [])
    end.
