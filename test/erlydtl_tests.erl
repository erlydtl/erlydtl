%%%-------------------------------------------------------------------
%%% File:      erlydtl_tests.erl
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2014 Andreas Stenius
%%% @doc
%%% Test suite for erlydtl
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2014 Andreas Stenius
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
%%% @since 2014 by Andreas Stenius
%%%-------------------------------------------------------------------
-module(erlydtl_tests).
-author('Andreas Stenius <kaos@astekk.se>').

-include_lib("eunit/include/eunit.hrl").
-include("testrunner.hrl").

var_test() ->
    {ok, test} = erlydtl:compile_template("String value is: {{ var1 }}", test, [warnings_as_errors, report, {out_dir, false}]),
    {ok, Output} = test:render([{var1, "foo"}]),
    ?assertMatch(<<"String value is: foo">>, iolist_to_binary(Output)).

runner_test() ->
    run_test(
      #test{
         source = {template, "String value is: {{ var1 }}"},
         output = <<"String value is: foo">>,
         render_vars = [{var1, "foo"}]
        }).

all_defs_test_() ->
    [{T#test.title,
      fun () -> run_test(T) end}
     || T <- [erlydtl_test_defs:def_to_record(G, D)
              || {G, Ds} <- erlydtl_test_defs:tests(),
                 D <- Ds
                 %% G == "vars"
             ]
    ].
