%%%-------------------------------------------------------------------
%%% File:      erlydtl.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc  
%%% Helper module to start and stop ErlyDTL application and for 
%%% creating yecc-grammar based template parser
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2008 Roberto Saccon, Evan Miller
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
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(erlydtl).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

%% API
-export([create_parser/0, parser_src/0]).

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-define(PRINT_ERR_WARNS, [report_warnings, report_errors]). 

    
%%--------------------------------------------------------------------
%% @spec () ->  Ok::atom() | Err::tuple()
%% @doc creates the yecc-based ErlyDTL parser
%% @end 
%%--------------------------------------------------------------------
create_parser() ->
    create_parser("src/erlydtl/erlydtl_parser", "ebin").

 
%%--------------------------------------------------------------------
%% @spec () -> string()
%% @doc creates the yecc-based ErlyDTL parser
%% @end 
%%--------------------------------------------------------------------   
parser_src() ->
    {file, Ebin} = code:is_loaded(?MODULE),
    filename:join([filename:dirname(filename:dirname(Ebin)), 
        "src", "erlydtl", "erlydtl_parser.yrl"]).
    
         
%%====================================================================
%% Internal functions
%%====================================================================

create_parser(Path, Outdir) ->
    case yecc:file(Path) of
        {ok, _} ->
            compile_parser(Path, Outdir);
        _ ->
            {error, "yecc parser generation failed"}
    end.


compile_parser(Path, Outdir) ->
    case compile:file(Path, ?PRINT_ERR_WARNS ++ [{outdir, Outdir}]) of
        {ok, Bin} ->
            code:purge(Bin),
            case code:load_file(Bin) of
                {module, _} ->
                    ok;
                _ ->
                    {error, "yecc parser reload failed"}
            end;
        _ ->
            {error, "yecc parser compilation failed"}
    end.