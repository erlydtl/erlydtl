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
-export([start/0, stop/0, create_parser/0, reload/2, write_beam/3]).

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-ifdef(debug). 
-define(PRINT_ERR_WARNS, [report_warnings, report_errors]). 
-else. 
-define(PRINT_ERR_WARNS, []). 
-endif.


%% @spec start() -> ok
%% @doc Start the erlydtl server.
start() ->
    application:start(erlydtl).


%% @spec stop() -> ok
%% @doc Stop the erlydtl server.
stop() ->
    application:stop(erlydtl).
    
    
%%--------------------------------------------------------------------
%% @spec 
%% @doc
%% @end 
%%--------------------------------------------------------------------
create_parser() ->
    create_parser("src/erlydtl/erlydtl_parser", "ebin").


%%--------------------------------------------------------------------
%% @spec (ModuleName::string(), Bin,::binary()) -> Ok::atom() | Error::atom()
%% @doc reloads byte code
%% @end 
%%--------------------------------------------------------------------
reload(Module, Bin) ->
    code:purge(Module),
    SrcName = atom_to_list(Module) ++ ".erl",
    case code:load_binary(Module, SrcName, Bin) of
        {module, _} -> ok;
        _ -> error
    end.


%%--------------------------------------------------------------------
%% @spec (ModuleName::string(), Bin,::binary(), Dir::string()) -> any()
%% @doc writes  byte code to beam file
%% @end 
%%--------------------------------------------------------------------    
write_beam(ModuleName, Bin, Dir) ->
    File = filename:join([Dir, atom_to_list(ModuleName) ++ ".beam"]),
    file:write_file(File, Bin).


%%====================================================================
%% Internal functions
%%====================================================================

create_parser(Path, Outdir) ->
    case yecc:file(Path) of
        {ok, _} ->
            compile_reload_parser(Path, Outdir);
        Err ->
            io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, Path ++ ": yecc failed"]),
            Err
    end.


compile_reload_parser(Path, Outdir) ->
    case compile:file(Path, ?PRINT_ERR_WARNS ++ [{outdir, Outdir}]) of
        {ok, Bin} ->
            code:purge(Bin),
            code:load_file(Bin);
        Err ->
            io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, Path ++ ": compilation failed"]),
            Err
    end.