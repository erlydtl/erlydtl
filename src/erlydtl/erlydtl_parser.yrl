%%%-------------------------------------------------------------------
%%% File:      erlydtl_parser.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon, Tait Larson
%%% @doc Template language grammar
%%% @reference  See <a href="http://erlydtl.googlecode.com" target="_top">http://erlydtl.googlecode.com</a> for more information
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
%%% @since 2007-11-11 by Roberto Saccon
%%%-------------------------------------------------------------------


Nonterminals 
    Elements
    Element.

Terminals 
    string
    var
    extends
    block
    endblock
    tag
    for
    endfor.

Rootsymbol    
    Elements. 


%% -------------------------------------------------------------------
%% Rules
%% -------------------------------------------------------------------

Elements -> '$empty' : nil.
Elements -> Elements Element : ['$1', '$2'].

Element -> string : string('$1').
Element -> var : var('$1').
Element -> extends : extends('$1').
Element -> block Elements endblock : block('$1', '$2').
Element -> tag : tag('$1').
Element -> for Elements endfor : for('$1', '$2').


Erlang code.

string({_, String}) ->
    erl_syntax:binary([erl_syntax:binary_field(erl_syntax:integer(X)) || X <- String]).


var({_, Line, Var}) ->
    case string:tokens(Var, ".") of
        [Namespace, Var1] ->
            {var, Line, list_to_atom("A" ++ Namespace), list_to_atom(Var1)};
        _ ->
            {var, Line, list_to_atom("A" ++ Var)}
    end.


extends({_, Line, [Name]}) ->
    %% TODO: check if string (enclosed with  "") or variable. 
    %% for now we handle it (even not enclosed with "") as string
    {extends, Line, string:strip(Name, both, $")}.


block({_, Line, [Name]}, Content) ->
    {block, Line, list_to_atom(Name), Content}.


tag({_, Line, [TagName | Args]}) ->
    %% TODO: check if string (enclosed with  "") or variable. 
    %% for now we handle it (even not enclosed with "") as string
	Args2 = lists:foldl(fun(X, Acc) ->
	        case string:chr(X, $=) of
				0 ->
				    Acc;
				Pos ->
			        Var = list_to_atom(string:sub_string(X, 1, Pos-1)),
			        Val = string:sub_string(X, Pos+1),
			        Val2 = string:strip(Val, both, $"),
					[{Var, Val2}| Acc]
			end
		end,
    	[],
    	Args),
    {tag, Line, TagName, Args2}.


for({_, Line, [Iterator, _, Var]}, Content) ->
    {for, Line, list_to_atom("A" ++ Iterator), list_to_atom(Var), Content}.