%%%-------------------------------------------------------------------
%%% File:      erlydtl_scanner.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon
%%% @doc Template language scanner
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
-module(erlydtl_scanner).
-author('rsaccon@gmail.com').

%% API
-export([scan/1]).  

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec scan(T::template()) -> {ok, S::tokens()} | {error, Reason}
%% @type template() = string() | binary(). Template to parse
%% @type tokens() = [tuple()].
%% @doc Scan the template string T and return the a token list or
%% an error.
%% @end
%%--------------------------------------------------------------------

scan(Template) ->
    scan(Template, [], 1).

scan([], Scanned, _Line) ->
    Tokens = fold_strings(lists:reverse(Scanned), [], []),
    {ok, Tokens};

scan([$<, $\!, $-, $-, ${, ${ | T], Scanned, Line) ->
  Rules = [until(fun is_var_end/1), 
           until(fun is_html_comment_end/1)],
  Scan = scan2(Rules),
  case Scan(T) of
      {ok, Token, LinesScanned, Rest} ->
          scan(Rest, [{var, Line, Token} | Scanned], Line + LinesScanned);
      {error, Reason} -> 
          {error, {var, Line, Reason}}
  end;             

scan([${, ${ | T], Scanned, Line) ->
  Rules = [until(fun is_var_end/1)],
  Scan = scan2(Rules),
  case Scan(T) of
      {ok, Token, LinesScanned, Rest} ->
          scan(Rest, [{var, Line, Token} | Scanned], Line + LinesScanned);
      {error, Reason} -> 
          {error, {var, Line, Reason}}
  end;

scan([$<, $\!, $-, $-, ${, $\% | T], Scanned, Line) ->
  Rules = [until(fun is_tag_end/1), 
           until(fun is_html_comment_end/1)],
  Scan = scan2(Rules),
  case Scan(T) of
      {ok, Token, LinesScanned, Rest} ->
          scan(Rest, [{tag, Line, Token} | Scanned], Line + LinesScanned);
      {error, Reason} -> 
          {error, {tag, Line, Reason}}
  end;

scan([${, $\% | T], Scanned, Line) ->
  Rules = [until(fun is_tag_end/1)],
  Scan = scan2(Rules),
  case Scan(T) of
      {ok, Token, LinesScanned, Rest} ->
          scan(Rest, [{tag, Line, Token} | Scanned], Line + LinesScanned);
      {error, Reason} -> 
          {error, {tag, Line, Reason}}
  end;

scan([${, $# | T], Scanned, Line) ->
  Rules = [until(fun is_comment_end/1)],
  Scan = scan2(Rules), 
  case Scan(T) of
      {ok, _Token, LinesScanned, Rest} ->
          scan(Rest, Scanned, Line + LinesScanned);
      {error, Reason} -> 
          {error, {var, Line, Reason}}
  end;

scan([H | T], Scanned, Line) when [H] == "\r" andalso hd(T) == "\n" ->
    scan(tl(T), ["\r\n" | Scanned], Line+1);

scan([H | T], Scanned, Line) when [H] == "\r" orelse [H] == "\n" ->
    scan(T, [H | Scanned], Line+1);

scan([H | T], Scanned, Line) ->
    scan(T, [H | Scanned], Line).


%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------   

scan2(Rules) ->
    fun(Tmpl) ->
	    scan2(Rules, Tmpl, [], 0)
    end.


scan2([], Tmpl, SoFar, Line) ->
    {ok, lists:reverse(SoFar), Line, Tmpl};

scan2([Rule | T], Tmpl, SoFar, Line) ->
    case Rule(Tmpl) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Rest, LinesScanned} ->
	    scan2(T, Rest, SoFar, Line + LinesScanned);
	{ok, Tok, LinesScanned, Rest} ->
	    scan2(T, Rest, [Tok | SoFar], Line + LinesScanned)
    end.

fold_strings([], Folded, []) ->
    lists:reverse(Folded);

fold_strings([], Folded, Acc) ->
    S = {string, lists:reverse(Acc)},
    lists:reverse([S | Folded]);

fold_strings([H | T], Folded, []) when is_tuple(H) ->
    fold_strings(T, [translate_token(H) | Folded], []);

fold_strings([H | T], Folded, Acc) when is_tuple(H) ->
    S = {string, lists:reverse(Acc)},
    fold_strings(T, [translate_token(H), S | Folded], []);

fold_strings([H | T], Folded, Acc) ->
    fold_strings(T, Folded, [H | Acc]).


translate_token({var, Line, [[S] | _]}) ->
    {var, Line, S};

translate_token({tag, Line, [[H | T] | _]}) ->
    translate_tag(H, T, Line) ;

translate_token(Token) ->
    io:format("TRACE ~p:~p unrecognized token: ~p~n",[?MODULE, ?LINE, Token]),
    Token.

translate_tag("extends" = H, T, Line) ->
    {list_to_atom(H), Line, T};
    
translate_tag("block" = H, T, Line) ->
    {list_to_atom(H), Line, T};   
    
translate_tag("endblock" = H, T, Line) ->
    {list_to_atom(H), Line, T};    
    
translate_tag("for" = H, T, Line) ->
    {list_to_atom(H), Line, T};   

translate_tag("endfor" = H, T, Line) ->
    {list_to_atom(H), Line, T};    
         
translate_tag(H, T, Line) ->
    {tag, Line, [list_to_atom(H) | T]}.


until(P) ->
    fun (Tmpl) -> 
            until(P, Tmpl, 0, []) 
    end.

until(_P, [], _Line, _Scanned) ->    
    {error, end_not_found};

until(P, [H|T], Line, Scanned) when [H]=="\r" andalso hd(T)=="\n" ->
    until(P, tl(T), Line+1, Scanned);

until(P, [H|T], Line, Scanned) when [H]=="\n" orelse [H]== "\r" ->
    until(P, T, Line+1, Scanned);

until(P, [H|T]=Tmpl, Line, Scanned) ->
    case P(Tmpl) of
	{true, R} ->
            Scanned1 = string:strip(lists:reverse(Scanned)),
            Scanned2 = string:tokens(Scanned1, " "),
	    {ok, Scanned2, Line, R};
	_ ->
	    until(P, T, Line, [H | Scanned])
    end.


is_var_end([$}, $} | T]) -> {true, T};
is_var_end(_) -> false.
   

is_tag_end([$\%, $} | T]) -> {true, T};
is_tag_end(_) -> false.


is_comment_end([$#, $} | T]) -> {true, T};
is_comment_end(_) -> false.


is_html_comment_end([$-, $-, $> | T]) -> {true, T};
is_html_comment_end(_) -> false.
