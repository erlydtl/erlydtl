%%%-------------------------------------------------------------------
%%% File:      erlydtl_tsd_scanner.xrl
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2013 Andreas Stenius
%%% @doc
%%% Template Scanner Definition scanner
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

Definitions.

KEYWORDS = (any|until|skip|\+|-|:|,|\.)

Rules.

\%\%.*\n : skip_token.
(\s|\t)+ : skip_token.
\n : skip_token.
{KEYWORDS} : {token, {list_to_atom(TokenChars), TokenLine}}.
(form|expr)(.|\\\n)+end : {token, {code, TokenLine, parse_code(TokenChars)}}.
[a-zA-Z_]+ : {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.
(\\.|[^a-zA-Z:.,+\-\s\t\n\\])+ : {token, {symbols, TokenLine, unescape(TokenChars)}}.


Erlang code.

unescape($n) -> $\n;
unescape($r) -> $\r;
unescape($t) -> $\t;
unescape($s) -> $\s;
unescape(C) when is_integer(C) -> C;
unescape(Cs) when is_list(Cs) -> unescape(Cs, []).

unescape([], Acc) -> lists:reverse(Acc);
unescape([$\\,C|Cs], Acc) -> unescape(Cs, [unescape(C)|Acc]);
unescape([C|Cs], Acc) -> unescape(Cs, [C|Acc]).

parse_code(Code) ->
    {ok, Tokens, _} = erl_scan:string(
                        unescape(string:substr(Code, 5, string:len(Code) - 7))
                        ++ "."),
    {ok, Exprs} = case Code of
                      "form" ++ _ ->
                          erl_parse:parse_form(Tokens);
                      "expr" ++ _ ->
                          erl_parse:parse_exprs(Tokens)
                  end,
    Exprs.
