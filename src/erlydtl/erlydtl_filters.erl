%%%-------------------------------------------------------------------
%%% File:      erlydtl_filters.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc 
%%% Template filters
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
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
-module(erlydtl_filters).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

-compile(export_all).

capfirst(Input) ->
    [H|T] = lists:flatten(Input),
    [string:to_upper(H)] ++ T.

center(Input, Number) ->
    string:centre(lists:flatten(Input), Number).

first([[First|_Rest]]) ->
    [First].

fix_ampersands(Input) ->
    fix_ampersands(lists:flatten(Input), []).

force_escape(Input) when is_list(Input) ->
    escape(lists:flatten(Input), []);
force_escape(Input) when is_binary(Input) ->
    escape(binary_to_list(Input), []);
force_escape(Input) ->
    Input.

format_integer(Input) ->
    case Input of
        N when is_integer(N) ->
            integer_to_list(N);
        Other ->
            Other
    end.

join([Input], Separator) when is_list(Input) ->
    string:join(Input, Separator).

last([Input]) when is_list(Input) ->
    [lists:last(Input)].

length([Input]) when is_list(Input) ->
    integer_to_list(erlang:length(Input));
length(Input) when is_list(Input) ->
    integer_to_list(erlang:length(Input)).

length_is(Input, Number) when is_list(Input) ->
    lists:concat([erlang:length(Input) =:= Number]).

linebreaksbr(Input) ->
    linebreaksbr(lists:flatten(Input), []).

ljust(Input, Number) ->
    string:left(lists:flatten(Input), Number).

lower(Input) ->
    string:to_lower(lists:flatten(Input)).

rjust(Input, Number) ->
    string:right(lists:flatten(Input), Number).

plus([Input], Number) when is_list(Input) ->
    integer_to_list(list_to_integer(Input) + Number);
plus(Input, Number) when is_list(Input) ->
    integer_to_list(list_to_integer(Input) + Number);
plus(Input, Number) when is_integer(Input) ->
    Input + Number.

upper(Input) ->
    string:to_upper(lists:flatten(Input)).

urlencode(Input) ->
    urlencode(lists:flatten(Input), []).

% internal

escape([], Acc) ->
    lists:reverse(Acc);
escape("<" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&lt;", Acc));
escape(">" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&gt;", Acc));
escape("&" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&amp;", Acc));
escape("\"" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&quot;", Acc));
escape("'" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&#039;", Acc));
escape([C | Rest], Acc) ->
    escape(Rest, [C | Acc]).

fix_ampersands([], Acc) ->
    lists:reverse(Acc);
fix_ampersands("&" ++ Rest, Acc) ->
    fix_ampersands(Rest, lists:reverse("&amp;", Acc));
fix_ampersands([C | Rest], Acc) ->
    fix_ampersands(Rest, [C | Acc]).

linebreaksbr([], Acc) ->
    lists:reverse(Acc);
linebreaksbr("\r\n" ++ Rest, Acc) ->
    linebreaksbr(Rest, lists:reverse("<br />", Acc));
linebreaksbr("\n" ++ Rest, Acc) ->
    linebreaksbr(Rest, lists:reverse("<br />", Acc));
linebreaksbr([C | Rest], Acc) ->
    linebreaksbr(Rest, [C | Acc]).

% Taken from quote_plus of mochiweb_util
urlencode([], Acc) ->
    lists:reverse(Acc);
urlencode([C | Rest], Acc) when ((C >= $a andalso C =< $z) orelse
                                  (C >= $A andalso C =< $Z) orelse
                                  (C >= $0 andalso C =< $9) orelse
                                  (C =:= $\. orelse C =:= $- 
                                      orelse C =:= $~ orelse C =:= $_)) ->
    urlencode(Rest, [C | Acc]);
urlencode([$\s | Rest], Acc) ->
    urlencode(Rest, [$+ | Acc]);
urlencode([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    urlencode(Rest, [hexdigit(Lo), hexdigit(Hi), $\% | Acc]).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).
