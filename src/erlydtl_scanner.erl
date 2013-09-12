%%%-------------------------------------------------------------------
%%% File:      erlydtl_scanner.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @copyright 2013 Andreas Stenius
%%% @doc 
%%% Template language scanner
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
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
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%% @since 2013-09-12 by Andreas Stenius
%%%-------------------------------------------------------------------
-module(erlydtl_scanner).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').
-author('Andreas Stenius <kaos@astekk.se>').

-export([scan/1, resume/1, scan_ex/1]).
-include("erlydtl_ext.hrl").

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(tag, {in, tag, open, close, on_open, on_close, on_scan }).
-record(state, {
          template=[],
          pos={1, 1},
          scanned=[],
          scope=[#tag{ tag=text, on_scan=fun scan_text/2 }],
          tags=
              [#tag{ in=text, tag=comment, open="<!--{#", close="#}-->" },
               #tag{ in=text, tag=comment, open="{#", close="#}" },
               #tag{ in=text, tag=code, open="{{", close="}}",
                     on_open=open_var, on_close=close_var,
                     on_scan=fun scan_code/2 },
               #tag{ in=code, tag=string, open="\"", close="\"",
                     on_open=string_literal,
                     on_scan=fun scan_string/2 },
               #tag{ in=code, tag=string, open="\'", close="\'",
                     on_open=string_literal,
                     on_scan=fun scan_string/2 },
               #tag{ in=code, tag=identifier, open=fun is_alpha/1,
                     on_open=identifier,
                     on_scan=fun scan_identifier/2 },
               #tag{ in=code, tag=number, open=fun is_digit/1,
                     on_open=number_literal,
                     on_scan=fun scan_number/2 },
               #tag{ in=string, tag=escape, open="\\", close="",
                     on_scan=fun scan_string/2 }
              ]
         }).

%%====================================================================
%% API
%%====================================================================

scan_ex(Template) ->
    do_scan({fun open_tag/1, #state{ template=Template }}).

do_scan({_, #state{ template=[], scanned=Scanned}}) ->
    {ok, [process_token(T) || T <- lists:reverse(Scanned)]};
do_scan({Next, State}) when is_function(Next, 1)-> do_scan(Next(State));
do_scan(Err) -> Err.

process_token({string, Pos, Text}) -> {string, Pos, lists:reverse(Text)};
process_token({identifier, Pos, Chars}) -> {identifier, Pos, list_to_atom(lists:reverse(Chars))};
process_token(Token) -> Token.
    
open_tag(#state{ template=Template, scope=[#tag{ tag=In }|_], tags=Tags }=State) ->
    case find_open_tag(Template, [T || T <- Tags, T#tag.in == In]) of
        undefined -> {fun close_tag/1, State};
        Tag -> {fun open_tag/1, do_open_tag(Tag, State)}
    end.

close_tag(#state{ scope=[#tag{ close=undefined }|_] }=State) ->
    {fun scan_tag/1, State};
close_tag(#state{ template=Template, scope=[T|_] }=State) ->
    case lists:prefix(T#tag.close, Template) of
        true -> {fun open_tag/1, do_close_tag(State)};
        false -> {fun scan_tag/1, State}
    end.

scan_tag(#state{ scope=[#tag{ on_scan=Scan }|_] }=State) ->
    {fun open_tag/1, do_scan_tag(Scan, State)}.

find_open_tag(Template, Tags) ->
    case lists:dropwhile(
           fun(#tag{ open=Prefix }) when is_list(Prefix) ->
                   not lists:prefix(Prefix, Template);
              (#tag{ open=OpenTest }) when is_function(OpenTest, 1) ->
                   not OpenTest(hd(Template))
           end, Tags)
    of
        [Tag|_] -> Tag;
        [] -> undefined
    end.

do_open_tag(#tag{ open=Prefix, on_open=Open }=Tag,
            #state{ scope=Scope }=State) ->
    move(Prefix, State#state{
                   scanned=scan_event(Open, Prefix, State),
                   scope=[Tag|Scope] }).

do_close_tag(#state{ scope=[#tag{ close=Prefix, 
                                  on_close=Close }
                            |Scope] }=State) ->
    move(Prefix, State#state{ 
                   scanned=scan_event(Close, Prefix, State),
                   scope=Scope }).

do_scan_tag(undefined, #state{ template=[C|_] }=State) -> move(C, State);
do_scan_tag(OnScan, #state{ template=[C|_] }=State)
  when is_function(OnScan, 2) -> move(C, OnScan(C, State)).

scan_event(undefined, _, #state{ scanned=Scanned }) -> Scanned;
scan_event(Type, Prefix, #state{ pos=Pos, scanned=Scanned })
  when is_list(Prefix) ->
    [{Type, Pos, list_to_atom(Prefix)}|Scanned];
scan_event(Type, Prefix, #state{ template=[C|_], pos=Pos, scanned=Scanned })
  when is_function(Prefix, 1) ->
    [{Type, Pos, [C]}|Scanned].

move([C|Cs], State) -> move(Cs, move(C, State));
move([], State) -> State;
move(F, #state{ template=[C|_] }=State)
  when is_function(F, 1) -> move(C, State);
move(undefined, State) -> State;
move(C, #state{ template=[C|Template], pos=Pos }=State) -> 
    State#state{ template=Template, pos=update_pos(C, Pos) }.

update_pos($\n, {Row, _Col}) -> {Row + 1, 1};
update_pos(_, {Row, Col}) -> {Row, Col + 1}.

append_char(C, Type, #state{ scanned=[{Type, Pos, Chars}|Scanned] }=State) ->
    State#state{ scanned=[{Type, Pos, [C|Chars]}|Scanned] };
append_char(C, Type, #state{ pos=Pos, scanned=Scanned }=State) ->
    State#state{ scanned=[{Type, Pos, [C]}|Scanned] }.

%%% Tag callbacks
scan_text(C, State) ->
    append_char(C, string, State).

scan_code(_, State) ->
    State.

scan_string(_, State) ->
    State.

is_alpha(C) -> ((C >= $a) andalso (C =< $z)) orelse ((C >= $A) andalso (C =< $Z)) orelse (C == $_).
is_digit(C) -> (C >= $0) andalso (C =< $9).

scan_identifier(C, State) ->
    case is_alpha(C) orelse is_digit(C) of
        true -> append_char(C, identifier, State);
        false -> do_close_tag(State)
    end.

scan_number(_, State) ->
    State.


-ifdef (TEST).

simple_test() ->
    T = "foo {{ bar }}",
    E = {ok, [{string, {1, 1}, "foo "},
              {open_var, {1, 5}, '{{'},
              {identifier, {1, 8}, bar},
              {close_var, {1, 12}, '}}'}]},
    ?assertEqual(E, scan(T)),
    ?assertEqual(E, scan_ex(T)).
    

-endif.














%%--------------------------------------------------------------------
%% @spec scan(T::template()) -> {ok, S::tokens()} | {error, Reason}
%% @type template() = string() | binary(). Template to parse
%% @type tokens() = [tuple()].
%% @doc Scan the template string T and return the a token list or
%% an error.
%% @end
%%--------------------------------------------------------------------
scan(Template) ->
    scan(Template, [], {1, 1}, in_text).    

resume(#scanner_state{ template=Template, scanned=Scanned, 
		     pos=Pos, state=State}) ->
    scan(Template, Scanned, Pos, State).

scan([], Scanned, _, in_text) ->
    Tokens = lists:reverse(Scanned),
    FixedTokens = reverse_strings(Tokens),
    MarkedTokens = mark_keywords(FixedTokens),
    AtomizedTokens = atomize_identifiers(MarkedTokens),
    {ok, AtomizedTokens};

scan([], _Scanned, _, {in_comment, _}) ->
    {error, "Reached end of file inside a comment."};

scan([], _Scanned, _, _) ->
    {error, "Reached end of file inside a code block."};

scan("<!--{{" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_var, {Row, Column}, '<!--{{'} | Scanned], {Row, Column + length("<!--{{")}, {in_code, "}}-->"});

scan("{{" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_var, {Row, Column}, '{{'} | Scanned], {Row, Column + length("{{")}, {in_code, "}}"});

scan("<!--{#" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, Scanned, {Row, Column + length("<!--{#")}, {in_comment, "#}-->"});

scan("{#" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, Scanned, {Row, Column + length("{#")}, {in_comment, "#}"});

scan("#}-->" ++ T, Scanned, {Row, Column}, {in_comment, "#}-->"}) ->
    scan(T, Scanned, {Row, Column + length("#}-->")}, in_text);

scan("#}" ++ T, Scanned, {Row, Column}, {in_comment, "#}"}) ->
    scan(T, Scanned, {Row, Column + length("#}")}, in_text);

scan("<!--{%" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_tag, {Row, Column}, '<!--{%'} | Scanned], 
	 {Row, Column + length("<!--{%")}, {in_code, "%}-->"});

scan("{%" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_tag, {Row, Column}, '{%'} | Scanned], 
	 {Row, Column + length("{%")}, {in_code, "%}"});

scan([_ | T], Scanned, {Row, Column}, {in_comment, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_comment, Closer});

scan("\n" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, append_text_char(Scanned, {Row, Column}, $\n), {Row + 1, 1}, in_text);

scan([H | T], Scanned, {Row, Column}, in_text) ->
    scan(T, append_text_char(Scanned, {Row, Column}, H), {Row, Column + 1}, in_text);

scan("\"" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, "\""} | Scanned], {Row, Column + 1}, {in_double_quote, Closer});

scan("\"" ++ T, Scanned, {Row, Column}, {in_identifier, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, "\""} | Scanned], {Row, Column + 1}, {in_double_quote, Closer});

scan("\'" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, "\""} | Scanned], {Row, Column + 1}, {in_single_quote, Closer});

scan("\'" ++ T, Scanned, {Row, Column}, {in_identifier, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, "\""} | Scanned], {Row, Column + 1}, {in_single_quote, Closer});

scan([$\\ | T], Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, $\\), {Row, Column + 1}, {in_double_quote_slash, Closer});

scan([H | T], Scanned, {Row, Column}, {in_double_quote_slash, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_double_quote, Closer});

scan([$\\ | T], Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, $\\), {Row, Column + 1}, {in_single_quote_slash, Closer});

scan([H | T], Scanned, {Row, Column}, {in_single_quote_slash, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_single_quote, Closer});

						% end quote
scan("\"" ++ T, Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, 34), {Row, Column + 1}, {in_code, Closer});

						% treat single quotes the same as double quotes
scan("\'" ++ T, Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, 34), {Row, Column + 1}, {in_code, Closer});

scan([H | T], Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_double_quote, Closer});

scan([H | T], Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_single_quote, Closer});


scan("}}-->" ++ T, Scanned, {Row, Column}, {_, "}}-->"}) ->
    scan(T, [{close_var, {Row, Column}, '}}-->'} | Scanned], 
	 {Row, Column + length("}}-->")}, in_text);

scan("}}" ++ T, Scanned, {Row, Column}, {_, "}}"}) ->
    scan(T, [{close_var, {Row, Column}, '}}'} | Scanned], {Row, Column + 2}, in_text);

scan("%}-->" ++ T, Scanned, {Row, Column}, {_, "%}-->"}) ->
    scan(T, [{close_tag, {Row, Column}, '%}-->'} | Scanned], 
	 {Row, Column + length("%}-->")}, in_text);

scan("%}" ++ T, [{identifier, _, "mitabrev"}, {open_tag, _, '{%'}|Scanned], {Row, Column}, {_, "%}"}) ->
    scan(T, [{string, {Row, Column + 2}, ""}|Scanned], {Row, Column + 2}, {in_verbatim, undefined});

scan("%}" ++ T, [{identifier, _, ReversedTag}, {identifier, _, "mitabrev"}, {open_tag, _, '{%'}|Scanned], 
     {Row, Column}, {_, "%}"}) ->
    scan(T, [{string, {Row, Column + 2}, ""}|Scanned], {Row, Column + 2}, {in_verbatim, ReversedTag});

scan("%}" ++ T, Scanned, {Row, Column}, {_, "%}"}) ->
    scan(T, [{close_tag, {Row, Column}, '%}'} | Scanned], 
	 {Row, Column + 2}, in_text);

scan("{%" ++ T, Scanned, {Row, Column}, {in_verbatim, Tag}) ->
    scan(T, Scanned, {Row, Column + 2}, {in_verbatim_code, lists:reverse("{%"), Tag});

scan(" " ++ T, Scanned, {Row, Column}, {in_verbatim_code, BackTrack, Tag}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_verbatim_code, [$\ |BackTrack], Tag});

scan("endverbatim%}" ++ T, Scanned, {Row, Column}, {in_verbatim_code, _BackTrack, undefined}) ->
    scan(T, Scanned, {Row, Column + length("endverbatim%}")}, in_text);

scan("endverbatim " ++ T, Scanned, {Row, Column}, {in_verbatim_code, BackTrack, Tag}) ->
    scan(T, Scanned, {Row, Column + length("endverbatim ")}, 
	 {in_endverbatim_code, "", lists:reverse("endverbatim ", BackTrack), Tag});

scan(" " ++ T, Scanned, {Row, Column}, {in_endverbatim_code, "", BackTrack, Tag}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_endverbatim_code, "", [$\ |BackTrack], Tag});

scan([H|T], Scanned, {Row, Column}, {in_endverbatim_code, EndTag, BackTrack, Tag}) when H >= $a, H =< $z; H >= $0, H =< $9; H =:= $_  ->
    scan(T, Scanned, {Row, Column + 1}, {in_endverbatim_code, [H|EndTag], [H|BackTrack], Tag});

scan(" " ++ T, Scanned, {Row, Column}, {in_endverbatim_code, Tag, BackTrack, Tag}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_endverbatim_code, Tag, [$\ |BackTrack], Tag});

scan("%}" ++ T, Scanned, {Row, Column}, {in_endverbatim_code, Tag, _BackTrack, Tag}) ->
    scan(T, Scanned, {Row, Column + 2}, in_text);

scan("%}" ++ T, Scanned, {Row, Column}, {in_endverbatim_code, "", _BackTrack, undefined}) ->
    scan(T, Scanned, {Row, Column + 2}, in_text);

scan([H|T], [{string, Pos, Data}|Scanned], {Row, Column}, {in_endverbatim_code, _, BackTrack, Tag}) ->
    NewPos = case H of $\n -> {Row + 1, 1}; _ -> {Row, Column + 1} end,
    scan(T, [{string, Pos, [H|BackTrack] ++ Data}|Scanned], NewPos, {in_verbatim, Tag});

scan([H|T], [{string, Pos, Data}|Scanned], {Row, Column}, {in_verbatim_code, BackTrack, Tag}) ->
    NewPos = case H of $\n -> {Row + 1, 1}; _ -> {Row, Column + 1} end,
    scan(T, [{string, Pos, [H|BackTrack] ++ Data}|Scanned], NewPos, {in_verbatim, Tag});

scan([H|T], [{string, Pos, Data}|Scanned], {Row, Column}, {in_verbatim, Tag}) ->
    NewPos = case H of $\n -> {Row + 1, 1}; _ -> {Row, Column + 1} end,
    scan(T, [{string, Pos, [H|Data]}|Scanned], NewPos, {in_verbatim, Tag});

scan("==" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'==', {Row, Column}} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("!=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'!=', {Row, Column}} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan(">=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'>=', {Row, Column}} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("<=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'<=', {Row, Column}} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("<" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'<', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(">" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'>', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("("++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'(', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(")" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{')', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("," ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{',', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("|" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'|', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'=', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(":" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{':', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("." ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'.', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("_(" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, lists:reverse([{'_', {Row, Column}}, {'(', {Row, Column + 1}}], Scanned), {Row, Column + 2}, {in_code, Closer});

scan(" " ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_code, Closer});


scan([H | T], Scanned, {Row, Column}, {in_code, Closer}) ->
    case char_type(H) of
        letter_underscore ->
            scan(T, [{identifier, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_identifier, Closer});
        hyphen_minus ->
            scan(T, [{number_literal, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_number, Closer});
        digit ->
            scan(T, [{number_literal, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_number, Closer});
        _ ->
            {error, {Row, ?MODULE, lists:concat(["Illegal character in column ", Column])},
	     #scanner_state{ template=[H|T], scanned=Scanned, pos={Row, Column}, state={in_code, Closer}}}
    end;

scan([H | T], Scanned, {Row, Column}, {in_number, Closer}) ->
    case char_type(H) of
        digit ->
            scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_number, Closer});
        _ ->
            {error, {Row, ?MODULE, lists:concat(["Illegal character in column ", Column])},
	     #scanner_state{ template=[H|T], scanned=Scanned, pos={Row, Column}, state={in_code, Closer}}}
    end;

scan([H | T], Scanned, {Row, Column}, {in_identifier, Closer}) ->
    case char_type(H) of
        letter_underscore ->
            scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_identifier, Closer});
        digit ->
            scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_identifier, Closer});
        _ ->
            {error, {Row, ?MODULE, lists:concat(["Illegal character in column ", Column])},
	     #scanner_state{ template=[H|T], scanned=Scanned, pos={Row, Column}, state={in_code, Closer}}}
    end.

						% internal functions

append_char([{Type, Pos, Chars}|Scanned], Char) ->
    [{Type, Pos, [Char | Chars]} | Scanned].

append_text_char([], {Row, Column}, Char) ->
    [{string, {Row, Column}, [Char]}];
append_text_char([{string, StrPos, Chars} |Scanned1], _, Char) ->
    [{string, StrPos, [Char | Chars]} | Scanned1];
append_text_char(Scanned, {Row, Column}, Char) ->
    [{string, {Row, Column}, [Char]} | Scanned].

char_type(C) when ((C >= $a) andalso (C =< $z)) orelse ((C >= $A) andalso (C =< $Z)) orelse (C == $_) ->
    letter_underscore;
char_type(C) when ((C >= $0) andalso (C =< $9)) ->
    digit;
char_type($-) ->
    hyphen_minus;
char_type(_C) ->
    undefined.

reverse_strings(Tokens) ->
    reverse_strings(Tokens, []).

reverse_strings([], Acc) ->
    lists:reverse(Acc);
reverse_strings([{Category, Pos, String}|T], Acc) when Category =:= string; Category =:= identifier;
                                                       Category =:= string_literal; Category =:= number_literal ->
    reverse_strings(T, [{Category, Pos, lists:reverse(String)}|Acc]);
reverse_strings([Other|T], Acc) ->
    reverse_strings(T, [Other|Acc]).

mark_keywords(Tokens) ->
    mark_keywords(Tokens, []).

mark_keywords([], Acc) ->
    lists:reverse(Acc);
mark_keywords([{identifier, Pos, "in" = String}|T], Acc) ->
    mark_keywords(T, [{in_keyword, Pos, String}|Acc]);
mark_keywords([{identifier, Pos, "not" = String}|T], Acc) ->
    mark_keywords(T, [{not_keyword, Pos, String}|Acc]);
mark_keywords([{identifier, Pos, "or" = String}|T], Acc) ->
    mark_keywords(T, [{or_keyword, Pos, String}|Acc]);
mark_keywords([{identifier, Pos, "and" = String}|T], Acc) ->
    mark_keywords(T, [{and_keyword, Pos, String}|Acc]);
mark_keywords([{identifier, Pos, "as" = String}|T], Acc) ->
    mark_keywords(T, [{as_keyword, Pos, String}|Acc]);
mark_keywords([{identifier, Pos, "by" = String}|T], Acc) ->
    mark_keywords(T, [{by_keyword, Pos, String}|Acc]);
mark_keywords([{identifier, Pos, "with" = String}|T], Acc) ->
    mark_keywords(T, [{with_keyword, Pos, String}|Acc]);
						% These must be succeeded by a close_tag
mark_keywords([{identifier, Pos, "only" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{only_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "parsed" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{parsed_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "noop" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{noop_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "reversed" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{reversed_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "openblock" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{openblock_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "closeblock" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{closeblock_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "openvariable" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{openvariable_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "closevariable" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{closevariable_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "openbrace" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{openbrace_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "closebrace" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{closebrace_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "opencomment" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{opencomment_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "closecomment" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{closecomment_keyword, Pos, String}, CloseTag], Acc));
						% The rest must be preceded by an open_tag.
						% This allows variables to have the same names as tags.
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "autoescape" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {autoescape_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endautoescape" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endautoescape_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "block" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {block_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endblock" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endblock_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "comment" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {comment_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endcomment" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endcomment_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "cycle" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {cycle_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "extends" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {extends_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "filter" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {filter_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endfilter" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endfilter_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "firstof" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {firstof_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "for" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {for_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "empty" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {empty_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endfor" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endfor_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "if" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {if_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "elif" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {elif_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "else" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {else_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endif" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endif_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "ifchanged" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {ifchanged_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endifchanged" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endifchanged_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "ifequal" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {ifequal_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endifequal" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endifequal_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "ifnotequal" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {ifnotequal_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endifnotequal" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endifnotequal_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "include" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {include_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "now" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {now_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "regroup" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {regroup_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endregroup" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endregroup_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "spaceless" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {spaceless_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endspaceless" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endspaceless_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "ssi" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {ssi_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "templatetag" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {templatetag_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "widthratio" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {widthratio_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "call" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {call_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endwith" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endwith_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "trans" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {trans_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "blocktrans" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {blocktrans_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endblocktrans" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endblocktrans_keyword, Pos, String}], Acc));
mark_keywords([H|T], Acc) ->
    mark_keywords(T, [H|Acc]).

atomize_identifiers(Tokens) ->
    atomize_identifiers(Tokens, []).

atomize_identifiers([], Acc) ->
    lists:reverse(Acc);
atomize_identifiers([{identifier, Pos, String}|T], Acc) ->
    atomize_identifiers(T, [{identifier, Pos, list_to_atom(String)}|Acc]);
atomize_identifiers([H|T], Acc) ->
    atomize_identifiers(T, [H|Acc]).
