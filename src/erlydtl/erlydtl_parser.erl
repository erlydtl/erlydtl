-module(erlydtl_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).

-file("/Users/rsaccon/R11B/erlang/lib/parsetools-1.4.1.1/include/yeccpre.hrl", 0).
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id $
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    yeccpars0(Tokens, false).

parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {F, A});
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{M, F}, A}).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser
% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function,{return_error,2}}).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
                   Error % probably from return_error/1
    end.

% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {F, A}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(Tokens, {F, A}, State, States, Vstack);
        {eof, _Endline} ->
            yeccpars1([], false, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);
yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/erlydtl/erlydtl_parser.erl", 97).

yeccpars2(0, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_0_(__Stack),
 yeccpars2(1, __Cat, [0 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(1, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(1, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_2_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(3, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_3_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(4, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_4_(__Stack),
 yeccpars2(88, __Cat, [4 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(5, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_5_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_6_(__Stack),
 yeccpars2(83, __Cat, [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_7_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_8_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_9_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_10_(__Stack),
 yeccpars2(78, __Cat, [10 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_11_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_12_(__Stack),
 yeccpars2(73, __Cat, [12 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_13_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_14_(__Stack),
 yeccpars2(68, __Cat, [14 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_15_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(16, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(17, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(18, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_18_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(19, close_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(20, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_20_(__Stack),
 yeccpars2(yeccgoto('Variable', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(21, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Variable', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Variable', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(23, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(24, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_24_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Variable', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_25_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('VariableBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(26, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_27_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Variable', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(28, colon, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_28_(__Stack),
 yeccpars2(yeccgoto('Filter', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(29, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(30, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_30_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Filter', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Literal', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Literal', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(33, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(34, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(35, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(36, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(37, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(38, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_38_(__Stack),
 yeccpars2(48, __Cat, [38 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(39, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, not_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(40, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(41, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(42, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_42_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('IncludeTag', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(43, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('IfExpression', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(44, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(45, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, not_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(46, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_46_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('IfExpression', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_47_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('IfBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(48, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(49, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_49_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('CustomTag', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(50, equal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(51, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(52, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_52_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('Args', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(53, comma, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, in_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(54, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(55, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('ForExpression', hd(__Ss)), close_tag, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(55, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('ForGroup', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(56, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_56_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('ForBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(57, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(58, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [58 | __Ss], [__T | __Stack]);
yeccpars2(58, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(59, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_59_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ForExpression', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(60, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_60_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ForGroup', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(61, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(62, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_62_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('ExtendsTag', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(63, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_63_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('CommentBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(64, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(65, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_65_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('BlockBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(66, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(67, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_67_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('AutoEscapeBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(68, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(69, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_69_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('AutoEscapeBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(70, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, endautoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(71, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(72, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_72_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndAutoEscapeBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(73, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(74, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_74_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('BlockBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(75, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, endblock_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(76, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(77, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_77_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndBlockBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(78, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(79, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_79_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('CommentBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(80, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, endcomment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(82, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_82_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndCommentBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(83, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(84, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_84_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ForBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(85, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, endfor_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(86, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(87, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_87_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndForBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(88, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(89, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_89_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('IfBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(90, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_90_(__Stack),
 yeccpars2(96, __Cat, [90 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(91, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, else_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, endif_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(92, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(93, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(94, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_94_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndIfBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(95, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_95_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ElseBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(96, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(97, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_97_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('IfBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(98, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, endif_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto('Args', 38) ->
 48;
yeccgoto('AutoEscapeBlock', 1) ->
 15;
yeccgoto('AutoEscapeBlock', 68) ->
 15;
yeccgoto('AutoEscapeBlock', 73) ->
 15;
yeccgoto('AutoEscapeBlock', 78) ->
 15;
yeccgoto('AutoEscapeBlock', 83) ->
 15;
yeccgoto('AutoEscapeBlock', 88) ->
 15;
yeccgoto('AutoEscapeBlock', 96) ->
 15;
yeccgoto('AutoEscapeBraced', 1) ->
 14;
yeccgoto('AutoEscapeBraced', 68) ->
 14;
yeccgoto('AutoEscapeBraced', 73) ->
 14;
yeccgoto('AutoEscapeBraced', 78) ->
 14;
yeccgoto('AutoEscapeBraced', 83) ->
 14;
yeccgoto('AutoEscapeBraced', 88) ->
 14;
yeccgoto('AutoEscapeBraced', 96) ->
 14;
yeccgoto('BlockBlock', 1) ->
 13;
yeccgoto('BlockBlock', 68) ->
 13;
yeccgoto('BlockBlock', 73) ->
 13;
yeccgoto('BlockBlock', 78) ->
 13;
yeccgoto('BlockBlock', 83) ->
 13;
yeccgoto('BlockBlock', 88) ->
 13;
yeccgoto('BlockBlock', 96) ->
 13;
yeccgoto('BlockBraced', 1) ->
 12;
yeccgoto('BlockBraced', 68) ->
 12;
yeccgoto('BlockBraced', 73) ->
 12;
yeccgoto('BlockBraced', 78) ->
 12;
yeccgoto('BlockBraced', 83) ->
 12;
yeccgoto('BlockBraced', 88) ->
 12;
yeccgoto('BlockBraced', 96) ->
 12;
yeccgoto('CommentBlock', 1) ->
 11;
yeccgoto('CommentBlock', 68) ->
 11;
yeccgoto('CommentBlock', 73) ->
 11;
yeccgoto('CommentBlock', 78) ->
 11;
yeccgoto('CommentBlock', 83) ->
 11;
yeccgoto('CommentBlock', 88) ->
 11;
yeccgoto('CommentBlock', 96) ->
 11;
yeccgoto('CommentBraced', 1) ->
 10;
yeccgoto('CommentBraced', 68) ->
 10;
yeccgoto('CommentBraced', 73) ->
 10;
yeccgoto('CommentBraced', 78) ->
 10;
yeccgoto('CommentBraced', 83) ->
 10;
yeccgoto('CommentBraced', 88) ->
 10;
yeccgoto('CommentBraced', 96) ->
 10;
yeccgoto('CustomTag', 1) ->
 9;
yeccgoto('CustomTag', 68) ->
 9;
yeccgoto('CustomTag', 73) ->
 9;
yeccgoto('CustomTag', 78) ->
 9;
yeccgoto('CustomTag', 83) ->
 9;
yeccgoto('CustomTag', 88) ->
 9;
yeccgoto('CustomTag', 96) ->
 9;
yeccgoto('Elements', 0) ->
 1;
yeccgoto('Elements', 4) ->
 88;
yeccgoto('Elements', 6) ->
 83;
yeccgoto('Elements', 10) ->
 78;
yeccgoto('Elements', 12) ->
 73;
yeccgoto('Elements', 14) ->
 68;
yeccgoto('Elements', 90) ->
 96;
yeccgoto('ElseBraced', 88) ->
 90;
yeccgoto('EndAutoEscapeBraced', 68) ->
 69;
yeccgoto('EndBlockBraced', 73) ->
 74;
yeccgoto('EndCommentBraced', 78) ->
 79;
yeccgoto('EndForBraced', 83) ->
 84;
yeccgoto('EndIfBraced', 88) ->
 89;
yeccgoto('EndIfBraced', 96) ->
 97;
yeccgoto('ExtendsTag', 1) ->
 8;
yeccgoto('ExtendsTag', 68) ->
 8;
yeccgoto('ExtendsTag', 73) ->
 8;
yeccgoto('ExtendsTag', 78) ->
 8;
yeccgoto('ExtendsTag', 83) ->
 8;
yeccgoto('ExtendsTag', 88) ->
 8;
yeccgoto('ExtendsTag', 96) ->
 8;
yeccgoto('Filter', 26) ->
 27;
yeccgoto('ForBlock', 1) ->
 7;
yeccgoto('ForBlock', 68) ->
 7;
yeccgoto('ForBlock', 73) ->
 7;
yeccgoto('ForBlock', 78) ->
 7;
yeccgoto('ForBlock', 83) ->
 7;
yeccgoto('ForBlock', 88) ->
 7;
yeccgoto('ForBlock', 96) ->
 7;
yeccgoto('ForBraced', 1) ->
 6;
yeccgoto('ForBraced', 68) ->
 6;
yeccgoto('ForBraced', 73) ->
 6;
yeccgoto('ForBraced', 78) ->
 6;
yeccgoto('ForBraced', 83) ->
 6;
yeccgoto('ForBraced', 88) ->
 6;
yeccgoto('ForBraced', 96) ->
 6;
yeccgoto('ForExpression', 37) ->
 54;
yeccgoto('ForGroup', 37) ->
 53;
yeccgoto('IfBlock', 1) ->
 5;
yeccgoto('IfBlock', 68) ->
 5;
yeccgoto('IfBlock', 73) ->
 5;
yeccgoto('IfBlock', 78) ->
 5;
yeccgoto('IfBlock', 83) ->
 5;
yeccgoto('IfBlock', 88) ->
 5;
yeccgoto('IfBlock', 96) ->
 5;
yeccgoto('IfBraced', 1) ->
 4;
yeccgoto('IfBraced', 68) ->
 4;
yeccgoto('IfBraced', 73) ->
 4;
yeccgoto('IfBraced', 78) ->
 4;
yeccgoto('IfBraced', 83) ->
 4;
yeccgoto('IfBraced', 88) ->
 4;
yeccgoto('IfBraced', 96) ->
 4;
yeccgoto('IfExpression', 39) ->
 44;
yeccgoto('IfExpression', 45) ->
 46;
yeccgoto('IncludeTag', 1) ->
 3;
yeccgoto('IncludeTag', 68) ->
 3;
yeccgoto('IncludeTag', 73) ->
 3;
yeccgoto('IncludeTag', 78) ->
 3;
yeccgoto('IncludeTag', 83) ->
 3;
yeccgoto('IncludeTag', 88) ->
 3;
yeccgoto('IncludeTag', 96) ->
 3;
yeccgoto('Literal', 29) ->
 30;
yeccgoto('Variable', 17) ->
 19;
yeccgoto('Variable', 39) ->
 43;
yeccgoto('Variable', 45) ->
 43;
yeccgoto('Variable', 51) ->
 52;
yeccgoto('VariableBraced', 1) ->
 2;
yeccgoto('VariableBraced', 68) ->
 2;
yeccgoto('VariableBraced', 73) ->
 2;
yeccgoto('VariableBraced', 78) ->
 2;
yeccgoto('VariableBraced', 83) ->
 2;
yeccgoto('VariableBraced', 88) ->
 2;
yeccgoto('VariableBraced', 96) ->
 2;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_0_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 104).
yeccpars2_0_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_2_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 106).
yeccpars2_2_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_3_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 108).
yeccpars2_3_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_4_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 104).
yeccpars2_4_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_5_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 112).
yeccpars2_5_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_6_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 104).
yeccpars2_6_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_7_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 111).
yeccpars2_7_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 107).
yeccpars2_8_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 109).
yeccpars2_9_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 104).
yeccpars2_10_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 114).
yeccpars2_11_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 104).
yeccpars2_12_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 110).
yeccpars2_13_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 104).
yeccpars2_14_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 113).
yeccpars2_15_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 105).
yeccpars2_18_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 119).
yeccpars2_20_([__1 | __Stack]) ->
 [begin
   { variable , { __1 } }
  end | __Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 120).
yeccpars2_24_([__3,__2,__1 | __Stack]) ->
 [begin
   { variable , { __1 , __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 116).
yeccpars2_25_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_27_([__3,__2,__1 | __Stack]) ->
 [begin
   { apply_filter , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 161).
yeccpars2_28_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 162).
yeccpars2_30_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 129).
yeccpars2_38_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 125).
yeccpars2_42_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { include , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 151).
yeccpars2_46_([__2,__1 | __Stack]) ->
 [begin
   { 'not' , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 150).
yeccpars2_47_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 127).
yeccpars2_49_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { tag , __2 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 130).
yeccpars2_52_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ { __2 , __4 } ]
  end | __Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 141).
yeccpars2_56_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 144).
yeccpars2_59_([__3,__2,__1 | __Stack]) ->
 [begin
   { in , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 146).
yeccpars2_60_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 124).
yeccpars2_62_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { extends , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_63_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 133).
yeccpars2_65_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 158).
yeccpars2_67_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 157).
yeccpars2_69_([__3,__2,__1 | __Stack]) ->
 [begin
   { autoescape , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_72_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 132).
yeccpars2_74_([__3,__2,__1 | __Stack]) ->
 [begin
   { block , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_77_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 136).
yeccpars2_79_([__3,__2,__1 | __Stack]) ->
 [begin
   { comment , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_82_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 140).
yeccpars2_84_([__3,__2,__1 | __Stack]) ->
 [begin
   { for , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_87_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_89_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 149).
yeccpars2_89_([__3,__2,__1 | __Stack]) ->
 [begin
   { 'if' , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_90_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 104).
yeccpars2_90_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_94_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_94_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_95_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_95_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_97_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 148).
yeccpars2_97_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { ifelse , __1 , __2 , __4 }
  end | __Stack].


