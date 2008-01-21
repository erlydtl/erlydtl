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
 yeccpars1(__Ts, __Tzr, 20, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [1 | __Ss], [__T | __Stack]);
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
 yeccpars2(122, __Cat, [4 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(5, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_5_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_6_(__Stack),
 yeccpars2(113, __Cat, [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_7_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_8_(__Stack),
 yeccpars2(102, __Cat, [8 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_9_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_10_(__Stack),
 yeccpars2(97, __Cat, [10 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_11_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_12_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_13_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_14_(__Stack),
 yeccpars2(92, __Cat, [14 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_15_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_16_(__Stack),
 yeccpars2(87, __Cat, [16 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_17_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(18, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_18_(__Stack),
 yeccpars2(82, __Cat, [18 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_19_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(20, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, ifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(21, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_22_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(23, close_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(24, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_24_(__Stack),
 yeccpars2(yeccgoto('Variable', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Variable', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Variable', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(27, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(28, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_28_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Variable', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(29, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_29_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('VariableBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(30, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_31_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Variable', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(32, colon, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_32_(__Stack),
 yeccpars2(yeccgoto('Filter', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(33, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(34, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_34_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Filter', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(35, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Literal', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(36, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Literal', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(37, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(38, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(39, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(40, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(41, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(42, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_42_(__Stack),
 yeccpars2(62, __Cat, [42 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(43, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, not_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(44, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(45, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(46, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(47, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(48, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_48_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('IncludeTag', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(49, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [49 | __Ss], [__T | __Stack]);
yeccpars2(49, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('IfNotEqualExpression', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(50, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(51, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(52, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_52_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('IfNotEqualBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(53, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('IfEqualExpression', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(54, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(55, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(56, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_56_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('IfEqualBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(57, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('IfExpression', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(58, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [58 | __Ss], [__T | __Stack]);
yeccpars2(58, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(59, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, not_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(60, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_60_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('IfExpression', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(61, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_61_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('IfBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(62, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(63, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_63_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('CustomTag', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(64, equal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(65, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(66, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_66_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('Args', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(67, comma, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, in_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(68, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(69, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('ForExpression', hd(__Ss)), close_tag, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(69, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('ForGroup', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(70, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_70_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('ForBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(71, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(72, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(73, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_73_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ForExpression', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(74, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_74_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ForGroup', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(75, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(76, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_76_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('ExtendsTag', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(77, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_77_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('CommentBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(78, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(79, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_79_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('BlockBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(80, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_81_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('AutoEscapeBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(82, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(83, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_83_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('AutoEscapeBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(84, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, endautoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, ifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(85, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(86, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_86_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndAutoEscapeBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(87, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(88, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_88_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('BlockBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(89, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, endblock_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, ifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(90, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(91, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_91_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndBlockBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(92, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(93, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_93_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('CommentBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(94, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, endcomment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, ifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(95, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(96, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_96_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndCommentBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(97, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(98, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_98_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ForBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(99, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, endfor_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, ifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(100, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(101, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_101_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndForBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(102, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(103, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_103_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('IfBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(104, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_104_(__Stack),
 yeccpars2(110, __Cat, [104 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(105, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, else_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, endif_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, ifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(106, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(107, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(108, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_108_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndIfBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(109, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_109_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ElseBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(110, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(111, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_111_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('IfBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(112, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, endif_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, ifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(113, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 116, [113 | __Ss], [__T | __Stack]);
yeccpars2(113, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [113 | __Ss], [__T | __Stack]);
yeccpars2(113, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [113 | __Ss], [__T | __Stack]);
yeccpars2(113, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(114, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_114_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('IfEqualBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(115, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_115_(__Stack),
 yeccpars2(119, __Cat, [115 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(116, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, else_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, endifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, ifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(117, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(118, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_118_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndIfEqualBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(119, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 121, [119 | __Ss], [__T | __Stack]);
yeccpars2(119, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [119 | __Ss], [__T | __Stack]);
yeccpars2(119, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [119 | __Ss], [__T | __Stack]);
yeccpars2(119, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(120, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_120_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('IfEqualBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(121, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, endifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, ifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(122, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(123, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_123_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('IfNotEqualBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(124, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_124_(__Stack),
 yeccpars2(128, __Cat, [124 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(125, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, else_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, endifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, ifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(126, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(127, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_127_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndIfNotEqualBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(128, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [128 | __Ss], [__T | __Stack]);
yeccpars2(128, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [128 | __Ss], [__T | __Stack]);
yeccpars2(128, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [128 | __Ss], [__T | __Stack]);
yeccpars2(128, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(129, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_129_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('IfNotEqualBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(130, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, endifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, ifnotequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto('Args', 42) ->
 62;
yeccgoto('AutoEscapeBlock', 1) ->
 19;
yeccgoto('AutoEscapeBlock', 82) ->
 19;
yeccgoto('AutoEscapeBlock', 87) ->
 19;
yeccgoto('AutoEscapeBlock', 92) ->
 19;
yeccgoto('AutoEscapeBlock', 97) ->
 19;
yeccgoto('AutoEscapeBlock', 102) ->
 19;
yeccgoto('AutoEscapeBlock', 110) ->
 19;
yeccgoto('AutoEscapeBlock', 113) ->
 19;
yeccgoto('AutoEscapeBlock', 119) ->
 19;
yeccgoto('AutoEscapeBlock', 122) ->
 19;
yeccgoto('AutoEscapeBlock', 128) ->
 19;
yeccgoto('AutoEscapeBraced', 1) ->
 18;
yeccgoto('AutoEscapeBraced', 82) ->
 18;
yeccgoto('AutoEscapeBraced', 87) ->
 18;
yeccgoto('AutoEscapeBraced', 92) ->
 18;
yeccgoto('AutoEscapeBraced', 97) ->
 18;
yeccgoto('AutoEscapeBraced', 102) ->
 18;
yeccgoto('AutoEscapeBraced', 110) ->
 18;
yeccgoto('AutoEscapeBraced', 113) ->
 18;
yeccgoto('AutoEscapeBraced', 119) ->
 18;
yeccgoto('AutoEscapeBraced', 122) ->
 18;
yeccgoto('AutoEscapeBraced', 128) ->
 18;
yeccgoto('BlockBlock', 1) ->
 17;
yeccgoto('BlockBlock', 82) ->
 17;
yeccgoto('BlockBlock', 87) ->
 17;
yeccgoto('BlockBlock', 92) ->
 17;
yeccgoto('BlockBlock', 97) ->
 17;
yeccgoto('BlockBlock', 102) ->
 17;
yeccgoto('BlockBlock', 110) ->
 17;
yeccgoto('BlockBlock', 113) ->
 17;
yeccgoto('BlockBlock', 119) ->
 17;
yeccgoto('BlockBlock', 122) ->
 17;
yeccgoto('BlockBlock', 128) ->
 17;
yeccgoto('BlockBraced', 1) ->
 16;
yeccgoto('BlockBraced', 82) ->
 16;
yeccgoto('BlockBraced', 87) ->
 16;
yeccgoto('BlockBraced', 92) ->
 16;
yeccgoto('BlockBraced', 97) ->
 16;
yeccgoto('BlockBraced', 102) ->
 16;
yeccgoto('BlockBraced', 110) ->
 16;
yeccgoto('BlockBraced', 113) ->
 16;
yeccgoto('BlockBraced', 119) ->
 16;
yeccgoto('BlockBraced', 122) ->
 16;
yeccgoto('BlockBraced', 128) ->
 16;
yeccgoto('CommentBlock', 1) ->
 15;
yeccgoto('CommentBlock', 82) ->
 15;
yeccgoto('CommentBlock', 87) ->
 15;
yeccgoto('CommentBlock', 92) ->
 15;
yeccgoto('CommentBlock', 97) ->
 15;
yeccgoto('CommentBlock', 102) ->
 15;
yeccgoto('CommentBlock', 110) ->
 15;
yeccgoto('CommentBlock', 113) ->
 15;
yeccgoto('CommentBlock', 119) ->
 15;
yeccgoto('CommentBlock', 122) ->
 15;
yeccgoto('CommentBlock', 128) ->
 15;
yeccgoto('CommentBraced', 1) ->
 14;
yeccgoto('CommentBraced', 82) ->
 14;
yeccgoto('CommentBraced', 87) ->
 14;
yeccgoto('CommentBraced', 92) ->
 14;
yeccgoto('CommentBraced', 97) ->
 14;
yeccgoto('CommentBraced', 102) ->
 14;
yeccgoto('CommentBraced', 110) ->
 14;
yeccgoto('CommentBraced', 113) ->
 14;
yeccgoto('CommentBraced', 119) ->
 14;
yeccgoto('CommentBraced', 122) ->
 14;
yeccgoto('CommentBraced', 128) ->
 14;
yeccgoto('CustomTag', 1) ->
 13;
yeccgoto('CustomTag', 82) ->
 13;
yeccgoto('CustomTag', 87) ->
 13;
yeccgoto('CustomTag', 92) ->
 13;
yeccgoto('CustomTag', 97) ->
 13;
yeccgoto('CustomTag', 102) ->
 13;
yeccgoto('CustomTag', 110) ->
 13;
yeccgoto('CustomTag', 113) ->
 13;
yeccgoto('CustomTag', 119) ->
 13;
yeccgoto('CustomTag', 122) ->
 13;
yeccgoto('CustomTag', 128) ->
 13;
yeccgoto('Elements', 0) ->
 1;
yeccgoto('Elements', 4) ->
 122;
yeccgoto('Elements', 6) ->
 113;
yeccgoto('Elements', 8) ->
 102;
yeccgoto('Elements', 10) ->
 97;
yeccgoto('Elements', 14) ->
 92;
yeccgoto('Elements', 16) ->
 87;
yeccgoto('Elements', 18) ->
 82;
yeccgoto('Elements', 104) ->
 110;
yeccgoto('Elements', 115) ->
 119;
yeccgoto('Elements', 124) ->
 128;
yeccgoto('ElseBraced', 102) ->
 104;
yeccgoto('ElseBraced', 113) ->
 115;
yeccgoto('ElseBraced', 122) ->
 124;
yeccgoto('EndAutoEscapeBraced', 82) ->
 83;
yeccgoto('EndBlockBraced', 87) ->
 88;
yeccgoto('EndCommentBraced', 92) ->
 93;
yeccgoto('EndForBraced', 97) ->
 98;
yeccgoto('EndIfBraced', 102) ->
 103;
yeccgoto('EndIfBraced', 110) ->
 111;
yeccgoto('EndIfEqualBraced', 113) ->
 114;
yeccgoto('EndIfEqualBraced', 119) ->
 120;
yeccgoto('EndIfNotEqualBraced', 122) ->
 123;
yeccgoto('EndIfNotEqualBraced', 128) ->
 129;
yeccgoto('ExtendsTag', 1) ->
 12;
yeccgoto('ExtendsTag', 82) ->
 12;
yeccgoto('ExtendsTag', 87) ->
 12;
yeccgoto('ExtendsTag', 92) ->
 12;
yeccgoto('ExtendsTag', 97) ->
 12;
yeccgoto('ExtendsTag', 102) ->
 12;
yeccgoto('ExtendsTag', 110) ->
 12;
yeccgoto('ExtendsTag', 113) ->
 12;
yeccgoto('ExtendsTag', 119) ->
 12;
yeccgoto('ExtendsTag', 122) ->
 12;
yeccgoto('ExtendsTag', 128) ->
 12;
yeccgoto('Filter', 30) ->
 31;
yeccgoto('ForBlock', 1) ->
 11;
yeccgoto('ForBlock', 82) ->
 11;
yeccgoto('ForBlock', 87) ->
 11;
yeccgoto('ForBlock', 92) ->
 11;
yeccgoto('ForBlock', 97) ->
 11;
yeccgoto('ForBlock', 102) ->
 11;
yeccgoto('ForBlock', 110) ->
 11;
yeccgoto('ForBlock', 113) ->
 11;
yeccgoto('ForBlock', 119) ->
 11;
yeccgoto('ForBlock', 122) ->
 11;
yeccgoto('ForBlock', 128) ->
 11;
yeccgoto('ForBraced', 1) ->
 10;
yeccgoto('ForBraced', 82) ->
 10;
yeccgoto('ForBraced', 87) ->
 10;
yeccgoto('ForBraced', 92) ->
 10;
yeccgoto('ForBraced', 97) ->
 10;
yeccgoto('ForBraced', 102) ->
 10;
yeccgoto('ForBraced', 110) ->
 10;
yeccgoto('ForBraced', 113) ->
 10;
yeccgoto('ForBraced', 119) ->
 10;
yeccgoto('ForBraced', 122) ->
 10;
yeccgoto('ForBraced', 128) ->
 10;
yeccgoto('ForExpression', 41) ->
 68;
yeccgoto('ForGroup', 41) ->
 67;
yeccgoto('IfBlock', 1) ->
 9;
yeccgoto('IfBlock', 82) ->
 9;
yeccgoto('IfBlock', 87) ->
 9;
yeccgoto('IfBlock', 92) ->
 9;
yeccgoto('IfBlock', 97) ->
 9;
yeccgoto('IfBlock', 102) ->
 9;
yeccgoto('IfBlock', 110) ->
 9;
yeccgoto('IfBlock', 113) ->
 9;
yeccgoto('IfBlock', 119) ->
 9;
yeccgoto('IfBlock', 122) ->
 9;
yeccgoto('IfBlock', 128) ->
 9;
yeccgoto('IfBraced', 1) ->
 8;
yeccgoto('IfBraced', 82) ->
 8;
yeccgoto('IfBraced', 87) ->
 8;
yeccgoto('IfBraced', 92) ->
 8;
yeccgoto('IfBraced', 97) ->
 8;
yeccgoto('IfBraced', 102) ->
 8;
yeccgoto('IfBraced', 110) ->
 8;
yeccgoto('IfBraced', 113) ->
 8;
yeccgoto('IfBraced', 119) ->
 8;
yeccgoto('IfBraced', 122) ->
 8;
yeccgoto('IfBraced', 128) ->
 8;
yeccgoto('IfEqualBlock', 1) ->
 7;
yeccgoto('IfEqualBlock', 82) ->
 7;
yeccgoto('IfEqualBlock', 87) ->
 7;
yeccgoto('IfEqualBlock', 92) ->
 7;
yeccgoto('IfEqualBlock', 97) ->
 7;
yeccgoto('IfEqualBlock', 102) ->
 7;
yeccgoto('IfEqualBlock', 110) ->
 7;
yeccgoto('IfEqualBlock', 113) ->
 7;
yeccgoto('IfEqualBlock', 119) ->
 7;
yeccgoto('IfEqualBlock', 122) ->
 7;
yeccgoto('IfEqualBlock', 128) ->
 7;
yeccgoto('IfEqualBraced', 1) ->
 6;
yeccgoto('IfEqualBraced', 82) ->
 6;
yeccgoto('IfEqualBraced', 87) ->
 6;
yeccgoto('IfEqualBraced', 92) ->
 6;
yeccgoto('IfEqualBraced', 97) ->
 6;
yeccgoto('IfEqualBraced', 102) ->
 6;
yeccgoto('IfEqualBraced', 110) ->
 6;
yeccgoto('IfEqualBraced', 113) ->
 6;
yeccgoto('IfEqualBraced', 119) ->
 6;
yeccgoto('IfEqualBraced', 122) ->
 6;
yeccgoto('IfEqualBraced', 128) ->
 6;
yeccgoto('IfEqualExpression', 44) ->
 54;
yeccgoto('IfExpression', 43) ->
 58;
yeccgoto('IfExpression', 59) ->
 60;
yeccgoto('IfNotEqualBlock', 1) ->
 5;
yeccgoto('IfNotEqualBlock', 82) ->
 5;
yeccgoto('IfNotEqualBlock', 87) ->
 5;
yeccgoto('IfNotEqualBlock', 92) ->
 5;
yeccgoto('IfNotEqualBlock', 97) ->
 5;
yeccgoto('IfNotEqualBlock', 102) ->
 5;
yeccgoto('IfNotEqualBlock', 110) ->
 5;
yeccgoto('IfNotEqualBlock', 113) ->
 5;
yeccgoto('IfNotEqualBlock', 119) ->
 5;
yeccgoto('IfNotEqualBlock', 122) ->
 5;
yeccgoto('IfNotEqualBlock', 128) ->
 5;
yeccgoto('IfNotEqualBraced', 1) ->
 4;
yeccgoto('IfNotEqualBraced', 82) ->
 4;
yeccgoto('IfNotEqualBraced', 87) ->
 4;
yeccgoto('IfNotEqualBraced', 92) ->
 4;
yeccgoto('IfNotEqualBraced', 97) ->
 4;
yeccgoto('IfNotEqualBraced', 102) ->
 4;
yeccgoto('IfNotEqualBraced', 110) ->
 4;
yeccgoto('IfNotEqualBraced', 113) ->
 4;
yeccgoto('IfNotEqualBraced', 119) ->
 4;
yeccgoto('IfNotEqualBraced', 122) ->
 4;
yeccgoto('IfNotEqualBraced', 128) ->
 4;
yeccgoto('IfNotEqualExpression', 45) ->
 50;
yeccgoto('IncludeTag', 1) ->
 3;
yeccgoto('IncludeTag', 82) ->
 3;
yeccgoto('IncludeTag', 87) ->
 3;
yeccgoto('IncludeTag', 92) ->
 3;
yeccgoto('IncludeTag', 97) ->
 3;
yeccgoto('IncludeTag', 102) ->
 3;
yeccgoto('IncludeTag', 110) ->
 3;
yeccgoto('IncludeTag', 113) ->
 3;
yeccgoto('IncludeTag', 119) ->
 3;
yeccgoto('IncludeTag', 122) ->
 3;
yeccgoto('IncludeTag', 128) ->
 3;
yeccgoto('Literal', 33) ->
 34;
yeccgoto('Variable', 21) ->
 23;
yeccgoto('Variable', 43) ->
 57;
yeccgoto('Variable', 44) ->
 53;
yeccgoto('Variable', 45) ->
 49;
yeccgoto('Variable', 50) ->
 51;
yeccgoto('Variable', 54) ->
 55;
yeccgoto('Variable', 59) ->
 57;
yeccgoto('Variable', 65) ->
 66;
yeccgoto('VariableBraced', 1) ->
 2;
yeccgoto('VariableBraced', 82) ->
 2;
yeccgoto('VariableBraced', 87) ->
 2;
yeccgoto('VariableBraced', 92) ->
 2;
yeccgoto('VariableBraced', 97) ->
 2;
yeccgoto('VariableBraced', 102) ->
 2;
yeccgoto('VariableBraced', 110) ->
 2;
yeccgoto('VariableBraced', 113) ->
 2;
yeccgoto('VariableBraced', 119) ->
 2;
yeccgoto('VariableBraced', 122) ->
 2;
yeccgoto('VariableBraced', 128) ->
 2;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_0_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_0_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_2_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 120).
yeccpars2_2_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_3_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 122).
yeccpars2_3_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_4_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_4_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_5_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_5_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_6_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_6_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_7_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 127).
yeccpars2_7_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_8_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 126).
yeccpars2_9_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_10_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 125).
yeccpars2_11_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 121).
yeccpars2_12_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 123).
yeccpars2_13_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_14_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 130).
yeccpars2_15_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_16_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 124).
yeccpars2_17_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_18_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 129).
yeccpars2_19_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 119).
yeccpars2_22_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_24_([__1 | __Stack]) ->
 [begin
   { variable , { __1 } }
  end | __Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 136).
yeccpars2_28_([__3,__2,__1 | __Stack]) ->
 [begin
   { variable , { __1 , __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 132).
yeccpars2_29_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 134).
yeccpars2_31_([__3,__2,__1 | __Stack]) ->
 [begin
   { apply_filter , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 189).
yeccpars2_32_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 190).
yeccpars2_34_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 145).
yeccpars2_42_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 141).
yeccpars2_48_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { include , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 181).
yeccpars2_52_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 , __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 175).
yeccpars2_56_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 , __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 167).
yeccpars2_60_([__2,__1 | __Stack]) ->
 [begin
   { 'not' , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 166).
yeccpars2_61_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 143).
yeccpars2_63_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { tag , __2 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 146).
yeccpars2_66_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ { __2 , __4 } ]
  end | __Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 157).
yeccpars2_70_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 160).
yeccpars2_73_([__3,__2,__1 | __Stack]) ->
 [begin
   { in , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 162).
yeccpars2_74_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 140).
yeccpars2_76_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { extends , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_77_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 149).
yeccpars2_79_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 186).
yeccpars2_81_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_83_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 185).
yeccpars2_83_([__3,__2,__1 | __Stack]) ->
 [begin
   { autoescape , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_86_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_88_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 148).
yeccpars2_88_([__3,__2,__1 | __Stack]) ->
 [begin
   { block , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_91_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_91_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 152).
yeccpars2_93_([__3,__2,__1 | __Stack]) ->
 [begin
   { comment , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_96_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_96_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 156).
yeccpars2_98_([__3,__2,__1 | __Stack]) ->
 [begin
   { for , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_101_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_103_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 165).
yeccpars2_103_([__3,__2,__1 | __Stack]) ->
 [begin
   { 'if' , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_104_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_108_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_109_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_111_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 164).
yeccpars2_111_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { ifelse , __1 , __2 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_114_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 174).
yeccpars2_114_([__3,__2,__1 | __Stack]) ->
 [begin
   { ifequal , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_115_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_115_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_118_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_118_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_120_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 173).
yeccpars2_120_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { ifequalelse , __1 , __2 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_123_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 180).
yeccpars2_123_([__3,__2,__1 | __Stack]) ->
 [begin
   { ifnotequal , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_124_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_124_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_127_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_127_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_129_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 179).
yeccpars2_129_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { ifnotequalelse , __1 , __2 , __4 }
  end | __Stack].


