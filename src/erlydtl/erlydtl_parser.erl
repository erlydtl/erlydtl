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
 yeccpars1(__Ts, __Tzr, 18, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [1 | __Ss], [__T | __Stack]);
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
 yeccpars2(106, __Cat, [4 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(5, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_5_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_6_(__Stack),
 yeccpars2(95, __Cat, [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_7_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_8_(__Stack),
 yeccpars2(90, __Cat, [8 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_9_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_10_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_11_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_12_(__Stack),
 yeccpars2(85, __Cat, [12 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_13_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_14_(__Stack),
 yeccpars2(80, __Cat, [14 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_15_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_16_(__Stack),
 yeccpars2(75, __Cat, [16 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_17_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(18, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(19, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(20, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_20_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Elements', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(21, close_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(22, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_22_(__Stack),
 yeccpars2(yeccgoto('Variable', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Variable', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(24, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Variable', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(25, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_26_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Variable', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_27_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('VariableBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(28, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(29, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_29_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Variable', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(30, colon, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_30_(__Stack),
 yeccpars2(yeccgoto('Filter', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(31, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_32_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Filter', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(33, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Literal', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(34, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Literal', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(35, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(36, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(37, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(38, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(39, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(40, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_40_(__Stack),
 yeccpars2(55, __Cat, [40 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(41, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, not_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(42, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(43, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(44, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(45, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_45_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('IncludeTag', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(46, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('IfEqualExpression', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(47, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(48, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(49, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_49_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('IfEqualBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(50, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('IfExpression', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(51, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(52, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, not_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(53, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_53_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('IfExpression', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(54, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_54_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('IfBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(55, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(56, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_56_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('CustomTag', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(57, equal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(58, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [58 | __Ss], [__T | __Stack]);
yeccpars2(58, number_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [58 | __Ss], [__T | __Stack]);
yeccpars2(58, string_literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [58 | __Ss], [__T | __Stack]);
yeccpars2(58, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(59, pipe, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_59_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('Args', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(60, comma, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, in_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(61, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(62, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('ForExpression', hd(__Ss)), close_tag, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(62, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('ForGroup', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(63, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_63_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('ForBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(64, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(65, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(66, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_66_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ForExpression', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(67, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_67_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ForGroup', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(68, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(69, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_69_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('ExtendsTag', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(70, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_70_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('CommentBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(71, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(72, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_72_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('BlockBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(73, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(74, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_74_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('AutoEscapeBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(75, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(76, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_76_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('AutoEscapeBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(77, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, endautoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(78, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(79, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_79_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndAutoEscapeBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(80, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_81_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('BlockBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(82, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, endblock_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(83, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(84, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_84_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndBlockBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(85, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(86, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_86_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('CommentBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(87, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, endcomment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(88, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(89, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_89_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndCommentBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(90, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(91, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_91_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ForBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(92, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, endfor_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(93, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(94, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_94_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndForBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(95, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(96, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_96_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('IfBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(97, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_97_(__Stack),
 yeccpars2(103, __Cat, [97 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(98, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, else_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, endif_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(99, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(100, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(101, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_101_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndIfBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(102, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_102_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ElseBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(103, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [103 | __Ss], [__T | __Stack]);
yeccpars2(103, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [103 | __Ss], [__T | __Stack]);
yeccpars2(103, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [103 | __Ss], [__T | __Stack]);
yeccpars2(103, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(104, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_104_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('IfBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(105, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, endif_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(106, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(107, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_107_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('IfEqualBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(108, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_108_(__Stack),
 yeccpars2(112, __Cat, [108 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(109, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, else_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, endifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(110, close_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(111, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_111_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EndIfEqualBraced', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(112, open_tag, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, open_var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, text, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(113, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_113_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('IfEqualBlock', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(114, autoescape_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, block_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, comment_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, endifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, extends_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, for_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, identifier, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, if_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, ifequal_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, include_keyword, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto('Args', 40) ->
 55;
yeccgoto('AutoEscapeBlock', 1) ->
 17;
yeccgoto('AutoEscapeBlock', 75) ->
 17;
yeccgoto('AutoEscapeBlock', 80) ->
 17;
yeccgoto('AutoEscapeBlock', 85) ->
 17;
yeccgoto('AutoEscapeBlock', 90) ->
 17;
yeccgoto('AutoEscapeBlock', 95) ->
 17;
yeccgoto('AutoEscapeBlock', 103) ->
 17;
yeccgoto('AutoEscapeBlock', 106) ->
 17;
yeccgoto('AutoEscapeBlock', 112) ->
 17;
yeccgoto('AutoEscapeBraced', 1) ->
 16;
yeccgoto('AutoEscapeBraced', 75) ->
 16;
yeccgoto('AutoEscapeBraced', 80) ->
 16;
yeccgoto('AutoEscapeBraced', 85) ->
 16;
yeccgoto('AutoEscapeBraced', 90) ->
 16;
yeccgoto('AutoEscapeBraced', 95) ->
 16;
yeccgoto('AutoEscapeBraced', 103) ->
 16;
yeccgoto('AutoEscapeBraced', 106) ->
 16;
yeccgoto('AutoEscapeBraced', 112) ->
 16;
yeccgoto('BlockBlock', 1) ->
 15;
yeccgoto('BlockBlock', 75) ->
 15;
yeccgoto('BlockBlock', 80) ->
 15;
yeccgoto('BlockBlock', 85) ->
 15;
yeccgoto('BlockBlock', 90) ->
 15;
yeccgoto('BlockBlock', 95) ->
 15;
yeccgoto('BlockBlock', 103) ->
 15;
yeccgoto('BlockBlock', 106) ->
 15;
yeccgoto('BlockBlock', 112) ->
 15;
yeccgoto('BlockBraced', 1) ->
 14;
yeccgoto('BlockBraced', 75) ->
 14;
yeccgoto('BlockBraced', 80) ->
 14;
yeccgoto('BlockBraced', 85) ->
 14;
yeccgoto('BlockBraced', 90) ->
 14;
yeccgoto('BlockBraced', 95) ->
 14;
yeccgoto('BlockBraced', 103) ->
 14;
yeccgoto('BlockBraced', 106) ->
 14;
yeccgoto('BlockBraced', 112) ->
 14;
yeccgoto('CommentBlock', 1) ->
 13;
yeccgoto('CommentBlock', 75) ->
 13;
yeccgoto('CommentBlock', 80) ->
 13;
yeccgoto('CommentBlock', 85) ->
 13;
yeccgoto('CommentBlock', 90) ->
 13;
yeccgoto('CommentBlock', 95) ->
 13;
yeccgoto('CommentBlock', 103) ->
 13;
yeccgoto('CommentBlock', 106) ->
 13;
yeccgoto('CommentBlock', 112) ->
 13;
yeccgoto('CommentBraced', 1) ->
 12;
yeccgoto('CommentBraced', 75) ->
 12;
yeccgoto('CommentBraced', 80) ->
 12;
yeccgoto('CommentBraced', 85) ->
 12;
yeccgoto('CommentBraced', 90) ->
 12;
yeccgoto('CommentBraced', 95) ->
 12;
yeccgoto('CommentBraced', 103) ->
 12;
yeccgoto('CommentBraced', 106) ->
 12;
yeccgoto('CommentBraced', 112) ->
 12;
yeccgoto('CustomTag', 1) ->
 11;
yeccgoto('CustomTag', 75) ->
 11;
yeccgoto('CustomTag', 80) ->
 11;
yeccgoto('CustomTag', 85) ->
 11;
yeccgoto('CustomTag', 90) ->
 11;
yeccgoto('CustomTag', 95) ->
 11;
yeccgoto('CustomTag', 103) ->
 11;
yeccgoto('CustomTag', 106) ->
 11;
yeccgoto('CustomTag', 112) ->
 11;
yeccgoto('Elements', 0) ->
 1;
yeccgoto('Elements', 4) ->
 106;
yeccgoto('Elements', 6) ->
 95;
yeccgoto('Elements', 8) ->
 90;
yeccgoto('Elements', 12) ->
 85;
yeccgoto('Elements', 14) ->
 80;
yeccgoto('Elements', 16) ->
 75;
yeccgoto('Elements', 97) ->
 103;
yeccgoto('Elements', 108) ->
 112;
yeccgoto('ElseBraced', 95) ->
 97;
yeccgoto('ElseBraced', 106) ->
 108;
yeccgoto('EndAutoEscapeBraced', 75) ->
 76;
yeccgoto('EndBlockBraced', 80) ->
 81;
yeccgoto('EndCommentBraced', 85) ->
 86;
yeccgoto('EndForBraced', 90) ->
 91;
yeccgoto('EndIfBraced', 95) ->
 96;
yeccgoto('EndIfBraced', 103) ->
 104;
yeccgoto('EndIfEqualBraced', 106) ->
 107;
yeccgoto('EndIfEqualBraced', 112) ->
 113;
yeccgoto('ExtendsTag', 1) ->
 10;
yeccgoto('ExtendsTag', 75) ->
 10;
yeccgoto('ExtendsTag', 80) ->
 10;
yeccgoto('ExtendsTag', 85) ->
 10;
yeccgoto('ExtendsTag', 90) ->
 10;
yeccgoto('ExtendsTag', 95) ->
 10;
yeccgoto('ExtendsTag', 103) ->
 10;
yeccgoto('ExtendsTag', 106) ->
 10;
yeccgoto('ExtendsTag', 112) ->
 10;
yeccgoto('Filter', 28) ->
 29;
yeccgoto('ForBlock', 1) ->
 9;
yeccgoto('ForBlock', 75) ->
 9;
yeccgoto('ForBlock', 80) ->
 9;
yeccgoto('ForBlock', 85) ->
 9;
yeccgoto('ForBlock', 90) ->
 9;
yeccgoto('ForBlock', 95) ->
 9;
yeccgoto('ForBlock', 103) ->
 9;
yeccgoto('ForBlock', 106) ->
 9;
yeccgoto('ForBlock', 112) ->
 9;
yeccgoto('ForBraced', 1) ->
 8;
yeccgoto('ForBraced', 75) ->
 8;
yeccgoto('ForBraced', 80) ->
 8;
yeccgoto('ForBraced', 85) ->
 8;
yeccgoto('ForBraced', 90) ->
 8;
yeccgoto('ForBraced', 95) ->
 8;
yeccgoto('ForBraced', 103) ->
 8;
yeccgoto('ForBraced', 106) ->
 8;
yeccgoto('ForBraced', 112) ->
 8;
yeccgoto('ForExpression', 39) ->
 61;
yeccgoto('ForGroup', 39) ->
 60;
yeccgoto('IfBlock', 1) ->
 7;
yeccgoto('IfBlock', 75) ->
 7;
yeccgoto('IfBlock', 80) ->
 7;
yeccgoto('IfBlock', 85) ->
 7;
yeccgoto('IfBlock', 90) ->
 7;
yeccgoto('IfBlock', 95) ->
 7;
yeccgoto('IfBlock', 103) ->
 7;
yeccgoto('IfBlock', 106) ->
 7;
yeccgoto('IfBlock', 112) ->
 7;
yeccgoto('IfBraced', 1) ->
 6;
yeccgoto('IfBraced', 75) ->
 6;
yeccgoto('IfBraced', 80) ->
 6;
yeccgoto('IfBraced', 85) ->
 6;
yeccgoto('IfBraced', 90) ->
 6;
yeccgoto('IfBraced', 95) ->
 6;
yeccgoto('IfBraced', 103) ->
 6;
yeccgoto('IfBraced', 106) ->
 6;
yeccgoto('IfBraced', 112) ->
 6;
yeccgoto('IfEqualBlock', 1) ->
 5;
yeccgoto('IfEqualBlock', 75) ->
 5;
yeccgoto('IfEqualBlock', 80) ->
 5;
yeccgoto('IfEqualBlock', 85) ->
 5;
yeccgoto('IfEqualBlock', 90) ->
 5;
yeccgoto('IfEqualBlock', 95) ->
 5;
yeccgoto('IfEqualBlock', 103) ->
 5;
yeccgoto('IfEqualBlock', 106) ->
 5;
yeccgoto('IfEqualBlock', 112) ->
 5;
yeccgoto('IfEqualBraced', 1) ->
 4;
yeccgoto('IfEqualBraced', 75) ->
 4;
yeccgoto('IfEqualBraced', 80) ->
 4;
yeccgoto('IfEqualBraced', 85) ->
 4;
yeccgoto('IfEqualBraced', 90) ->
 4;
yeccgoto('IfEqualBraced', 95) ->
 4;
yeccgoto('IfEqualBraced', 103) ->
 4;
yeccgoto('IfEqualBraced', 106) ->
 4;
yeccgoto('IfEqualBraced', 112) ->
 4;
yeccgoto('IfEqualExpression', 42) ->
 47;
yeccgoto('IfExpression', 41) ->
 51;
yeccgoto('IfExpression', 52) ->
 53;
yeccgoto('IncludeTag', 1) ->
 3;
yeccgoto('IncludeTag', 75) ->
 3;
yeccgoto('IncludeTag', 80) ->
 3;
yeccgoto('IncludeTag', 85) ->
 3;
yeccgoto('IncludeTag', 90) ->
 3;
yeccgoto('IncludeTag', 95) ->
 3;
yeccgoto('IncludeTag', 103) ->
 3;
yeccgoto('IncludeTag', 106) ->
 3;
yeccgoto('IncludeTag', 112) ->
 3;
yeccgoto('Literal', 31) ->
 32;
yeccgoto('Variable', 19) ->
 21;
yeccgoto('Variable', 41) ->
 50;
yeccgoto('Variable', 42) ->
 46;
yeccgoto('Variable', 47) ->
 48;
yeccgoto('Variable', 52) ->
 50;
yeccgoto('Variable', 58) ->
 59;
yeccgoto('VariableBraced', 1) ->
 2;
yeccgoto('VariableBraced', 75) ->
 2;
yeccgoto('VariableBraced', 80) ->
 2;
yeccgoto('VariableBraced', 85) ->
 2;
yeccgoto('VariableBraced', 90) ->
 2;
yeccgoto('VariableBraced', 95) ->
 2;
yeccgoto('VariableBraced', 103) ->
 2;
yeccgoto('VariableBraced', 106) ->
 2;
yeccgoto('VariableBraced', 112) ->
 2;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_0_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 111).
yeccpars2_0_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_2_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 113).
yeccpars2_2_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_3_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 115).
yeccpars2_3_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_4_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 111).
yeccpars2_4_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_5_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 120).
yeccpars2_5_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_6_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 111).
yeccpars2_6_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_7_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 119).
yeccpars2_7_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 111).
yeccpars2_8_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 118).
yeccpars2_9_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 114).
yeccpars2_10_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 116).
yeccpars2_11_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 111).
yeccpars2_12_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 122).
yeccpars2_13_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 111).
yeccpars2_14_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 117).
yeccpars2_15_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 111).
yeccpars2_16_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 121).
yeccpars2_17_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 112).
yeccpars2_20_([__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 127).
yeccpars2_22_([__1 | __Stack]) ->
 [begin
   { variable , { __1 } }
  end | __Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_26_([__3,__2,__1 | __Stack]) ->
 [begin
   { variable , { __1 , __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 124).
yeccpars2_27_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 126).
yeccpars2_29_([__3,__2,__1 | __Stack]) ->
 [begin
   { apply_filter , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 175).
yeccpars2_30_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 176).
yeccpars2_32_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_40_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 137).
yeccpars2_40_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 133).
yeccpars2_45_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { include , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 167).
yeccpars2_49_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 , __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 159).
yeccpars2_53_([__2,__1 | __Stack]) ->
 [begin
   { 'not' , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 158).
yeccpars2_54_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_56_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { tag , __2 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 138).
yeccpars2_59_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ { __2 , __4 } ]
  end | __Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 149).
yeccpars2_63_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 152).
yeccpars2_66_([__3,__2,__1 | __Stack]) ->
 [begin
   { in , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 154).
yeccpars2_67_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 132).
yeccpars2_69_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { extends , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_70_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 141).
yeccpars2_72_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 172).
yeccpars2_74_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 171).
yeccpars2_76_([__3,__2,__1 | __Stack]) ->
 [begin
   { autoescape , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_79_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 140).
yeccpars2_81_([__3,__2,__1 | __Stack]) ->
 [begin
   { block , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_84_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 144).
yeccpars2_86_([__3,__2,__1 | __Stack]) ->
 [begin
   { comment , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_89_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_89_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_91_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 148).
yeccpars2_91_([__3,__2,__1 | __Stack]) ->
 [begin
   { for , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_94_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_94_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_96_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 157).
yeccpars2_96_([__3,__2,__1 | __Stack]) ->
 [begin
   { 'if' , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_97_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 111).
yeccpars2_97_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_101_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_102_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_102_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 156).
yeccpars2_104_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { ifelse , __1 , __2 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_107_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 166).
yeccpars2_107_([__3,__2,__1 | __Stack]) ->
 [begin
   { ifequal , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 111).
yeccpars2_108_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_111_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_111_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_113_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 165).
yeccpars2_113_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { ifequalelse , __1 , __2 , __4 }
  end | __Stack].


