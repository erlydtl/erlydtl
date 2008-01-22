-module(erlydtl_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).

-file("/usr/local/lib/erlang/lib/parsetools-1.4.2/include/yeccpre.hrl", 0).
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
-spec(return_error/2 :: (integer(), any()) -> no_return()).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.2").

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, State} ->
                    Desc = {State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
            Error % probably from return_error/2
    end.

yecc_error_type(function_clause, [{?MODULE,F,[_,_,_,_,Token,_,_]} | _]) ->
    "yeccpars2" ++ _ = atom_to_list(F),
    {syntax_error, Token};
yecc_error_type({case_clause,{State}}, [{?MODULE,yeccpars2,_}|_]) ->
    %% Inlined goto-function
    {missing_in_goto_table, State};
yecc_error_type(function_clause, [{?MODULE,F,[State]}|_]) ->
    "yeccgoto_" ++ SymbolL = atom_to_list(F),
    {ok,[{atom,_,Symbol}]} = erl_scan:string(SymbolL),
    {missing_in_goto_table, Symbol, State}.

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

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Stack1, [Token | Tokens], 
          Tokenizer) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Stack1 | Vstack], Token, Tokens, Tokenizer);
yeccpars1(State1, State, States, Vstack, Stack1, [], {F, A}) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(State1, State, States, Vstack, Stack1, Tokens, {F, A});
        {eof, _Endline} ->
            yeccpars1(State1, State, States, Vstack, Stack1, [], false);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1(State1, State, States, Vstack, Stack1, [], false) ->
    yeccpars2(State, '$end', [State1 | States], [Stack1 | Vstack],
              {'$end', 999999}, [], false).

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



-file("erlydtl_parser.erl", 148).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.2",{missing_state_in_action_table, Other}}).

yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2(1, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_1(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccpars2(122, Cat, [4 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccpars2(113, Cat, [6 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccpars2(102, Cat, [8 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccpars2(97, Cat, [10 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_13_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 yeccpars2(92, Cat, [14 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccpars2(87, Cat, [16 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 yeccpars2(82, Cat, [18 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_20(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, number_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_23(S, close_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 yeccpars2('yeccgoto_\'Variable\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Variable\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Variable\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Variable\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'VariableBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_30(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Variable\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_32(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 yeccpars2('yeccgoto_\'Filter\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_33(S, number_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Filter\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr).

yeccpars2_39(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccpars2(62, Cat, [42 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_43(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, not_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, number_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_44: see yeccpars2_21

%% yeccpars2_45: see yeccpars2_21

yeccpars2_46(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'IncludeTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_49(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'IfNotEqualExpression\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_50: see yeccpars2_21

yeccpars2_51(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_52_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfNotEqualBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_53(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'IfEqualExpression\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_54: see yeccpars2_21

yeccpars2_55(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_56_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfEqualBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_57(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'IfExpression\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_59: see yeccpars2_43

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_60_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'IfExpression\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_61_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'IfBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_62(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_63_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'CustomTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_64(S, equal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_65: see yeccpars2_21

yeccpars2_66(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'Args\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_67(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, in_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr).

yeccpars2_68(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr).

yeccpars2_69(_S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'ForExpression\''(hd(Ss)), close_tag, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_69_(Stack),
 yeccpars2('yeccgoto_\'ForGroup\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_70_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'ForBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_71(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr).

yeccpars2_72(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_73_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ForExpression\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_74_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ForGroup\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_75(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'ExtendsTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_77_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CommentBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_78(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_79_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'BlockBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_80(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'AutoEscapeBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_82(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'AutoEscapeBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_84(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, endautoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr).

yeccpars2_85(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_86_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndAutoEscapeBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_87(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_88_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'BlockBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_89(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, endblock_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr).

yeccpars2_90(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_91_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndBlockBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_92(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CommentBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_94(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, endcomment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr).

yeccpars2_95(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndCommentBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_97(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ForBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_99(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, endfor_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr).

yeccpars2_100(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr).

yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndForBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_102(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'IfBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 yeccpars2(110, Cat, [104 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_105(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr).

yeccpars2_106(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr).

yeccpars2_107(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr).

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndIfBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ElseBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_110(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_111_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_112(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr).

yeccpars2_113(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'IfEqualBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_115_(Stack),
 yeccpars2(119, Cat, [115 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_116(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, endifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr).

yeccpars2_117(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr).

yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_118_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndIfEqualBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_119(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_120_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfEqualBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_121(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, endifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_123_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'IfNotEqualBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_124_(Stack),
 yeccpars2(128, Cat, [124 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_125(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, endifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr).

yeccpars2_126(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr).

yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_127_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndIfNotEqualBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_128(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_129_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfNotEqualBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_130(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, endifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Args\''(42) -> 62.

'yeccgoto_\'AutoEscapeBlock\''(1) -> 19;
'yeccgoto_\'AutoEscapeBlock\''(82) -> 19;
'yeccgoto_\'AutoEscapeBlock\''(87) -> 19;
'yeccgoto_\'AutoEscapeBlock\''(92) -> 19;
'yeccgoto_\'AutoEscapeBlock\''(97) -> 19;
'yeccgoto_\'AutoEscapeBlock\''(102) -> 19;
'yeccgoto_\'AutoEscapeBlock\''(110) -> 19;
'yeccgoto_\'AutoEscapeBlock\''(113) -> 19;
'yeccgoto_\'AutoEscapeBlock\''(119) -> 19;
'yeccgoto_\'AutoEscapeBlock\''(122) -> 19;
'yeccgoto_\'AutoEscapeBlock\''(128) -> 19.

'yeccgoto_\'AutoEscapeBraced\''(1) -> 18;
'yeccgoto_\'AutoEscapeBraced\''(82) -> 18;
'yeccgoto_\'AutoEscapeBraced\''(87) -> 18;
'yeccgoto_\'AutoEscapeBraced\''(92) -> 18;
'yeccgoto_\'AutoEscapeBraced\''(97) -> 18;
'yeccgoto_\'AutoEscapeBraced\''(102) -> 18;
'yeccgoto_\'AutoEscapeBraced\''(110) -> 18;
'yeccgoto_\'AutoEscapeBraced\''(113) -> 18;
'yeccgoto_\'AutoEscapeBraced\''(119) -> 18;
'yeccgoto_\'AutoEscapeBraced\''(122) -> 18;
'yeccgoto_\'AutoEscapeBraced\''(128) -> 18.

'yeccgoto_\'BlockBlock\''(1) -> 17;
'yeccgoto_\'BlockBlock\''(82) -> 17;
'yeccgoto_\'BlockBlock\''(87) -> 17;
'yeccgoto_\'BlockBlock\''(92) -> 17;
'yeccgoto_\'BlockBlock\''(97) -> 17;
'yeccgoto_\'BlockBlock\''(102) -> 17;
'yeccgoto_\'BlockBlock\''(110) -> 17;
'yeccgoto_\'BlockBlock\''(113) -> 17;
'yeccgoto_\'BlockBlock\''(119) -> 17;
'yeccgoto_\'BlockBlock\''(122) -> 17;
'yeccgoto_\'BlockBlock\''(128) -> 17.

'yeccgoto_\'BlockBraced\''(1) -> 16;
'yeccgoto_\'BlockBraced\''(82) -> 16;
'yeccgoto_\'BlockBraced\''(87) -> 16;
'yeccgoto_\'BlockBraced\''(92) -> 16;
'yeccgoto_\'BlockBraced\''(97) -> 16;
'yeccgoto_\'BlockBraced\''(102) -> 16;
'yeccgoto_\'BlockBraced\''(110) -> 16;
'yeccgoto_\'BlockBraced\''(113) -> 16;
'yeccgoto_\'BlockBraced\''(119) -> 16;
'yeccgoto_\'BlockBraced\''(122) -> 16;
'yeccgoto_\'BlockBraced\''(128) -> 16.

'yeccgoto_\'CommentBlock\''(1) -> 15;
'yeccgoto_\'CommentBlock\''(82) -> 15;
'yeccgoto_\'CommentBlock\''(87) -> 15;
'yeccgoto_\'CommentBlock\''(92) -> 15;
'yeccgoto_\'CommentBlock\''(97) -> 15;
'yeccgoto_\'CommentBlock\''(102) -> 15;
'yeccgoto_\'CommentBlock\''(110) -> 15;
'yeccgoto_\'CommentBlock\''(113) -> 15;
'yeccgoto_\'CommentBlock\''(119) -> 15;
'yeccgoto_\'CommentBlock\''(122) -> 15;
'yeccgoto_\'CommentBlock\''(128) -> 15.

'yeccgoto_\'CommentBraced\''(1) -> 14;
'yeccgoto_\'CommentBraced\''(82) -> 14;
'yeccgoto_\'CommentBraced\''(87) -> 14;
'yeccgoto_\'CommentBraced\''(92) -> 14;
'yeccgoto_\'CommentBraced\''(97) -> 14;
'yeccgoto_\'CommentBraced\''(102) -> 14;
'yeccgoto_\'CommentBraced\''(110) -> 14;
'yeccgoto_\'CommentBraced\''(113) -> 14;
'yeccgoto_\'CommentBraced\''(119) -> 14;
'yeccgoto_\'CommentBraced\''(122) -> 14;
'yeccgoto_\'CommentBraced\''(128) -> 14.

'yeccgoto_\'CustomTag\''(1) -> 13;
'yeccgoto_\'CustomTag\''(82) -> 13;
'yeccgoto_\'CustomTag\''(87) -> 13;
'yeccgoto_\'CustomTag\''(92) -> 13;
'yeccgoto_\'CustomTag\''(97) -> 13;
'yeccgoto_\'CustomTag\''(102) -> 13;
'yeccgoto_\'CustomTag\''(110) -> 13;
'yeccgoto_\'CustomTag\''(113) -> 13;
'yeccgoto_\'CustomTag\''(119) -> 13;
'yeccgoto_\'CustomTag\''(122) -> 13;
'yeccgoto_\'CustomTag\''(128) -> 13.

'yeccgoto_\'Elements\''(0) -> 1;
'yeccgoto_\'Elements\''(4) -> 122;
'yeccgoto_\'Elements\''(6) -> 113;
'yeccgoto_\'Elements\''(8) -> 102;
'yeccgoto_\'Elements\''(10) -> 97;
'yeccgoto_\'Elements\''(14) -> 92;
'yeccgoto_\'Elements\''(16) -> 87;
'yeccgoto_\'Elements\''(18) -> 82;
'yeccgoto_\'Elements\''(104) -> 110;
'yeccgoto_\'Elements\''(115) -> 119;
'yeccgoto_\'Elements\''(124) -> 128.

'yeccgoto_\'ElseBraced\''(102) -> 104;
'yeccgoto_\'ElseBraced\''(113) -> 115;
'yeccgoto_\'ElseBraced\''(122) -> 124.

'yeccgoto_\'EndAutoEscapeBraced\''(82) -> 83.

'yeccgoto_\'EndBlockBraced\''(87) -> 88.

'yeccgoto_\'EndCommentBraced\''(92) -> 93.

'yeccgoto_\'EndForBraced\''(97) -> 98.

'yeccgoto_\'EndIfBraced\''(102) -> 103;
'yeccgoto_\'EndIfBraced\''(110) -> 111.

'yeccgoto_\'EndIfEqualBraced\''(113) -> 114;
'yeccgoto_\'EndIfEqualBraced\''(119) -> 120.

'yeccgoto_\'EndIfNotEqualBraced\''(122) -> 123;
'yeccgoto_\'EndIfNotEqualBraced\''(128) -> 129.

'yeccgoto_\'ExtendsTag\''(1) -> 12;
'yeccgoto_\'ExtendsTag\''(82) -> 12;
'yeccgoto_\'ExtendsTag\''(87) -> 12;
'yeccgoto_\'ExtendsTag\''(92) -> 12;
'yeccgoto_\'ExtendsTag\''(97) -> 12;
'yeccgoto_\'ExtendsTag\''(102) -> 12;
'yeccgoto_\'ExtendsTag\''(110) -> 12;
'yeccgoto_\'ExtendsTag\''(113) -> 12;
'yeccgoto_\'ExtendsTag\''(119) -> 12;
'yeccgoto_\'ExtendsTag\''(122) -> 12;
'yeccgoto_\'ExtendsTag\''(128) -> 12.

'yeccgoto_\'Filter\''(30) -> 31.

'yeccgoto_\'ForBlock\''(1) -> 11;
'yeccgoto_\'ForBlock\''(82) -> 11;
'yeccgoto_\'ForBlock\''(87) -> 11;
'yeccgoto_\'ForBlock\''(92) -> 11;
'yeccgoto_\'ForBlock\''(97) -> 11;
'yeccgoto_\'ForBlock\''(102) -> 11;
'yeccgoto_\'ForBlock\''(110) -> 11;
'yeccgoto_\'ForBlock\''(113) -> 11;
'yeccgoto_\'ForBlock\''(119) -> 11;
'yeccgoto_\'ForBlock\''(122) -> 11;
'yeccgoto_\'ForBlock\''(128) -> 11.

'yeccgoto_\'ForBraced\''(1) -> 10;
'yeccgoto_\'ForBraced\''(82) -> 10;
'yeccgoto_\'ForBraced\''(87) -> 10;
'yeccgoto_\'ForBraced\''(92) -> 10;
'yeccgoto_\'ForBraced\''(97) -> 10;
'yeccgoto_\'ForBraced\''(102) -> 10;
'yeccgoto_\'ForBraced\''(110) -> 10;
'yeccgoto_\'ForBraced\''(113) -> 10;
'yeccgoto_\'ForBraced\''(119) -> 10;
'yeccgoto_\'ForBraced\''(122) -> 10;
'yeccgoto_\'ForBraced\''(128) -> 10.

'yeccgoto_\'ForExpression\''(41) -> 68.

'yeccgoto_\'ForGroup\''(41) -> 67.

'yeccgoto_\'IfBlock\''(1) -> 9;
'yeccgoto_\'IfBlock\''(82) -> 9;
'yeccgoto_\'IfBlock\''(87) -> 9;
'yeccgoto_\'IfBlock\''(92) -> 9;
'yeccgoto_\'IfBlock\''(97) -> 9;
'yeccgoto_\'IfBlock\''(102) -> 9;
'yeccgoto_\'IfBlock\''(110) -> 9;
'yeccgoto_\'IfBlock\''(113) -> 9;
'yeccgoto_\'IfBlock\''(119) -> 9;
'yeccgoto_\'IfBlock\''(122) -> 9;
'yeccgoto_\'IfBlock\''(128) -> 9.

'yeccgoto_\'IfBraced\''(1) -> 8;
'yeccgoto_\'IfBraced\''(82) -> 8;
'yeccgoto_\'IfBraced\''(87) -> 8;
'yeccgoto_\'IfBraced\''(92) -> 8;
'yeccgoto_\'IfBraced\''(97) -> 8;
'yeccgoto_\'IfBraced\''(102) -> 8;
'yeccgoto_\'IfBraced\''(110) -> 8;
'yeccgoto_\'IfBraced\''(113) -> 8;
'yeccgoto_\'IfBraced\''(119) -> 8;
'yeccgoto_\'IfBraced\''(122) -> 8;
'yeccgoto_\'IfBraced\''(128) -> 8.

'yeccgoto_\'IfEqualBlock\''(1) -> 7;
'yeccgoto_\'IfEqualBlock\''(82) -> 7;
'yeccgoto_\'IfEqualBlock\''(87) -> 7;
'yeccgoto_\'IfEqualBlock\''(92) -> 7;
'yeccgoto_\'IfEqualBlock\''(97) -> 7;
'yeccgoto_\'IfEqualBlock\''(102) -> 7;
'yeccgoto_\'IfEqualBlock\''(110) -> 7;
'yeccgoto_\'IfEqualBlock\''(113) -> 7;
'yeccgoto_\'IfEqualBlock\''(119) -> 7;
'yeccgoto_\'IfEqualBlock\''(122) -> 7;
'yeccgoto_\'IfEqualBlock\''(128) -> 7.

'yeccgoto_\'IfEqualBraced\''(1) -> 6;
'yeccgoto_\'IfEqualBraced\''(82) -> 6;
'yeccgoto_\'IfEqualBraced\''(87) -> 6;
'yeccgoto_\'IfEqualBraced\''(92) -> 6;
'yeccgoto_\'IfEqualBraced\''(97) -> 6;
'yeccgoto_\'IfEqualBraced\''(102) -> 6;
'yeccgoto_\'IfEqualBraced\''(110) -> 6;
'yeccgoto_\'IfEqualBraced\''(113) -> 6;
'yeccgoto_\'IfEqualBraced\''(119) -> 6;
'yeccgoto_\'IfEqualBraced\''(122) -> 6;
'yeccgoto_\'IfEqualBraced\''(128) -> 6.

'yeccgoto_\'IfEqualExpression\''(44) -> 54.

'yeccgoto_\'IfExpression\''(43) -> 58;
'yeccgoto_\'IfExpression\''(59) -> 60.

'yeccgoto_\'IfNotEqualBlock\''(1) -> 5;
'yeccgoto_\'IfNotEqualBlock\''(82) -> 5;
'yeccgoto_\'IfNotEqualBlock\''(87) -> 5;
'yeccgoto_\'IfNotEqualBlock\''(92) -> 5;
'yeccgoto_\'IfNotEqualBlock\''(97) -> 5;
'yeccgoto_\'IfNotEqualBlock\''(102) -> 5;
'yeccgoto_\'IfNotEqualBlock\''(110) -> 5;
'yeccgoto_\'IfNotEqualBlock\''(113) -> 5;
'yeccgoto_\'IfNotEqualBlock\''(119) -> 5;
'yeccgoto_\'IfNotEqualBlock\''(122) -> 5;
'yeccgoto_\'IfNotEqualBlock\''(128) -> 5.

'yeccgoto_\'IfNotEqualBraced\''(1) -> 4;
'yeccgoto_\'IfNotEqualBraced\''(82) -> 4;
'yeccgoto_\'IfNotEqualBraced\''(87) -> 4;
'yeccgoto_\'IfNotEqualBraced\''(92) -> 4;
'yeccgoto_\'IfNotEqualBraced\''(97) -> 4;
'yeccgoto_\'IfNotEqualBraced\''(102) -> 4;
'yeccgoto_\'IfNotEqualBraced\''(110) -> 4;
'yeccgoto_\'IfNotEqualBraced\''(113) -> 4;
'yeccgoto_\'IfNotEqualBraced\''(119) -> 4;
'yeccgoto_\'IfNotEqualBraced\''(122) -> 4;
'yeccgoto_\'IfNotEqualBraced\''(128) -> 4.

'yeccgoto_\'IfNotEqualExpression\''(45) -> 50.

'yeccgoto_\'IncludeTag\''(1) -> 3;
'yeccgoto_\'IncludeTag\''(82) -> 3;
'yeccgoto_\'IncludeTag\''(87) -> 3;
'yeccgoto_\'IncludeTag\''(92) -> 3;
'yeccgoto_\'IncludeTag\''(97) -> 3;
'yeccgoto_\'IncludeTag\''(102) -> 3;
'yeccgoto_\'IncludeTag\''(110) -> 3;
'yeccgoto_\'IncludeTag\''(113) -> 3;
'yeccgoto_\'IncludeTag\''(119) -> 3;
'yeccgoto_\'IncludeTag\''(122) -> 3;
'yeccgoto_\'IncludeTag\''(128) -> 3.

'yeccgoto_\'Literal\''(33) -> 34.

'yeccgoto_\'Variable\''(21) -> 23;
'yeccgoto_\'Variable\''(43) -> 57;
'yeccgoto_\'Variable\''(44) -> 53;
'yeccgoto_\'Variable\''(45) -> 49;
'yeccgoto_\'Variable\''(50) -> 51;
'yeccgoto_\'Variable\''(54) -> 55;
'yeccgoto_\'Variable\''(59) -> 57;
'yeccgoto_\'Variable\''(65) -> 66.

'yeccgoto_\'VariableBraced\''(1) -> 2;
'yeccgoto_\'VariableBraced\''(82) -> 2;
'yeccgoto_\'VariableBraced\''(87) -> 2;
'yeccgoto_\'VariableBraced\''(92) -> 2;
'yeccgoto_\'VariableBraced\''(97) -> 2;
'yeccgoto_\'VariableBraced\''(102) -> 2;
'yeccgoto_\'VariableBraced\''(110) -> 2;
'yeccgoto_\'VariableBraced\''(113) -> 2;
'yeccgoto_\'VariableBraced\''(119) -> 2;
'yeccgoto_\'VariableBraced\''(122) -> 2;
'yeccgoto_\'VariableBraced\''(128) -> 2.

-compile({inline,{yeccpars2_0_,1}}).
-file("erlydtl_parser.yrl", 118).
yeccpars2_0_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_2_,1}}).
-file("erlydtl_parser.yrl", 120).
yeccpars2_2_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_3_,1}}).
-file("erlydtl_parser.yrl", 122).
yeccpars2_3_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_4_,1}}).
-file("erlydtl_parser.yrl", 118).
yeccpars2_4_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_5_,1}}).
-file("erlydtl_parser.yrl", 128).
yeccpars2_5_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_6_,1}}).
-file("erlydtl_parser.yrl", 118).
yeccpars2_6_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_7_,1}}).
-file("erlydtl_parser.yrl", 127).
yeccpars2_7_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("erlydtl_parser.yrl", 118).
yeccpars2_8_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("erlydtl_parser.yrl", 126).
yeccpars2_9_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("erlydtl_parser.yrl", 118).
yeccpars2_10_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("erlydtl_parser.yrl", 125).
yeccpars2_11_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("erlydtl_parser.yrl", 121).
yeccpars2_12_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("erlydtl_parser.yrl", 123).
yeccpars2_13_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("erlydtl_parser.yrl", 118).
yeccpars2_14_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("erlydtl_parser.yrl", 130).
yeccpars2_15_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("erlydtl_parser.yrl", 118).
yeccpars2_16_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("erlydtl_parser.yrl", 124).
yeccpars2_17_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("erlydtl_parser.yrl", 118).
yeccpars2_18_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("erlydtl_parser.yrl", 129).
yeccpars2_19_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("erlydtl_parser.yrl", 119).
yeccpars2_22_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("erlydtl_parser.yrl", 135).
yeccpars2_24_([__1 | Stack]) ->
 [begin
   { variable , { __1 } }
  end | Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("erlydtl_parser.yrl", 136).
yeccpars2_28_([__3,__2,__1 | Stack]) ->
 [begin
   { variable , { __1 , __3 } }
  end | Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("erlydtl_parser.yrl", 132).
yeccpars2_29_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("erlydtl_parser.yrl", 134).
yeccpars2_31_([__3,__2,__1 | Stack]) ->
 [begin
   { apply_filter , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("erlydtl_parser.yrl", 189).
yeccpars2_32_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("erlydtl_parser.yrl", 190).
yeccpars2_34_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("erlydtl_parser.yrl", 145).
yeccpars2_42_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("erlydtl_parser.yrl", 141).
yeccpars2_48_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { include , __3 }
  end | Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("erlydtl_parser.yrl", 181).
yeccpars2_52_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 , __4 ]
  end | Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("erlydtl_parser.yrl", 175).
yeccpars2_56_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 , __4 ]
  end | Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("erlydtl_parser.yrl", 167).
yeccpars2_60_([__2,__1 | Stack]) ->
 [begin
   { 'not' , __2 }
  end | Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("erlydtl_parser.yrl", 166).
yeccpars2_61_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("erlydtl_parser.yrl", 143).
yeccpars2_63_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { tag , __2 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("erlydtl_parser.yrl", 146).
yeccpars2_66_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __1 ++ [ { __2 , __4 } ]
  end | Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("erlydtl_parser.yrl", 161).
yeccpars2_69_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("erlydtl_parser.yrl", 157).
yeccpars2_70_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("erlydtl_parser.yrl", 160).
yeccpars2_73_([__3,__2,__1 | Stack]) ->
 [begin
   { in , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("erlydtl_parser.yrl", 162).
yeccpars2_74_([__3,__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __3 ]
  end | Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("erlydtl_parser.yrl", 140).
yeccpars2_76_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { extends , __3 }
  end | Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("erlydtl_parser.yrl", 0).
yeccpars2_77_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("erlydtl_parser.yrl", 149).
yeccpars2_79_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("erlydtl_parser.yrl", 186).
yeccpars2_81_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_83_,1}}).
-file("erlydtl_parser.yrl", 185).
yeccpars2_83_([__3,__2,__1 | Stack]) ->
 [begin
   { autoescape , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("erlydtl_parser.yrl", 0).
yeccpars2_86_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_88_,1}}).
-file("erlydtl_parser.yrl", 148).
yeccpars2_88_([__3,__2,__1 | Stack]) ->
 [begin
   { block , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_91_,1}}).
-file("erlydtl_parser.yrl", 0).
yeccpars2_91_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("erlydtl_parser.yrl", 152).
yeccpars2_93_([__3,__2,__1 | Stack]) ->
 [begin
   { comment , __2 }
  end | Stack].

-compile({inline,{yeccpars2_96_,1}}).
-file("erlydtl_parser.yrl", 0).
yeccpars2_96_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("erlydtl_parser.yrl", 156).
yeccpars2_98_([__3,__2,__1 | Stack]) ->
 [begin
   { for , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("erlydtl_parser.yrl", 0).
yeccpars2_101_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_103_,1}}).
-file("erlydtl_parser.yrl", 165).
yeccpars2_103_([__3,__2,__1 | Stack]) ->
 [begin
   { 'if' , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("erlydtl_parser.yrl", 118).
yeccpars2_104_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("erlydtl_parser.yrl", 0).
yeccpars2_108_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("erlydtl_parser.yrl", 0).
yeccpars2_109_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_111_,1}}).
-file("erlydtl_parser.yrl", 164).
yeccpars2_111_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { ifelse , __1 , __2 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_114_,1}}).
-file("erlydtl_parser.yrl", 174).
yeccpars2_114_([__3,__2,__1 | Stack]) ->
 [begin
   { ifequal , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_115_,1}}).
-file("erlydtl_parser.yrl", 118).
yeccpars2_115_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_118_,1}}).
-file("erlydtl_parser.yrl", 0).
yeccpars2_118_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_120_,1}}).
-file("erlydtl_parser.yrl", 173).
yeccpars2_120_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { ifequalelse , __1 , __2 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_123_,1}}).
-file("erlydtl_parser.yrl", 180).
yeccpars2_123_([__3,__2,__1 | Stack]) ->
 [begin
   { ifnotequal , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_124_,1}}).
-file("erlydtl_parser.yrl", 118).
yeccpars2_124_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_127_,1}}).
-file("erlydtl_parser.yrl", 0).
yeccpars2_127_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_129_,1}}).
-file("erlydtl_parser.yrl", 179).
yeccpars2_129_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { ifnotequalelse , __1 , __2 , __4 }
  end | Stack].


