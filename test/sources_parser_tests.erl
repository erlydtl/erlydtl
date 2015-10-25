-module(sources_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/erlydtl_ext.hrl").

all_sources_parser_test_() ->
    [{Title, [test_fun(Test) || Test <- Tests]}
     || {Title, Tests} <- test_defs()].

test_fun({Name, Content, Output}) ->
    {Name, fun () ->
                   Tokens = (catch sources_parser:process_content("dummy_path", Content)),
                   ?assertMatch(Output, Tokens)
           end}.

test_defs() ->
    [{"trans",
      [{"block with no trans",
        <<"<html>{% block main %} {% endblock %}</html>">>,
        []},
       {"block with trans",
        <<"<html>{% block main %} {% trans \"Hello\" %} {% endblock %}</html>">>,
        [{"Hello",{"dummy_path",1,33}}]},
       {"for with trans",
        <<"<html>{% block main %} {%for thing in things %}{% trans \"Hello inside a for\" %}  {% endfor %} {% endblock %}</html>">>,
        [{"Hello inside a for",{"dummy_path",1,57}}]},
       {"if with trans",
        <<"<html>{% block content %}{% if thing %} {% trans \"Hello inside an if\" %} {% endif %} {% endblock %}</html>">>,
        [{"Hello inside an if",{"dummy_path",1,50}}]},
       {"if with trans inside a for",
        <<"<html>{% block content %}{%for thin in things %}{% if thing %} {% trans \"Hello inside an if inside a for\" %} {% endif %} {% endfor %}{% endblock %}</html>">>,
        [{"Hello inside an if inside a for",{"dummy_path",1,73}}]},
       {"if and else both with trans",
        <<"<html>{% block content %}{% if thing %} {% trans \"Hello inside an if\" %} {% else %} {% trans \"Hello inside an else\" %} {% endif %} {% endblock %}</html>">>,
        [{"Hello inside an else",{"dummy_path",1,94}}, {"Hello inside an if",{"dummy_path",1,50}}]},
       {"blocktrans with pretty format",
        <<"<html>{% blocktrans %}\n  This is a multiline\n  message... \n{% endblocktrans %}">>,
        [{"\n  This is a multiline\n  message... \n", {"dummy_path",1,10}}]},
       {"blocktrans with pretty format, trimmed",
        <<"<html>{% blocktrans trimmed %}\n  This is a multiline\n  message... \n{% endblocktrans %}">>,
        [{"This is a multiline message...", {"dummy_path",1,18}}]}
      ]}
    ].


all_sources_parser_ext_test_() ->
    [test_ext_fun(Test) || Test <- ext_test_defs()].

test_ext_fun({Name, Tpl, {Fields, Output}}) ->
    {Name, fun() ->
                   Tokens = [sources_parser:phrase_info(Fields, P)
                             || P <- sources_parser:parse_content("dummy_path", Tpl)],
                   ?assertEqual(Output, Tokens)
           end}.

ext_test_defs() ->
    [{"trans with inline comments",
      <<"{#TrAnSlATORs: hi!#}{%trans 'phrase'%}">>,
      {[msgid, comment], [["phrase", "TrAnSlATORs: hi!"]]}},
     {"trans with comments",
      <<"{%comment%}translators: com{{me}}nt{%endcomment%}{%trans 'phrase'%}">>,
      {[msgid, comment], [["phrase", "translators: com{{ me }}nt"]]}},
     {"blocktrans with comments",
      <<"{%comment%}translators: comment{%endcomment%}{%blocktrans with a=b%}B={{b}}{%endblocktrans%}">>,
      {[msgid, comment], [["B={{ b }}", "translators: comment"]]}},
     {"blocktrans with context",
      <<"{%blocktrans context 'ctxt'%}msg{%endblocktrans%}">>,
      {[msgid, context], [["msg", "ctxt"]]}},
     {"blocktrans with plural form",
      <<"{%blocktrans%}msg{%plural%}msgs{%endblocktrans%}">>,
      {[msgid, msgid_plural], [["msg", "msgs"]]}},
     {"trans with context",
      <<"{% trans 'msg' context 'ctxt' %}">>,
      {[msgid, context], [["msg", "ctxt"]]}},
     {"trans noop",
      <<"{% trans 'msg' noop %}">>,
      {[msgid], [["msg"]]}},
     {"trans noop with context",
      <<"{% trans 'msg' noop context 'ctxt' %}">>,
      {[msgid, context], [["msg", "ctxt"]]}}
    ].

unparser_test_() ->
    [test_unparser_fun(Test) || Test <- unparser_test_defs()].

test_unparser_fun({Name, Tpl}) ->
    {Name, fun() ->
                   %% take input Tpl value, parse it, "unparse" it, then parse it again.
                   %% both parsed values should be equvialent, even if the source versions
                   %% are not an exact match (there can be whitespace differences)
                   case erlydtl_compiler:do_parse_template(
                          Tpl, #dtl_context{}) of
                       {ok, Dpt} ->
                           Unparsed = erlydtl_unparser:unparse(Dpt),
                           case erlydtl_compiler:do_parse_template(
                                  Unparsed, #dtl_context{}) of
                               {ok, DptU} ->
                                   case catch compare_tree(Dpt, DptU) of
                                       ok -> ok;
                                       Err -> throw({compare_failed, Err, {test_ast, Dpt}, {unparsed, {source, Unparsed}, {ast, DptU}}})
                                   end;
                               Err ->
                                   throw({unparsed_source, Err})
                           end;
                       Err ->
                           throw({test_source, Err})
                   end
           end}.

unparser_test_defs() ->
    [{"comment tag", <<"here it is: {# this is my comment #} <-- it was right there.">>},
     {"blocktrans plain", <<"{% blocktrans %}foo bar{% endblocktrans %}">>},
     {"blocktrans trimmed", <<"{% blocktrans trimmed %}\n foo \n   bar \n\n{% endblocktrans %}">>},
     {"blocktrans with args", <<"{% blocktrans with var1=foo var2=bar count c=d %}blarg{% endblocktrans %}">>},
     {"blocktrans with all", <<"{% blocktrans with var1=foo var2=bar trimmed context 'baz' count c=d %}blarg{% endblocktrans %}">>}
    ].


compare_tree([], []) -> ok;
compare_tree([H1|T1], [H2|T2]) ->
    compare_token(H1, H2),
    compare_tree(T1, T2).

compare_token({'extends', Value1}, {'extends', Value2}) ->
    ?assertEqual(Value1, Value2);
compare_token({'autoescape', OnOrOff1, Contents1}, {'autoescape', OnOrOff2, Contents2}) ->
    ?assertEqual(OnOrOff1, OnOrOff2),
    compare_tree(Contents1, Contents2);
compare_token({'block', Identifier1, Contents1}, {'block', Identifier2, Contents2}) ->
    compare_identifier(Identifier1, Identifier2),
    compare_tree(Contents1, Contents2);
compare_token({'blocktrans', Args1, Contents1, Plural1}, {'blocktrans', Args2, Contents2, Plural2}) ->
    compare_blocktrans_args(Args1, Args2),
    compare_tree(Contents1, Contents2),
    case {Plural1, Plural2} of
        {undefined, undefined} -> ok;
        _ -> compare_tree(Plural1, Plural2)
    end;
compare_token({'call', Identifier1}, {'call', Identifier2}) ->
    compare_identifier(Identifier1, Identifier2);
compare_token({'call', Identifier1, With1}, {'call', Identifier2, With2}) ->
    ?assertEqual(With1, With2),
    compare_identifier(Identifier1, Identifier2);
compare_token({'comment', Contents1}, {'comment', Contents2}) ->
    compare_tree(Contents1, Contents2);
compare_token({'comment_tag', _Pos, Text1}, {'comment_tag', _Pos, Text2}) ->
    ?assertEqual(Text1, Text2);
compare_token({'cycle', Names1}, {'cycle', Names2}) ->
    compare_tree(Names1, Names2);
compare_token({'cycle_compat', Names1}, {'cycle_compat', Names2}) ->
    compare_cycle_compat_names(Names1, Names2);
compare_token({'date', 'now', Value1}, {'date', 'now', Value2}) ->
    compare_value(Value1, Value2);
compare_token({'filter', FilterList1, Contents1}, {'filter', FilterList2, Contents2}) ->
    compare_filters(FilterList1, FilterList2),
    compare_tree(Contents1, Contents2);
compare_token({'firstof', Vars1}, {'firstof', Vars2}) ->
    compare_tree(Vars1, Vars2);
%% TODO...
%% compare_token({'for', {'in', IteratorList, Identifier}, Contents}, {'for', {'in', IteratorList, Identifier}, Contents}) -> ok;
%% compare_token({'for', {'in', IteratorList, Identifier}, Contents, EmptyPartsContents}, {'for', {'in', IteratorList, Identifier}, Contents, EmptyPartsContents}) -> ok;
compare_token({'if', Expression1, Contents1}, {'if', Expression2, Contents2}) ->
    compare_expression(Expression1, Expression2),
    compare_tree(Contents1, Contents2);
%% compare_token({'ifchanged', Expression, IfContents}, {'ifchanged', Expression, IfContents}) -> ok;
%% compare_token({'ifchangedelse', Expression, IfContents, ElseContents}, {'ifchangedelse', Expression, IfContents, ElseContents}) -> ok;
%% compare_token({'ifelse', Expression, IfContents, ElseContents}, {'ifelse', Expression, IfContents, ElseContents}) -> ok;
%% compare_token({'ifequal', [Arg1, Arg2], Contents}, {'ifequal', [Arg1, Arg2], Contents}) -> ok;
%% compare_token({'ifequalelse', [Arg1, Arg2], IfContents, ElseContents}, {'ifequalelse', [Arg1, Arg2], IfContents, ElseContents}) -> ok;
%% compare_token({'ifnotequal', [Arg1, Arg2], Contents}, {'ifnotequal', [Arg1, Arg2], Contents}) -> ok;
%% compare_token({'ifnotequalelse', [Arg1, Arg2], IfContents, ElseContents}, {'ifnotequalelse', [Arg1, Arg2], IfContents, ElseContents}) -> ok;
%% compare_token({'include', Value, []}, {'include', Value, []}) -> ok;
%% compare_token({'include', Value, Args}, {'include', Value, Args}) -> ok;
%% compare_token({'include_only', Value, []}, {'include_only', Value, []}) -> ok;
%% compare_token({'include_only', Value, Args}, {'include_only', Value, Args}) -> ok;
%% compare_token({'regroup', {Variable, Identifier1, Identifier2}, Contents}, {'regroup', {Variable, Identifier1, Identifier2}, Contents}) -> ok;
%% compare_token({'spaceless', Contents}, {'spaceless', Contents}) -> ok;
%% compare_token({'ssi', Arg}, {'ssi', Arg}) -> ok;
%% compare_token({'ssi_parsed', Arg}, {'ssi_parsed', Arg}) -> ok;
compare_token({'string', _, String1}, {'string', _, String2}) ->
    ?assertEqual(String1, String2);
%% compare_token({'tag', Identifier, []}, {'tag', Identifier, []}) -> ok;
%% compare_token({'tag', Identifier, Args}, {'tag', Identifier, Args}) -> ok;
%% compare_token({'templatetag', Identifier}, {'templatetag', Identifier}) -> ok;
%% compare_token({'trans', Value}, {'trans', Value}) -> ok;
%% compare_token({'widthratio', Numerator, Denominator, Scale}, {'widthratio', Numerator, Denominator, Scale}) -> ok;
%% compare_token({'with', Args, Contents}, {'with', Args, Contents}) -> ok;
compare_token(ValueToken1, ValueToken2) ->
    compare_value(ValueToken1, ValueToken2).

compare_identifier({identifier, _, Name1}, {identifier, _, Name2}) ->
    ?assertEqual(Name1, Name2).

compare_filters(FilterList1, FilterList2) ->
    [compare_filter(F1, F2)
     || {F1, F2} <- lists:zip(FilterList1, FilterList2)].

compare_filter([Identifier1], [Identifier2]) ->
    compare_identifier(Identifier1, Identifier2);
compare_filter([Identifier1, Arg1], [Identifier2, Arg2]) ->
    compare_identifier(Identifier1, Identifier2),
    compare_value(Arg1, Arg2).

compare_expression({'expr', _, Arg11, Arg12}, {'expr', _, Arg21, Arg22}) ->
    compare_value(Arg11, Arg21),
    compare_value(Arg12, Arg22);
compare_expression({'expr', "not", Expr1}, {'expr', "not", Expr2}) ->
    compare_expression(Expr1, Expr2);
compare_expression(Other1, Other2) ->
    compare_value(Other1, Other2).

compare_value({'string_literal', _, Value1}, {'string_literal', _, Value2}) ->
    ?assertEqual(Value1, Value2);
compare_value({'number_literal', _, Value1}, {'number_literal', _, Value2}) ->
    ?assertEqual(Value1, Value2);
compare_value({'apply_filter', Variable1, Filter1}, {'apply_filter', Variable2, Filter2}) ->
    compare_value(Variable1, Variable2),
    compare_filter(Filter1, Filter2);
compare_value({'attribute', {Variable1, Identifier1}}, {'attribute', {Variable2, Identifier2}}) ->
    compare_value(Variable1, Variable2),
    compare_identifier(Identifier1, Identifier2);
compare_value({'variable', Identifier1}, {'variable', Identifier2}) ->
    compare_identifier(Identifier1, Identifier2).

compare_args(Args1, Args2) when length(Args1) =:= length(Args2) ->
    [compare_arg(A1, A2)
     || {A1, A2} <- lists:zip(Args1, Args2)].

compare_arg(Arg, Arg) when is_atom(Arg) -> ok;
compare_arg({{identifier, _, Name1}, Value1}, {{identifier, _, Name2}, Value2}) ->
    ?assertEqual(Name1, Name2),
    compare_value(Value1, Value2).

compare_blocktrans_args([], []) -> ok;
compare_blocktrans_args([{args, WithArgs1}|Args1], Args2) ->
    {value, {args, WithArgs2}, Args3} = lists:keytake(args, 1, Args2),
    compare_args(WithArgs1, WithArgs2),
    compare_blocktrans_args(Args1, Args3);
compare_blocktrans_args([{count, Count1}|Args1], Args2) ->
    {value, {count, Count2}, Args3} = lists:keytake(count, 1, Args2),
    compare_arg(Count1, Count2),
    compare_blocktrans_args(Args1, Args3);
compare_blocktrans_args([{context, Context1}|Args1], Args2) ->
    {value, {context, Context2}, Args3} = lists:keytake(context, 1, Args2),
    compare_value(Context1, Context2),
    compare_blocktrans_args(Args1, Args3);
compare_blocktrans_args([trimmed|Args1], Args2) ->
    Args3 = Args2 -- [trimmed],
    if Args2 =/= Args3 ->
            compare_blocktrans_args(Args1, Args3)
    end.

compare_cycle_compat_names(Names1, Names2) ->
    [compare_identifier(N1, N2)
     || {N1, N2} <- lists:zip(Names1, Names2)].
