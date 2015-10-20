-module(erlydtl_unparser).
-export([unparse/1, unparse/2]).

unparse(DjangoParseTree, undefined) ->
    unparse(DjangoParseTree);
unparse(DjangoParseTree, true) ->
    Text = unparse(DjangoParseTree),
    Trimmed = re:replace(Text, <<"(^\\s+)|(\\s+$)|\n">>, <<"">>, [global, multiline]),
    Joined = join_iolist(Trimmed, " "),
    binary_to_list(iolist_to_binary(Joined)).

join_iolist(IOList, Sep) ->
    join_iolist(IOList, Sep, []).

join_iolist([[]|IOList], Sep, Acc) ->
    join_iolist(IOList, Sep, Acc);
join_iolist([Data|IOList], Sep, Acc) ->
    join_iolist(IOList, Sep, [Sep, Data|Acc]);
join_iolist([], _, [_|Acc]) ->
    lists:reverse(Acc);
join_iolist(IOList, _, Acc) ->
    lists:reverse([IOList|Acc]).


unparse(DjangoParseTree) ->
    do_unparse(DjangoParseTree).

do_unparse(DjangoParseTree) ->
    do_unparse(DjangoParseTree, []).

do_unparse([], Acc) ->
    lists:flatten(lists:reverse(Acc));
do_unparse([{'extends', Value}|Rest], Acc) ->
    do_unparse(Rest, [["{% extends ", unparse_value(Value), " %}"]|Acc]);
do_unparse([{'autoescape', OnOrOff, Contents}|Rest], Acc) ->
    do_unparse(Rest, [["{% autoescape ", unparse_identifier(OnOrOff), " %}", do_unparse(Contents), "{% endautoescape %}"]|Acc]);
do_unparse([{'block', Identifier, Contents}|Rest], Acc) ->
    do_unparse(Rest, [["{% block ", unparse_identifier(Identifier), " %}", do_unparse(Contents), "{% endblock %}"]|Acc]);
do_unparse([{'blocktrans', Args, Contents, undefined}|Rest], Acc) ->
    do_unparse(Rest, [["{% blocktrans ", unparse_blocktrans_args(Args), " %}", do_unparse(Contents), "{% endblocktrans %}"]|Acc]);
do_unparse([{'blocktrans', Args, Contents, PluralContents}|Rest], Acc) ->
    do_unparse(Rest, [["{% blocktrans ", unparse_blocktrans_args(Args), " %}",
                    do_unparse(Contents),
                    "{% plural %}",
                    do_unparse(PluralContents),
                    "{% endblocktrans %}"]|Acc]);
do_unparse([{'call', Identifier}|Rest], Acc) ->
    do_unparse(Rest, [["{% call ", unparse_identifier(Identifier), " %}"]|Acc]);
do_unparse([{'call', Identifier, With}|Rest], Acc) ->
    do_unparse(Rest, [["{% call ", unparse_identifier(Identifier), " with ", unparse_args(With), " %}"]|Acc]);
do_unparse([{'comment', Contents}|Rest], Acc) ->
    do_unparse(Rest, [["{% comment %}", do_unparse(Contents), "{% endcomment %}"]|Acc]);
do_unparse([{'comment_tag', _Pos, Text}|Rest], Acc) ->
    do_unparse(Rest, [["{#", Text, "#}"]|Acc]);
do_unparse([{'cycle', Names}|Rest], Acc) ->
    do_unparse(Rest, [["{% cycle ", do_unparse(Names), " %}"]|Acc]);
do_unparse([{'cycle_compat', Names}|Rest], Acc) ->
    do_unparse(Rest, [["{% cycle ", unparse_cycle_compat_names(Names), " %}"]|Acc]);
do_unparse([{'date', 'now', Value}|Rest], Acc) ->
    do_unparse(Rest, [["{% now ", unparse_value(Value), " %}"]|Acc]);
do_unparse([{'filter', FilterList, Contents}|Rest], Acc) ->
    do_unparse(Rest, [["{% filter ", unparse_filters(FilterList), " %}", do_unparse(Contents), "{% endfilter %}"]|Acc]);
do_unparse([{'firstof', Vars}|Rest], Acc) ->
    do_unparse(Rest, [["{% firstof ", do_unparse(Vars), " %}"]|Acc]);
do_unparse([{'for', {'in', IteratorList, Identifier}, Contents}|Rest], Acc) ->
    do_unparse(Rest, [["{% for ", unparse_identifier(Identifier), " in ", do_unparse(IteratorList), " %}",
                    do_unparse(Contents),
                    "{% endfor %}"]|Acc]);
do_unparse([{'for', {'in', IteratorList, Identifier}, Contents, EmptyPartsContents}|Rest], Acc) ->
    do_unparse(Rest, [["{% for ", unparse_identifier(Identifier), " in ", do_unparse(IteratorList), " %}",
                    do_unparse(Contents),
                    "{% empty %}",
                    do_unparse(EmptyPartsContents),
                    "{% endfor %}"]|Acc]);
do_unparse([{'if', Expression, Contents}|Rest], Acc) ->
    do_unparse(Rest, [["{% if ", unparse_expression(Expression), " %}",
                    do_unparse(Contents),
                    "{% endif %}"]|Acc]);
do_unparse([{'ifchanged', Expression, IfContents}|Rest], Acc) ->
    do_unparse(Rest, [["{% ifchanged ", unparse_expression(Expression), " %}",
                    do_unparse(IfContents),
                    "{% endifchanged %}"]|Acc]);
do_unparse([{'ifchangedelse', Expression, IfContents, ElseContents}|Rest], Acc) ->
    do_unparse(Rest, [["{% ifchanged ", unparse_expression(Expression), " %}",
                    do_unparse(IfContents),
                    "{% else %}",
                    do_unparse(ElseContents),
                    "{% endifchanged %}"]|Acc]);
do_unparse([{'ifelse', Expression, IfContents, ElseContents}|Rest], Acc) ->
    do_unparse(Rest, [["{% if ", unparse_expression(Expression), " %}",
                    do_unparse(IfContents),
                    "{% else %}",
                    do_unparse(ElseContents),
                    "{% endif %}"]|Acc]);
do_unparse([{'ifequal', [Arg1, Arg2], Contents}|Rest], Acc) ->
    do_unparse(Rest, [["{% ifequal ", unparse_value(Arg1), " ", unparse_value(Arg2), " %}",
                    do_unparse(Contents),
                    "{% endifequal %}"]|Acc]);
do_unparse([{'ifequalelse', [Arg1, Arg2], IfContents, ElseContents}|Rest], Acc) ->
    do_unparse(Rest, [["{% ifequal ", unparse_value(Arg1), " ", unparse_value(Arg2), " %}",
                    do_unparse(IfContents),
                    "{% else %}",
                    do_unparse(ElseContents),
                    "{% endifequal %}"]|Acc]);
do_unparse([{'ifnotequal', [Arg1, Arg2], Contents}|Rest], Acc) ->
    do_unparse(Rest, [["{% ifnotequal ", unparse_value(Arg1), " ", unparse_value(Arg2), " %}",
                    do_unparse(Contents),
                    "{% endifnotequal %}"]|Acc]);
do_unparse([{'ifnotequalelse', [Arg1, Arg2], IfContents, ElseContents}|Rest], Acc) ->
    do_unparse(Rest, [["{% ifnotequal ", unparse_value(Arg1), " ", unparse_value(Arg2), " %}",
                    do_unparse(IfContents),
                    "{% else %}",
                    do_unparse(ElseContents),
                    "{% endifnotequal %}"]|Acc]);
do_unparse([{'include', Value, []}|Rest], Acc) ->
    do_unparse(Rest, [["{% include ", unparse_value(Value), " %}"]|Acc]);
do_unparse([{'include', Value, Args}|Rest], Acc) ->
    do_unparse(Rest, [["{% include ", unparse_value(Value), " with ", unparse_args(Args)]|Acc]);
do_unparse([{'include_only', Value, []}|Rest], Acc) ->
    do_unparse(Rest, [["{% include ", unparse_value(Value), " only %}"]|Acc]);
do_unparse([{'include_only', Value, Args}|Rest], Acc) ->
    do_unparse(Rest, [["{% include ", unparse_value(Value), " with ", unparse_args(Args), " only %}"]|Acc]);
do_unparse([{'regroup', {Variable, Identifier1, Identifier2}, Contents}|Rest], Acc) ->
    do_unparse(Rest, [["{% regroup ", unparse_value(Variable), " by ", unparse_identifier(Identifier1), " as ", unparse_identifier(Identifier2), " %}",
                    do_unparse(Contents),
                    "{% endregroup %}"]|Acc]);
do_unparse([{'spaceless', Contents}|Rest], Acc) ->
    do_unparse(Rest, [["{% spaceless %}", do_unparse(Contents), "{% endspaceless %}"]|Acc]);
do_unparse([{'ssi', Arg}|Rest], Acc) ->
    do_unparse(Rest, [["{% ssi ", unparse_value(Arg), " %}"]|Acc]);
do_unparse([{'ssi_parsed', Arg}|Rest], Acc) ->
    do_unparse(Rest, [["{% ssi ", unparse_value(Arg), " parsed %}"]|Acc]);
do_unparse([{'string', _, String}|Rest], Acc) ->
    do_unparse(Rest, [[String]|Acc]);
do_unparse([{'tag', Identifier, []}|Rest], Acc) ->
    do_unparse(Rest, [["{% ", unparse_identifier(Identifier), " %}"]|Acc]);
do_unparse([{'tag', Identifier, Args}|Rest], Acc) ->
    do_unparse(Rest, [["{% ", unparse_identifier(Identifier), " ", unparse_args(Args), " %}"]|Acc]);
do_unparse([{'templatetag', Identifier}|Rest], Acc) ->
    do_unparse(Rest, [["{% templatetag ", unparse_identifier(Identifier), " %}"]|Acc]);
do_unparse([{'trans', Value}|Rest], Acc) ->
    do_unparse(Rest, [["{% trans ", unparse_value(Value), " %}"]|Acc]);
do_unparse([{'widthratio', Numerator, Denominator, Scale}|Rest], Acc) ->
    do_unparse(Rest, [["{% widthratio ", unparse_value(Numerator), " ", unparse_value(Denominator), " ", unparse_value(Scale), " %}"]|Acc]);
do_unparse([{'with', Args, Contents}|Rest], Acc) ->
    do_unparse(Rest, [["{% with ", unparse_args(Args), " %}",
                    do_unparse(Contents),
                    "{% endwidth %}"]|Acc]);
do_unparse([ValueToken|Rest], Acc) ->
    do_unparse(Rest, [["{{ ", unparse_value(ValueToken), " }}"]|Acc]).


unparse_identifier({identifier, _, Name}) ->
    atom_to_list(Name).

unparse_filters(FilterList) ->
    unparse_filters(FilterList, []).

unparse_filters([], Acc) ->
    lists:reverse(Acc);
unparse_filters([Filter], Acc) ->
    unparse_filters([], [unparse_filter(Filter)|Acc]);
unparse_filters([Filter|Rest], Acc) ->
    unparse_filters(Rest, lists:reverse([unparse_filter(Filter), "|"], Acc)).

unparse_filter([Identifier]) ->
    unparse_identifier(Identifier);
unparse_filter([Identifier, Arg]) ->
    [unparse_identifier(Identifier), ":", unparse_value(Arg)].

unparse_expression({'expr', "in", Arg1, Arg2}) ->
    [unparse_value(Arg1), " in ", unparse_value(Arg2)];
unparse_expression({'expr', "not", {'expr', "in", Arg1, Arg2}}) ->
    [unparse_value(Arg1), " not in ", unparse_value(Arg2)];
unparse_expression({'expr', "not", Expr}) ->
    ["not ", unparse_expression(Expr)];
unparse_expression({'expr', "eq", Arg1, Arg2}) ->
    [unparse_value(Arg1), " == ", unparse_value(Arg2)];
unparse_expression({'expr', "ne", Arg1, Arg2}) ->
    [unparse_value(Arg1), " != ", unparse_value(Arg2)];
unparse_expression({'expr', "ge", Arg1, Arg2}) ->
    [unparse_value(Arg1), " >= ", unparse_value(Arg2)];
unparse_expression({'expr', "le", Arg1, Arg2}) ->
    [unparse_value(Arg1), " <= ", unparse_value(Arg2)];
unparse_expression({'expr', "gt", Arg1, Arg2}) ->
    [unparse_value(Arg1), " > ", unparse_value(Arg2)];
unparse_expression({'expr', "lt", Arg1, Arg2}) ->
    [unparse_value(Arg1), " < ", unparse_value(Arg2)];
unparse_expression({'expr', "or", Arg1, Arg2}) ->
    [unparse_expression(Arg1), " or ", unparse_expression(Arg2)];
unparse_expression({'expr', "and", Arg1, Arg2}) ->
    [unparse_expression(Arg1), " and ", unparse_expression(Arg2)];
unparse_expression(Other) ->
    unparse_value(Other).

unparse_value({'string_literal', _, Value}) ->
    Value;
unparse_value({'number_literal', _, Value}) ->
    Value;
unparse_value({'apply_filter', Variable, Filter}) ->
    [unparse_value(Variable), "|", unparse_filter(Filter)];
unparse_value({'attribute', {Variable, Identifier}}) ->
    [unparse_value(Variable), ".", unparse_identifier(Identifier)];
unparse_value({'variable', Identifier}) ->
    unparse_identifier(Identifier).

unparse_args(Args) ->
    unparse_args(Args, []).

unparse_args([], Acc) ->
    collect_args_acc(Acc);
unparse_args([{{identifier, _, Name}, Value}|Args], Acc) ->
    unparse_args(Args, [[atom_to_list(Name), "=", unparse_value(Value)]|Acc]).

unparse_cycle_compat_names(Names) ->
    unparse_cycle_compat_names(Names, []).

unparse_cycle_compat_names([], Acc) ->
    lists:reverse(Acc);
unparse_cycle_compat_names([{identifier, _, Name}], Acc) ->
    unparse_cycle_compat_names([], [atom_to_list(Name)|Acc]);
unparse_cycle_compat_names([{identifier, _, Name}|Rest], Acc) ->
    unparse_cycle_compat_names(Rest, lists:reverse([atom_to_list(Name), ", "], Acc)).

unparse_blocktrans_args(Args) ->
    unparse_blocktrans_args(Args, []).

unparse_blocktrans_args([], Acc) ->
    collect_args_acc(Acc);
unparse_blocktrans_args([{args, WithArgs}|Args], Acc) ->
    unparse_blocktrans_args(
      Args, [["with ", unparse_args(WithArgs)]|Acc]);
unparse_blocktrans_args([{count, Count}|Args], Acc) ->
    unparse_blocktrans_args(
      Args, [["count ", unparse_args([Count])]|Acc]);
unparse_blocktrans_args([{context, Context}|Args], Acc) ->
    unparse_blocktrans_args(
      Args, [["context ", unparse_value(Context)]|Acc]);
unparse_blocktrans_args([trimmed|Args], Acc) ->
    unparse_blocktrans_args(
      Args, ["trimmed"|Acc]).

collect_args_acc(Acc) ->
    lists:flatten(string:join(lists:reverse(Acc), " ")).
