-module(sources_parser_tests).

-include_lib("eunit/include/eunit.hrl").

all_sources_parser_test_() ->
    [{Title, [test_fun(Test) || Test <- Tests]}
     || {Title, Tests} <- test_defs()].

all_sources_parser_ext_test_() ->
    [test_ext_fun(Test) || Test <- ext_test_defs()].


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
        [ {"Hello inside an else",{"dummy_path",1,94}}, {"Hello inside an if",{"dummy_path",1,50}}]}
      ]}
    ].


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
      {[msgid, comment], [["B={{ b }}", "translators: comment"]]}}
    ].
