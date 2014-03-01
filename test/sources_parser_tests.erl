-module(sources_parser_tests).

-include_lib("eunit/include/eunit.hrl").

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
        [ {"Hello inside an else",{"dummy_path",1,94}}, {"Hello inside an if",{"dummy_path",1,50}}]}
      ]}
    ].
