-module(erlydtl_translation_tests).

-include_lib("eunit/include/eunit.hrl").

all_sources_parser_test_() ->
    [{Title, [test_fun(Test) || Test <- Tests]}
     || {Title, Tests} <- test_defs()].

test_fun({Name, Template, Variables, Options, Output}) ->
    {Name, fun () ->
                   Tokens = (catch compile_and_render(Template, Variables, Options)),
                   ?assertMatch(Output, Tokens)
           end}.



compile_and_render(Template, Variables, Options) ->
    {ok, test} = erlydtl:compile_template(Template, test),
    {ok, R}  = test:render(Variables, Options),
    iolist_to_binary(R).


test_defs() ->
    [
        {"trans", [
            {"simple", "{% trans \"hello\" %}", [], [], <<"hello">>},
            {"with_fun", "{% trans \"text\" %}", [], [{translation_fun, fun(_ID, _L) -> "hola" end}], <<"hola">>},
            {"with_fun_utf8", "{% trans \"text\" %}", [],
                [{translation_fun, fun(_ID, _L) -> <<"привет"/utf8>> end}], <<"привет"/utf8>>}
        ]},
        {"blocktrans", [
            {"simple", "{% blocktrans %} hello {% endblocktrans %}", [], [], <<" hello ">>},
            {"with_fun", "{% blocktrans %} hello {% endblocktrans %}", [],
                [{translation_fun, fun(_ID, _L) -> "hola" end}], <<"hola">>},
    
            {"s_param_no_fun", "{% blocktrans %} hello {{ p }} {% endblocktrans %}", [{p, "mundo"}],
                [], <<" hello mundo ">>},
    
            {"s_param", "{% blocktrans %} hello {{ p }} {% endblocktrans %}", [{p, "mundo"}],
                [{translation_fun, fun(_ID, _L) -> "hola {{ p }}" end}], <<"hola mundo">>},
            {"b_param", "{% blocktrans %} hello {{ p }} {% endblocktrans %}", [{p, <<"mundo">>}],
                [{translation_fun, fun(_ID, _L) -> "hola {{ p }}" end}], <<"hola mundo">>},
            {"i_param", "{% blocktrans %} hello {{ p }} {% endblocktrans %}", [{p, 1}],
                [{translation_fun, fun(_ID, _L) -> "hola {{ p }}" end}], <<"hola 1">>},
            {"f_param", "{% blocktrans %} hello {{ p }} {% endblocktrans %}", [{p, 3.1415}],
                [{translation_fun, fun(_ID, _L) -> "hola {{ p }}" end}], <<"hola 3.1415">>},
    
            {"b_xss", "{% blocktrans %} hello {{ p }} {% endblocktrans %}",
                [{p, <<"<script>alert('pwnd');</script>">>}],
                [{translation_fun, fun(_ID, _L) -> "hola {{ p }}" end}],
                <<"hola &lt;script&gt;alert(&#039;pwnd&#039;);&lt;/script&gt;">>},
            {"s_xss", "{% blocktrans %} hello {{ p }} {% endblocktrans %}",
                [{p, "<script>alert('pwnd');</script>"}],
                [{translation_fun, fun(_ID, _L) -> "hola {{ p }}" end}],
                <<"hola &lt;script&gt;alert(&#039;pwnd&#039;);&lt;/script&gt;">>},
    
            {"b_autoecape_off",
                "{% autoescape off %}{% blocktrans %} hello {{ p }} {% endblocktrans %}{% endautoescape %}",
                [{p, <<"<script>alert('pwnd');</script>">>}],
                [{translation_fun, fun(_ID, _L) -> "hola {{ p }}" end}],
                <<"hola <script>alert('pwnd');</script>">>},
            {"b_autoecape_nested",
                "{% autoescape off %}{% autoescape on %}{% blocktrans %} hello {{ p }} {% endblocktrans %}{% endautoescape %}{% endautoescape %}",
                [{p, <<"<script>alert('pwnd');</script>">>}],
                [{translation_fun, fun(_ID, _L) -> "hola {{ p }}" end}],
                <<"hola &lt;script&gt;alert(&#039;pwnd&#039;);&lt;/script&gt;">>},
             {"term_hack_", "{% blocktrans %} hello {{ p }} {% endblocktrans %}",
                [{p, {"<script>alert('pwnd');</script>"}}],
                [{translation_fun, fun(_ID, _L) -> "hola {{ p }}" end}],
                <<"hola {&quot;&lt;script&gt;alert(&#039;pwnd&#039;);&lt;/script&gt;&quot;}">>},
            {"plural_2",
                "{% blocktrans count counter=p %} hello world {% plural %} hello {{ p }} worlds {% endblocktrans %}",
                [{p, 2}],
                [{translation_fun, fun({" hello world ", {" hello {{ p }} worlds ", 2}}, _L)  ->
                                        "hola {{ p }} mundos"
                                   end}],
                <<"hola 2 mundos">>}
        ]}
    ].


