%% -*- coding: utf-8 -*-
-module(erlydtl_test_defs).

-export([tests/0, extra_reader/2]).
-include("testrunner.hrl").
-record(testrec, {foo, bar, baz}).
-record(person, {first_name, gender}).

%% {Name, DTL, Vars, Output}
%% {Name, DTL, Vars, RenderOpts, Output}
%% {Name, DTL, Vars, RenderOpts, CompilerOpts, Output}
%% {Name, DTL, Vars, RenderOpts, CompilerOpts, Output, Warnings}

tests() ->
    [def_to_test(G, D) || {G, Ds} <- all_test_defs(), D <- Ds].

all_test_defs() ->
    [{"vars",
      [{"string",
        <<"String value is: {{ var1 }}">>,
        [{var1, "foo"}], <<"String value is: foo">>},
       {"int",
        <<"The magic number is: {{ var1 }}">>,
        [{var1, 42}], <<"The magic number is: 42">>},
       {"float",
        <<"The price of milk is: {{ var1 }}">>,
        [{var1, 0.42}], <<"The price of milk is: 0.42">>},
       {"No spaces",
        <<"{{var1}}">>,
        [{var1, "foo"}], <<"foo">>},
       {"Variable name is a tag name",
        <<"{{ comment }}">>,
        [{comment, "Nice work!"}], <<"Nice work!">>},
       #test{
          title = "reserved name ok as variable name",
          source = <<"{{ from }}">>,
          render_vars = [{from, "test"}],
          output = <<"test">>
         }
      ]},
     {"maps",
      case erlang:is_builtin(erlang, is_map, 1) of
          false -> [];
          true ->
              [#test{
                  title = "simple test",
                  source = <<"{{ msg.hello }}">>,
                  render_vars = [{msg, maps:put(hello, "world", maps:new())}],
                  output = <<"world">>
                 },
               #test{
                  title = "various key types",
                  source = <<"{{ msg.key1 }},{{ msg.key2 }},{{ msg.key3 }},{{ msg.4 }}">>,
                  render_vars = [{msg, maps:from_list([{key1, 1}, {"key2", 2}, {<<"key3">>, 3}, {4, "value4"}])}],
                  output = <<"1,2,3,value4">>
                 }
              ]
      end},
     {"comment",
      [{"comment block is excised",
        <<"bob {% comment %}(moron){% endcomment %} loblaw">>,
        [], <<"bob  loblaw">>},
       {"inline comment is excised",
        <<"you're {# not #} a very nice person">>,
        [], <<"you're  a very nice person">>}
      ]},
     {"autoescape",
      [{"Autoescape works",
        <<"{% autoescape on %}{{ var1 }}{% endautoescape %}">>,
        [{var1, "<b>bold</b>"}], <<"&lt;b&gt;bold&lt;/b&gt;">>},
       {"Nested autoescape",
        <<"{% autoescape on %}{{ var1 }}{% autoescape off %}{{ var1 }}{% endautoescape %}{% endautoescape %}">>,
        [{var1, "<b>"}], <<"&lt;b&gt;<b>">>},
       {"default auto escape",
        <<"{{ var1 }}">>, [{var1, "&"}], [], [auto_escape],
        <<"&amp;">>},
       {"intermixed autoescape",
        <<"{% autoescape on %}1:{{ var1 }}{% endautoescape %} 2:{{ var1 }}{% autoescape on %} 3:{{ var1 }}{% endautoescape %}">>,
        [{var1, "&"}],
        <<"1:&amp; 2:& 3:&amp;">>}
      ]},
     {"string literal",
      [{"Render literal",
        <<"{{ \"foo\" }} is my name">>, [], <<"foo is my name">>},
       {"Newlines are escaped",
        <<"{{ \"foo\\n\" }}">>, [], <<"foo\n">>},
       {"strip quotes",
        <<"{{ \"foo\"|add:\"\\\"\" }}">>, [], <<"foo\"">>}
      ]},
     {"cycle",
      [#test{
          title = "deprecated cycle syntax",
          source = <<"{% for i in test %}{% cycle a,b %}{{ i }},{% endfor %}">>,
          render_vars = [{test, [0,1,2,3,4]}],
          output = <<"a0,b1,a2,b3,a4,">>
         },
       {"Cycling through quoted strings",
        <<"{% for i in test %}{% cycle 'a' 'b' %}{{ i }},{% endfor %}">>,
        [{test, ["0", "1", "2", "3", "4"]}], <<"a0,b1,a2,b3,a4,">>},
       {"Cycling through normal variables",
        <<"{% for i in test %}{% cycle aye bee %}{{ i }},{% endfor %}">>,
        [{test, ["0", "1", "2", "3", "4"]}, {aye, "a"}, {bee, "b"}],
        <<"a0,b1,a2,b3,a4,">>},
       #test{
          title = "mix strings and variables",
          source = <<"{% for i in test %}{% cycle 'a' b 'c' %}{{ i }},{% endfor %}">>,
          render_vars = [{test, [0,1,2,3,4]}, {b, 'B'}],
          output = <<"a0,B1,c2,a3,B4,">>
         },
       #test{
          title = "keep current value in local variable",
          source = <<"{% for i in test %}{% cycle 'a' 'b' as c %}{{ i }}{{ c }},{% endfor %}">>,
          render_vars = [{test, [0,1,2,3,4]}],
          output = <<"a0a,b1b,a2a,b3b,a4a,">>
         },
       #test{
          title = "keep current value silently in local variable",
          source = <<"{% for i in test %}{% cycle 'a' 'b' as c silent %}{{ i }}{{ c }},{% endfor %}">>,
          render_vars = [{test, [0,1,2,3,4]}],
          output = <<"0a,1b,2a,3b,4a,">>
         }
      ]},
     {"number literal",
      [{"Render integer",
        <<"{{ 5 }}">>, [], <<"5">>}
      ]},
     {"variable",
      [{"Render variable",
        <<"{{ var1 }} is my game">>, [{var1, "bar"}], <<"bar is my game">>},
       {"Render variable with attribute",
        <<"I enjoy {{ var1.game }}">>, [{var1, [{game, "Othello"}]}], <<"I enjoy Othello">>},
       {"Render variable with string-key attribute",
        <<"I also enjoy {{ var1.game }}">>, [{var1, [{"game", "Parcheesi"}]}], <<"I also enjoy Parcheesi">>},
       {"Render variable with binary-key attribute",
        <<"I also enjoy {{ var1.game }}">>, [{var1, [{<<"game">>, "Parcheesi"}]}], <<"I also enjoy Parcheesi">>},
       {"Render variable with tuple wrapped proplist",
        <<"I also enjoy {{ var1.game }}">>, [{var1, {[{<<"game">>, "Parcheesi"}]}}], <<"I also enjoy Parcheesi">>},
       {"Render variable in dict",
        <<"{{ var1 }}">>, dict:store(var1, "bar", dict:new()), <<"bar">>},
       {"Render variable with missing attribute in dict",
        <<"{{ var1.foo }}">>, [{var1, dict:store(bar, "Othello", dict:new())}], <<"">>},
       {"Render variable in a two elements tuple",
        <<"{{ var1.2 }}">>, [{var1,{12,[bar]}}], <<"bar">>},
       {"Render variable in gb_tree",
        <<"{{ var1 }}">>, gb_trees:insert(var1, "bar", gb_trees:empty()), <<"bar">>},
       {"Render variable in arity-1 func",
        <<"I enjoy {{ var1 }}">>, fun (var1) -> "Othello" end, <<"I enjoy Othello">>},
       {"Render variable with attribute in dict",
        <<"{{ var1.attr }}">>, [{var1, dict:store(attr, "Othello", dict:new())}], <<"Othello">>},
       {"Render variable with attribute in gb_tree",
        <<"{{ var1.attr }}">>, [{var1, gb_trees:insert(attr, "Othello", gb_trees:empty())}], <<"Othello">>},
       {"Render variable with attribute in arity-1 func",
        <<"I enjoy {{ var1.game }}">>, [{var1, fun (game) -> "Othello" end}], <<"I enjoy Othello">>},
       %% {"Render variable in parameterized module",
       %% <<"{{ var1.some_var }}">>, [{var1, erlydtl_example_variable_storage:new("foo")}], <<"foo">>},
       {"Nested attributes",
        <<"{{ person.city.state.country }}">>, [{person, [{city, [{state, [{country, "Italy"}]}]}]}],
        <<"Italy">>},
       {"Index list variable",
        <<"{{ var1.2 }}">>, [{var1, [a, b, c]}],
        <<"b">>},
       {"Index tuple variable",
        <<"{{ var1.2 }}">>, [{var1, {a, b, c}}],
        <<"b">>},
       {"Index all elements of list (default, 1-based)",
        <<"{{ var1.1 }},{{ var1.2 }},{{ var1.3 }}.">>,
        [{var1, [a, b, c]}],
        <<"a,b,c.">>},
       {"Index all list elements 0-based (selected at compile time)",
        <<"{{ var1.0 }},{{ var1.1 }},{{ var1.2 }}.">>,
        [{var1, [a, b, c]}], [], [lists_0_based],
        <<"a,b,c.">>},
       {"Index all list elements 0-based (selected at render time)",
        <<"{{ var1.0 }},{{ var1.1 }},{{ var1.2 }}.">>,
        [{var1, [a, b, c]}], [lists_0_based], [{lists_0_based, defer}],
        <<"a,b,c.">>},
       {"Index all list elements 1-based (selected at render time)",
        <<"{{ var1.1 }},{{ var1.2 }},{{ var1.3 }}.">>,
        [{var1, [a, b, c]}], [], [{lists_0_based, defer}],
        <<"a,b,c.">>},
       {"Index all elements of tuple (default, 1-based)",
        <<"{{ var1.1 }},{{ var1.2 }},{{ var1.3 }}.">>,
        [{var1, {a, b, c}}],
        <<"a,b,c.">>},
       {"Index all tuple elements 0-based (selected at compile time)",
        <<"{{ var1.0 }},{{ var1.1 }},{{ var1.2 }}.">>,
        [{var1, {a, b, c}}], [], [tuples_0_based],
        <<"a,b,c.">>},
       {"Index all tuple elements 0-based (selected at render time)",
        <<"{{ var1.0 }},{{ var1.1 }},{{ var1.2 }}.">>,
        [{var1, {a, b, c}}], [tuples_0_based], [{tuples_0_based, defer}],
        <<"a,b,c.">>},
       {"Index all tuple elements 1-based (selected at render time)",
        <<"{{ var1.1 }},{{ var1.2 }},{{ var1.3 }}.">>,
        [{var1, {a, b, c}}], [], [{tuples_0_based, defer}],
        <<"a,b,c.">>},
       {"Index tuple using a \"reserved\" keyword",
        <<"{{ list.count }}">>,
        [{list, [{count, 123}]}],
        <<"123">>},
       {"Index list value",
        <<"{{ content.description }}">>,
        [{content, "test"}], <<"">>},
       {"Index binary value",
        <<"{{ content.description }}">>,
        [{content, <<"test">>}], <<"">>}
      ]},
     {"now",
      [{"now functional",
        <<"It is the {% now \"jS \\o\\f F Y\" %}.">>, [{var1, ""}], generate_test_date()}
     ]},
      {"now",
      [{"now function with translation", % notice, that only date output is traslated. While you might want to transle the whole format string ('F'->'E')
        <<"It is the {% now \"jS \\o\\f F Y\" %}.">>, [{var1, ""}], [{locale, <<"ru">>}, {translation_fun, fun date_translation/2}], generate_test_date(russian)}
     ]},
     {"if",
      [{"If/else",
        <<"{% if var1 %}boo{% else %}yay{% endif %}">>, [{var1, ""}], <<"yay">>},
       {"If elif",
        <<"{% if var1 %}boo{% elif var2 %}yay{% endif %}">>, [{var1, ""}, {var2, "happy"}], <<"yay">>},
       {"If elif/else",
        <<"{% if var1 %}boo{% elif var2 %}sad{% else %}yay{% endif %}">>, [{var1, ""}, {var2, ""}], <<"yay">>},
       {"If elif/elif/else",
        <<"{% if var1 %}boo{% elif var2 %}yay{% elif var3 %}sad{% else %}noo{% endif %}">>,
        [{var1, ""}, {var2, "happy"}, {var3, "not_taken"}],
        <<"yay">>},
       {"If",
        <<"{% if var1 %}boo{% endif %}">>, [{var1, ""}], <<>>},
       {"If not",
        <<"{% if not var1 %}yay{% endif %}">>, [{var1, ""}], <<"yay">>},
       {"If \"0\"",
        <<"{% if var1 %}boo{% endif %}">>, [{var1, "0"}], <<>>},
       {"If 0",
        <<"{% if var1 %}boo{% endif %}">>, [{var1, 0}], <<>>},
       {"If false",
        <<"{% if var1 %}boo{% endif %}">>, [{var1, false}], <<>>},
       {"If false string",
        <<"{% if var1 %}boo{% endif %}">>, [{var1, "false"}], <<"boo">>},
       {"If undefined",
        <<"{% if var1 %}boo{% endif %}">>, [{var1, undefined}], <<>>},
       {"If other atom",
        <<"{% if var1 %}yay{% endif %}">>, [{var1, foobar}], <<"yay">>},
       {"If non-empty string",
        <<"{% if var1 %}yay{% endif %}">>, [{var1, "hello"}], <<"yay">>},
       {"If proplist",
        <<"{% if var1 %}yay{% endif %}">>, [{var1, [{foo, "bar"}]}], <<"yay">>},
       {"If complex",
        <<"{% if foo.bar.baz %}omgwtfbbq{% endif %}">>, [], <<"">>}
      ]},
     {"if .. in ..",
      [{"If substring in string",
        <<"{% if var1 in var2 %}yay{% endif %}">>, [{var1, "rook"}, {var2, "Crooks"}], <<"yay">>},
       {"If substring in string (false)",
        <<"{% if var1 in var2 %}boo{% endif %}">>, [{var1, "Cook"}, {var2, "Crooks"}], <<>>},
       {"If substring not in string",
        <<"{% if var1 not in var2 %}yay{% endif %}">>, [{var1, "Cook"}, {var2, "Crooks"}], <<"yay">>},
       {"If substring not in string (false)",
        <<"{% if var1 not in var2 %}boo{% endif %}">>, [{var1, "rook"}, {var2, "Crooks"}], <<>>},
       {"If literal substring in string",
        <<"{% if \"man\" in \"Ottoman\" %}yay{% endif %}">>, [], <<"yay">>},
       {"If literal substring in string (false)",
        <<"{% if \"woman\" in \"Ottoman\" %}boo{% endif %}">>, [], <<>>},
       {"If element in list",
        <<"{% if var1 in var2 %}yay{% endif %}">>, [{var1, "foo"}, {var2, ["bar", "foo", "baz"]}], <<"yay">>},
       {"If element in list (false)",
        <<"{% if var1 in var2 %}boo{% endif %}">>, [{var1, "FOO"}, {var2, ["bar", "foo", "baz"]}], <<>>}
      ]},
     {"if .. and ..",
      [{"If true and true",
        <<"{% if var1 and var2 %}yay{% endif %}">>, [{var1, true}, {var2, true}], <<"yay">>},
       {"If true and false",
        <<"{% if var1 and var2 %}yay{% endif %}">>, [{var1, true}, {var2, false}], <<"">>},
       {"If false and true",
        <<"{% if var1 and var2 %}yay{% endif %}">>, [{var1, false}, {var2, true}], <<"">>},
       {"If false and false ",
        <<"{% if var1 and var2 %}yay{% endif %}">>, [{var1, false}, {var2, false}], <<"">>}
      ]},
     {"if .. or ..",
      [{"If true or true",
        <<"{% if var1 or var2 %}yay{% endif %}">>, [{var1, true}, {var2, true}], <<"yay">>},
       {"If true or false",
        <<"{% if var1 or var2 %}yay{% endif %}">>, [{var1, true}, {var2, false}], <<"yay">>},
       {"If false or true",
        <<"{% if var1 or var2 %}yay{% endif %}">>, [{var1, false}, {var2, true}], <<"yay">>},
       {"If false or false ",
        <<"{% if var1 or var2 %}yay{% endif %}">>, [{var1, false}, {var2, false}], <<"">>}
      ]},
     {"if equality",
      [{"If int equals number literal",
        <<"{% if var1 == 2 %}yay{% endif %}">>, [{var1, 2}], <<"yay">>},
       {"If int equals number literal (false)",
        <<"{% if var1 == 2 %}yay{% endif %}">>, [{var1, 3}], <<"">>},
       {"If string equals string literal",
        <<"{% if var1 == \"2\" %}yay{% endif %}">>, [{var1, "2"}], <<"yay">>},
       {"If string equals string literal (false)",
        <<"{% if var1 == \"2\" %}yay{% endif %}">>, [{var1, "3"}], <<"">>},
       {"If int not equals number literal",
        <<"{% if var1 != 2 %}yay{% endif %}">>, [{var1, 3}], <<"yay">>},
       {"If string not equals string literal",
        <<"{% if var1 != \"2\" %}yay{% endif %}">>, [{var1, "3"}], <<"yay">>},
       {"If filter result equals number literal",
        <<"{% if var1|length == 2 %}yay{% endif %}">>, [{var1, ["fo", "bo"]}], <<"yay">>},
       {"If filter result equals string literal",
        <<"{% if var1|capfirst == \"Foo\" %}yay{% endif %}">>, [{var1, "foo"}], <<"yay">>}
      ]},
     {"if size comparison",
      [{"If int greater than number literal",
        <<"{% if var1 > 2 %}yay{% endif %}">>, [{var1, 3}], <<"yay">>},
       {"If int greater than negative number literal",
        <<"{% if var1 > -2 %}yay{% endif %}">>, [{var1, -1}], <<"yay">>},
       {"If int greater than number literal (false)",
        <<"{% if var1 > 2 %}yay{% endif %}">>, [{var1, 2}], <<"">>},

       {"If int greater than or equal to number literal",
        <<"{% if var1 >= 2 %}yay{% endif %}">>, [{var1, 3}], <<"yay">>},
       {"If int greater than or equal to number literal (2)",
        <<"{% if var1 >= 2 %}yay{% endif %}">>, [{var1, 2}], <<"yay">>},
       {"If int greater than or equal to number literal (false)",
        <<"{% if var1 >= 2 %}yay{% endif %}">>, [{var1, 1}], <<"">>},

       {"If int less than number literal",
        <<"{% if var1 < 2 %}yay{% endif %}">>, [{var1, 1}], <<"yay">>},
       {"If int less than number literal (false)",
        <<"{% if var1 < 2 %}yay{% endif %}">>, [{var1, 2}], <<"">>},

       {"If int less than or equal to number literal",
        <<"{% if var1 <= 2 %}yay{% endif %}">>, [{var1, 1}], <<"yay">>},
       {"If int less than or equal to number literal",
        <<"{% if var1 <= 2 %}yay{% endif %}">>, [{var1, 2}], <<"yay">>},
       {"If int less than or equal to number literal (false)",
        <<"{% if var1 <= 2 %}yay{% endif %}">>, [{var1, 3}], <<"">>}
      ]},
     {"if complex bool",
      [{"If (true or false) and true",
        <<"{% if (var1 or var2) and var3 %}yay{% endif %}">>,
        [{var1, true}, {var2, false}, {var3, true}], <<"yay">>},
       {"If true or (false and true)",
        <<"{% if var1 or (var2 and var3) %}yay{% endif %}">>,
        [{var1, true}, {var2, false}, {var3, true}], <<"yay">>}
      ]},
     {"for",
      [{"Simple loop",
        <<"{% for x in list %}{{ x }}{% endfor %}">>, [{'list', ["1", "2", "3"]}],
        <<"123">>},
       {"Reversed loop",
        <<"{% for x in list reversed %}{{ x }}{% endfor %}">>, [{'list', ["1", "2", "3"]}],
        <<"321">>},
       {"Expand list",
        <<"{% for x, y in list %}{{ x }},{{ y }}\n{% endfor %}">>, [{'list', [["X", "1"], ["X", "2"]]}],
        <<"X,1\nX,2\n">>},
       {"Expand tuple",
        <<"{% for x, y in list %}{{ x }},{{ y }}\n{% endfor %}">>, [{'list', [{"X", "1"}, {"X", "2"}]}],
        <<"X,1\nX,2\n">>},
       {"Resolve variable attribute",
        <<"{% for number in person.numbers %}{{ number }}\n{% endfor %}">>, [{person, [{numbers, ["411", "911"]}]}],
        <<"411\n911\n">>},
       {"Resolve nested variable attribute",
        <<"{% for number in person.home.numbers %}{{ number }}\n{% endfor %}">>, [{person, [{home, [{numbers, ["411", "911"]}]}]}],
        <<"411\n911\n">>},
       {"Counter0",
        <<"{% for number in numbers %}{{ forloop.counter0 }}. {{ number }}\n{% endfor %}">>,
        [{numbers, ["Zero", "One", "Two"]}], <<"0. Zero\n1. One\n2. Two\n">>},
       {"Counter",
        <<"{% for number in numbers %}{{ forloop.counter }}. {{ number }}\n{% endfor %}">>,
        [{numbers, ["One", "Two", "Three"]}], <<"1. One\n2. Two\n3. Three\n">>},
       {"Reverse Counter0",
        <<"{% for number in numbers %}{{ forloop.revcounter0 }}. {{ number }}\n{% endfor %}">>,
        [{numbers, ["Two", "One", "Zero"]}], <<"2. Two\n1. One\n0. Zero\n">>},
       {"Reverse Counter",
        <<"{% for number in numbers %}{{ forloop.revcounter }}. {{ number }}\n{% endfor %}">>,
        [{numbers, ["Three", "Two", "One"]}], <<"3. Three\n2. Two\n1. One\n">>},
       {"Counter \"first\"",
        <<"{% for number in numbers %}{% if forloop.first %}{{ number }}{% endif %}{% endfor %}">>,
        [{numbers, ["One", "Two", "Three"]}], <<"One">>},
       {"Counter \"last\"",
        <<"{% for number in numbers %}{% if forloop.last %}{{ number }}{% endif %}{% endfor %}">>,
        [{numbers, ["One", "Two", "Three"]}], <<"Three">>},
       {"Nested for loop",
        <<"{% for outer in list %}{% for inner in outer %}{{ inner }}\n{% endfor %}{% endfor %}">>,
        [{'list', [["Al", "Albert"], ["Jo", "Joseph"]]}],
        <<"Al\nAlbert\nJo\nJoseph\n">>},
       {"Unused variable in foreach proplist",
        <<"{% for k,v in plist %}{{v}}{% endfor %}">>,
        [{'plist',[{1,"one"},{2,"two"}]}], [], [], <<"onetwo">>,
        [error_info([{0, erl_lint, {unused_var, 'Var_k/1_1:8'}}])]},
       {"Unused variable in foreach proplist, prefixed with underscore",
        <<"{% for _k,v in plist %}{{v}}{% endfor %}">>,
        [{'plist',[{1,"one"},{2,"two"}]}], [], [], <<"onetwo">>},
       {"Access parent loop counters",
        <<"{% for outer in list %}{% for inner in outer %}({{ forloop.parentloop.counter0 }}, {{ forloop.counter0 }})\n{% endfor %}{% endfor %}">>,
        [{'list', [["One", "two"], ["One", "two"]]}], [], [], <<"(0, 0)\n(0, 1)\n(1, 0)\n(1, 1)\n">>,
        %% the warnings we get from the erlang compiler still needs some care..
        [error_info([{0, erl_lint, {unused_var, 'Var_inner/3_1:31'}}])]},
       {"If changed",
        <<"{% for x in list %}{% ifchanged %}{{ x }}\n{% endifchanged %}{% endfor %}">>,
        [{'list', ["one", "two", "two", "three", "three", "three"]}], <<"one\ntwo\nthree\n">>},
       {"If changed/2",
        <<"{% for x, y in list %}{% ifchanged %}{{ x|upper }}{% endifchanged %}{% ifchanged %}{{ y|lower }}{% endifchanged %}\n{% endfor %}">>,
        [{'list', [["one", "a"], ["two", "A"], ["two", "B"], ["three", "b"], ["three", "c"], ["Three", "b"]]}], <<"ONEa\nTWO\nb\nTHREE\nc\nb\n">>},
       {"If changed/else",
        <<"{% for x in list %}{% ifchanged %}{{ x }}\n{% else %}foo\n{% endifchanged %}{% endfor %}">>,
        [{'list', ["one", "two", "two", "three", "three", "three"]}], <<"one\ntwo\nfoo\nthree\nfoo\nfoo\n">>},
       {"If changed/param",
        <<"{% for date in list %}{% ifchanged date.month %} {{ date.month }}:{{ date.day }}{% else %},{{ date.day }}{% endifchanged %}{% endfor %}\n">>,
        [{'list', [[{month,"Jan"},{day,1}],[{month,"Jan"},{day,2}],[{month,"Apr"},{day,10}],
                   [{month,"Apr"},{day,11}],[{month,"May"},{day,4}]]}],
        <<" Jan:1,2 Apr:10,11 May:4\n">>},
       {"If changed/param2",
        <<"{% for x, y in list %}{% ifchanged y|upper %}{{ x|upper }}{% endifchanged %}\n{% endfor %}">>,
        [{'list', [["one", "a"], ["two", "A"], ["two", "B"], ["three", "b"], ["three", "c"], ["Three", "b"]]}], <<"ONE\n\nTWO\n\nTHREE\nTHREE\n">>},
       {"If changed/param2 combined",
        <<"{% for x, y in list %}{% ifchanged x y|upper %}{{ x }}{% endifchanged %}\n{% endfor %}">>,
        [{'list', [["one", "a"], ["two", "A"], ["two", "B"], ["three", "b"], ["three", "B"], ["three", "c"]]}], <<"one\ntwo\ntwo\nthree\n\nthree\n">>},
       {"If changed/resolve",
        <<"{% for x in list %}{% ifchanged x.name|first %}{{ x.value }}{% endifchanged %}\n{% endfor %}">>,
        [{'list', [[{"name", ["nA","nB"]},{"value","1"}],[{"name", ["nA","nC"]},{"value","2"}],
                   [{"name", ["nB","nC"]},{"value","3"}],[{"name", ["nB","nA"]},{"value","4"}]]}],
        <<"1\n\n3\n\n">>},

       {"Loop undefined var",
        <<"{% for i in undef %}i = {{ i }}.\n{% endfor %}">>,
        [],
        <<"">>},
       {"Loop filtered value rather than variable",
        <<"{% for x in 123|make_list %}{% if not forloop.first %}, {% endif %}{{ x }}{% endfor %}">>,
        [],
        <<"1, 2, 3">>}
      ]},
     {"for/empty",
      [{"Simple loop",
        <<"{% for x in list %}{{ x }}{% empty %}shucks{% endfor %}">>, [{'list', ["1", "2", "3"]}],
        <<"123">>},
       {"Simple loop (empty)",
        <<"{% for x in list %}{{ x }}{% empty %}shucks{% endfor %}">>, [{'list', []}],
        <<"shucks">>}
      ]},
     {"ifequal",
      [{"Compare variable to literal",
        <<"{% ifequal var1 \"foo\" %}yay{% endifequal %}">>,
        [{var1, "foo"}], <<"yay">>},
       {"Compare variable to unequal literal",
        <<"{% ifequal var1 \"foo\" %}boo{% endifequal %}">>,
        [{var1, "bar"}], <<>>},
       {"Compare literal to variable",
        <<"{% ifequal \"foo\" var1 %}yay{% endifequal %}">>,
        [{var1, "foo"}], <<"yay">>},
       {"Compare literal to unequal variable",
        <<"{% ifequal \"foo\" var1 %}boo{% endifequal %}">>,
        [{var1, "bar"}], <<>>},
       {"Compare variable to literal (int string)",
        <<"{% ifequal var1 \"2\" %}yay{% else %}boo{% endifequal %}">>,
        [{var1, "2"}], <<"yay">>},
       {"Compare variable to literal (int)",
        <<"{% ifequal var1 2 %}yay{% else %}boo{% endifequal %}">>,
        [{var1, 2}], <<"yay">>},
       {"Compare variable to unequal literal (int)",
        <<"{% ifequal var1 2 %}boo{% else %}yay{% endifequal %}">>,
        [{var1, 3}], <<"yay">>},
       {"Compare variable to equal literal (atom)",
        <<"{% ifequal var1 \"foo\"%}yay{% endifequal %}">>,
        [{var1, foo}], <<"yay">>},
       {"Compare variable to unequal literal (atom)",
        <<"{% ifequal var1 \"foo\"%}yay{% else %}boo{% endifequal %}">>,
        [{var1, bar}], <<"boo">>}
      ]},
     {"ifequal/else",
      [{"Compare variable to literal",
        <<"{% ifequal var1 \"foo\" %}yay{% else %}boo{% endifequal %}">>,
        [{var1, "foo"}], <<"yay">>},
       {"Compare variable to unequal literal",
        <<"{% ifequal var1 \"foo\" %}boo{% else %}yay{% endifequal %}">>,
        [{var1, "bar"}], <<"yay">>},
       {"Compare literal to variable",
        <<"{% ifequal \"foo\" var1 %}yay{% else %}boo{% endifequal %}">>,
        [{var1, "foo"}], <<"yay">>},
       {"Compare literal to unequal variable",
        <<"{% ifequal \"foo\" var1 %}boo{% else %}yay{% endifequal %}">>,
        [{var1, "bar"}], <<"yay">>}
      ]},
     {"ifnotequal",
      [{"Compare variable to literal",
        <<"{% ifnotequal var1 \"foo\" %}boo{% endifnotequal %}">>,
        [{var1, "foo"}], <<>>},
       {"Compare variable to unequal literal",
        <<"{% ifnotequal var1 \"foo\" %}yay{% endifnotequal %}">>,
        [{var1, "bar"}], <<"yay">>},
       {"Compare literal to variable",
        <<"{% ifnotequal \"foo\" var1 %}boo{% endifnotequal %}">>,
        [{var1, "foo"}], <<>>},
       {"Compare literal to unequal variable",
        <<"{% ifnotequal \"foo\" var1 %}yay{% endifnotequal %}">>,
        [{var1, "bar"}], <<"yay">>}
      ]},
     {"ifnotequal/else",
      [{"Compare variable to literal",
        <<"{% ifnotequal var1 \"foo\" %}boo{% else %}yay{% endifnotequal %}">>,
        [{var1, "foo"}], <<"yay">>},
       {"Compare variable to unequal literal",
        <<"{% ifnotequal var1 \"foo\" %}yay{% else %}boo{% endifnotequal %}">>,
        [{var1, "bar"}], <<"yay">>},
       {"Compare literal to variable",
        <<"{% ifnotequal \"foo\" var1 %}boo{% else %}yay{% endifnotequal %}">>,
        [{var1, "foo"}], <<"yay">>},
       {"Compare literal to unequal variable",
        <<"{% ifnotequal \"foo\" var1 %}yay{% else %}boo{% endifnotequal %}">>,
        [{var1, "bar"}], <<"yay">>}
      ]},
     {"filter tag",
      [{"Apply a filter",
        <<"{% filter escape %}&{% endfilter %}">>, [], <<"&amp;">>},
       {"Chained filters",
        <<"{% filter linebreaksbr|escape %}\n{% endfilter %}">>, [], <<"&lt;br /&gt;">>}
      ]},
     {"filters",
      [{"Filter a literal",
        <<"{{ \"pop\"|capfirst }}">>, [],
        <<"Pop">>},
       {"Filters applied in order",
        <<"{{ var1|force_escape|length }}">>, [{var1, <<"&">>}],
        <<"5">>},
       {"Escape is applied last",
        <<"{{ var1|escape|linebreaksbr }}">>, [{var1, <<"\n">>}],
        <<"&lt;br /&gt;">>},
       {"add; lhs number, rhs number",
        <<"{{ one|add:4}}">>, [{one, 1}],
        <<"5">>},
       {"add; lhs numeric string, rhs number",
        <<"{{ one|add:4}}">>, [{one, "1"}],
        <<"5">>},
       {"add; lhs number, rhs numeric string",
        <<"{{ one|add:'4'}}">>, [{one, 1}],
        <<"5">>},
       {"add; lhs non-numeric string, rhs number",
        <<"{{ one|add:4}}">>, [{one, "foo"}],
        <<"foo4">>},
       {"add; lhs number, rhs non-numeric string",
        <<"{{ one|add:'foo'}}">>, [{one, 1}],
        <<"1foo">>},
       {"add; lhs non-numeric string, rhs non-numeric string",
        <<"{{ one|add:'bar'}}">>, [{one, "foo"}],
        <<"foobar">>},
       {"add; lhs numeric string, rhs numeric string",
        <<"{{ one|add:'4'}}">>, [{one, "1"}],
        <<"5">>},
       {"|addslashes",
        <<"{{ var1|addslashes }}">>, [{var1, "Jimmy's \"great\" meats'n'things"}],
        <<"Jimmy\\'s \\\"great\\\" meats\\'n\\'things">>},
       {"|capfirst",
        <<"{{ var1|capfirst }}">>, [{var1, "dana boyd"}],
        <<"Dana boyd">>},
       {"|center:10",
        <<"{{ var1|center:10 }}">>, [{var1, "MB"}],
        <<"    MB    ">>},
       {"|center:1",
        <<"{{ var1|center:1 }}">>, [{var1, "KBR"}],
        <<"B">>},
       {"|cut:\" \"",
        <<"{{ var1|cut:\" \" }}">>, [{var1, "String with spaces"}],
        <<"Stringwithspaces">>},
       {"|date 1",
        <<"{{ var1|date:\"jS F Y H:i\" }}">>,
        [{var1, {1975,7,24}}],
        <<"24th July 1975 00:00">>},
       {"|date 2",
        <<"{{ var1|date:\"jS F Y H:i\" }}">>,
        [{var1, {{1975,7,24}, {7,13,1}}}],
        <<"24th July 1975 07:13">>},
       {"|date 3",
        <<"{{ var1|date }}">>,
        [{var1, {{1975,7,24}, {7,13,1}}}],
        <<"July 24, 1975">>},
        % I doubt someone need first two, but test we support it
        {"|date a translation",
        <<"{{ var1|date:\"a\" }}">>,
        [{var1, {{1975,7,24},{12,00,00}}}],[{translation_fun, fun date_translation/2},{locale, <<"ru">>}],
        <<"п.п."/utf8>>},
        {"|date A translation",
        <<"{{ var1|date:\"A\" }}">>,
        [{var1, {{1975,7,24},{12,00,00}}}],[{translation_fun, fun date_translation/2},{locale, <<"ru">>}],
        <<"ПП"/utf8>>},
        {"|date b translation", 
        <<"{{ var1|date:\"b\" }}">>,
        [{var1, {1975,7,24}}],[{translation_fun, fun date_translation/2},{locale, <<"ru">>}],
        <<"июл"/utf8>>},
        {"|date D translation",
        <<"{{ var1|date:\"D\" }}">>,
        [{var1, {1975,7,24}}],[{translation_fun, fun date_translation/2},{locale, <<"ru">>}],
        <<"Чтв"/utf8>>},
        {"|date E translation",
        <<"{{ var1|date:\"E\" }}">>,
        [{var1, {1975,7,24}}],[{translation_fun, fun date_translation/2},{locale, <<"ru">>}],
        <<"Июля"/utf8>>},
        {"|date F translation",
        <<"{{ var1|date:\"F\" }}">>,
        [{var1, {1975,7,24}}],[{translation_fun, fun date_translation/2},{locale, <<"ru">>}],
        <<"Июль"/utf8>>},
        {"|date l translation",
        <<"{{ var1|date:\"l\" }}">>,
        [{var1, {1975,7,24}}],[{translation_fun, fun date_translation/2},{locale, <<"ru">>}],
        <<"Четверг"/utf8>>},
        {"|date M translation",
        <<"{{ var1|date:\"M\" }}">>,
        [{var1, {1986,9,24}}],[{translation_fun, fun date_translation/2},{locale, <<"ru">>}],
        <<"Сен"/utf8>>},
        {"|date N translation",
        <<"{{ var1|date:\"N\" }}">>,
        [{var1, {1986,9,24}}],[{translation_fun, fun date_translation/2},{locale, <<"ru">>}],
        <<"Сен."/utf8>>},
        {"|date P translation",
        <<"{{ var1|date:\"P\" }}">>,
        [{var1, {{1986,9,24},{12,0,0}}}],[{translation_fun, fun date_translation/2},{locale, <<"ru">>}],
        <<"полдень"/utf8>>},

       {"|default:\"foo\" 1",
        <<"{{ var1|default:\"foo\" }}">>, [], <<"foo">>},
       {"|default:\"foo\" 2",
        <<"{{ var1|default:\"foo\" }}">>, [{var1, "bar"}], <<"bar">>},
       {"|default:\"foo\" 3",
        <<"{{ var1|default:\"foo\" }}">>, [{var1, "0"}], <<"foo">>},
       {"|default_if_none:\"foo\"",
        <<"{{ var1|default_if_none:\"foo\" }}">>, [], <<"foo">>},
       {"|default_if_none:\"foo\" 2",
        <<"{{ var1|default_if_none:\"foo\" }}">>, [{var1, "bar"}], <<"bar">>},
       {"|dictsort 1",
        <<"{{ var1|dictsort:\"foo\" }}">>,
        [{var1,[[{foo,2}],[{foo,1}]]}], <<"{foo,1}{foo,2}">>},
       {"|dictsort 2",
        <<"{{ var1|dictsort:\"foo.bar\" }}">>,
        [{var1,[[{foo,[{bar,2}]}],[{foo,[{bar,1}]}]]}],
        <<"{foo,[{bar,1}]}{foo,[{bar,2}]}">>},
       {"|divisibleby:\"3\"",
        <<"{% if var1|divisibleby:\"3\" %}yay{% endif %}">>, [{var1, 21}], <<"yay">>},
       {"|divisibleby:\"3\"",
        <<"{% if var1|divisibleby:\"3\" %}yay{% endif %}">>, [{var1, 22}], <<"">>},
       {"|escape",
        <<"{% autoescape on %}{{ var1|escape|escape|escape }}{% endautoescape %}">>, [{var1, ">&1"}], <<"&gt;&amp;1">>},
       {"|escapejs",
        <<"{{ var1|escapejs }}">>, [{var1, "testing\r\njavascript 'string\" <b>escaping</b>"}],
        <<"testing\\u000D\\u000Ajavascript \\u0027string\\u0022 \\u003Cb\\u003Eescaping\\u003C/b\\u003E">>},
       {"|filesizeformat (bytes)",
        <<"{{ var1|filesizeformat }}">>, [{var1, 1023}], <<"1023 bytes">>},
       {"|filesizeformat (KB)",
        <<"{{ var1|filesizeformat }}">>, [{var1, 3487}], <<"3.4 KB">>},
       {"|filesizeformat (MB)",
        <<"{{ var1|filesizeformat }}">>, [{var1, 6277098}], <<"6.0 MB">>},
       {"|filesizeformat (GB)",
        <<"{{ var1|filesizeformat }}">>, [{var1, 1024 * 1024 * 1024}], <<"1.0 GB">>},
       {"|first",
        <<"{{ var1|first }}">>, [{var1, "James"}],
        <<"J">>},
       {"|fix_ampersands",
        <<"{{ var1|fix_ampersands }}">>, [{var1, "Ben & Jerry's"}],
        <<"Ben &amp; Jerry's">>},

       {"|floatformat:\"-1\"",
        <<"{{ var1|floatformat:\"-1\" }}">>, [{var1, 34.23234}],
        <<"34.2">>},
       {"int |floatformat",
        <<"{{ var1|floatformat:\"-1\" }}">>, [{var1, 123}],
        <<"123">>},
       {"string |floatformat",
        <<"{{ var1|floatformat:\"-1\" }}">>, [{var1, "123.321"}],
        <<"123.3">>},
       {"binary |floatformat",
        <<"{{ var1|floatformat:\"-1\" }}">>, [{var1, <<"123.321">>}],
        <<"123.3">>},

       %% from: https://docs.djangoproject.com/en/1.6/ref/templates/builtins/#floatformat
       {"1.a) |floatformat",
        <<"{{ var1|floatformat }}">>, [{var1, 34.23234}],
        <<"34.2">>},
       {"1.b) |floatformat",
        <<"{{ var1|floatformat }}">>, [{var1, 34.00000}],
        <<"34">>},
       {"1.c) |floatformat",
        <<"{{ var1|floatformat }}">>, [{var1, 34.26000}],
        <<"34.3">>},
       {"2.a) |floatformat:\"3\"",
        <<"{{ var1|floatformat:\"3\" }}">>, [{var1, 34.23234}],
        <<"34.232">>},
       {"2.b) |floatformat:\"3\"",
        <<"{{ var1|floatformat:\"3\" }}">>, [{var1, 34.00000}],
        <<"34.000">>},
       {"2.c) |floatformat:\"3\"",
        <<"{{ var1|floatformat:\"3\" }}">>, [{var1, 34.26000}],
        <<"34.260">>},
       {"3.a) |floatformat:\"0\"",
        <<"{{ var1|floatformat:\"0\" }}">>, [{var1, 34.23234}],
        <<"34">>},
       {"3.b) |floatformat:\"0\"",
        <<"{{ var1|floatformat:\"0\" }}">>, [{var1, 34.00000}],
        <<"34">>},
       {"3.c) |floatformat:\"0\"",
        <<"{{ var1|floatformat:\"0\" }}">>, [{var1, 39.56000}],
        <<"40">>},
       {"4.a) |floatformat:\"-3\"",
        <<"{{ var1|floatformat:\"-3\" }}">>, [{var1, 34.23234}],
        <<"34.232">>},
       {"4.b) |floatformat:\"-3\"",
        <<"{{ var1|floatformat:\"-3\" }}">>, [{var1, 34.00000}],
        <<"34">>},
       {"4.c) |floatformat:\"-3\"",
        <<"{{ var1|floatformat:\"-3\" }}">>, [{var1, 34.26000}],
        <<"34.260">>},

       {"|force_escape",
        <<"{{ var1|force_escape }}">>, [{var1, "Ben & Jerry's <=> \"The World's Best Ice Cream\""}],
        <<"Ben &amp; Jerry&#039;s &lt;=&gt; &quot;The World&#039;s Best Ice Cream&quot;">>},
       {"iolist |force_escape",
        <<"{{ var1|force_escape }}">>, [{var1, ["'a'"]}],
        <<"&#039;a&#039;">>},
       {"nested iolist |force_escape",
        <<"{{ var1|force_escape }}">>, [{var1, ["a'", <<"b">>, [<<"<c">>, "d", ["e>"]]]}],
        <<"a&#039;b&lt;cde&gt;">>},
       {"|format_integer",
        <<"{{ var1|format_integer }}">>, [{var1, 28}], <<"28">>},
       {"|format_number 1",
        <<"{{ var1|format_number }}">>, [{var1, 28}], <<"28">>},
       {"|format_number 2",
        <<"{{ var1|format_number }}">>, [{var1, 23.77}], <<"23.77">>},
       {"|format_number 3",
        <<"{{ var1|format_number }}">>, [{var1, "28.77"}], <<"28.77">>},
       {"|format_number 4",
        <<"{{ var1|format_number }}">>, [{var1, "23.77"}], <<"23.77">>},
       {"|format_number 5",
        <<"{{ var1|format_number }}">>, [{var1, fun() -> 29 end}], <<"29">>},
       {"|format_number 6",
        <<"{{ var1|format_number }}">>, [{var1, fun() -> fun() -> 31 end end}], <<"31">>},
       {"|get_digit:\"2\"",
        <<"{{ var1|get_digit:\"2\" }}">>, [{var1, 42}], <<"4">>},
       {"|iriencode",
        <<"{{ url|iriencode }}">>, [{url, "You #$*@!!"}], <<"You+#$*@!!">>},
       {"|join:\", \" (list)",
        <<"{{ var1|join:\", \" }}">>, [{var1, ["Liberte", "Egalite", "Fraternite"]}],
        <<"Liberte, Egalite, Fraternite">>},
       {"|join:\", \" (binary)",
        <<"{{ var1|join:\", \" }}">>, [{var1, [<<"Liberte">>, "Egalite", <<"Fraternite">>]}],
        <<"Liberte, Egalite, Fraternite">>},
       {"|join:\", \" (numbers)",
        <<"{{ var1|join:\", \" }}">>, [{var1, [1, 2, 3]}],
        <<"1, 2, 3">>},
       {"|last",
        <<"{{ var1|last }}">>, [{var1, "XYZ"}],
        <<"Z">>},
       {"|length",
        <<"{{ var1|length }}">>, [{var1, "antidisestablishmentarianism"}],
        <<"28">>},
       {"|linebreaks",
        <<"{{ var1|linebreaks }}">>, [{var1, "Joel\nis a slug"}],
        <<"<p>Joel<br />is a slug</p>">>},
       {"|linebreaks",
        <<"{{ var1|linebreaks }}">>, [{var1, "Joel\n\n\n\nis a slug"}],
        <<"<p>Joel</p><p>is a slug</p>">>},
       {"|linebreaks",
        <<"{{ var1|linebreaks }}">>, [{var1, "Joel\n\nis a \nslug"}],
        <<"<p>Joel</p><p>is a <br />slug</p>">>},
       {"|linebreaksbr",
        <<"{{ var1|linebreaksbr }}">>, [{var1, "One\nTwo\n\nThree\n\n\n"}],
        <<"One<br />Two<br /><br />Three<br /><br /><br />">>},
       {"|linebreaksbr",
        <<"{{ \"One\\nTwo\\n\\nThree\\n\\n\\n\"|linebreaksbr }}">>, [],
        <<"One<br />Two<br /><br />Three<br /><br /><br />">>},
       {"|linenumbers",
        <<"{{ var1|linenumbers }}">>, [{var1, "a\nb\nc"}],
        <<"1. a\n2. b\n3. c">>},
       {"|linenumbers",
        <<"{{ var1|linenumbers }}">>, [{var1, "a"}],
        <<"1. a">>},
       {"|linenumbers",
        <<"{{ var1|linenumbers }}">>, [{var1, "a\n"}],
        <<"1. a\n2. ">>},
       {"|ljust:10",
        <<"{{ var1|ljust:10 }}">>, [{var1, "Gore"}],
        <<"Gore      ">>},
       {"|lower",
        <<"{{ var1|lower }}">>, [{var1, "E. E. Cummings"}],
        <<"e. e. cummings">>},
       {"|makelist",
        <<"{{ list|make_list }}">>, [{list, "Joel"}],
        <<"J","o","e","l">>},
       {"|pluralize",
        <<"{{ num|pluralize }}">>, [{num, 1}],
        <<"">>},
       {"|pluralize",
        <<"{{ num|pluralize }}">>, [{num, 2}],
        <<"s">>},
       {"|pluralize:\"s\"",
        <<"{{ num|pluralize }}">>, [{num, 1}],
        <<"">>},
       {"|pluralize:\"s\"",
        <<"{{ num|pluralize }}">>, [{num, 2}],
        <<"s">>},
       {"|pluralize:\"y,es\" (list)",
        <<"{{ num|pluralize:\"y,es\" }}">>, [{num, 1}],
        <<"y">>},
       {"|pluralize:\"y,es\" (list)",
        <<"{{ num|pluralize:\"y,es\" }}">>, [{num, 2}],
        <<"es">>},
       {"|length|pluralize",
        <<"{{ list|length|pluralize:\"plural\" }}">>, [{list, [foo, bar]}],
        <<"plural">>},
       {"|length|pluralize",
        <<"{{ list|length|pluralize:\"plural\" }}">>, [{list, [foo]}],
        <<"">>},
       {"|random",
        <<"{{ var1|random }}">>, [{var1, ["foo", "foo", "foo"]}],
        <<"foo">>},
       {"|removetags:\"b span\"",
        <<"{{ var1|removetags:\"b span\" }}">>, [{var1, "<B>Joel</B> <button>is</button> a <span>slug</span>"}],
        <<"<B>Joel</B> <button>is</button> a slug">>},
       {"|rjust:10",
        <<"{{ var1|rjust:10 }}">>, [{var1, "Bush"}],
        <<"      Bush">>},
       {"|safe",
        <<"{% autoescape on %}{{ var1|safe|escape }}{% endautoescape %}">>, [{var1, "&"}],
        <<"&">>},
       {"|safe is local",
        <<"{{ var1 }}{{ var1|safe }}{{ var1 }}">>, [{var1, "&"}], [], [auto_escape],
        <<"&amp;&&amp;">>},
       %%python/django slice is zero based, erlang lists are 1 based
       %%first number included, second number not
       %%negative numbers are allowed
       %%regex to convert from erlydtl_filters_tests:
                                                % for slice: \?assert.*\( \[(.*)\], erlydtl_filters:(.*)\((.*),"(.*)"\)\),
                                                % {"|slice:\"$4\"", <<"{{ var|$2:\"$4\" }}">>, [{var, $3}],<<$1>>},
                                                % \t\t{"|slice:\"$4\"",\n\t\t\t\t\t <<"{{ var|$2:\"$4\" }}">>, [{var, $3}],\n\t\t\t\t\t<<$1>>},
                                                %
                                                % for stringformat:
                                                % \?assert.*\( (.*), erlydtl_filters:(.*)\((.*), "(.*)"\) \)
                                                % \t\t{"|stringformat:\"$4\"",\n\t\t\t\t\t <<"{{ var|$2:\"$4\" }}">>, [{var, $3}],\n\t\t\t\t\t<<$1>>}

       {"|slice:\":\"",
        <<"{{ var|slice:\":\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<1,2,3,4,5,6,7,8,9>>},
       {"|slice:\"1\"",
        <<"{{ var|slice:\"1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<"2">>},
       {"|slice:\"100\"",
        <<"{{ var|slice:\"100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<"indexError">>},
       {"|slice:\"-1\"",
        <<"{{ var|slice:\"-1\" }}">>, [{var, ["a","b","c","d","e","f","g","h","i"]}],
        <<"i">>},
       {"|slice:\"-1\"",
        <<"{{ var|slice:\"-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<"9">>},
       {"|slice:\"-100\"",
        <<"{{ var|slice:\"-100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<"indexError">>},
       {"|slice:\"1:\"",
        <<"{{ var|slice:\"1:\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<2,3,4,5,6,7,8,9>>},
       {"|slice:\"100:\"",
        <<"{{ var|slice:\"100:\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},
       {"|slice:\"-1:\"",
        <<"{{ var|slice:\"-1:\" }}">>, [{var, ["a","b","c","d","e","f","h","i","j"]}],
        <<"j">>},
       {"|slice:\"-1:\"",
        <<"{{ var|slice:\"-1:\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<9>>},
       {"|slice:\"-100:\"",
        <<"{{ var|slice:\"-100:\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<1,2,3,4,5,6,7,8,9>>},

       {"|slice:\":1\"",
        <<"{{ var|slice:\":1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<1>>},
       {"|slice:\":100\"",
        <<"{{ var|slice:\":100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<1,2,3,4,5,6,7,8,9>>},
       {"|slice:\":-1\"",
        <<"{{ var|slice:\":-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<1,2,3,4,5,6,7,8>>},
       {"|slice:\":-100\"",
        <<"{{ var|slice:\":-100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},

       {"|slice:\"-1:-1\"",
        <<"{{ var|slice:\"-1:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},
       {"|slice:\"1:1\"",
        <<"{{ var|slice:\"1:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},
       {"|slice:\"1:-1\"",
        <<"{{ var|slice:\"1:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<2,3,4,5,6,7,8>>},
       {"|slice:\"-1:1\"",
        <<"{{ var|slice:\"-1:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},

       {"|slice:\"-100:-100\"",
        <<"{{ var|slice:\"-100:-100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},
       {"|slice:\"100:100\"",
        <<"{{ var|slice:\"100:100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},
       {"|slice:\"100:-100\"",
        <<"{{ var|slice:\"100:-100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},
       {"|slice:\"-100:100\"",
        <<"{{ var|slice:\"-100:100\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<1,2,3,4,5,6,7,8,9>>},


       {"|slice:\"1:3\"",
        <<"{{ var|slice:\"1:3\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<2,3>>},

       {"|slice:\"::\"",
        <<"{{ var|slice:\"::\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<1,2,3,4,5,6,7,8,9>>},
       {"|slice:\"1:9:1\"",
        <<"{{ var|slice:\"1:9:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<2,3,4,5,6,7,8,9>>},
       {"|slice:\"10:1:-1\"",
        <<"{{ var|slice:\"10:1:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<9,8,7,6,5,4,3>>},
       {"|slice:\"-111:-1:1\"",
        <<"{{ var|slice:\"-111:-1:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<1,2,3,4,5,6,7,8>>},

       {"|slice:\"-111:-111:1\"",
        <<"{{ var|slice:\"-111:-111:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},
       {"|slice:\"111:111:1\"",
        <<"{{ var|slice:\"111:111:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},
       {"|slice:\"-111:111:1\"",
        <<"{{ var|slice:\"-111:111:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<1,2,3,4,5,6,7,8,9>>},
       {"|slice:\"111:-111:1\"",
        <<"{{ var|slice:\"111:-111:1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},

       {"|slice:\"-111:-111:-1\"",
        <<"{{ var|slice:\"-111:-111:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},
       {"|slice:\"111:111:-1\"",
        <<"{{ var|slice:\"111:111:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},
       {"|slice:\"-111:111:-1\"",
        <<"{{ var|slice:\"-111:111:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<>>},
       {"|slice:\"111:-111:-1\"",
        <<"{{ var|slice:\"111:-111:-1\" }}">>, [{var, [1,2,3,4,5,6,7,8,9]}],
        <<9,8,7,6,5,4,3,2,1>>},              {"|phone2numeric",
                                              <<"{{ var1|phone2numeric }}">>, [{var1, "1-800-COLLECT"}],
                                              <<"1-800-2655328">>},
       {"|slugify",
        <<"{{ var1|slugify }}">>, [{var1, "What The $#_! Was He Thinking?"}],
        <<"what-the-_-was-he-thinking">>},
       {"|slice:\"s\"",
        <<"{{ var|stringformat:\"s\" }}">>, [{var, "test"}],
        <<"test">>},
       {"|stringformat:\"s\"",
        <<"{{ var|stringformat:\"s\" }}">>, [{var, "test"}],
        <<"test">>},
       {"|stringformat:\"s\"",
        <<"{{ var|stringformat:\"s\" }}">>, [{var, "1"}],
        <<"1">>},
       {"|stringformat:\"s\"",
        <<"{{ var|stringformat:\"s\" }}">>, [{var, "test"}],
        <<"test">>},
       {"|stringformat:\"10s\"",
        <<"{{ var|stringformat:\"10s\" }}">>, [{var, "test"}],
        <<"      test">>},
       {"|stringformat:\"-10s\"",
        <<"{{ var|stringformat:\"-10s\" }}">>, [{var, "test"}],
        <<"test      ">>},

       {"|stringformat:\"d\"",
        <<"{{ var|stringformat:\"d\" }}">>, [{var, "90"}],
        <<"90">>},
       {"|stringformat:\"10d\"",
        <<"{{ var|stringformat:\"10d\" }}">>, [{var, "90"}],
        <<"        90">>},
       {"|stringformat:\"-10d\"",
        <<"{{ var|stringformat:\"-10d\" }}">>, [{var, "90"}],
        <<"90        ">>},
       {"|stringformat:\"i\"",
        <<"{{ var|stringformat:\"i\" }}">>, [{var, "90"}],
        <<"90">>},
       {"|stringformat:\"10i\"",
        <<"{{ var|stringformat:\"10i\" }}">>, [{var, "90"}],
        <<"        90">>},
       {"|stringformat:\"-10i\"",
        <<"{{ var|stringformat:\"-10i\" }}">>, [{var, "90"}],
        <<"90        ">>},
       {"|stringformat:\"0.2d\"",
        <<"{{ var|stringformat:\"0.2d\" }}">>, [{var, "9"}],
        <<"09">>},
       {"|stringformat:\"10.4d\"",
        <<"{{ var|stringformat:\"10.4d\" }}">>, [{var, "9"}],
        <<"      0009">>},
       {"|stringformat:\"-10.4d\"",
        <<"{{ var|stringformat:\"-10.4d\" }}">>, [{var, "9"}],
        <<"0009      ">>},

       {"|stringformat:\"f\"",
        <<"{{ var|stringformat:\"f\" }}">>, [{var, "1"}],
        <<"1.000000">>},
       {"|stringformat:\".2f\"",
        <<"{{ var|stringformat:\".2f\" }}">>, [{var, "1"}],
        <<"1.00">>},
       {"|stringformat:\"0.2f\"",
        <<"{{ var|stringformat:\"0.2f\" }}">>, [{var, "1"}],
        <<"1.00">>},
       {"|stringformat:\"-0.2f\"",
        <<"{{ var|stringformat:\"-0.2f\" }}">>, [{var, "1"}],
        <<"1.00">>},
       {"|stringformat:\"10.2f\"",
        <<"{{ var|stringformat:\"10.2f\" }}">>, [{var, "1"}],
        <<"      1.00">>},
       {"|stringformat:\"-10.2f\"",
        <<"{{ var|stringformat:\"-10.2f\" }}">>, [{var, "1"}],
        <<"1.00      ">>},
       {"|stringformat:\".2f\"",
        <<"{{ var|stringformat:\".2f\" }}">>, [{var, "1"}],
        <<"1.00">>},
       {"|stringformat:\"x\"",
        <<"{{ var|stringformat:\"x\" }}">>, [{var, "90"}],
        <<"5a">>},
       {"|stringformat:\"X\"",
        <<"{{ var|stringformat:\"X\" }}">>, [{var, "90"}],
        <<"5A">>},

       {"|stringformat:\"o\"",
        <<"{{ var|stringformat:\"o\" }}">>, [{var, "90"}],
        <<"132">>},

       {"|stringformat:\"e\"",
        <<"{{ var|stringformat:\"e\" }}">>, [{var, "90"}],
        <<"9.000000e+01">>},
       {"|stringformat:\"e\"",
        <<"{{ var|stringformat:\"e\" }}">>, [{var, "90000000000"}],
        <<"9.000000e+10">>},
       {"|stringformat:\"E\"",
        <<"{{ var|stringformat:\"E\" }}">>, [{var, "90"}],
        <<"9.000000E+01">>},
       {"|striptags",
        <<"{{ var|striptags }}">>, [{var, "<b>Joel</b> <button>is</button> a <span>slug</span>"}],
        <<"Joel is a slug">>},
       {"|striptags",
        <<"{{ var|striptags }}">>, [{var, "<B>Joel</B> <button>is</button> a <span>slug</Span>"}],
        <<"Joel is a slug">>},
       {"|striptags",
        <<"{{ var|striptags }}">>, [{var, "Check out <a href=\"http://www.djangoproject.com\" rel=\"nofollow\">http://www.djangoproject.com</a>"}],
        <<"Check out http://www.djangoproject.com">>},
       {"|time:\"H:i\"",
        <<"{{ var|time:\"H:i\" }}">>, [{var, {{2010,12,1}, {10,11,12}} }],
        <<"10:11">>},
       {"|time",
        <<"{{ var|time }}">>, [{var, {{2010,12,1}, {10,11,12}} }],
        <<"10:11 a.m.">>},
       {"|timesince:from_date",
        <<"{{ from_date|timesince:conference_date }}">>, [{conference_date, {{2006,6,1},{8,0,0}} }, {from_date, {{2006,6,1},{0,0,0}} }],
        <<"8 hours">>},
       {"|timesince:from_date",
        <<"{{ from_date|timesince:conference_date }}">>, [{conference_date, {{2010,6,1},{8,0,0}} },{from_date, {{2006,6,1},{0,0,0}} }],
        <<"4 years, 1 day">>}, % leap year
       {"|timesince:from_date",
        <<"{{ from_date|timesince:conference_date }}">>, [{conference_date, {{2006,7,15},{8,0,0}} },{from_date, {{2006,6,1},{0,0,0}} }],
        <<"1 month, 2 weeks">>},
       {"|timeuntil:from_date",
        <<"{{ conference_date|timeuntil:from_date }}">>, [{conference_date, {{2006,6,1},{8,0,0}} }, {from_date, {{2006,6,1},{0,0,0}} }],
        <<"8 hours">>},
       {"|timeuntil:from_date",
        <<"{{ conference_date|timeuntil:from_date }}">>, [{conference_date, {{2010,6,1},{8,0,0}} },{from_date, {{2006,6,1},{0,0,0}} }],
        <<"4 years, 1 day">>},
       {"|timeuntil:from_date",
        <<"{{ conference_date|timeuntil:from_date }}">>, [{conference_date, {{2006,7,15},{8,0,0}} },{from_date, {{2006,6,1},{0,0,0}} }],
        <<"1 month, 2 weeks">>},
       {"|title",
        <<"{{ \"my title case\"|title }}">>, [],
        <<"My Title Case">>},
       {"|title (pre-formatted)",
        <<"{{ \"My Title Case\"|title }}">>, [],
        <<"My Title Case">>},
       {"|title (wacky separators)",
        <<"{{ \"my-title!case\"|title }}">>, [],
        <<"My-Title!Case">>},
       {"|title (numbers)",
        <<"{{ \"my-title123CaSe\"|title }}">>, [],
        <<"My-Title123case">>},
       {"|title (Irish names)",
        <<"{{ \"who's o'malley?\"|title }}">>, [],
        <<"Who's O'Malley?">>},
       {"|truncatechars:0",
        <<"{{ var1|truncatechars:0 }}">>, [{var1, "Empty Me"}],
        <<"...">>},
       {"|truncatechars:14",
        <<"{{ var1|truncatechars:14 }}">>, [{var1, "Truncate Me Please"}],
        <<"Truncate Me...">>},
       {"|truncatechars:17",
        <<"{{ var1|truncatechars:17 }}">>, [{var1, "Don't Truncate Me"}],
        <<"Don't Truncate Me">>},
       {"|truncatechars:4 (UTF-8)",
        <<"{{ var1|truncatechars:4 }}">>, [{var1, "\x{E2}\x{82}\x{AC}1.99"}],
        <<"\x{E2}\x{82}\x{AC}...">>},
       {"|truncatechars:5 (UTF-8)",
        <<"{{ var1|truncatechars:5 }}">>, [{var1, "\x{E2}\x{82}\x{AC} 1.99"}],
        <<"\x{E2}\x{82}\x{AC} ...">>},
       {"|truncatewords:0",
        <<"{{ var1|truncatewords:0 }}">>, [{var1, "Empty Me"}],
        <<" ...">>},
       {"|truncatewords:2",
        <<"{{ var1|truncatewords:2 }}">>, [{var1, "Truncate Me Please"}],
        <<"Truncate Me ...">>},
       {"|truncatewords:3",
        <<"{{ var1|truncatewords:3 }}">>, [{var1, "Don't Truncate Me"}],
        <<"Don't Truncate Me">>},
       {"|truncatewords_html:4",
        <<"{{ var1|truncatewords_html:4 }}">>, [{var1, "<p>The <strong>Long and <em>Winding</em> Road</strong> is too long</p>"}],
        <<"<p>The <strong>Long and <em>Winding</em>...</strong></p>">>},
       {"|truncatewords_html:50",
        <<"{{ var1|truncatewords_html:50 }}">>, [{var1, "<p>The <strong>Long and <em>Winding</em> Road</strong> is too long</p>"}],
        <<"<p>The <strong>Long and <em>Winding</em> Road</strong> is too long</p>">>},
       {"|unordered_list",
        <<"{{ var1|unordered_list }}">>, [{var1, ["States", ["Kansas", ["Lawrence", "Topeka"], "Illinois"]]}],
        <<"<li>States<ul><li>Kansas<ul><li>Lawrence</li><li>Topeka</li></ul></li><li>Illinois</li></ul></li>">>},
       {"|upper",
        <<"{{ message|upper }}">>, [{message, "That man has a gun."}],
        <<"THAT MAN HAS A GUN.">>},
       {"|urlencode",
        <<"{{ url|urlencode }}">>, [{url, "You #$*@!!"}],
        <<"You%20%23%24%2A%40%21%21">>},
       {"|urlencode",
        <<"{{ url|urlencode }}">>, [{url, "http://www.example.org/foo?a=b&c=d"}],
        <<"http%3A//www.example.org/foo%3Fa%3Db%26c%3Dd">>},
       {"|urlencode",
        <<"{{ url|urlencode:\"\" }}">>, [{url, "http://www.example.org/foo?a=b&c=d"}],
        <<"http%3A%2F%2Fwww.example.org%2Ffoo%3Fa%3Db%26c%3Dd">>},
       {"|urlencode",
        <<"{{ url|urlencode:\":/?=&\" }}">>, [{url, "http://www.example.org/foo?a=b&c=d"}],
        <<"http://www.example.org/foo?a=b&c=d">>},
       {"|urlize",
        <<"{{ var|urlize }}">>, [{var, "Check out www.djangoproject.com"}],
        <<"Check out <a href=\"http://www.djangoproject.com\" rel=\"nofollow\">www.djangoproject.com</a>">>},
       {"|urlize",
        <<"{{ var|urlize }}">>, [{var, "Check out http://www.djangoproject.com"}],
        <<"Check out <a href=\"http://www.djangoproject.com\" rel=\"nofollow\">http://www.djangoproject.com</a>">>},
       {"|urlize",
        <<"{{ var|urlize }}">>, [{var, "Check out \"http://www.djangoproject.com\""}],
        <<"Check out \"<a href=\"http://www.djangoproject.com\" rel=\"nofollow\">http://www.djangoproject.com</a>\"">>},
       {"|urlizetrunc:15",
        <<"{{ var|urlizetrunc:15 }}">>, [{var, "Check out www.djangoproject.com"}],
        <<"Check out <a href=\"http://www.djangoproject.com\" rel=\"nofollow\">www.djangopr...</a>">>},
       {"|wordcount",
        <<"{{ words|wordcount }}">>, [{words, "Why Hello There!"}],
        <<"3">>},
       {"|wordwrap:2",
        <<"{{ words|wordwrap:2 }}">>, [{words, "this is"}],
        <<"this \nis">>},
       {"|wordwrap:100",
        <<"{{ words|wordwrap:100 }}">>, [{words, "testing    testing"}],
        <<"testing    testing">>},
       {"|wordwrap:10",
        <<"{{ words|wordwrap:10 }}">>, [{words, ""}],
        <<"">>},
       {"|wordwrap:1",
        <<"{{ words|wordwrap:1 }}">>, [{words, "two"}],
        <<"two">>},
       %% yesno match: \?assert.*\( (.*), erlydtl_filters:(.*)\((.*), "(.*)"\)\)
       %% yesno replace: \t\t{"|$2:\"$4\"",\n\t\t\t\t\t <<"{{ var|$2:\"$4\" }}">>, [{var, $3}],\n\t\t\t\t\t<<$1>>}
       {"|yesno:\"yeah,no,maybe\"",
        <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, true}],
        <<"yeah">>},
       {"|yesno:\"yeah,no,maybe\"",
        <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, false}],
        <<"no">>},
       {"|yesno:\"yeah,no\"",
        <<"{{ var|yesno:\"yeah,no\" }}">>, [{var, undefined}],
        <<"no">>},
       {"|yesno:\"yeah,no,maybe\"",
        <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, undefined}],
        <<"maybe">>},

       {"string |yesno:\"yeah,no,maybe\"",
        <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, "non-empty string"}],
        <<"yeah">>},
       {"binary |yesno:\"yeah,no,maybe\"",
        <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, <<"non-empty binary">>}],
        <<"yeah">>},
       {"empty string |yesno:\"yeah,no,maybe\"",
        <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, ""}],
        <<"no">>},
       {"empty binary |yesno:\"yeah,no\"",
        <<"{{ var|yesno:\",no\" }}">>, [{var, <<"">>}],
        <<"no">>},
       {"term |yesno:\"yeah,,maybe\"",
        <<"{{ var|yesno:\"yeah,no,maybe\" }}">>, [{var, {my, [term, "test"]}}],
        <<"yeah">>},
       {"|yesno:\"yeah,\"",
        <<"{{ var|yesno:\"yeah,\" }}">>, [{var, false}],
        <<"">>},
       {"|yesno:\"yeah,,maybe\"",
        <<"{{ var|yesno:\"yeah,,maybe\" }}">>, [{var, false}],
        <<"">>},
       #test{
          title = "|yesno:\"missing_false_choice\"",
          source = <<"{{ var|yesno:\"missing_false_choice\" }}">>,
          render_vars = [{var, true}],
          output = {error, {yesno, choices}}
         },
       {"escape only once (#150) - no auto escape",
        %% note that auto_escape is off by default in the test suite
        %% due to how the tests have been written (and it's too much
        %% work for me to rewrite them)
        <<"{{ foo }}{{ foo|add:'bar' }}">>,
        [{foo, "foo&"}],
        <<"foo&foo&bar">>},
       {"escape only once (#150) - auto escape block",
        <<"{% autoescape on %}{{ foo }}{{ foo|add:'bar' }}{% endautoescape %}">>,
        [{foo, "foo&"}],
        <<"foo&amp;foo&amp;bar">>},
       {"escape only once (#150) - auto escape",
        <<"{{ foo }}{{ foo|add:'bar' }}">>,
        [{foo, "foo&"}], [], [auto_escape],
        <<"foo&amp;foo&amp;bar">>},
       {"escape only once (#150) - auto escape, safe",
        <<"{{ foo|safe }}{{ foo|add:'bar'|safe }}&{{ foo|safe|add:'bar' }}">>,
        [{foo, "foo&"}], [], [auto_escape],
        <<"foo&foo&bar&foo&bar">>},
       {"escape only once (#150) - escape filter",
        <<"{{ foo|escape }}{{ foo|add:'bar'|escape }}&{{ foo|escape|add:'bar' }}">>,
        [{foo, "foo&"}],
        <<"foo&amp;foo&amp;bar&foo&amp;bar">>},
       {"escape only once (#150) - auto escape + escape filter",
        <<"{{ foo|escape }}{{ foo|add:'bar'|escape }}&{{ foo|escape|add:'bar' }}">>,
        [{foo, "foo&"}], [], [auto_escape],
        <<"foo&amp;foo&amp;bar&foo&amp;bar">>}
      ]},
     {"filters_if",
      [{"Filter if 1.1",
        <<"{% if var1|length_is:0 %}Y{% else %}N{% endif %}">>,
        [{var1, []}],
        <<"Y">>},
       {"Filter if 1.2",
        <<"{% if var1|length_is:1 %}Y{% else %}N{% endif %}">>,
        [{var1, []}],
        <<"N">>},
       {"Filter if 1.3",
        <<"{% if var1|length_is:7 %}Y{% else %}N{% endif %}">>,
        [{var1, []}],
        <<"N">>},
       {"Filter if 2.1",
        <<"{% if var1|length_is:0 %}Y{% else %}N{% endif %}">>,
        [{var1, ["foo"]}],
        <<"N">>},
       {"Filter if 2.2",
        <<"{% if var1|length_is:1 %}Y{% else %}N{% endif %}">>,
        [{var1, ["foo"]}],
        <<"Y">>},
       {"Filter if 2.3",
        <<"{% if var1|length_is:7 %}Y{% else %}N{% endif %}">>,
        [{var1, ["foo"]}],
        <<"N">>},
       {"Filter if 3.1",
        <<"{% ifequal var1|length 0 %}Y{% else %}N{% endifequal %}">>,
        [{var1, []}],
        <<"Y">>},
       {"Filter if 3.2",
        <<"{% ifequal var1|length 1 %}Y{% else %}N{% endifequal %}">>,
        [{var1, []}],
        <<"N">>},
       {"Filter if 4.1",
        <<"{% ifequal var1|length 3 %}Y{% else %}N{% endifequal %}">>,
        [{var1, ["foo", "bar", "baz"]}],
        <<"Y">>},
       {"Filter if 4.2",
        <<"{% ifequal var1|length 0 %}Y{% else %}N{% endifequal %}">>,
        [{var1, ["foo", "bar", "baz"]}],
        <<"N">>},
       {"Filter if 4.3",
        <<"{% ifequal var1|length 1 %}Y{% else %}N{% endifequal %}">>,
        [{var1, ["foo", "bar", "baz"]}],
        <<"N">>}
      ]},
     {"firstof",
      [{"Firstof first",
        <<"{% firstof foo bar baz %}">>,
        [{foo, "1"},{bar, "2"}],
        <<"1">>},
       {"Firstof second",
        <<"{% firstof foo bar baz %}">>,
        [{bar, "2"}],
        <<"2">>},
       {"Firstof none",
        <<"{% firstof foo bar baz %}">>,
        [],
        <<"">>},
       {"Firstof complex",
        <<"{% firstof foo.bar.baz bar %}">>,
        [{foo, [{bar, [{baz, "quux"}]}]}],
        <<"quux">>},
       {"Firstof undefined complex",
        <<"{% firstof foo.bar.baz bar %}">>,
        [{bar, "bar"}],
        <<"bar">>},
       {"Firstof literal",
        <<"{% firstof foo bar \"baz\" %}">>,
        [],
        <<"baz">>}
      ]},
     {"regroup .. endregroup",
      [{"Ordered",
        <<"{% regroup people by gender as gender_list %}{% for gender in gender_list %}{{ gender.grouper }}\n{% for item in gender.list %}{{ item.first_name }}\n{% endfor %}{% endfor %}{% endregroup %}">>,
        [{people, [[{first_name, "George"}, {gender, "Male"}], [{first_name, "Bill"}, {gender, "Male"}],
                   [{first_name, "Margaret"}, {gender, "Female"}], [{first_name, "Condi"}, {gender, "Female"}]]}],
        <<"Male\nGeorge\nBill\nFemale\nMargaret\nCondi\n">>},
       {"Unordered",
        <<"{% regroup people by gender as gender_list %}{% for gender in gender_list %}{{ gender.grouper }}\n{% for item in gender.list %}{{ item.first_name }}\n{% endfor %}{% endfor %}{% endregroup %}">>,
        [{people, [[{first_name, "George"}, {gender, "Male"}],
                   [{first_name, "Margaret"}, {gender, "Female"}],
                   [{first_name, "Condi"}, {gender, "Female"}],
                   [{first_name, "Bill"}, {gender, "Male"}]
                  ]}],
        <<"Male\nGeorge\nFemale\nMargaret\nCondi\nMale\nBill\n">>},
       {"NestedOrdered",
        <<"{% regroup people by name.last as lastname_list %}{% for lastname in lastname_list %}{{ lastname.grouper }}\n{% for item in lastname.list %}{{ item.name.first }}\n{% endfor %}{% endfor %}{% endregroup %}">>,
        [{people, [[{name, [{first,"George"},{last,"Costanza"}]}],
                   [{name, [{first,"Margaret"},{last,"Costanza"}]}],
                   [{name, [{first,"Bill"},{last,"Buffalo"}]}],
                   [{name, [{first,"Condi"},{last,"Buffalo"}]}]]}],
        <<"Costanza\nGeorge\nMargaret\nBuffalo\nBill\nCondi\n">>},
       {"NestedUnordered",
        <<"{% regroup people by name.last as lastname_list %}{% for lastname in lastname_list %}{{ lastname.grouper }}\n{% for item in lastname.list %}{{ item.name.first }}\n{% endfor %}{% endfor %}{% endregroup %}">>,
        [{people, [[{name, [{first,"George"},{last,"Costanza"}]}],
                   [{name, [{first,"Bill"},{last,"Buffalo"}]}],
                   [{name, [{first,"Margaret"},{last,"Costanza"}]}],
                   [{name, [{first,"Condi"},{last,"Buffalo"}]}]]}],
        <<"Costanza\nGeorge\nBuffalo\nBill\nCostanza\nMargaret\nBuffalo\nCondi\n">>},
       {"Filter",
        <<"{% regroup people|dictsort:\"name.last\" by name.last as lastname_list %}{% for lastname in lastname_list %}{{ lastname.grouper }}\n{% for item in lastname.list %}{{ item.name.first }}\n{% endfor %}{% endfor %}{% endregroup %}">>,
        [{people, [[{name, [{first,"George"},{last,"Costanza"}]}],
                   [{name, [{first,"Bill"},{last,"Buffalo"}]}],
                   [{name, [{first,"Margaret"},{last,"Costanza"}]}],
                   [{name, [{first,"Condi"},{last,"Buffalo"}]}]]}],
        <<"Buffalo\nBill\nCondi\nCostanza\nGeorge\nMargaret\n">>}
      ]},
     {"regroup",
      [{"Ordered",
        <<"{% regroup people by gender as gender_list %}{% for gender in gender_list %}{{ gender.grouper }}\n{% for item in gender.list %}{{ item.first_name }}\n{% endfor %}{% endfor %}">>,
        [{people, [[{first_name, "George"}, {gender, "Male"}], [{first_name, "Bill"}, {gender, "Male"}],
                   [{first_name, "Margaret"}, {gender, "Female"}], [{first_name, "Condi"}, {gender, "Female"}]]}],
        <<"Male\nGeorge\nBill\nFemale\nMargaret\nCondi\n">>},
       {"Unordered",
        <<"{% regroup people by gender as gender_list %}{% for gender in gender_list %}{{ gender.grouper }}\n{% for item in gender.list %}{{ item.first_name }}\n{% endfor %}{% endfor %}">>,
        [{people, [[{first_name, "George"}, {gender, "Male"}],
                   [{first_name, "Margaret"}, {gender, "Female"}],
                   [{first_name, "Condi"}, {gender, "Female"}],
                   [{first_name, "Bill"}, {gender, "Male"}]
                  ]}],
        <<"Male\nGeorge\nFemale\nMargaret\nCondi\nMale\nBill\n">>},
       {"NestedOrdered",
        <<"{% regroup people by name.last as lastname_list %}{% for lastname in lastname_list %}{{ lastname.grouper }}\n{% for item in lastname.list %}{{ item.name.first }}\n{% endfor %}{% endfor %}">>,
        [{people, [[{name, [{first,"George"},{last,"Costanza"}]}],
                   [{name, [{first,"Margaret"},{last,"Costanza"}]}],
                   [{name, [{first,"Bill"},{last,"Buffalo"}]}],
                   [{name, [{first,"Condi"},{last,"Buffalo"}]}]]}],
        <<"Costanza\nGeorge\nMargaret\nBuffalo\nBill\nCondi\n">>},
       {"NestedUnordered",
        <<"{% regroup people by name.last as lastname_list %}{% for lastname in lastname_list %}{{ lastname.grouper }}\n{% for item in lastname.list %}{{ item.name.first }}\n{% endfor %}{% endfor %}">>,
        [{people, [[{name, [{first,"George"},{last,"Costanza"}]}],
                   [{name, [{first,"Bill"},{last,"Buffalo"}]}],
                   [{name, [{first,"Margaret"},{last,"Costanza"}]}],
                   [{name, [{first,"Condi"},{last,"Buffalo"}]}]]}],
        <<"Costanza\nGeorge\nBuffalo\nBill\nCostanza\nMargaret\nBuffalo\nCondi\n">>},
       {"Filter",
        <<"{% regroup people|dictsort:\"name.last\" by name.last as lastname_list %}{% for lastname in lastname_list %}{{ lastname.grouper }}\n{% for item in lastname.list %}{{ item.name.first }}\n{% endfor %}{% endfor %}">>,
        [{people, [[{name, [{first,"George"},{last,"Costanza"}]}],
                   [{name, [{first,"Bill"},{last,"Buffalo"}]}],
                   [{name, [{first,"Margaret"},{last,"Costanza"}]}],
                   [{name, [{first,"Condi"},{last,"Buffalo"}]}]]}],
        <<"Buffalo\nBill\nCondi\nCostanza\nGeorge\nMargaret\n">>},
       {"With surrounding context",
        <<"People: {% regroup people by gender as gender_list %}{% for gender in gender_list %}{{ gender.grouper }}\n{% for item in gender.list %}{{ item.first_name }}\n{% endfor %}{% endfor %}Done.">>,
        [{people, [[{first_name, "George"}, {gender, "Male"}], [{first_name, "Bill"}, {gender, "Male"}],
                   [{first_name, "Margaret"}, {gender, "Female"}], [{first_name, "Condi"}, {gender, "Female"}]]}],
        <<"People: Male\nGeorge\nBill\nFemale\nMargaret\nCondi\nDone.">>},
       #test{
          title = "regroup record",
          source = <<"{% regroup people by gender as gender_list %}{% for gender in gender_list %}{{ gender.grouper }}:\n{% for person in gender.list %} - {{ person.first_name }}\n{% endfor %}{% endfor %}">>,
          compile_opts = [{record_info, [{person, record_info(fields, person)}]} | (#test{})#test.compile_opts],
          render_vars = [{people, [#person{ first_name = "George", gender = "Male" },
                                   #person{ first_name = "Bill", gender = "Male" },
                                   #person{ first_name = "Margaret", gender = "Female" },
                                   #person{ first_name = "Condi", gender = "Female" }
                                  ]}
                        ],
          output = <<"Male:\n - George\n - Bill\nFemale:\n - Margaret\n - Condi\n">>
         }
      ]},
     {"spaceless",
      [{"Beginning", <<"{% spaceless %}    <b>foo</b>{% endspaceless %}">>, [], <<"<b>foo</b>">>},
       {"Middle", <<"{% spaceless %}<b>foo</b>  <b>bar</b>{% endspaceless %}">>, [], <<"<b>foo</b><b>bar</b>">>},
       {"End", <<"{% spaceless %}<b>foo</b>  {% endspaceless %}">>, [], <<"<b>foo</b>">>},
       {"NewLine", <<"{% spaceless %}\n<div> \n <b>foo</b> \n </div>\n {% endspaceless %}">>, [], <<"<div><b>foo</b></div>">>}
      ]},
     {"templatetag",
      [{"openblock", <<"{% templatetag openblock %}">>, [], <<"{%">>},
       {"closeblock", <<"{% templatetag closeblock %}">>, [], <<"%}">>},
       {"openvariable", <<"{% templatetag openvariable %}">>, [], <<"{{">>},
       {"closevariable", <<"{% templatetag closevariable %}">>, [], <<"}}">>},
       {"openbrace", <<"{% templatetag openbrace %}">>, [], <<"{">>},
       {"closebrace", <<"{% templatetag closebrace %}">>, [], <<"}">>},
       {"opencomment", <<"{% templatetag opencomment %}">>, [], <<"{#">>},
       {"closecomment", <<"{% templatetag closecomment %}">>, [], <<"#}">>}
      ]},
     {"trans",
      [{"trans functional default locale",
        <<"Hello {% trans \"Hi\" %}">>, [], <<"Hello Hi">>
       },
       {"trans functional reverse locale",
        <<"Hello {% trans \"Hi\" %}">>, [], [{locale, "reverse"}],
        [{locales, ["reverse"]}, {translation_fun, fun("Hi"=Key, "reverse") -> list_to_binary(lists:reverse(Key)) end}],
        <<"Hello iH">>
       },
       {"trans literal at run-time",
        <<"Hello {% trans \"Hi\" %}">>, [], [{translation_fun, fun("Hi") -> "Konichiwa" end}], [],
        <<"Hello Konichiwa">>},
       {"trans variable at run-time",
        <<"Hello {% trans var1 %}">>, [{var1, <<"Hi">>}], [{translation_fun, fun(<<"Hi">>) -> <<"Konichiwa">> end}], [],
        <<"Hello Konichiwa">>},
       {"trans literal at run-time: No-op",
        <<"Hello {% trans \"Hi\" noop %}">>, [], [{translation_fun, fun("Hi") -> <<"Konichiwa">> end}], [],
        <<"Hello Hi">>},
       {"trans variable at run-time: No-op",
        <<"Hello {% trans var1 noop %}">>, [{var1, <<"Hi">>}], [{translation_fun, fun(<<"Hi">>) -> <<"Konichiwa">> end}], [],
        <<"Hello Hi">>},
       {"trans as",
        <<"{% trans 'Hans' as name %}Hello {{ name }}">>, [],
        <<"Hello Hans">>},
       {"trans value",
        <<"{{ _('foo') }}">>, [], [], [{locale, default}, {translation_fun, fun ("foo") -> "bar" end}],
        <<"bar">>},
       {"filtered value",
        <<"{{ _('foo')|reverse }}">>, [], [],
        [{locale, default},
         {translation_fun, fun ("foo") -> "bar" end},
         {default_libraries, [test1]},
         {libraries, [{test1, erlydtl_lib_test1}]}],
        <<"rab">>}
      ]},
     {"blocktrans",
      [{"blocktrans default locale",
        <<"{% blocktrans %}Hello{% endblocktrans %}">>, [], <<"Hello">>},
       {"blocktrans choose locale",
        <<"{% blocktrans %}Hello, {{ name }}{% endblocktrans %}">>, [{name, "Mr. President"}], [{locale, "de"}],
        [{locales, ["de"]}, {translation_fun, fun("Hello, {{ name }}", "de") -> <<"Guten tag, {{ name }}">> end}], <<"Guten tag, Mr. President">>},
       {"blocktrans with args",
        <<"{% blocktrans with var1=foo %}{{ var1 }}{% endblocktrans %}">>, [{foo, "Hello"}], <<"Hello">>},
       #test{
          title = "blocktrans blocks in content not allowed",
          source = <<"{% blocktrans %}Hello{%if name%}, {{ name }}{%endif%}!{% endblocktrans %}">>,
          errors = [error_info([{{1, 24}, erlydtl_parser, ["syntax error before: ",["\"if\""]]}])]
         },
       #test{
          title = "blocktrans nested variables not allowed",
          source = <<"{% blocktrans %}Hello, {{ user.name }}!{% endblocktrans %}">>,
          errors = [error_info([{{1,31}, erlydtl_parser, ["syntax error before: ","'.'"]}])]
         },
       {"blocktrans runtime",
        <<"{%blocktrans with v1=foo%}Hello, {{ name }}! See {{v1}}.{%endblocktrans%}">>,
        [{name, "Mr. President"}, {foo, <<"rubber-duck">>}],
        [{translation_fun, fun("Hello, {{ name }}! See {{ v1 }}.") -> <<"Guten tag, {{name}}! Sehen {{    v1   }}.">> end}],
        [], <<"Guten tag, Mr. President! Sehen rubber-duck.">>},
       {"trimmed",
        <<"{% blocktrans trimmed %}\n  foo  \n   bar   here .\n \n   \n baz{% endblocktrans %}">>,
        [], [{translation_fun, fun ("foo bar   here . baz") -> "ok" end}],
        <<"ok">>}
      ]},
     {"extended translation features (#131)",
      [{"trans default locale",
        <<"test {% trans 'message' %}">>,
        [], [{translation_fun, fun ("message", default) -> "ok" end}],
        <<"test ok">>},
       {"trans foo locale",
        <<"test {% trans 'message' %}">>,
        [], [{locale, "foo"}, {translation_fun, fun ("message", "foo") -> "ok" end}],
        <<"test ok">>},
       {"trans context (run-time)",
        <<"test {% trans 'message' context 'foo' %}">>,
        [], [{translation_fun, fun ("message", {default, "foo"}) -> "ok" end}],
        <<"test ok">>},
       {"trans context (compile-time)",
        <<"test {% trans 'message' context 'foo' %}">>,
        [], [{locale, "baz"}],
        [{locales, ["bar", "baz"]},
         {translation_fun, fun ("message", {L, "foo"}) ->
                                  case L of
                                      "bar" -> "rab";
                                      "baz" -> "ok"
                                  end
                          end}],
        <<"test ok">>},
       {"trans context noop",
        <<"{% trans 'message' noop context 'foo' %}">>, [], [],
        <<"message">>},
       {"blocktrans context (run-time)",
        <<"{% blocktrans context 'bar' %}translate this{% endblocktrans %}">>,
        [], [{locale, "foo"}, {translation_fun,
                               fun ("translate this", {"foo", "bar"}) ->
                                       "got it"
                               end}],
        <<"got it">>},
       {"blocktrans context (compile-time)",
        <<"{% blocktrans context 'bar' %}translate this{% endblocktrans %}">>,
        [], [{locale, "foo"}],
        [{locale, "foo"}, {translation_fun,
                           fun ("translate this", {"foo", "bar"}) ->
                                   "got it"
                           end}],
        <<"got it">>},
       {"blocktrans plural",
        <<"{% blocktrans count foo=bar %}",
          "There is just one foo..",
          "{% plural %}",
          "There are many foo's..",
          "{% endblocktrans %}">>,
        [{bar, 2}], [{locale, "baz"},
                     {translation_fun,
                      fun ({"There is just one foo..", {"There are many foo's..", 2}}, "baz") ->
                              "ok"
                      end}],
        <<"ok">>},
       {"blocktrans a lot of stuff",
        <<"{% blocktrans with foo=a.b count c=a|length context 'quux' %}"
          "foo={{ foo }};bar={{ bar }};c={{ c }}:"
          "{% plural %}"
          "FOO:{{ foo }},BAR:{{ bar }},C:{{ c }}."
          "{% endblocktrans %}">>,
        [{a, [{b, "B"}]}, {bar, "BAR"}],
        [{locale, "rub"},
         {translation_fun, fun ({Single, {Plural, "1"=_Count}}, {Locale, Context}) ->
                                   [Single, Plural, Locale, Context]
                           end}],
        <<"foo=B;bar=BAR;c=1:"
          "FOO:B,BAR:BAR,C:1."
          "rub" "quux">>},
       {"new translation options",
        <<"{% trans foo %}{% blocktrans %}abc{% endblocktrans %}">>,
        [{foo, "1234"}], [{locale, "test"}, {translation_fun, fun (Msg) -> lists:reverse(Msg) end}],
        [{locale, "foo"}, {locale, "test"}, {locales, ["bar", "baz"]},
         {translation_fun, fun (Msg, _) -> [Msg, lists:reverse(Msg)] end}],
        <<"4321" "abccba">>}

       %% This does work, but always prints a warning to std err.. :/
       %% Warning: template translation: variable not closed: "bar {{ 123"
       %% {"variable error",
       %%  <<"{% blocktrans %}foo{{ bar }}{% endblocktrans %}">>,
       %%  [], [{translation_fun, fun (_) -> "bar {{ 123" end}],
       %%  <<"foo">>}
      ]},
     {"i18n",
      [{"setup translation context, using fun, at render time",
        <<"{% trans 'foo' %}">>, [],
        [{translation_fun, fun () -> fun (Msg) -> string:to_upper(Msg) end end}],
        <<"FOO">>},
       {"setup translation context, using fun, at compile time",
        <<"{% trans 'foo' %}">>, [], [],
        [{locale, default}, {translation_fun, fun () -> fun lists:reverse/1 end}],
        <<"oof">>}
      ]},
     {"language",
      [{"override locale",
        <<"{% trans 'foo' %}{% language 'other' %}{% trans 'foo' %}{% endlanguage %}">>,
        [], [{locale, <<"default">>}, {translation_fun, fun ("foo", <<"default">>) -> "1"; ("foo", <<"other">>) -> "2"; (A, B) -> [A, B] end}],
        <<"12">>}
      ]},
     {"verbatim",
      [{"Plain verbatim",
        <<"{% verbatim %}{{ oh no{% foobar %}{% endverbatim %}">>, [],
        <<"{{ oh no{% foobar %}">>},
       {"Named verbatim",
        <<"{% verbatim foobar %}{% verbatim %}{% endverbatim foobar2 %}{% endverbatim foobar %}">>, [],
        <<"{% verbatim %}{% endverbatim foobar2 %}">>}
      ]},
     {"widthratio",
      [{"Literals", <<"{% widthratio 5 10 100 %}">>, [], <<"50">>},
       {"Rounds up", <<"{% widthratio a b 100 %}">>, [{a, 175}, {b, 200}], <<"88">>}
      ]},
     {"with",
      [{"Cache literal",
        <<"{% with a=1 %}{{ a }}{% endwith %}">>, [], <<"1">>},
       {"Cache variable",
        <<"{% with a=b %}{{ a }}{% endwith %}">>, [{b, "foo"}], <<"foo">>},
       {"Cache variable with attribute",
        <<"I enjoy {% with a = var1 %}{{ a.game }}{% endwith %}">>, [{var1, [{game, "Othello"}]}], <<"I enjoy Othello">>},
       {"Cache variable attribute",
        <<"I enjoy {% with a = var1.game %}{{ a }}{% endwith %}">>, [{var1, [{game, "Othello"}]}], <<"I enjoy Othello">>},
       {"Cache multiple",
        <<"{% with alpha=1 beta=b %}{{ alpha }}/{{ beta }}{% endwith %}">>, [{b, 2}], <<"1/2">>}
      ]},
     {"unicode",
      [{"(tm) somewhere",
        <<"™">>, [], <<"™">>}
      ]},
     {"contrib_humanize",
      [{"intcomma",
        <<"{{ a|intcomma }} {{ b|intcomma }} {{ c|intcomma }} {{ d|intcomma }}">>,
        [{a, 999}, {b, 123456789}, {c, 12345}, {d, 1234567890}], [],
        [{custom_filters_modules, [erlydtl_contrib_humanize]}],
        <<"999 123,456,789 12,345 1,234,567,890">>}
      ]},
     %% custom syntax stuff
     {"extension_module",
      [ %% the erlydtl_test_extension module replaces a foo identifier with bar when hitting a # following foo.
        {"replace parsed token", <<"{{ foo # }}">>, [{bar, "ok"}], [],
         [{extension_module, erlydtl_test_extension}], <<"ok">>},
        #test{
           title = "proper error message",
           source = <<"{{ bar # }}">>,
           render_vars = [{bar, "ok"}],
           compile_opts = [{extension_module, erlydtl_test_extension},
                           report, return, force_recompile, {out_dir, false}],
           errors = [error_info([{{1,8},erlydtl_scanner,{illegal_char, $#}}])]
          },
        %% accept identifiers as expressions (this is a dummy functionality to test the parser extensibility)
        {"identifiers as expressions", <<"{{ foo.bar or baz }}">>, [{baz, "ok"}], [],
         [{extension_module, erlydtl_test_extension}], <<"ok">>}
      ]},
     {"records",
      [{"field access",
        <<"{{ r.baz }}">>, [{r, #testrec{ foo="Foo", bar="Bar", baz="Baz" }}], [],
        [{record_info, [{testrec, record_info(fields, testrec)}]}],
        <<"Baz">>}
      ]},
     {"error reporting",
      [#test{
          title = "no out dir warning",
          source = <<"foo bar">>,
          compile_opts = [report, return, force_recompile],
          output = <<"foo bar">>,
          warnings = [error_info([no_out_dir])]
         },
       #test{
          title = "warnings as errors",
          source = <<"foo bar">>,
          compile_opts = [report, return, warnings_as_errors, force_recompile],
          errors = [error_info([no_out_dir])]
         },
       #test{
          title = "illegal character",
          source = <<"{{{">>,
          errors = [error_info([{{1,3},erlydtl_scanner,{illegal_char, ${}}])]
         },
       #test{
          title = "unexpected end of file - in code",
          source = <<"{{">>,
          errors = [error_info([{{1,3},erlydtl_scanner,{eof, in_code}}])]
         },
       #test{
          title = "unexpected end of file - in comment",
          source = <<"{#">>,
          errors = [error_info([{{1,3},erlydtl_scanner,{eof, in_comment}}])]
         },
       {"unknown library",
        <<"{% load foo %}">>, [], [], [],
        <<>>,
        [error_info(
           [{{1,9},erlydtl_compiler_utils,{load_library,foo,foo,nofile}}
           ])]
       },
       {"not a library",
        <<"{% load foo %}">>, [], [],
        [{libraries, [{foo, ?MODULE}]}],
        <<>>,
        [error_info(
           [{{1,9},erlydtl_compiler_utils,{load_library,foo,?MODULE,behaviour}}
           ])]
       },
       {"library version",
        <<"{% load foo %}">>, [], [],
        [{libraries, [{foo, erlydtl_lib_testversion}]}],
        <<>>,
        [error_info(
           [{{1,9},erlydtl_compiler_utils,{load_library,foo,erlydtl_lib_testversion,{version,invalid}}}
           ])]
       },
       {"not in library",
        <<"{% load foo bar from test1 %}\n{{ \"w00t\"|reverse }}">>, [], [],
        [{libraries, [{test1, erlydtl_lib_test1}]}],
        <<"\n">>,
        [error_info(
           [{{2,11},erlydtl_beam_compiler,{unknown_filter,reverse,1}},
            {{1,22},erlydtl_compiler_utils,{load_from,test1,erlydtl_lib_test1,foo}},
            {{1,22},erlydtl_compiler_utils,{load_from,test1,erlydtl_lib_test1,bar}}
           ])]
       },
       {"pre load unknown library",
        <<"{{ '123'|reverse }}">>, [], [],
        [{default_libraries, [test1]}],
        <<"">>,
        [error_info(
           [{{1,10},erlydtl_beam_compiler,{unknown_filter,reverse,1}},
            {none,erlydtl_compiler_utils,{load_library,test1,test1,nofile}}
           ])]
       },
       {"pre load unknown legacy library",
        <<"{% foo %}">>, [], [],
        [{custom_tags_modules, [foo]}],
        <<"">>,
        [error_info(
           [{none,erlydtl_beam_compiler,{unknown_tag, foo}},
            {none,erlydtl_compiler,{load_library,'(custom-legacy)',foo,nofile}}
           ])]
       },
       {"unknown filter",
        <<"{{ '123'|foo }}">>, [], [], [],
        <<"">>,
        [error_info([{{1,10},erlydtl_beam_compiler,{unknown_filter,foo,1}}])]
       },
       {"unknown tag",
        <<"a{% b %}c">>, [], [], [],
        <<"ac">>,
        [error_info([{none,erlydtl_beam_compiler,{unknown_tag, b}}])]
       },
       {"ssi file not found",
        <<"{% ssi 'foo' %}">>, [],
        {error, {read_file, <<"./foo">>, enoent}}
       },
       {"deprecated compile options",
        <<"">>, [], [],
        [{blocktrans_locales, []}, {blocktrans_fun, fun (_) -> [] end}],
        <<"">>,
        [error_info([{deprecated_option, O, N}
                     || {O, N} <- [{blocktrans_locales, locales},
                                   {blocktrans_fun, translation_fun}]],
                    erlydtl_compiler)]
       }
      ]},
     {"load",
      [{"filter",
        <<"{% load test1 %}{{ \"1234\"|reverse }}">>, [], [],
        [{libraries, [{test1, erlydtl_lib_test1}]}],
        <<"4321">>
       },
       {"named",
        <<"{% load reverse from test1 %}{{ \"abcd\"|reverse }}">>, [], [],
        [{libraries, [{test1, erlydtl_lib_test1}]}],
        <<"dcba">>
       },
       {"pre loaded",
        <<"{{ QWER|reverse }}">>, [{'QWER', "Qwerty"}], [],
        [{default_libraries, [test1]},
         {libraries, [{test1, erlydtl_lib_test1}]}],
        <<"ytrewQ">>
       },
       {"lib with multiple behaviours",
        <<"{{ QWER|reverse }}">>, [{'QWER', "Qwerty"}], [],
        [{default_libraries, [test2]},
         {libraries, [{test2, erlydtl_lib_test2}]}],
        <<"ytrewQ">>
       },
       {"lib with multiple behaviors (alternative spelling)",
        <<"{{ QWER|reverse }}">>, [{'QWER', "Qwerty"}], [],
        [{default_libraries, [test2]},
         {libraries, [{test2, erlydtl_lib_test2a}]}],
        <<"ytrewQ">>
       }
      ]},
     {"compile time default vars/constants",
      begin
          Tpl = <<"Test {{ var1 }}:{{ var2 }}.">>,
          Txt = <<"Test 123:abc.">>,
          Fun = fun (F) ->
                        fun (#test{ module=M }) ->
                                M:F()
                        end
                end,
          [{"default vars",
            Tpl, [], [],
            [{default_vars, [{var1, 123}, {var2, abc}]}], Txt},
           {"default vars (using fun)",
            Tpl, [], [],
            [{default_vars, [{var1, 123}, {var2, fun () -> abc end}]}], Txt},
           {"override default vars",
            Tpl, [{var2, abc}], [],
            [{default_vars, [{var1, 123}, {var2, 456}]}], Txt},
           {"constants",
            Tpl, [], [],
            [{constants, [{var1, 123}, {var2, abc}]}], Txt},
           {"constants (using fun)",
            Tpl, [], [],
            [{constants, [{var1, 123}, {var2, fun () -> abc end}]}], Txt},
           {"constants non-overridable",
            Tpl, [{var1, ohno}, {var2, noway}], [],
            [{constants, [{var1, 123}, {var2, "abc"}]}], Txt}
           |[#test{ title = T,
                    source = Tpl,
                    compile_vars = undefined,
                    compile_opts = CO ++ (#test{})#test.compile_opts,
                    renderer = Fun(F),
                    output = O
                  }
             || {T, F, O, CO} <-
                    [{"variables/0",
                      variables, [var1, var2], []},
                     {"variables/0 w. defaults",
                      variables, [var1, var2], [{default_vars, [{var1, aaa}]}]},
                     {"variables/0 w. constants",
                      variables, [var2], [{constants, [{var1, bbb}]}]},
                     {"default_variables/0",
                      default_variables, [], []},
                     {"default_variables/0 w. defaults",
                      default_variables, [var1], [{default_vars, [{var1, aaa}]}]},
                     {"default_variables/0 w. constants",
                      default_variables, [], [{constants, [{var1, bbb}]}]},
                     {"constants/0",
                      constants, [], []},
                     {"constants/0 w. defaults",
                      constants, [], [{default_vars, [{var1, aaa}]}]},
                     {"constants/0 w. constants",
                      constants, [var1], [{constants, [{var1, bbb}]}]}
                    ]
            ]]
      end},
     {"functional",
      [functional_test(F)
       %% order is important for a few of these tests, unfortunately.

       || F <- ["autoescape", "comment", "extends", "filters", "for", "for_list", "for_tuple",
                "for_list_preset", "for_preset", "for_records", "for_records_preset", "include",
                "if", "if_preset", "ifequal", "ifequal_preset", "ifnotequal", "ifnotequal_preset",
                "now", "var", "var_preset", "cycle", "custom_tag", "custom_tag1", "custom_tag2",
                "custom_tag3", "custom_tag4", "custom_tag_var", "custom_tag_lib_var", "custom_call", "include_template", "include_path",
                "ssi", "extends_path", "extends_path2", "trans", "extends_for", "extends2",
                "extends3", "recursive_block", "extend_recursive_block", "missing", "block_super",
                "wrapper", "extends4", "super_escaped", "extends_chain", "reader_options", "ssi_reader_options",
                "extend_doubleblock"]
      ]},
     {"compile_dir",
      [setup_compile(T)
       || T <- [#test{
                   title = "non-existing dir",
                   source = {dir, "non-existing-made-up-dir"},
                   renderer = fun(#test{ source={dir, Dir} }) -> Dir end,
                   output = "non-existing-made-up-dir"
                  },
                #test{
                   title = "path1",
                   source = {dir, template_file(input, "path1")},
                   renderer = fun(#test{ module=M, render_vars=V, render_opts=O }) ->
                                      M:render(base1, V, O)
                              end
                  }
               ]
      ]}
    ].

%% {Name, DTL, Vars, Output}
%% {Name, DTL, Vars, RenderOpts, Output}
%% {Name, DTL, Vars, RenderOpts, CompilerOpts, Output}
%% {Name, DTL, Vars, RenderOpts, CompilerOpts, Output, Warnings}

def_to_test(Group, #test{ title=Name }=T) ->
    T#test{ title = lists:concat([Group, ": ", Name]) };
def_to_test(Group, {Name, DTL, Vars, Output}) ->
    def_to_test(Group, {Name, DTL, Vars, [], [], Output, default_warnings()});
def_to_test(Group, {Name, DTL, Vars, RenderOpts, Output}) ->
    def_to_test(Group, {Name, DTL, Vars, RenderOpts, [], Output, default_warnings()});
def_to_test(Group, {Name, DTL, Vars, RenderOpts, CompilerOpts, Output}) ->
    def_to_test(Group, {Name, DTL, Vars, RenderOpts, CompilerOpts, Output, default_warnings()});
def_to_test(Group, {Name, DTL, Vars, RenderOpts, CompilerOpts, Output, Warnings}) ->
    #test{
       title = lists:concat([Group, ": ", Name]),
       source = {template, DTL},
       render_vars = Vars,
       render_opts = RenderOpts,
       compile_vars = undefined,
       compile_opts = CompilerOpts ++ (#test{})#test.compile_opts,
       output = Output,
       warnings = Warnings
      }.


date_translation(Val, LC) when is_list(Val) ->
    io:format("Translating ~p~n", [Val]),
    date_translation(list_to_binary(Val),LC);
% date a
date_translation(<<"p.m.">>, <<"ru">>) ->
    <<"п.п."/utf8>>;
% date A
date_translation(<<"PM">>, <<"ru">>) ->
    <<"ПП"/utf8>>;
% date b
date_translation(<<"jul">>, <<"ru">>) ->
    <<"июл"/utf8>>;
% date D
date_translation(<<"Thu">>, <<"ru">>) ->
    <<"Чтв"/utf8>>;
% date E
date_translation(<<"July">>, {<<"ru">>, <<"alt. month">>}) ->
    <<"Июля"/utf8>>;
% date F
date_translation(<<"July">>, <<"ru">>) ->
    <<"Июль"/utf8>>;
% date l
date_translation(<<"Thursday">>, <<"ru">>) ->
    <<"Четверг"/utf8>>;
% date M
date_translation(<<"Sep">>, <<"ru">>) ->
    <<"Сен"/utf8>>;
% date N
date_translation(<<"Sept.">>, {<<"ru">>, <<"abbrev. month">>}) ->
    <<"Сен."/utf8>>;
% date P
date_translation(<<"noon">>, <<"ru">>) ->
    <<"полдень"/utf8>>;
date_translation(Text, <<"ru">>) ->
    proplists:get_value(Text,
                        lists:zip(
                              lists:map(fun list_to_binary/1, en_months()),
                              ru_months()),
                        Text);
date_translation(Text, _) ->
    Text.

ru_months() -> [ <<"Январь"/utf8>>, <<"Февраль"/utf8>>, <<"Март"/utf8>>, <<"Апрель"/utf8>>,
             <<"Май"/utf8>>, <<"Июнь"/utf8>>, <<"Июль"/utf8>>, <<"Август"/utf8>>, <<"Сентябрь"/utf8>>,
             <<"Октябрь"/utf8>>, <<"Ноябрь"/utf8>>, <<"Декабрь"/utf8>>].
en_months() -> ["January", "February", "March", "April",
             "May", "June", "July", "August", "September",
             "October", "November", "December"].



generate_test_date() ->
    generate_test_date(false).
generate_test_date(Translation) ->
    {{Y,M,D}, _} = erlang:localtime(),
    MonthName = case Translation of
                    russian -> ru_months();
                    _ -> en_months()
                end,
    OrdinalSuffix = [
                     "st","nd","rd","th","th","th","th","th","th","th", % 1-10
                     "th","th","th","th","th","th","th","th","th","th", % 10-20
                     "st","nd","rd","th","th","th","th","th","th","th", % 20-30
                     "st"
                    ],
    list_to_binary([
                    "It is the ",
                    integer_to_list(D),
                    lists:nth(D, OrdinalSuffix),
                    " of ", lists:nth(M, MonthName),
                    " ", integer_to_list(Y), "."
                   ]).


default_warnings() -> [].

error_info(File, Ws, Mod) ->
    {File, [error_info(W, Mod) || W <- Ws]}.

error_info({Line, ErrorDesc}, Mod)
  when is_integer(Line); Line =:= none ->
  {Line, Mod, ErrorDesc};
error_info({Line, Module, _}=ErrorDesc, _Mod)
  when is_integer(Line), is_atom(Module) ->
    ErrorDesc;
error_info({none, Module, _}=ErrorDesc, _Mod)
  when is_atom(Module) ->
    ErrorDesc;
error_info({{Line, Col}, Module, _}=ErrorDesc, _Mod)
  when is_integer(Line), is_integer(Col), is_atom(Module) ->
    ErrorDesc;
error_info(Ws, Mod) when is_list(Ws) ->
    error_info("erly_test", Ws, Mod);
error_info(ErrorDesc, Mod) ->
    {none, Mod, ErrorDesc}.

error_info(Ei) ->
    error_info(Ei, erlydtl_beam_compiler).


template_file(Dir, Name) -> filename:join(["../test/files", Dir, Name]).

functional_test(F) ->
    setup_compile(#test{
                     title = F,
                     module = list_to_atom("functional_test_" ++ F),
                     source = {file, template_file(input, F)}
                    }).

setup_compile(#test{ title=F, compile_opts=Opts }=T) ->
    CompileOpts = [{doc_root, "../test/files/input"}|Opts],
    case setup_compile(F) of
        {ok, [CV|Other]} ->
            CO = proplists:get_value(compile_opts, Other, []),
            Ws = proplists:get_value(warnings, Other, []),
            setup(T#test{
                    compile_vars = CV,
                    compile_opts = CO ++ CompileOpts,
                    warnings = Ws
                   });
        {error, Es, Ws} ->
            T#test{
              errors = Es,
              warnings = Ws,
              compile_opts = CompileOpts
             }
    end;
setup_compile("for_list_preset") ->
    CompileVars = [{fruit_list, [["apple", "apples"], ["banana", "bananas"], ["coconut", "coconuts"]]}],
    {ok, [CompileVars]};
setup_compile("for_preset") ->
    CompileVars = [{fruit_list, ["preset-apple", "preset-banana", "preset-coconut"]}],
    {ok, [CompileVars]};
setup_compile("for_records_preset") ->
    Link1a = [{name, "Amazon (preset)"}, {url, "http://amazon.com"}],
    Link2a = [{name, "Google (preset)"}, {url, "http://google.com"}],
    Link3a = [{name, "Microsoft (preset)"}, {url, "http://microsoft.com"}],
    CompileVars = [{software_links, [Link1a, Link2a, Link3a]}],
    {ok, [CompileVars]};
setup_compile("if_preset") ->
    CompileVars = [{var1, "something"}],
    {ok, [CompileVars]};
setup_compile("ifequal_preset") ->
    CompileVars = [{var1, "foo"}, {var2, "foo"}],
    {ok, [CompileVars]};
setup_compile("ifnotequal_preset") ->
    CompileVars = [{var1, "foo"}, {var2, "foo"}],
    {ok, [CompileVars]};
setup_compile("var_preset") ->
    CompileVars = [{preset_var1, "preset-var1"}, {preset_var2, "preset-var2"}],
    {ok, [CompileVars]};
setup_compile("extends_for") ->
    CompileVars = [{veggie_list, ["broccoli", "beans", "peas", "carrots"]}],
    {ok, [CompileVars]};
setup_compile("extends2") ->
    File = template_file(input, "extends2"),
    Error = {none, erlydtl_beam_compiler, unexpected_extends_tag},
    {error, [{File, [Error]}], []};
setup_compile("extends3") ->
    File = template_file(input, "extends3"),
    Include = template_file(input, "imaginary"),
    Error = {none, erlydtl_beam_compiler, {read_file, Include, enoent}},
    {error, [{File, [Error]}], []};
setup_compile("extends4") ->
    File = template_file(input, "extends4"),
    Warning = {{1,21}, erlydtl_beam_compiler, non_block_tag},
    {ok, [[]|[{warnings, [{File, [Warning]}]}]]};
setup_compile("missing") ->
    File = template_file(input, "missing"),
    Error = {none, erlydtl_compiler, {read_file, File, enoent}},
    {error, [{File, [Error]}], []};
setup_compile("custom_tag") ->
    {ok, [[]|[{compile_opts, [{custom_tags_modules, [erlydtl_custom_tags]}]}]]};
setup_compile("custom_tag1") -> setup_compile("custom_tag");
setup_compile("custom_tag2") -> setup_compile("custom_tag");
setup_compile("custom_tag3") -> setup_compile("custom_tag");
setup_compile("custom_tag4") -> setup_compile("custom_tag");
setup_compile("custom_tag_var") -> setup_compile("custom_tag");
setup_compile("custom_tag_lib_var") ->
 {ok, [[]|[{compile_opts, [{libraries, [{custom_tag_lib,erlydtl_custom_tags_lib}]}, {default_libraries, [custom_tag_lib]}]}]]};
setup_compile("super_escaped") ->
    {ok, [[]|[{compile_opts, [auto_escape]}]]};
setup_compile("reader_options") ->
 {ok, [[]|[{compile_opts, [{reader, {?MODULE, extra_reader}}, {reader_options, [{user_id, <<"007">>}, {user_name, <<"Agent">>}]}]}]]};
setup_compile("ssi_reader_options") ->
 {ok, [[]|[{compile_opts, [{reader, {?MODULE, extra_reader}}, {reader_options, [{user_id, <<"007">>}, {user_name, <<"Agent">>}]}]}]]};
%%setup_compile("path1") ->
%%    {ok, [[]|[{compile_opts, [debug_compiler]}]]};
setup_compile(_) ->
    {ok, [[]]}.

extra_reader(FileName, ReaderOptions) ->
 UserID = proplists:get_value(user_id, ReaderOptions, <<"IDUnknown">>),
 UserName = proplists:get_value(user_name, ReaderOptions, <<"NameUnknown">>),
 case file:read_file(FileName) of
  {ok, Data} when UserID == <<"007">>, UserName == <<"Agent">> ->
   {ok, Data};
  {ok, _Data} ->
   {error, "Not Found"};
  Err ->
   Err
 end.

expected(File) ->
    Filename = template_file(expect, File),
    case file:read_file(Filename) of
        {ok, Data} -> Data;
        _ -> fun (Data) ->
                     ok = file:write_file(Filename, Data),
                     io:format(
                       user,
                       "## Saved expected output for test ~p to ~p.~n"
                       "   Verify the contents, as it is used to pass the test on subsequent test runs.~n"
                       "~n",
                       [File, Filename]),
                     throw({verify_new_expected_output, Filename})
             end
    end.

setup(#test{ title = F, output=undefined }=T) ->
    {Vars, Opts, Result} =
        case setup(F) of
            {ok, V} -> {V, [], expected(F)};
            {ok, V, O} -> {V, O, expected(F)};
            {ok, V, O, skip_check} -> {V, O, fun (_) -> ok end};
            {ok, V, O, R} -> {V, O, R}
        end,
    T#test{
      render_vars = Vars,
      render_opts = Opts,
      output = Result
     };
setup(#test{}=T) -> T;
setup("autoescape") ->
    RenderVars = [{var1, "<b>bold</b>"}],
    {ok, RenderVars};
setup("extends") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("include_template") -> setup("extends");
setup("include_path") -> setup("extends");
setup("extends_path") -> setup("extends");
setup("extends_path2") -> setup("extends");
setup("block_super") -> setup("extends");
setup("filters") ->
    RenderVars = [
                  {date_var1, {1975,7,24}},
                  {datetime_var1, {{1975,7,24}, {7,13,1}}},
                  {'list', ["eins", "zwei", "drei"]}
                 ],
    {ok, RenderVars};
setup("for") ->
    RenderVars = [{fruit_list, ["apple", "banana", "coconut"]}],
    {ok, RenderVars};
setup("for_list") ->
    RenderVars = [{fruit_list, [["apple", "apples", "$1"], ["banana", "bananas", "$2"], ["coconut", "coconuts", "$500"]]}],
    {ok, RenderVars};
setup("for_tuple") ->
    RenderVars = [{fruit_list, [{"apple", "apples"}, {"banana", "bananas"}, {"coconut", "coconuts"}]}],
    {ok, RenderVars};
setup("for_records") ->
    Link1 = [{name, "Amazon"}, {url, "http://amazon.com"}],
    Link2 = [{name, "Google"}, {url, "http://google.com"}],
    Link3 = [{name, "Microsoft"}, {url, "http://microsoft.com"}],
    RenderVars = [{link_list, [Link1, Link2, Link3]}],
    {ok, RenderVars};
setup("for_records_preset") ->
    Link1b = [{name, "Canon"}, {url, "http://canon.com"}],
    Link2b = [{name, "Leica"}, {url, "http://leica.com"}],
    Link3b = [{name, "Nikon"}, {url, "http://nikon.com"}],
    RenderVars = [{photo_links, [Link1b, Link2b, Link3b]}],
    {ok, RenderVars};
setup("include") ->
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}],
    {ok, RenderVars};
setup("if") ->
    RenderVars = [{var1, "something"}],
    {ok, RenderVars};
setup("ifequal") ->
    RenderVars = [{var1, "foo"}, {var2, "foo"}, {var3, "bar"}],
    {ok, RenderVars};
setup("ifequal_preset") ->
    RenderVars = [{var3, "bar"}],
    {ok, RenderVars};
setup("ifnotequal") ->
    RenderVars = [{var1, "foo"}, {var2, "foo"}, {var3, "bar"}],
    {ok, RenderVars};
setup("now") ->
    {ok, [], [], skip_check};
setup("var") ->
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}, {var_not_used, "foostring3"}],
    {ok, RenderVars};
setup("var_preset") ->
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}],
    {ok, RenderVars};
setup("cycle") ->
    RenderVars = [{test, [integer_to_list(X) || X <- lists:seq(1, 20)]},
                  {a, "Apple"}, {b, "Banana"}, {c, "Cherry"}],
    {ok, RenderVars};
setup("trans") ->
    RenderOpts = [{translation_fun, fun lists:reverse/1}],
    {ok, [], RenderOpts};
setup("locale") ->
    {ok, _RenderVars = [{locale, "ru"}]};
setup("custom_tag1") ->
    {ok, [{a, <<"a1">>}], [{locale, ru}], <<"b1\n">>};
setup("custom_tag2") ->
    {ok, [{a, <<"a1">>}], [{locale, ru}, {foo, bar}], <<"b2\n">>};
setup("custom_tag3") ->
    {ok, [{a, <<"a1">>}], [{locale, ru}], <<"b3\n">>};
setup("custom_tag4") ->
    {ok, [], [], <<"a\n">>};
setup("custom_tag_var") ->
 {ok, [{a, <<"a1">>}], [{locale, ru}], <<"\nb1\n11\n">>};
setup("custom_tag_lib_var") ->
 {ok, [{a, <<"a1">>}], [{locale, ru}], <<"\nb1\n11\n">>};
setup("ssi") ->
    RenderVars = [{path, "ssi_include.html"}],
    {ok, RenderVars};
setup("wrapper") ->
    RenderVars = [{types, ["b", "a", "c"]}],
    {ok, RenderVars};
setup("reader_options") ->
 RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
% Options = [],%[{compile_opts, [{reader, {?MODULE, extra_reader}}, {reader_options, [{user_id, <<"007">>}, {user_name, <<"Agent">>}]}]}],
  {ok, RenderVars};
setup("ssi_reader_options") ->
 RenderVars = [{path, "ssi_include.html"}],
 {ok, RenderVars};

%%--------------------------------------------------------------------
%% Custom tags
%%--------------------------------------------------------------------
setup("custom_call") ->
    RenderVars = [{var1, "something"}],
    {ok, RenderVars};

setup(_) ->
    {ok, []}.
