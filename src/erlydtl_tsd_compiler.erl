%%%-------------------------------------------------------------------
%%% File:      erlydtl_tsd_compiler.erl
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2013 Andreas Stenius
%%% @doc
%%% Template Scanner Definition compiler.
%%%
%%% Compiles a TSD file as close as possible to the original erlydtl_scanner
%%% implementation as possible, based on the rules in the TSD file.
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2013 Andreas Stenius
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2013-11-05 by Andreas Stenius
%%%-------------------------------------------------------------------
-module(erlydtl_tsd_compiler).

-export([compile/1, compile/2, format_error/1]).
-export([compile_to_source/1]).

-import(erl_syntax, [application/2, application/3, clause/3, clause/2,
                     variable/1, string/1, infix_expr/3, operator/1,
                     block_expr/1, match_expr/2, atom/1, list/1,
                     list/2, tuple/1, integer/1, if_expr/1,
                     fun_expr/1, case_expr/2, char/1, underscore/0,
                     function/2, attribute/2, arity_qualifier/2,
                     revert_forms/1, form_list/1]).
-import(proplists, [get_value/3, get_all_values/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(rule, {prefix, in_state, guard, body}).
-record(tag, {tag, guard, body}).


%% ===================================================================
%% API
%% ===================================================================

compile(Input) ->
    compile(Input, []).

compile(Data, _Options) when is_binary(Data) ->
    case scan_and_parse(binary_to_list(Data)) of
        {ok, Scanner} ->
            Forms = compile_module(Scanner),
            case compile:forms(revert_forms(Forms), [return]) of
                {ok, Mod, Bin, _} ->
                    code:purge(Mod),
                    case code:load_binary(Mod, atom_to_list(Mod) ++ ".tsd", Bin) of
                        {module, Mod} -> {ok, Mod, Bin};
                        Err -> Err
                    end;
                Err -> Err
            end;
        Err -> Err
    end;
compile(File, Options) ->
    {ok, Data} = file:read_file(File),
    case compile(Data, [{source, File}|Options]) of
        {ok, Mod, Bin}=Res ->
            case proplists:get_value(out_dir, Options) of
                undefined -> Res;
                OutDir ->
                    OutFile = filename:join(
                                [OutDir, atom_to_list(Mod) ++ ".beam"]),
                    case file:write_file(OutFile, Bin) of
                        ok -> {ok, Mod, OutFile};
                        Err -> Err
                    end
            end;
        Err -> Err
    end.

compile_to_source(Bin) when is_binary(Bin) ->
    {ok, Scanner} = scan_and_parse(binary_to_list(Bin)),
    io_lib:format("~s~n", [erl_prettypr:format(compile_module(Scanner))]);
compile_to_source(Filename) ->
    {ok, Data} = file:read_file(Filename),
    compile_to_source(Data).

format_error({multiple_append_actions, Prefix}) ->
    io_lib:format(
      "Multiple actions not supported with append, for ~s.",
      [case Prefix of
           {prefix, P} -> io_lib:format("prefix ~p", [P]);
           any_prefix -> "`any' prefix"
       end]).


%% ===================================================================
%% Internal functions
%% ===================================================================

get_value(Key, Props) ->
    get_value(Key, Props, default(Key)).

default(function) -> [scan];
default(init_state) -> [root];
default(_) -> undefined.

scan_and_parse(String) ->
    case erlydtl_tsd_scanner:string(String) of
        {ok, Tokens, _} ->
            case erlydtl_tsd_parser:parse(Tokens) of
                {ok, _}=Ok -> Ok;
                {error, Err, _State} -> {error, Err}
            end;
        {error, Err, _} -> {error, Err}
    end.

compile_module(Scanner) ->
    [Defs, Rules, Tags] = [get_all_values(Exp, Scanner) || Exp <- [def, rule, tag]],
    form_list(
      lists:foldr(
        fun (F, Acc) ->
                F({Defs, Rules, Tags}, Acc)
        end,
        [],
        [fun compile_head_forms/2,
         fun compile_funcs/2
        ]
       )).

compile_head_forms({Defs, _, _}, Acc) ->
    Mod = atom(hd(get_value(module, Defs))),
    Name = atom(hd(get_value(function, Defs))),
    [attribute(atom(module), [Mod]),
     attribute(atom(export), [list(
                                [arity_qualifier(Name, integer(1)),
                                 arity_qualifier(Name, integer(4))
                                ])])
     | get_all_values(code, Defs)] ++ Acc.

compile_funcs({Defs, Rules, Tags}, Acc) ->
    Name = atom(hd(get_value(function, Defs))),
    [compile_scanner_api(Name, Defs),
     compile_scanner(Name, Rules)
     | compile_post_process(atom(post_process), Tags, Acc)].

compile_scanner_api(Name, Defs) ->
    State = atom(hd(get_value(init_state, Defs))),
    function(
      Name, 
      [clause(
         [variable('Template')],
         [application(atom(is_list), [variable('Template')])],
         [application(
            Name, [variable('Template'), list([]),
                   tuple([integer(1), integer(1)]),
                   State])
         ])
      ]).

compile_scanner(Name, Rules) ->
    function(Name, [compile_rule(Name, Rule) || Rule <- Rules]).

compile_post_process(Name, Tags, Acc) ->
    TagClauses = [compile_tag(Name, Tag) || Tag <- Tags],
    CatchallClause = clause([underscore(), variable('T'), underscore()],
                            none,
                            [variable('T')]),

    %% pick up Tag trail to go from Name/2 to Name/3..
    ListClause = clause(
                   [list([variable('S')], variable('Ss')), variable('N')],
                   none,
                   [list([application(
                            Name,
                            [variable('Ss'), variable('S'), variable('N')])],
                         variable('Ss'))]),
    TagClause = clause(
                  [variable('T'), variable('N')],
                  none,
                  [application(Name, [atom(undefined), variable('T'), variable('N')])]),

    [function(Name, TagClauses ++ [CatchallClause]), %% Name/3
     function(Name, [ListClause, TagClause]) %% Name/2
     | Acc].

compile_rule(_Name, #rule{ prefix=Prefix, in_state=InState, body={code, Body} }=Rule) ->
    {Pprefix, _Apos} = compile_prefix(Prefix),
    Pstate = compile_match_state(InState, Prefix, keep_state),
    Ppos = match_expr(
             tuple([variable('R'), variable('C')]),
             variable('P')),

    clause(
      [Pprefix,
       variable('S'),
       Ppos,
       Pstate
      ],
      compile_guard(Rule),
      Body);

compile_rule(Name, #rule{ prefix=Prefix, in_state=InState, body={Actions, OutState} }=Rule) ->
    {Pprefix, Apos} = compile_prefix(Prefix),
    {Ppos, Ascanned} = compile_actions(Actions, Prefix),
    Pstate = compile_match_state(InState, Prefix, OutState),
    Astate = compile_new_state(OutState),

    clause(
      [Pprefix,
       variable('S'),
       Ppos,
       Pstate
      ],
      compile_guard(Rule),
      [application(
         Name,
         [variable('T'),
          Ascanned,
          Apos,
          Astate
         ])
      ]).

compile_prefix({prefix,P}) ->
    Prefix = lists:foldl(fun ($\n, {R,_}) -> {R+1,1};
                             (_  , {R,C}) -> {R,C+1}
                         end,
                         {0, 0},
                         P),
    {infix_expr(string(P), operator('++'), variable('T')),
     tuple(case Prefix of
               {0,C} ->
                   [variable('R'),
                    infix_expr(
                      variable('C'),
                      operator('+'),
                      integer(C)
                     )];
               {R,C} ->
                   [infix_expr(
                      variable('R'),
                      operator('+'),
                      integer(R)),
                    integer(C)]
           end)};
compile_prefix(any_prefix) ->
    {list([variable('H')], variable('T')),
     case_expr(
       variable('H'),
       [clause(
          [char($\n)], none, 
          [tuple([infix_expr(variable('R'), operator('+'), integer(1)),
                  integer(1)])]),
        clause(
          [underscore()], none,
          [tuple([variable('R'),
                  infix_expr(variable('C'), operator('+'), integer(1))])])
       ])};
compile_prefix(end_of_input) ->
    {list([]),
     tuple([variable('R'), variable('C')])}.

compile_actions([], _Prefix) ->
    {tuple([variable('R'), variable('C')]), variable('S')};
compile_actions(Actions, Prefix) ->
    {Tags, N} = lists:foldl(
                  fun (Action, {Ts, N0}) ->
                          {T, N} = compile_action(Action, Prefix, N0),
                          {[T|Ts], N0 + N}
                  end,
                  {[], 0}, Actions),
    {match_expr(
       tuple([variable('R'), variable('C')]),
       variable('P')),
     if N == 0 -> list(Tags, application(
                               atom(post_process),
                               [variable('S'),
                                atom(element(2, hd(Actions)))
                               ]));
        N == 1, length(Tags) == 1 -> hd(Tags);
        true -> throw({?MODULE, {multiple_append_actions, Prefix}})
     end}.

compile_action({Action, Tag, Value}, _Prefix, N) ->
    compile_action({Action, Tag}, {prefix, Value}, N);
compile_action({add, Tag}, Prefix, _N) ->
    {tuple(
       [atom(Tag), variable('P')
        | if is_atom(Tag) ->
                  case Prefix of
                      any_prefix -> [list([variable('H')])];
                      {prefix,P} -> [string(P)]
                  end;
             true -> []
          end
       ]), 0};
compile_action({append, Tag}, Prefix, _N) ->
    P = case Prefix of
            any_prefix -> [variable('H')];
            {prefix, S} -> string(lists:reverse(S))
        end,
    {case_expr(
       variable('S'),
       [clause(
          [list(
             [match_expr(
                tuple([atom(Tag), underscore(), variable('L')]),
                variable('M'))],
             variable('Ss'))
          ],
          none,
          [list([application(
                   atom(setelement),
                   [integer(3),
                    variable('M'),
                    if is_list(P) -> list(P, variable('L'));
                       true -> infix_expr(P, operator('++'),
                                          variable('L'))
                    end])
                ],
                variable('Ss'))
          ]),
        clause(
          [underscore()],
          none,
          [list(
             [tuple(
                [atom(Tag), variable('P'),
                 if is_list(P) -> list(P);
                    true -> P
                 end])
             ],
             application(
               atom(post_process),
               [variable('S'),
                atom(Tag)]))
          ])
       ]), 1}.

compile_match_state(any_stateless, Prefix, {state,S})
  when not is_tuple(S) -> throw({?MODULE, {bad_state_transition, Prefix, S}});
compile_match_state(any_stateless, Prefix, _) -> compile_in_state(any_stateless, Prefix);
compile_match_state(State, Prefix, keep_state) ->
    match_expr(
      compile_in_state(State, Prefix),
      variable('St'));
compile_match_state(State, Prefix, _) ->
    compile_in_state(State, Prefix).

compile_in_state({state, S}, _Prefix) -> tuple([atom(S), variable('E')]);
compile_in_state({stateless, S}, _Prefix) -> atom(S);
compile_in_state(any_state, _Prefix) -> tuple([underscore(), variable('E')]);
compile_in_state(any_stateless, _Prefix) -> variable('St');
compile_in_state(close_any_state, Prefix) ->
    case Prefix of
        any_prefix -> underscore();
        {prefix,P} -> tuple([underscore(), string(P)])
    end.

compile_new_state({state, {S, E}}) -> tuple([atom(S), string(E)]);
compile_new_state({state, S}) -> tuple([atom(S), variable('E')]);
compile_new_state({stateless, S}) -> atom(S);
compile_new_state(keep_state) -> variable('St').

compile_guard(#rule{ in_state=any_stateless, guard={guard, Guard} }) ->
    [application(atom(is_atom), [variable('St')])|Guard];
compile_guard(#rule{ guard={guard, Guard} }) -> Guard.

compile_tag(_Name, #tag{ tag=Tag, guard={guard, Guard}, body=Body }) ->
    clause(
      compile_tag_pattern(Tag),
      Guard,
      case Body of
          [{code, C}] -> C;
          _ -> [application(
                  atom(setelement),
                  [integer(3), variable('T'),
                   block_expr(
                     [match_expr(
                        tag_var(N),
                        compile_tag_body(B, N - 1))
                      || {B, N} <- lists:zip(
                                     Body,
                                     lists:seq(1, length(Body)))
                     ] ++ [tag_var(length(Body))])
                  ])
               ]
      end).

compile_tag_pattern({Ts, {S, T}})
  when S =:= state; S =:= stateless ->
    compile_tag_pattern(Ts, atom(T));
compile_tag_pattern(Ts) ->
    compile_tag_pattern(Ts, underscore()).

compile_tag_pattern([T|Ts], NextT) ->
    [compile_tag_trail(Ts),
     match_expr(
       case T of
           {state, S} ->
               tuple([atom(S), underscore(), tag_var(0)]);
           {stateless, S} ->
               tuple([atom(S), underscore()])
       end,
       variable('T')),
     NextT
    ].

compile_tag_trail([]) -> underscore();
compile_tag_trail(Ts) ->
    list([compile_tag_trail_pattern(S) || S <- Ts],
         underscore()).

compile_tag_trail_pattern({state, T}) -> tuple([atom(T), underscore(), underscore()]);
compile_tag_trail_pattern({stateless, T}) -> tuple([atom(T), underscore()]).

tag_var(0) -> variable('L');
tag_var(N) -> variable([$L|integer_to_list(N)]).

compile_tag_body([], N) -> tag_var(N);
compile_tag_body([M, F], N) ->
    application(atom(M), atom(F), [tag_var(N)]);
compile_tag_body([F], N) ->
    application(atom(F), [tag_var(N)]);
compile_tag_body({code, C}, _) -> block_expr(C).


-ifdef (TEST).

-define(test_scan_and_parse(String, Prop, Expect),
        {ok, Scanner} = scan_and_parse(String),
        Ps = get_all_values(Prop, Scanner),
        ?assertMatch(Expect, Ps)).

-define(test_compile(Prop, Value, Pretty),
        Clause = case Prop of
                     rule -> compile_rule(atom(t), Value);
                     tag -> compile_tag(atom(t), Value)
                 end,
        Tree = function(atom(t), [Clause]),
        ?assertEqual(
           Pretty,
           erl_prettypr:format(Tree))
       ).

-define(test_scan_parse_compile(String, Prop, Expect, Pretty),
        ?test_scan_and_parse(String, Prop, Expect),
        ?test_compile(Prop, hd(Ps), Pretty)).

-define(test_def(String, Def),
       ?test_scan_and_parse(String, def, [Def])).

-define(test_rule(String, Rule, Result),
        ?test_scan_parse_compile(String, rule, [Rule], Result)).

-define(test_tag(String, Tag, Result),
        ?test_scan_parse_compile(String, tag, [Tag], Result)).

-define(test_bad_rule(String, Rule, Error),
        ?test_scan_and_parse(String, rule, [Rule]),
        ?assertThrow(Error, compile_rule(atom(t), hd(Ps)))).

-define(_test_def(String, Def),
        {?LINE, fun () -> ?test_def(String, Def) end}).

-define(_test_rule(String, Rule, Result),
        {?LINE, fun () -> ?test_rule(String, Rule, Result) end}).

-define(_test_tag(String, Tag, Result),
        {?LINE, fun () -> ?test_tag(String, Tag, Result) end}).

-define(_test_bad_rule(String, Rule, Error),
        {?LINE, fun () -> ?test_bad_rule(String, Rule, Error) end}).

-define(_test_scan(T, E), ?_assertEqual(E, apply(M, F, [T]))).


def_test_() ->
    [?_test_def(
        "-module foo.",
        {module, ["foo"]})
    ].

compile_rule_test_() ->
    [?_test_rule(
        "{{ (in_text)-:(open_var),(in_code) .until}}.",
        {rule,
         {prefix,"{{"},
         {stateless,in_text},
         {guard,[]},
         {[{add,open_var}],
          {state,{in_code,"}}"}}} },
        "t(\"{{\" ++ T, S, {R, C} = P, in_text) ->\n"
        "    t(T, [{open_var, P, \"{{\"} | post_process(S, open_var)],\n"
        "      {R, C + 2}, {in_code, \"}}\"})."),
     ?_test_rule(
        "{# (in_text)-: (in_comment).until #}.",
        {rule,
         {prefix,"{#"},
         {stateless,in_text},
         {guard,[]},
         {[],{state,{in_comment,"#}"}}} },
        "t(\"{#\" ++ T, S, {R, C}, in_text) ->\n"
        "    t(T, S, {R, C + 2}, {in_comment, \"#}\"})."),
     ?_test_rule(
        ".any (in_text)-: +(string).",
        {rule,
         any_prefix,
         {stateless,in_text},
         {guard,[]},
         {[{append,string}],
          keep_state} },
        "t([H | T], S, {R, C} = P, in_text = St) ->\n"
        "    t(T,\n"
        "      case S of\n"
        "\t[{string, _, L} = M | Ss] ->\n"
        "\t    [setelement(3, M, [H | L]) | Ss];\n"
        "\t_ -> [{string, P, [H]} | post_process(S, string)]\n"
        "      end,\n"
        "      case H of\n"
        "\t$\\n -> {R + 1, 1};\n"
        "\t_ -> {R, C + 1}\n"
        "      end,\n"
        "      St)."),
     ?_test_rule(
        "\" (in_code): (string_literal), (in_double_quote).",
        {rule,
         {prefix,"\""},
         {state,in_code},
         {guard,[]},
         {[{add,string_literal}],
          {state,in_double_quote}} },
        "t(\"\\\"\" ++ T, S, {R, C} = P, {in_code, E}) ->\n"
        "    t(T,\n"
        "      [{string_literal, P, \"\\\"\"} | post_process(S,\n"
        "\t\t\t\t\t\tstring_literal)],\n"
        "      {R, C + 1}, {in_double_quote, E})."),
     ?_test_rule(
        "}} .any+: (close_var), (in_text)-.",
        {rule,
         {prefix,"}}"},
         close_any_state,
         {guard,[]},
         {[{add,close_var}],
          {stateless,in_text}} },
        "t(\"}}\" ++ T, S, {R, C} = P, {_, \"}}\"}) ->\n"
        "    t(T,\n"
        "      [{close_var, P, \"}}\"} | post_process(S, close_var)],\n"
        "      {R, C + 2}, in_text)."),
     ?_test_rule(
        "\" (in_double_quote): +(string_literal), (in_code).",
        {rule,
         {prefix,"\""},
         {state,in_double_quote},
         {guard,[]},
         {[{append,string_literal}],
          {state,in_code}} },
        "t(\"\\\"\" ++ T, S, {R, C} = P, {in_double_quote, E}) ->\n"
        "    t(T,\n"
        "      case S of\n"
        "\t[{string_literal, _, L} = M | Ss] ->\n"
        "\t    [setelement(3, M, \"\\\"\" ++ L) | Ss];\n"
        "\t_ ->\n"
        "\t    [{string_literal, P, \"\\\"\"} | post_process(S,\n"
        "\t\t\t\t\t\t      string_literal)]\n"
        "      end,\n"
        "      {R, C + 1}, {in_code, E})."),
     ?_test_rule(
        "== .any: ==, (in_code).",
        {rule,
         {prefix,"=="},
         any_state,
         {guard,[]},
         {[{add,"=="}],
          {state,in_code}} },
        "t(\"==\" ++ T, S, {R, C} = P, {_, E}) ->\n"
        "    t(T, [{'==', P} | post_process(S, '==')], {R, C + 2},\n"
        "      {in_code, E})."),
     ?_test_rule(
        "\\  (in_code): .skip.",
        {rule,
         {prefix," "},
         {state,in_code},
         {guard,[]},
         {[], keep_state} },
        "t(\" \" ++ T, S, {R, C}, {in_code, E} = St) ->\n"
        "    t(T, S, {R, C + 1}, St)."),
     ?_test_rule(
        "\\s .any: .skip, (in_code).",
        {rule,
         {prefix," "},
         any_state,
         {guard,[]},
         {[], {state,in_code}}},
        "t(\" \" ++ T, S, {R, C}, {_, E}) ->\n"
        "    t(T, S, {R, C + 1}, {in_code, E})."),
     ?_test_rule(
        ".any (in_code), +expr H >= $0 andalso H =< $9 orelse H == $- end"
        ": (number_literal), (in_number).",
        {rule,
         any_prefix,
         {state,in_code},
         {guard,[{op,1,'orelse',_,_}]},
         {[{add,number_literal}],
          {state,in_number}} },
        "t([H | T], S, {R, C} = P, {in_code, E})\n"
        "    when H >= $0 andalso H =< $9 orelse H == $- ->\n"
        "    t(T,\n"
        "      [{number_literal, P, [H]} | post_process(S,\n"
        "\t\t\t\t\t       number_literal)],\n"
        "      case H of\n"
        "\t$\\n -> {R + 1, 1};\n"
        "\t_ -> {R, C + 1}\n"
        "      end,\n"
        "      {in_number, E})."),
     ?_test_rule(
        "\! .any:: +expr io:write(\"custom code!\"), w00t end.",
        {rule,
         {prefix,"!"},
         any_state,
         {guard,[]},
         {code,[_,_]} },
        "t(\"!\" ++ T, S, {R, C} = P, {_, E} = St) ->\n"
        "    io:write(\"custom code!\"), w00t."),
     ?_test_rule(
        "=.any-:.skip.",
        {rule,
         {prefix,"="},
         any_stateless,
         {guard,[]},
         {[], keep_state}},
        "t(\"=\" ++ T, S, {R, C}, St) when is_atom(St) ->\n"
        "    t(T, S, {R, C + 1}, St)."),
     ?_test_bad_rule(
        ") .any-: .skip, (in_code).",
        {rule,
         {prefix,")"},
         any_stateless,
         {guard,[]},
         {[],{state,in_code}} },
        {?MODULE, {bad_state_transition,{prefix,")"},in_code}}),
     ?_test_rule(
        "\\( .any-: .skip, (in_code) .until ).",
        {rule,
         {prefix,"("},
         any_stateless,
         {guard,[]},
         {[],{state,{in_code,")"}}} },
        "t(\"(\" ++ T, S, {R, C}, St) when is_atom(St) ->\n"
        "    t(T, S, {R, C + 1}, {in_code, \")\"})."),
     ?_test_rule(
        "\\n .any: +(foo).",
        {rule,
         {prefix, "\n"},
         any_state,
         {guard,[]},
         {[{append,foo}],
          keep_state} },
        "t(\"\\n\" ++ T, S, {R, C} = P, {_, E} = St) ->\n"
        "    t(T,\n"
        "      case S of\n"
        "\t[{foo, _, L} = M | Ss] ->\n"
        "\t    [setelement(3, M, \"\\n\" ++ L) | Ss];\n"
        "\t_ -> [{foo, P, \"\\n\"} | post_process(S, foo)]\n"
        "      end,\n"
        "      {R + 1, 1}, St)."),
     ?_test_rule(
        "_( .any: \\_ \\(, (in_code).",
        {rule,
         {prefix, "_("},
         any_state,
         {guard,[]},
         {[{add,"_"},{add, "("}],
          {state,in_code}} },
        "t(\"_(\" ++ T, S, {R, C} = P, {_, E}) ->\n"
        "    t(T, [{'(', P}, {'_', P} | post_process(S, '_')],\n"
        "      {R, C + 2}, {in_code, E})."),
     ?_test_rule(
        "' (foo)-: (bar)-\", (baz)-.",
        {rule,
         {prefix, "'"},
         {stateless,foo},
         {guard,[]},
         {[{add,bar,"\""}],
          {stateless,baz}} },
        "t(\"'\" ++ T, S, {R, C} = P, foo) ->\n"
        "    t(T, [{bar, P, \"\\\"\"} | post_process(S, bar)],\n"
        "      {R, C + 1}, baz).")
    ].

compile_tag_test_() ->
    [?_test_tag(
        "(string): lists reverse.",
        {tag,
         [{state,string}],
         {guard,[]},
         [["lists","reverse"]]},
        "t(_, {string, _, L} = T, _) ->\n"
        "    setelement(3, T, begin L1 = lists:reverse(L), L1 end)."),
     ?_test_tag(
        "(identifier): lists (reverse), list_to_atom.",
        {tag,
         [{state,identifier}],
         {guard,[]},
         [["lists",reverse],["list_to_atom"]]},
        "t(_, {identifier, _, L} = T, _) ->\n"
        "    setelement(3, T,\n"
        "\t       begin\n"
        "\t\t L1 = lists:reverse(L), L2 = list_to_atom(L1), L2\n"
        "\t       end)."),
     ?_test_tag(
        "(foo): (dummy), :+expr test:dummy(L1) end.",
        {tag,
         [{state,foo}],
         {guard,[]},
         [[dummy], {code, _}]},
        "t(_, {foo, _, L} = T, _) ->\n"
        "    setelement(3, T,\n"
        "\t       begin\n"
        "\t\t L1 = dummy(L), L2 = begin test:dummy(L1) end, L2\n"
        "\t       end)."),
     ?_test_tag(
        "(foo)::+expr setelement(1, T, bar) end.",
        {tag,
         [{state,foo}],
         {guard,[]},
         [{code,_}]},
        "t(_, {foo, _, L} = T, _) -> setelement(1, T, bar)."),
     ?_test_tag(
        "(foo), (bar)::+expr setelement(1, T, baz) end.",
        {tag,
         {[{state,foo}],{state,bar}},
         {guard,[]},
         [{code,_}]},
        "t(_, {foo, _, L} = T, bar) -> setelement(1, T, baz)."),
     ?_test_tag(
        "(foo) (bar): to_atom.",
        {tag,
         [{state,bar},{state,foo}],
         {guard,[]},
         [["to_atom"]]},
        "t([{foo, _, _} | _], {bar, _, L} = T, _) ->\n"
        "    setelement(3, T, begin L1 = to_atom(L), L1 end).")
    ].

scanner_test_() ->
    {setup,
     fun () ->
             compile(filename:join(code:lib_dir(erlydtl, src),
                                   "erlydtl_new_scanner.tsd"))
     end,
     fun ({ok, M, _}) ->
             F = scan,
             [?_test_scan(
                 "foo bar",
                 {ok, [{string, {1, 1}, "foo bar"}]}),
              ?_test_scan(
                 "foo {{ bar }}",
                 {ok, [{string, {1, 1}, "foo "},
                       {open_var, {1, 5}, '{{'},
                       {identifier, {1, 8}, bar},
                       {close_var, {1, 12}, '}}'}]}),
              ?_test_scan(
                 "{{ \"test\" }}",
                 {ok, [{open_var, {1, 1}, '{{'},
                       {string_literal, {1, 4}, "\"test\""},
                       {close_var, {1, 11}, '}}'}]}),
              ?_test_scan(
                 "{{ 12345 }}",
                 {ok, [{open_var, {1, 1}, '{{'},
                       {number_literal, {1, 4}, "12345"},
                       {close_var, {1, 10}, '}}'}]}),
              ?_test_scan(
                 "{{ foo.bar }}",
                 {ok, [{open_var, {1, 1}, '{{'},
                       {identifier, {1, 4}, foo},
                       {'.', {1, 7}},
                       {identifier, {1, 8}, bar},
                       {close_var, {1, 12}, '}}'}]}),
              ?_test_scan(
                 "{% if 1 %}",
                 {ok, [{open_tag, {1, 1}, '{%'},
                       {if_keyword, {1, 4}, "if"},
                       {number_literal, {1, 7}, "1"},
                       {close_tag, {1, 9}, '%}'}]}),
              ?_test_scan(
                 "{{ ~ }}",
                 {error, {1, M, "Illegal character in column 4"},
                  {scanner_state, "~ }}", [{open_var,{1,1},"{{"}], {1,4}, {in_code, "}}"}}
                 }),
              ?_test_scan(
                 "{{ \"\\\" '\" }}",
                 {ok, [{open_var, {1, 1}, '{{'},
                       {string_literal, {1, 4}, "\"\\\" '\""},
                       {close_var, {1, 11}, '}}'}]}),
              ?_test_scan(
                 "{% cycle 'a' 'b' %}",
                 {ok, [{open_tag, {1, 1}, '{%'},
                       {cycle_keyword, {1, 4}, "cycle"},
                       {string_literal, {1, 10}, "\"a\""},
                       {string_literal, {1, 14}, "\"b\""},
                       {close_tag, {1, 18}, '%}'}]}),
              ?_test_scan(
                 "{%with a=1%}{{a}}{%endwith%}",
                 {ok, [{open_tag, {1, 1}, '{%'},
                       {with_keyword, {1, 3},  "with"},
                       {identifier, {1, 8}, a},
                       {'=', {1, 9}},
                       {number_literal, {1, 10}, "1"},
                       {close_tag, {1, 11}, '%}'},
                       {open_var, {1, 13}, '{{'},
                       {identifier, {1, 15}, a},
                       {close_var, {1, 16}, '}}'},
                       {open_tag, {1, 18}, '{%'},
                       {endwith_keyword, {1, 20}, "endwith"},
                       {close_tag, {1, 27}, '%}'}
                      ]}),
              ?_test_scan(
                 "foo{% verbatim %}bar{% endverbatim %}baz",
                 {ok, [{string, {1, 1}, "foo"},
                       {string, {1, 18}, "barbaz"}]}),
              ?_test_scan(
                 "foo{% verbatim %}{% endverbatim %}bar",
                 {ok, [{string, {1, 1}, "foo"},
                       {string, {1, 18}, "bar"}]}),
              ?_test_scan(
                 "foo{% verbatim bar %}bar{% verbatim %}baz{% endverbatim baz %}{% endverbatim bar %}",
                 {ok, [{string, {1, 1}, "foo"},
                       {string, {1, 22}, "bar{% verbatim %}baz{% endverbatim baz %}"}]}
                )
             ];
         (Err) -> ?_test(throw({setup_error, Err}))
     end}.

-endif.
