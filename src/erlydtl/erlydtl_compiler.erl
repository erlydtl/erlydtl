%%%-------------------------------------------------------------------
%%% File:      erlydtl_compiler.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc  
%%% ErlyDTL template compiler
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
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
%%% @since 2007-12-16 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(erlydtl_compiler).
-author('emmiller@gmail.com').
-author('emmiller@gmail.com').

-export([compile/2, compile/3, compile/4, compile/5, parse/1, scan/1]).

-record(dtl_context, {
        local_scopes = [], 
        block_dict = dict:new(), 
        auto_escape = off, 
        doc_root = "", 
        parse_trail = []}).

compile(File, Module) ->
    compile(File, "", Module).

compile(File, DocRoot, Module) ->
    compile(File, DocRoot, Module, "render").

compile(File, DocRoot, Module, Function) ->
    compile(File, DocRoot, Module, Function, "ebin").

compile(File, DocRoot, Module, Function, OutDir) ->
    case parse(File) of
        {ok, DjangoAst} ->
            Render1FunctionAst = erl_syntax:function(
                erl_syntax:atom(Function), 
                [erl_syntax:clause([erl_syntax:variable("Variables")], none, 
                        [body_ast(DjangoAst, #dtl_context{doc_root = DocRoot, parse_trail = [File]})])]),
            Render0FunctionAst = erl_syntax:function(
                erl_syntax:atom(Function),
                [erl_syntax:clause([], none, 
                        [erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Function),
                                [erl_syntax:list([])])]
                    )]),
            ExtensionFunctionAst = erl_syntax:function(
                erl_syntax:atom(file_extension),
                [erl_syntax:clause([], none, [erl_syntax:string(file_extension(File))])]),
            ModuleAst  = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
            CompileAst = erl_syntax:attribute(erl_syntax:atom(compile), [erl_syntax:atom("export_all")]),

            Forms = [erl_syntax:revert(X) || X <- [ModuleAst, CompileAst, ExtensionFunctionAst, 
                    Render0FunctionAst, Render1FunctionAst]],

            case compile:forms(Forms) of
                {ok, Module1, Bin} ->       
                    Path = filename:join([OutDir, atom_to_list(Module1) ++ ".beam"]),
                    case file:write_file(Path, Bin) of
                        ok ->
                            code:purge(Module1),
                            case code:load_binary(Module1, atom_to_list(Module1) ++ ".erl", Bin) of
                                {module, _} -> ok;
                                _ -> {error, "code reload failed"}
                            end;
                        {error, Reason} ->
                            {error, lists:concat(["beam generation failed (", Reason, "): ", Path])}
                    end;
                _ ->
                    {error, "compilation failed"}
            end;
        Error ->
            Error
    end.


scan(File) ->
    case file:read_file(File) of
        {ok, B} ->
            erlydtl_scanner:scan(binary_to_list(B));
        _ ->
            {error, "reading " ++ File ++ " failed "}
    end.

parse(File) ->
    case scan(File) of
        {ok, Tokens} ->
            erlydtl_parser:parse(Tokens);
        Err ->
            Err
    end.

full_path(File, DocRoot) ->
    filename:join([DocRoot, File]).

file_extension(File) ->
    lists:last(string:tokens(lists:flatten(File), ".")).

% child templates should only consist of blocks at the top level
body_ast([{extends, {string_literal, _Pos, String}} | ThisAst], Context) ->
    File = full_path(unescape_string_literal(String), Context#dtl_context.doc_root),
    case lists:member(File, Context#dtl_context.parse_trail) of
        true ->
            {error, "Circular file inclusion!"};
        _ ->
            {ok, ParentAst} = parse(File),
            BlockDict = lists:foldl(
                fun
                    ({block, {identifier, _, Name}, Contents}, Dict) ->
                        dict:store(Name, Contents, Dict);
                    (_, Dict) ->
                        Dict
                end, dict:new(), ThisAst),
            body_ast(ParentAst, Context#dtl_context{
                    block_dict = 
                    dict:merge(fun(_Key, _ParentVal, ChildVal) -> ChildVal end,
                        BlockDict, Context#dtl_context.block_dict),
                    parse_trail = [File | Context#dtl_context.parse_trail]
                })
    end;

body_ast(DjangoAst, Context) ->
    erl_syntax:list(
        lists:map(
            fun
                ({'block', {identifier, _, Name}, Contents}) ->
                    Block = case dict:find(Name, Context#dtl_context.block_dict) of
                        {ok, ChildBlock} ->
                            ChildBlock;
                        _ ->
                            Contents
                    end,
                    body_ast(Block, Context);
                ({'comment', _Contents}) ->
                    erl_syntax:list([]);
                ({'autoescape', {identifier, _, OnOrOff}, Contents}) ->
                    body_ast(Contents, Context#dtl_context{auto_escape = list_to_atom(OnOrOff)});
                ({'text', _Pos, String}) -> 
                    erl_syntax:string(String);
                ({'string_literal', _Pos, String}) ->
                    auto_escape(erl_syntax:string(unescape_string_literal(String)), Context);
                ({'number_literal', _Pos, Number}) ->
                    erl_syntax:string(Number);
                ({'variable', Variable}) ->
                    resolve_variable_ast(Variable, Context);
                ({'tag', {identifier, _, Name}, Args}) ->
                    tag_ast(Name, Args, Context);
                ({'include', {string_literal, _, File}}) ->
                    FilePath = full_path(unescape_string_literal(File), Context#dtl_context.doc_root),
                    {ok, IncludeAst} = parse(FilePath),
                    body_ast(IncludeAst, 
                        Context#dtl_context{parse_trail = 
                            [FilePath | Context#dtl_context.parse_trail]});
                ({'if', {variable, Variable}, Contents}) ->
                    ifelse_ast(Variable, body_ast(Contents, Context),
                        erl_syntax:list([]), Context);
                ({'if', {'not', {variable, Variable}}, Contents}) ->
                    ifelse_ast(Variable, erl_syntax:list([]),
                        body_ast(Contents, Context), Context);
                ({'ifelse', {variable, Variable}, IfContents, ElseContents}) ->
                    ifelse_ast(Variable, body_ast(IfContents, Context),
                        body_ast(ElseContents, Context), Context);
                ({'ifelse', {'not', {variable, Variable}}, IfContents, ElseContents}) ->
                    ifelse_ast(Variable, body_ast(ElseContents, Context), 
                        body_ast(IfContents, Context), Context);
                ({'apply_filter', Variable, Filter}) ->
                    filter_ast(Variable, Filter, Context);
                ({'for', {'in', {identifier, _, Iterator}, {identifier, _, List}}, Contents}) ->
                    for_loop_ast(Iterator, List, Contents, Context);
                ({'for', {'in', IteratorList, {identifier, _, List}}, Contents}) when is_list(IteratorList) ->
                    for_list_loop_ast(IteratorList, List, Contents, Context)
            end, DjangoAst)
    ).

filter_ast(Variable, Filter, Context) ->
    % the escape filter is special; it is always applied last, so we have to go digging for it

    % AutoEscape = 'did' means we (will have) decided whether to escape the current variable,
    % so don't do any more escaping
    case search_for_escape_filter(Variable, Filter, Context) of
        on ->
            erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(force_escape),
                [filter_ast_noescape(Variable, Filter, Context#dtl_context{auto_escape = did})]);
        _ ->
            filter_ast_noescape(Variable, Filter, Context#dtl_context{auto_escape = did})
    end.

filter_ast_noescape(Variable, [{identifier, _, "escape"}], Context) ->
    body_ast([Variable], Context);
filter_ast_noescape(Variable, [{identifier, _, Name} | Arg], Context) ->
    erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(Name), 
        [body_ast([Variable], Context) | case Arg of 
                [{string_literal, _, ArgName}] ->
                    [erl_syntax:string(unescape_string_literal(ArgName))];
                [{number_literal, _, ArgName}] ->
                    [erl_syntax:integer(list_to_integer(ArgName))];
                _ ->
                    []
            end]).

search_for_escape_filter(_, _, #dtl_context{auto_escape = on}) ->
    on;
search_for_escape_filter(_, _, #dtl_context{auto_escape = did}) ->
    off;
search_for_escape_filter(Variable, Filter, _) ->
    search_for_escape_filter(Variable, Filter).

search_for_escape_filter(_, [{identifier, _, "escape"}]) ->
    on;
search_for_escape_filter({apply_filter, Variable, Filter}, _) ->
    search_for_escape_filter(Variable, Filter);
search_for_escape_filter(_Variable, _Filter) ->
    off.

resolve_variable_ast({{identifier, _, VarName}}, Context) ->
    auto_escape(resolve_variable_name_ast(VarName, Context), Context);

resolve_variable_ast({{identifier, _, VarName}, {identifier, _, AttrName}}, Context) ->
    auto_escape(erl_syntax:application(
            erl_syntax:atom(proplists), erl_syntax:atom(get_value),
        [erl_syntax:atom(AttrName), resolve_variable_name_ast(VarName, Context)]), 
        Context).

resolve_variable_name_ast(VarName, Context) ->
    VarValue = lists:foldl(fun(Scope, Value) ->
                case Value of
                    undefined ->
                        proplists:get_value(list_to_atom(VarName), Scope);
                    _ ->
                        Value
                end
        end, undefined, Context#dtl_context.local_scopes),
    case VarValue of
        undefined ->
            erl_syntax:application(erl_syntax:atom(proplists), erl_syntax:atom(get_value),
                [erl_syntax:atom(VarName), erl_syntax:variable("Variables")]);
        _ ->
            VarValue
    end.

auto_escape(Value, Context) ->
    case Context#dtl_context.auto_escape of
        on ->
            erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(force_escape),
                [Value]);
        _ ->
            Value
    end.

ifelse_ast(Variable, IfContentsAst, ElseContentsAst, Context) ->
    erl_syntax:case_expr(resolve_variable_ast(Variable, Context),
        [erl_syntax:clause([erl_syntax:string("")], none, 
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:atom(undefined)], none,
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:integer(0)], none,
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:underscore()], none,
                [IfContentsAst])
        ]).

for_loop_ast(Iterator, List, Contents, Context) ->
    erl_syntax:application(erl_syntax:atom(lists), erl_syntax:atom(map),
        [erl_syntax:fun_expr([
                    erl_syntax:clause([erl_syntax:variable("Var_" ++ Iterator)], 
                        none, [body_ast(Contents, 
                                Context#dtl_context{local_scopes = 
                                    [[{list_to_atom(Iterator), erl_syntax:variable("Var_" ++ Iterator)}] 
                                        | Context#dtl_context.local_scopes]
                                })]
                    )]),
            resolve_variable_name_ast(list_to_atom(List), Context)]).

for_list_loop_ast(IteratorList, List, Contents, Context) ->
    erl_syntax:application(erl_syntax:atom(lists), erl_syntax:atom(map),
        [erl_syntax:fun_expr([
                    erl_syntax:clause([erl_syntax:list(
                                lists:map(fun({identifier, _, Iterator}) -> 
                                            erl_syntax:variable("Var_" ++ Iterator) 
                                    end, IteratorList))], 
                        none, [body_ast(Contents,
                                Context#dtl_context{local_scopes = [lists:map(fun({identifier, _, Iterator}) ->
                                                    {list_to_atom(Iterator), erl_syntax:variable("Var_" ++ Iterator)} 
                                            end, IteratorList)
                                        | Context#dtl_context.local_scopes]})]
                    )]),
            resolve_variable_name_ast(list_to_atom(List), Context)]).

tag_ast(Name, Args, Context) ->
    InterpretedArgs = lists:map(fun
            ({{identifier, _, Key}, {string_literal, _, Value}}) ->
                {list_to_atom(Key), erl_syntax:string(unescape_string_literal(Value))};
            ({{identifier, _, Key}, {variable, Value}}) ->
                {list_to_atom(Key), resolve_variable_ast(Value, Context)}
        end, Args),
    Source = filename:join([erlydtl_deps:get_base_dir(), "priv", "tags", Name]),
    case parse(Source) of
        {ok, TagAst} ->
            body_ast(TagAst, Context#dtl_context{
                    local_scopes = [ InterpretedArgs | Context#dtl_context.local_scopes ]});
        _ ->
            {error, Name, "Loading tag source failed: " ++ Source}
    end.

unescape_string_literal(String) ->
    unescape_string_literal(string:strip(String, both, 34), [], noslash).

unescape_string_literal([], Acc, noslash) ->
    lists:reverse(Acc);
unescape_string_literal([$\\ | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, Acc, slash);
unescape_string_literal([C | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, [C | Acc], noslash);
unescape_string_literal("n" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, ["\n" | Acc], noslash);
unescape_string_literal("r" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, ["\r" | Acc], noslash);
unescape_string_literal("t" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, ["\t" | Acc], noslash);
unescape_string_literal([C | Rest], Acc, slash) ->
    unescape_string_literal(Rest, [C | Acc], noslash).
