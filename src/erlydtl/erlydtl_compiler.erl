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
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

-export([compile/2, compile/3, compile/4, compile/5, compile/6, parse/1, scan/1]).

-record(dtl_context, {
        local_scopes = [], 
        block_dict = dict:new(), 
        auto_escape = off, 
        doc_root = "", 
        parse_trail = [],
        preset_vars = []}).

-record(ast_info, {
        dependencies = [],
        template_vars = [],
        pre_render_asts = []
    }).

compile(File, Module) ->
    compile(File, Module, "").

compile(File, Module, DocRoot) ->
    compile(File, Module, DocRoot, []).

compile(File, Module, DocRoot, Vars) ->
    compile(File, Module, DocRoot, Vars, "render").
    
compile(File, Module, DocRoot, Vars, Function) ->
    compile(File, Module, DocRoot, Vars, Function, "ebin").

compile(File, Module, DocRoot, Vars, Function, OutDir) ->   
    case parse(File) of
        {ok, DjangoParseTree} ->
            OldProcessDictVal = put(erlydtl_counter, 0),
            
            {BodyAst, BodyInfo} = body_ast(DjangoParseTree,
                #dtl_context{doc_root = DocRoot, parse_trail = [File], preset_vars = Vars}), 

            Render0FunctionAst = erl_syntax:function(erl_syntax:atom(Function),
                [erl_syntax:clause([], none, [erl_syntax:application(none, 
                    erl_syntax:atom(Function), [erl_syntax:list([])])])]),

            Function2 = erl_syntax:application(none, erl_syntax:atom(Function ++ "2"),
                [erl_syntax:variable("Variables")]),
            ClauseOk = erl_syntax:clause([erl_syntax:variable("Val")], none,
                [erl_syntax:tuple([erl_syntax:atom(ok), erl_syntax:variable("Val")])]),     
            ClauseCatch = erl_syntax:clause([erl_syntax:variable("Err")], none,
                [erl_syntax:tuple([erl_syntax:atom(error), erl_syntax:variable("Err")])]),            
            Render1FunctionAst = erl_syntax:function(erl_syntax:atom(Function),
                [erl_syntax:clause([erl_syntax:variable("Variables")], none, 
                    [erl_syntax:try_expr([Function2], [ClauseOk], [ClauseCatch])])]),  
                          
            SourceFunctionAst = erl_syntax:function(
                erl_syntax:atom(source),
                    [erl_syntax:clause([], none, [erl_syntax:string(File)])]),
                    
            DependenciesFunctionAst = erl_syntax:function(
                erl_syntax:atom(dependencies), [erl_syntax:clause([], none, 
                    [erl_syntax:list(lists:map(fun(Dep) -> erl_syntax:string(Dep) end, 
                        BodyInfo#ast_info.dependencies))])]),     
                                                   
            RenderInternalFunctionAst = erl_syntax:function(
                erl_syntax:atom(Function ++ "2"), 
                    [erl_syntax:clause([erl_syntax:variable("Variables")], none, 
                        [BodyAst])]),   
                                 
            ProplistsClauseErr = erl_syntax:clause([erl_syntax:atom(undefined)], none, 
                [erl_syntax:application(none, erl_syntax:atom(throw),
                    [erl_syntax:tuple([erl_syntax:atom(undefined_variable), erl_syntax:variable("Key")])])]),  
            ProplistsClauseOk = erl_syntax:clause([erl_syntax:variable("Val")], none, 
                [erl_syntax:variable("Val")]),       
            ProplistsFunctionAst = erl_syntax:function(erl_syntax:atom(get_value), 
                [erl_syntax:clause([erl_syntax:variable("Key"), erl_syntax:variable("L")], none, 
                    [erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(proplists), 
                        erl_syntax:atom(get_value), [erl_syntax:variable("Key"), erl_syntax:variable("L")]), 
                            [ProplistsClauseErr, ProplistsClauseOk])])]),

            ModuleAst  = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
            ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
                [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(Function), erl_syntax:integer(0)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(Function), erl_syntax:integer(1)),
                        erl_syntax:arity_qualifier(erl_syntax:atom(source), erl_syntax:integer(0)),
                            erl_syntax:arity_qualifier(erl_syntax:atom(dependencies), erl_syntax:integer(0))])]),
            
            Forms = [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, Render0FunctionAst,
                Render1FunctionAst, SourceFunctionAst, DependenciesFunctionAst, RenderInternalFunctionAst, 
                    ProplistsFunctionAst | BodyInfo#ast_info.pre_render_asts]],
                    
            case OldProcessDictVal of
                undefined -> erase(erlydtl_counter);
                _ -> put(erlydtl_counter, OldProcessDictVal)
            end,
            case compile:forms(Forms, []) of
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

        
% child templates should only consist of blocks at the top level
body_ast([{extends, {string_literal, _Pos, String}} | ThisParseTree], Context) ->
    File = full_path(unescape_string_literal(String), Context#dtl_context.doc_root),
    case lists:member(File, Context#dtl_context.parse_trail) of
        true ->
            {error, "Circular file inclusion!"};
        _ ->
            {ok, ParentParseTree} = parse(File),
            BlockDict = lists:foldl(
                fun
                    ({block, {identifier, _, Name}, Contents}, Dict) ->
                        dict:store(Name, Contents, Dict);
                    (_, Dict) ->
                        Dict
                end, dict:new(), ThisParseTree),
            with_dependency(File, body_ast(ParentParseTree, Context#dtl_context{
                    block_dict = dict:merge(fun(_Key, _ParentVal, ChildVal) -> ChildVal end,
                        BlockDict, Context#dtl_context.block_dict),
                    parse_trail = [File | Context#dtl_context.parse_trail]}))
    end;
    
body_ast(DjangoParseTree, Context) ->
    AstInfoList = lists:map(
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
                empty_ast();
            ({'autoescape', {identifier, _, OnOrOff}, Contents}) ->
                body_ast(Contents, Context#dtl_context{auto_escape = list_to_atom(OnOrOff)});
            ({'text', _Pos, String}) -> 
                string_ast(String);
            ({'string_literal', _Pos, String}) ->
                {auto_escape(erl_syntax:string(unescape_string_literal(String)), Context), #ast_info{}};
            ({'number_literal', _Pos, Number}) ->
                string_ast(Number);
            ({'variable', Variable}) ->
                {Ast, Var} = resolve_variable_ast(Variable, Context),
                {Ast, #ast_info{template_vars = [Var]}};
            ({'tag', {identifier, _, Name}, Args}) ->
                tag_ast(Name, Args, Context);
            ({'include', {string_literal, _, File}}) ->
                include_ast(unescape_string_literal(File), Context);
            ({'if', {variable, Variable}, Contents}) ->
                ifelse_ast(Variable, body_ast(Contents, Context), empty_ast(), Context);
            ({'if', {'not', {variable, Variable}}, Contents}) ->
                ifelse_ast(Variable, empty_ast(), body_ast(Contents, Context), Context);
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
        end, DjangoParseTree),
    {AstList, Info} = lists:mapfoldl(
        fun({Ast, Info}, InfoAcc) -> 
                PresetVars = lists:foldl(fun
                        (X, Acc) ->
                            case proplists:lookup(list_to_atom(X), Context#dtl_context.preset_vars) of
                                none ->
                                    Acc;
                                Val ->
                                    [erl_syntax:abstract(Val) | Acc]
                            end
                    end, [], Info#ast_info.template_vars),
                case PresetVars of
                    [] ->
                        {Ast, merge_info(Info, InfoAcc)};
                    _ ->
                        Id = get(erlydtl_counter),
                        put(erlydtl_counter, Id + 1),
                        Name = lists:concat([pre_render, Id]),
                        Ast1 = erl_syntax:application(none, erl_syntax:atom(Name),
                            [erl_syntax:list(PresetVars)]),
                        PreRenderAst = erl_syntax:function(erl_syntax:atom(Name),
                            [erl_syntax:clause([erl_syntax:variable("Variables")], none, [Ast])]),
                        PreRenderAsts = Info#ast_info.pre_render_asts,
                        Info1 = Info#ast_info{pre_render_asts = [PreRenderAst | PreRenderAsts]},     
                        {Ast1, merge_info(Info1, InfoAcc)}
                end
        end, #ast_info{}, AstInfoList),
    {erl_syntax:list(AstList), Info}.

merge_info(Info1, Info2) ->
    #ast_info{dependencies = 
        lists:merge(
            lists:sort(Info1#ast_info.dependencies), 
            lists:sort(Info2#ast_info.dependencies)),
        template_vars = 
            lists:merge(
                lists:sort(Info1#ast_info.template_vars), 
                lists:sort(Info2#ast_info.template_vars)),
        pre_render_asts = 
            lists:merge(
                Info1#ast_info.pre_render_asts,
                Info2#ast_info.pre_render_asts)}.

with_dependency(FilePath, {Ast, Info}) ->
    {Ast, Info#ast_info{dependencies = [FilePath | Info#ast_info.dependencies]}}.

empty_ast() ->
    {erl_syntax:list([]), #ast_info{}}.

string_ast(String) ->
    {erl_syntax:string(String), #ast_info{}}. %% less verbose AST, good for debugging
    % {erl_syntax:binary([erl_syntax:binary_field(erl_syntax:integer(X)) || X <- String]), #ast_info{}}.       

include_ast(File, Context) ->
    FilePath = full_path(File, Context#dtl_context.doc_root),
    {ok, InclusionParseTree} = parse(FilePath),
    with_dependency(FilePath, body_ast(InclusionParseTree, Context#dtl_context{parse_trail = 
                [FilePath | Context#dtl_context.parse_trail]})).

filter_ast(Variable, Filter, Context) ->
    % the escape filter is special; it is always applied last, so we have to go digging for it

    % AutoEscape = 'did' means we (will have) decided whether to escape the current variable,
    % so don't do any more escaping
    {UnescapedAst, Info} = filter_ast_noescape(Variable, Filter, 
        Context#dtl_context{auto_escape = did}),
    case search_for_escape_filter(Variable, Filter, Context) of
        on ->
            {erl_syntax:application(
                    erl_syntax:atom(erlydtl_filters), 
                    erl_syntax:atom(force_escape), 
                    [UnescapedAst]), 
                Info};
        _ ->
            {UnescapedAst, Info}
    end.

filter_ast_noescape(Variable, [{identifier, _, "escape"}], Context) ->
    body_ast([Variable], Context);
filter_ast_noescape(Variable, [{identifier, _, Name} | Arg], Context) ->
    {VariableAst, Info} = body_ast([Variable], Context),
    {erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(Name), 
        [VariableAst | case Arg of 
                [{string_literal, _, ArgName}] ->
                    [erl_syntax:string(unescape_string_literal(ArgName))];
                [{number_literal, _, ArgName}] ->
                    [erl_syntax:integer(list_to_integer(ArgName))];
                _ ->
                    []
            end]), Info}.

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

resolve_variable_ast(VarTuple, Context) ->
    resolve_variable_ast(VarTuple, Context, none).
 
resolve_ifvariable_ast(VarTuple, Context) ->
    resolve_variable_ast(VarTuple, Context, erl_syntax:atom(proplists)).
           
resolve_variable_ast({{identifier, _, VarName}}, Context, ModuleAst) ->
    {auto_escape(resolve_variable_name_ast(VarName, Context, ModuleAst), Context), VarName};

resolve_variable_ast({{identifier, _, VarName}, {identifier, _, AttrName}}, Context, ModuleAst) ->
    {auto_escape(erl_syntax:application(
            ModuleAst, erl_syntax:atom(get_value),
        [erl_syntax:atom(AttrName), resolve_variable_name_ast(VarName, Context)]), 
        Context), []}.
                
resolve_variable_name_ast(VarName, Context) ->
    resolve_variable_name_ast(VarName, Context, none).
    
resolve_variable_name_ast(VarName, Context, ModuleAst) ->
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
            erl_syntax:application(ModuleAst, erl_syntax:atom(get_value),
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

ifelse_ast(Variable, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, Context) ->
    Info2 = merge_info(IfContentsInfo, ElseContentsInfo),
    TemplateVars = Info2#ast_info.template_vars,
    {Ast, Var} = resolve_ifvariable_ast(Variable, Context),
    {erl_syntax:case_expr(Ast,
        [erl_syntax:clause([erl_syntax:string("")], none, 
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:atom(undefined)], none,
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:integer(0)], none,
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:underscore()], none,
                [IfContentsAst])
        ]), Info2#ast_info{template_vars = [Var | TemplateVars]}}.

for_loop_ast(Iterator, List, Contents, Context) ->
    {InnerAst, Info} = body_ast(Contents, 
        Context#dtl_context{local_scopes = 
            [[{list_to_atom(Iterator), erl_syntax:variable("Var_" ++ Iterator)}] 
                | Context#dtl_context.local_scopes]}),
    {erl_syntax:application(erl_syntax:atom(lists), erl_syntax:atom(map),
        [erl_syntax:fun_expr([
                    erl_syntax:clause([erl_syntax:variable("Var_" ++ Iterator)], 
                        none, [InnerAst])]),
            resolve_variable_name_ast(list_to_atom(List), Context)]), Info#ast_info{template_vars = [List]}}.

for_list_loop_ast(IteratorList, List, Contents, Context) ->
    Vars = erl_syntax:list(lists:map(
            fun({identifier, _, Iterator}) -> 
                    erl_syntax:variable("Var_" ++ Iterator) 
            end, IteratorList)),
    {InnerAst, Info} = body_ast(Contents,
        Context#dtl_context{local_scopes = [lists:map(
                    fun({identifier, _, Iterator}) ->
                            {list_to_atom(Iterator), erl_syntax:variable("Var_" ++ Iterator)} 
                    end, IteratorList) | Context#dtl_context.local_scopes]}),
    {erl_syntax:application(erl_syntax:atom(lists), erl_syntax:atom(map),
        [erl_syntax:fun_expr([erl_syntax:clause(
                        [Vars], none, [InnerAst])]),
            resolve_variable_name_ast(list_to_atom(List), Context)]), Info#ast_info{template_vars = [List]}}.

%% TODO: implement "laod" tag to make custom tags work like in original django
tag_ast(Name, Args, Context) ->
    InterpretedArgs = lists:map(fun
            ({{identifier, _, Key}, {string_literal, _, Value}}) ->
                {list_to_atom(Key), erl_syntax:string(unescape_string_literal(Value))};
            ({{identifier, _, Key}, {variable, Value}}) ->
                {list_to_atom(Key), resolve_variable_ast(Value, Context)}
        end, Args),
    Source = filename:join([erlydtl_deps:get_base_dir(), "priv", "tags", Name]),
    case parse(Source) of
        {ok, TagParseTree} ->
            with_dependency(Source, body_ast(TagParseTree, Context#dtl_context{
                    local_scopes = [ InterpretedArgs | Context#dtl_context.local_scopes ],
                    parse_trail = [ Source | Context#dtl_context.parse_trail ]}));
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
    