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

-export([compile/2, compile/3, compile/4, compile/5, compile/6,compile/7]).

-record(dtl_context, {
    local_scopes = [], 
    block_dict = dict:new(), 
    auto_escape = off, 
    doc_root = "", 
    parse_trail = [],
    preset_vars = [],
    custom_tags_dir = [],
    reader = {file, read_file},
    module = []}).

-record(ast_info, {
    dependencies = [],
    var_names = [],
    pre_render_asts = []}).
    
-record(treewalker, {
    counter = 0,
    custom_tags = []}).    


compile(File, Module) ->
    compile(File, Module, "").

compile(File, Module, DocRoot) ->
    compile(File, Module, DocRoot, []).

compile(File, Module, DocRoot, Vars) ->
    compile(File, Module, DocRoot, Vars, []).
        
compile(File, Module, DocRoot, Vars, CustomTagsDir) ->
    compile(File, Module, DocRoot, Vars, CustomTagsDir, {file, read_file}).
        
compile(File, Module, DocRoot, Vars, CustomTagsDir, Reader) ->
    compile(File, Module, DocRoot, Vars, CustomTagsDir, Reader, "ebin").


compile(File, Module, DocRoot, Vars, CustomTagsDir, Reader, OutDir) ->  
    crypto:start(),
    case parse(File, list_to_atom(Module), Reader) of  
        ok ->
            ok;
        {ok, DjangoParseTree, CheckSum} ->
            try body_ast(DjangoParseTree, #dtl_context{
                    doc_root = DocRoot,
                    custom_tags_dir = CustomTagsDir,
                    parse_trail = [File], 
                    preset_vars = Vars, 
                    reader = Reader,
                    module = list_to_atom(Module)}, #treewalker{}) of
                {{Ast, Info}, _} ->
                    case compile:forms(forms(File, Module, Ast, Info, CheckSum), []) of
                        {ok, Module1, Bin} ->       
                            BeamFile = filename:join([OutDir, atom_to_list(Module1) ++ ".beam"]),
                            case file:write_file(BeamFile, Bin) of
                                ok ->
                                    code:purge(Module1),
                                    case code:load_binary(Module1, atom_to_list(Module1) ++ ".erl", Bin) of
                                        {module, _} -> ok;
                                        _ -> {error, lists:concat(["code reload failed: ", BeamFile])}
                                    end;
                                {error, Reason} ->
                                    {error, lists:concat(["beam generation failed (", Reason, "): ", BeamFile])}
                            end;
                        error ->
                            {error, lists:concat(["compilation failed: ", File])};
                        OtherError ->
                            OtherError
                    end
            catch 
                throw:Error -> Error
            end;
        Err ->
            Err
    end.
    

is_up_to_date(CheckSum, Module, {M, F}) ->
    case catch Module:source() of
        {_, CheckSum} -> 
            case catch Module:dependencies() of
                L when is_list(L) ->
                    RecompileList = lists:foldl(fun
                            ({XFile, XCheckSum}, Acc) ->
                                case catch M:F(XFile) of
                                    {ok, Data} ->
                                        case binary_to_list(crypto:sha(Data)) of
                                            XCheckSum ->
                                                Acc;
                                            _ ->
                                                [recompile | Acc]
                                        end;
                                    _ ->
                                        [recompile | Acc]
                                end                                        
                        end, [], L),
                    case RecompileList of
                        [] -> true; 
                        _ -> false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.
    
    
parse(Data) ->
    case erlydtl_scanner:scan(binary_to_list(Data)) of
        {ok, Tokens} ->
            erlydtl_parser:parse(Tokens);
        Err ->
            Err
    end.        
  
        
parse(CheckSum, Module, Data, Reader) ->
    case is_up_to_date(CheckSum, Module, Reader) of
        true_ignoring_for_now ->  %% it works, but is disabled, to enable change to 'true' 
            ok;
        _ ->
            case parse(Data) of
                {ok, Val} ->
                    {ok, Val, CheckSum};
                Err ->
                    Err
            end
    end.
    

parse(File, Module, {M, F} = Reader) ->  
    case catch M:F(File) of
        {ok, Data} ->
            CheckSum = binary_to_list(crypto:sha(Data)),
            parse(CheckSum, Module, Data, Reader);
        _ ->
            {error, "reading " ++ File ++ " failed "}
    end.

  
forms(File, Module, BodyAst, BodyInfo, CheckSum) ->    
    Render0FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([], none, [erl_syntax:application(none, 
                        erl_syntax:atom(render), [erl_syntax:list([])])])]),
    Function2 = erl_syntax:application(none, erl_syntax:atom(render2),
        [erl_syntax:variable("Variables")]),
    ClauseOk = erl_syntax:clause([erl_syntax:variable("Val")], none,
        [erl_syntax:tuple([erl_syntax:atom(ok), erl_syntax:variable("Val")])]),     
    ClauseCatch = erl_syntax:clause([erl_syntax:variable("Err")], none,
        [erl_syntax:tuple([erl_syntax:atom(error), erl_syntax:variable("Err")])]),            
    Render1FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([erl_syntax:variable("Variables")], none, 
            [erl_syntax:try_expr([Function2], [ClauseOk], [ClauseCatch])])]),  
     
    SourceFunctionTuple = erl_syntax:tuple(
        [erl_syntax:string(File), erl_syntax:string(CheckSum)]),
    SourceFunctionAst = erl_syntax:function(
        erl_syntax:atom(source),
            [erl_syntax:clause([], none, [SourceFunctionTuple])]),
    
    DependenciesFunctionAst = erl_syntax:function(
        erl_syntax:atom(dependencies), [erl_syntax:clause([], none, 
            [erl_syntax:list(lists:map(fun 
                    ({XFile, XCheckSum}) -> 
                        erl_syntax:tuple([erl_syntax:string(XFile), erl_syntax:string(XCheckSum)])
                end, BodyInfo#ast_info.dependencies))])]),     
    
    RenderInternalFunctionAst = erl_syntax:function(
        erl_syntax:atom(render2), 
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
        [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(0)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(1)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(source), erl_syntax:integer(0)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(dependencies), erl_syntax:integer(0))])]),
    
    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, Render0FunctionAst,
            Render1FunctionAst, SourceFunctionAst, DependenciesFunctionAst, RenderInternalFunctionAst, 
            ProplistsFunctionAst | BodyInfo#ast_info.pre_render_asts]].    

        
% child templates should only consist of blocks at the top level
body_ast([{extends, {string_literal, _Pos, String}} | ThisParseTree], Context, TreeWalker) ->
    File = full_path(unescape_string_literal(String), Context#dtl_context.doc_root),
    case lists:member(File, Context#dtl_context.parse_trail) of
        true ->
            throw({error, "Circular file inclusion!"});
        _ ->
            %% TODO: check for errors
            {ok, ParentParseTree, CheckSum} = parse(File, Context#dtl_context.module, Context#dtl_context.reader),
            BlockDict = lists:foldl(
                fun
                    ({block, {identifier, _, Name}, Contents}, Dict) ->
                        dict:store(Name, Contents, Dict);
                    (_, Dict) ->
                        Dict
                end, dict:new(), ThisParseTree),
            with_dependency({File, CheckSum}, body_ast(ParentParseTree, Context#dtl_context{
                    block_dict = dict:merge(fun(_Key, _ParentVal, ChildVal) -> ChildVal end,
                        BlockDict, Context#dtl_context.block_dict),
                    parse_trail = [File | Context#dtl_context.parse_trail]}, TreeWalker))
    end;
 
    
body_ast(DjangoParseTree, Context, TreeWalker) ->
    {AstInfoList, TreeWalker2} = lists:mapfoldl(
        fun
            ({'block', {identifier, _, Name}, Contents}, TreeWalkerAcc) ->
                Block = case dict:find(Name, Context#dtl_context.block_dict) of
                    {ok, ChildBlock} ->
                        ChildBlock;
                    _ ->
                        Contents
                end,
                body_ast(Block, Context, TreeWalkerAcc);
            ({'comment', _Contents}, TreeWalkerAcc) ->
                empty_ast(TreeWalkerAcc);
            ({'autoescape', {identifier, _, OnOrOff}, Contents}, TreeWalkerAcc) ->
                body_ast(Contents, Context#dtl_context{auto_escape = list_to_atom(OnOrOff)}, TreeWalkerAcc);
            ({'text', _Pos, String}, TreeWalkerAcc) -> 
                string_ast(String, TreeWalkerAcc);
            ({'string_literal', _Pos, String}, TreeWalkerAcc) ->
                {{auto_escape(erl_syntax:string(unescape_string_literal(String)), Context), #ast_info{}}, TreeWalkerAcc};
            ({'number_literal', _Pos, Number}, TreeWalkerAcc) ->
                string_ast(Number, TreeWalkerAcc);
            ({'variable', Variable}, TreeWalkerAcc) ->
                {Ast, VarName} = resolve_variable_ast(Variable, Context),
                {{format(Ast, Context), #ast_info{var_names = [VarName]}}, TreeWalkerAcc};              
            ({'include', {string_literal, _, File}}, TreeWalkerAcc) ->
                include_ast(unescape_string_literal(File), Context, TreeWalkerAcc);
            ({'if', {variable, Variable}, Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'if', {'not', {variable, Variable}}, Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = empty_ast(TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(Contents, Context, TreeWalker1),
                ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifelse', {variable, Variable}, IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context, TreeWalker1),
                ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifelse', {'not', {variable, Variable}}, IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(ElseContents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(IfContents, Context, TreeWalker1),
                ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context, TreeWalker2);                  
            ({'ifequal', Args, Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifequalelse', Args, IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc), 
                {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context,TreeWalker1),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context, TreeWalker2);                
            ({'ifnotequal', Args, Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = empty_ast(TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(Contents, Context, TreeWalker1),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifnotequalelse', Args, IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(ElseContents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(IfContents, Context, TreeWalker1),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context, TreeWalker2);                    
            ({'apply_filter', Variable, Filter}, TreeWalkerAcc) ->
                filter_ast(Variable, Filter, Context, TreeWalkerAcc);
            ({'for', {'in', IteratorList, Variable}, Contents}, TreeWalkerAcc) ->
                for_loop_ast(IteratorList, Variable, Contents, Context, TreeWalkerAcc);
            ({'load', Names}, TreeWalkerAcc) ->
                load_ast(Names, Context, TreeWalkerAcc);
            ({'tag', {identifier, _, Name}, Args}, TreeWalkerAcc) ->
                tag_ast(Name, Args, Context, TreeWalkerAcc);            
            ({'call', {'identifier', _, Name}}, TreeWalkerAcc) ->
            	call_ast(Name, TreeWalkerAcc);
            ({'call', {'identifier', _, Name}, {variable, With}}, TreeWalkerAcc) ->
            	call_with_ast(Name, With, Context, TreeWalkerAcc)                
        end, TreeWalker, DjangoParseTree),   
    {AstList, {Info, TreeWalker3}} = lists:mapfoldl(
        fun({Ast, Info}, {InfoAcc, TreeWalkerAcc}) -> 
                PresetVars = lists:foldl(fun
                        (X, Acc) ->
                            case proplists:lookup(list_to_atom(X), Context#dtl_context.preset_vars) of
                                none ->
                                    Acc;
                                Val ->
                                    [erl_syntax:abstract(Val) | Acc]
                            end
                    end, [], Info#ast_info.var_names),
                case PresetVars of
                    [] ->
                        {Ast, {merge_info(Info, InfoAcc), TreeWalkerAcc}};
                    _ ->
                        Counter = TreeWalkerAcc#treewalker.counter,
                        Name = lists:concat([pre_render, Counter]),
                        Ast1 = erl_syntax:application(none, erl_syntax:atom(Name),
                            [erl_syntax:list(PresetVars)]),
                        PreRenderAst = erl_syntax:function(erl_syntax:atom(Name),
                            [erl_syntax:clause([erl_syntax:variable("Variables")], none, [Ast])]),
                        PreRenderAsts = Info#ast_info.pre_render_asts,
                        Info1 = Info#ast_info{pre_render_asts = [PreRenderAst | PreRenderAsts]},     
                        {Ast1, {merge_info(Info1, InfoAcc), TreeWalkerAcc#treewalker{counter = Counter + 1}}}
                end
        end, {#ast_info{}, TreeWalker2}, AstInfoList),
    {{erl_syntax:list(AstList), Info}, TreeWalker3}.


merge_info(Info1, Info2) ->
    #ast_info{dependencies = 
        lists:merge(
            lists:sort(Info1#ast_info.dependencies), 
            lists:sort(Info2#ast_info.dependencies)),
        var_names = 
            lists:merge(
                lists:sort(Info1#ast_info.var_names), 
                lists:sort(Info2#ast_info.var_names)),
        pre_render_asts = 
            lists:merge(
                Info1#ast_info.pre_render_asts,
                Info2#ast_info.pre_render_asts)}.


with_dependencies([], Args) ->
    Args;
with_dependencies([H, T], Args) ->
     with_dependencies(T, with_dependency(H, Args)).
        
with_dependency(FilePath, {{Ast, Info}, TreeWalker}) ->
    {{Ast, Info#ast_info{dependencies = [FilePath | Info#ast_info.dependencies]}}, TreeWalker}.


empty_ast(TreeWalker) ->
    {{erl_syntax:list([]), #ast_info{}}, TreeWalker}.


string_ast(String, TreeWalker) ->
    {{erl_syntax:string(String), #ast_info{}}, TreeWalker}. %% less verbose AST, better for development and debugging
    % {{erl_syntax:binary([erl_syntax:binary_field(erl_syntax:integer(X)) || X <- String]), #ast_info{}}, TreeWalker}.       


include_ast(File, Context, TreeWalker) ->
    FilePath = full_path(File, Context#dtl_context.doc_root),
    %% TODO: check for errors (and throw error report)
    {ok, InclusionParseTree, CheckSum} = parse(FilePath, Context#dtl_context.module, Context#dtl_context.reader),
    with_dependency({FilePath, CheckSum}, body_ast(InclusionParseTree, Context#dtl_context{
        parse_trail = [FilePath | Context#dtl_context.parse_trail]}, TreeWalker)).


filter_ast(Variable, Filter, Context, TreeWalker) ->
    % the escape filter is special; it is always applied last, so we have to go digging for it

    % AutoEscape = 'did' means we (will have) decided whether to escape the current variable,
    % so don't do any more escaping
    {{UnescapedAst, Info}, TreeWalker2} = filter_ast_noescape(Variable, Filter, 
        Context#dtl_context{auto_escape = did}, TreeWalker),
    case search_for_escape_filter(Variable, Filter, Context) of
        on ->
            {{erl_syntax:application(
                    erl_syntax:atom(erlydtl_filters), 
                    erl_syntax:atom(force_escape), 
                    [UnescapedAst]), 
                Info}, TreeWalker2};
        _ ->
            {{UnescapedAst, Info}, TreeWalker2}
    end.

filter_ast_noescape(Variable, [{identifier, _, "escape"}], Context, TreeWalker) ->
    body_ast([Variable], Context, TreeWalker);
filter_ast_noescape(Variable, [{identifier, _, Name} | Arg], Context, TreeWalker) ->
    {{VariableAst, Info}, TreeWalker2} = body_ast([Variable], Context, TreeWalker),
    {{erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(Name), 
        [VariableAst | case Arg of 
                [{string_literal, _, ArgName}] ->
                    [erl_syntax:string(unescape_string_literal(ArgName))];
                [{number_literal, _, ArgName}] ->
                    [erl_syntax:integer(list_to_integer(ArgName))];
                _ ->
                    []
            end]), Info}, TreeWalker2}.

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
    {resolve_variable_name_ast(VarName, Context, ModuleAst), VarName};

resolve_variable_ast({{identifier, _, VarName}, {identifier, _, AttrName}}, Context, ModuleAst) ->
    {erl_syntax:application(ModuleAst, erl_syntax:atom(get_value),
                    [erl_syntax:atom(AttrName), resolve_variable_name_ast(VarName, Context)]), VarName}.


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
            erl_syntax:application(ModuleAst, erl_syntax:atom('get_value'),
                [erl_syntax:atom(VarName), erl_syntax:variable("Variables")]);
        _ ->
            VarValue
    end.


format(Ast, Context) ->
    auto_escape(format_integer_ast(Ast), Context).


format_integer_ast(Ast) ->
    erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(format_integer),
        [Ast]).


auto_escape(Value, Context) ->
    case Context#dtl_context.auto_escape of
        on ->
            erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(force_escape),
                [Value]);
        _ ->
            Value
    end.


ifelse_ast(Variable, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    VarNames = Info#ast_info.var_names,
    {Ast, VarName} = resolve_ifvariable_ast(Variable, Context),
    {{erl_syntax:case_expr(Ast,
        [erl_syntax:clause([erl_syntax:string("")], none, 
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:atom(undefined)], none,
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:atom(false)], none,
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:string("0")], none,
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:underscore()], none,
                [IfContentsAst])
        ]), Info#ast_info{var_names = [VarName | VarNames]}}, TreeWalker}.

        
ifequalelse_ast(Args, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    {[Arg1Ast, Arg2Ast], VarNames} = lists:foldl(fun
            (X, {Asts, AccVarNames}) ->
                case X of
                    {variable, Var} ->
                        {Ast, VarName} = resolve_ifvariable_ast(Var, Context),
                        {[Ast | Asts], [VarName | AccVarNames]};
                    {string_literal, _, Literal} ->
                        {[erl_syntax:string(unescape_string_literal(Literal)) | Asts], AccVarNames};
                    {number_literal, _, Literal} ->
                        {[erl_syntax:integer(list_to_integer(Literal)) | Asts], AccVarNames}
                end                
        end,
        {[], Info#ast_info.var_names},
        Args),
    Ast = erl_syntax:application(none, erl_syntax:atom(apply), [erl_syntax:fun_expr(
        [erl_syntax:clause([erl_syntax:variable("Arg1"), erl_syntax:variable("Arg2")], none, 
            [erl_syntax:case_expr(erl_syntax:variable("Arg1"),
                [erl_syntax:clause([erl_syntax:variable("Arg2")], none, [IfContentsAst]),
                    erl_syntax:clause([erl_syntax:underscore()], none, [ElseContentsAst])])])]), 
                        erl_syntax:list([Arg1Ast, Arg2Ast])]),    
    {{Ast, Info#ast_info{var_names = VarNames}}, TreeWalker}.         


for_loop_ast(IteratorList, {variable, Variable}, Contents, Context, TreeWalker) ->
    Vars = lists:map(fun({identifier, _, Iterator}) -> 
                    erl_syntax:variable("Var_" ++ Iterator) 
            end, IteratorList),
    CounterVars = erl_syntax:list([
            erl_syntax:tuple([erl_syntax:atom('counter'), erl_syntax:variable("Counter")]),
            erl_syntax:tuple([erl_syntax:atom('counter0'), erl_syntax:variable("Counter0")])
        ]),
    {{InnerAst, Info}, TreeWalker2} = body_ast(Contents,
        Context#dtl_context{local_scopes = [
                [{'forloop', CounterVars} | lists:map(
                    fun({identifier, _, Iterator}) ->
                            {list_to_atom(Iterator), erl_syntax:variable("Var_" ++ Iterator)} 
                    end, IteratorList)] | Context#dtl_context.local_scopes]}, TreeWalker),
    CounterAst = erl_syntax:list([
            erl_syntax:tuple([erl_syntax:atom('counter'), 
                    erl_syntax:infix_expr(erl_syntax:variable("Counter"), erl_syntax:operator("+"), erl_syntax:integer(1))]),
            erl_syntax:tuple([erl_syntax:atom('counter0'),
                    erl_syntax:infix_expr(erl_syntax:variable("Counter0"), erl_syntax:operator("+"), erl_syntax:integer(1))])
        ]),
    {ListAst, VarName} = resolve_ifvariable_ast(Variable, Context),
    CounterVars0 = erl_syntax:list([
            erl_syntax:tuple([erl_syntax:atom('counter'), erl_syntax:integer(1)]),
            erl_syntax:tuple([erl_syntax:atom('counter0'), erl_syntax:integer(0)])
        ]),
    {{erl_syntax:application(
            erl_syntax:atom('erlang'), erl_syntax:atom('element'),
            [erl_syntax:integer(1), erl_syntax:application(
                    erl_syntax:atom('lists'), erl_syntax:atom('mapfoldl'),
                    [erl_syntax:fun_expr([
                                erl_syntax:clause([erl_syntax:tuple(Vars), CounterVars], none, 
                                    [erl_syntax:tuple([InnerAst, CounterAst])]),
                                erl_syntax:clause(case Vars of [H] -> [H, CounterVars];
                                        _ -> [erl_syntax:list(Vars), CounterVars] end, none, 
                                    [erl_syntax:tuple([InnerAst, CounterAst])])
                            ]),
                        CounterVars0, ListAst])]),
                Info#ast_info{var_names = [VarName]}}, TreeWalker2}.

load_ast(Names, _Context, TreeWalker) ->
    CustomTags = lists:merge([X || {identifier, _ , X} <- Names], TreeWalker#treewalker.custom_tags),
    {{erl_syntax:list([]), #ast_info{}}, TreeWalker#treewalker{custom_tags = CustomTags}}.  


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


full_path(File, DocRoot) ->
    filename:join([DocRoot, File]).
        
%%-------------------------------------------------------------------
%% Custom tags
%%-------------------------------------------------------------------

tag_ast(Name, Args, Context, TreeWalker) ->
    case lists:member(Name, TreeWalker#treewalker.custom_tags) of
        true ->
            InterpretedArgs = lists:map(fun
                    ({{identifier, _, Key}, {string_literal, _, Value}}) ->
                        {list_to_atom(Key), erl_syntax:string(unescape_string_literal(Value))};
                    ({{identifier, _, Key}, {variable, Value}}) ->
                        {list_to_atom(Key), format(resolve_variable_ast(Value, Context), Context)}
                end, Args),
            DefaultFilePath = filename:join([erlydtl_deps:get_base_dir(), "priv", "custom_tags", Name]),
            case Context#dtl_context.custom_tags_dir of
                [] ->
                    case parse(DefaultFilePath, Context#dtl_context.module, Context#dtl_context.reader) of
                        {ok, TagParseTree, CheckSum} ->
                            tag_ast2({DefaultFilePath, CheckSum}, TagParseTree, InterpretedArgs, Context, TreeWalker);
                        _ ->
                            Reason = lists:concat(["Loading tag source for '", Name, "' failed: ", 
                                DefaultFilePath]),
                            throw({error, Reason})
                    end;
                _ ->
                    CustomFilePath = filename:join([Context#dtl_context.custom_tags_dir, Name]),
                    case parse(CustomFilePath, Context#dtl_context.module, Context#dtl_context.reader) of
                        {ok, TagParseTree, CheckSum} ->
                            tag_ast2({CustomFilePath, CheckSum},TagParseTree, InterpretedArgs, Context, TreeWalker);
                        _ ->
                            case parse(DefaultFilePath, Context#dtl_context.module, Context#dtl_context.reader) of
                                {ok, TagParseTree, CheckSum} ->
                                    tag_ast2({DefaultFilePath, CheckSum}, TagParseTree, InterpretedArgs, Context, TreeWalker);
                                _ ->
                                    Reason = lists:concat(["Loading tag source for '", Name, "' failed: ", 
                                        CustomFilePath, ", ", DefaultFilePath]),
                                    throw({error, Reason})
                            end
                    end
            end;
        _ ->
            throw({error, lists:concat(["Custom tag '", Name, "' not loaded"])})
    end.
 
 tag_ast2({Source, _} = Dep, TagParseTree, InterpretedArgs, Context, TreeWalker) ->
    with_dependency(Dep, body_ast(TagParseTree, Context#dtl_context{
        local_scopes = [ InterpretedArgs | Context#dtl_context.local_scopes ],
        parse_trail = [ Source | Context#dtl_context.parse_trail ]}, TreeWalker)).


call_ast(Module, TreeWalkerAcc) ->
    call_ast(Module, erl_syntax:variable("Variables"), #ast_info{}, TreeWalkerAcc).

call_with_ast(Module, Variable, Context, TreeWalker) ->
    {VarAst, VarName} = resolve_variable_ast(Variable, Context),
    call_ast(Module, VarAst, #ast_info{var_names=[VarName]}, TreeWalker).
        
call_ast(Module, Variable, AstInfo, TreeWalker) ->
     AppAst = erl_syntax:application(
		erl_syntax:atom(Module),
		erl_syntax:atom(render),
		[Variable]),
    RenderedAst = erl_syntax:variable("Rendered"),
    OkAst = erl_syntax:clause(
	      [erl_syntax:tuple([erl_syntax:atom(ok), RenderedAst])], 
	      none,
	      [RenderedAst]),
    ReasonAst = erl_syntax:variable("Reason"),
    ErrStrAst = erl_syntax:application(
		  erl_syntax:atom(io_lib),
		  erl_syntax:atom(format),
		  [erl_syntax:string("error: ~p"), erl_syntax:list([ReasonAst])]),
    ErrorAst = erl_syntax:clause(
		 [erl_syntax:tuple([erl_syntax:atom(error), ReasonAst])], 
		 none,
		 [ErrStrAst]),
    CallAst = erl_syntax:case_expr(AppAst, [OkAst, ErrorAst]),   
    Module2 = list_to_atom(Module),
    with_dependencies(Module2:dependencies(), {{CallAst, AstInfo}, TreeWalker}).