%%%-------------------------------------------------------------------
%%% File:      erlydtl_beam_compiler.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @copyright 2014 Andreas Stenius
%%% @doc
%%% ErlyDTL template compiler for beam targets.
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%% Copyright (c) 2014 Andreas Stenius
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
%%% @since 2014 by Andreas Stenius
%%%-------------------------------------------------------------------
-module(erlydtl_beam_compiler).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').
-author('Andreas Stenius <kaos@astekk.se>').

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

-export([compile/3, compile_dir/2, format_error/1]).

%% internal use
-export([
         is_up_to_date/2,

         format/3,
         value_ast/5,
         resolve_scoped_variable_ast/2,
         resolve_scoped_variable_ast/3,
         interpret_args/3
        ]).

-import(erlydtl_compiler, [parse_file/2, do_parse_template/2]).

-import(erlydtl_compiler_utils,
         [unescape_string_literal/1, full_path/2,
          print/3, get_current_file/1, add_errors/2, add_warnings/2,
          merge_info/2, call_extension/3, init_treewalker/1
         ]).

-include_lib("merl/include/merl.hrl").
-include("erlydtl_ext.hrl").


%% --------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------

compile(DjangoParseTree, CheckSum, Context) ->
    compile_to_binary(DjangoParseTree, CheckSum, Context).

compile_dir(Dir, Context) ->
    do_compile_dir(Dir, Context).

format_error(no_out_dir) ->
    "Compiled template not saved (need out_dir option)";
format_error(unexpected_extends_tag) ->
    "The extends tag must be at the very top of the template";
format_error(circular_include) ->
    "Circular file inclusion!";
format_error({write_file, Error}) ->
    io_lib:format(
      "Failed to write file: ~s",
      [file:format_error(Error)]);
format_error(compile_beam) ->
    "Failed to compile template to .beam file";
format_error(Error) ->
    %% may be an error thrown from erlydtl_compiler...
    erlydtl_compiler:format_error(Error).


%%====================================================================
%% Internal functions
%%====================================================================

do_compile_dir(Dir, Context) ->
    %% Find all files in Dir (recursively), matching the regex (no
    %% files ending in "~").
    Files = filelib:fold_files(Dir, ".+[^~]$", true, fun(F1,Acc1) -> [F1 | Acc1] end, []),
    {ParserResults,
     #dtl_context{ errors=#error_info{ list=ParserErrors } }=Context1}
        = lists:foldl(
            fun (File, {ResultAcc, Ctx}) ->
                    case filename:basename(File) of
                        "."++_ ->
                            {ResultAcc, Ctx};
                        _ ->
                            FilePath = filename:absname(File),
                            case filelib:is_dir(FilePath) of
                                true ->
                                    {ResultAcc, Ctx};
                                false ->
                                    case parse_file(FilePath, Ctx) of
                                        up_to_date -> {ResultAcc, Ctx};
                                        {ok, DjangoParseTree, CheckSum} ->
                                            {[{File, DjangoParseTree, CheckSum}|ResultAcc], Ctx};
                                        {error, Reason} -> {ResultAcc, ?ERR(Reason, Ctx)}
                                    end
                            end
                    end
            end,
            {[], Context},
            Files),
    if length(ParserErrors) == 0 ->
            compile_multiple_to_binary(Dir, ParserResults, Context1);
       true -> Context1
    end.

compile_multiple_to_binary(Dir, ParserResults, Context0) ->
    MatchAst = options_match_ast(Context0),
    {Functions,
     {AstInfo, _,
      #dtl_context{ errors=#error_info{ list=Errors } }=Context1}}
        = lists:mapfoldl(
            fun({File, DjangoParseTree, CheckSum}, {AstInfo, TreeWalker, Ctx}) ->
                    try
                        FilePath = full_path(File, Ctx#dtl_context.doc_root),
                        {{BodyAst, BodyInfo}, TreeWalker1} = with_dependency(
                                                               {FilePath, CheckSum},
                                                               body_ast(DjangoParseTree, Ctx, TreeWalker)),
                        FunctionName = filename:rootname(filename:basename(File)),
                        Function1 = ?Q("_@FunctionName@(_Variables) -> _@FunctionName@(_Variables, [])"),
                        Function2 = erl_syntax:function(
                                      erl_syntax:atom(FunctionName),
                                      [erl_syntax:clause(
                                         [erl_syntax:variable("_Variables"),
                                          erl_syntax:variable("RenderOptions")],
                                         none,
                                         MatchAst ++ stringify(BodyAst, Ctx))
                                      ]),
                        {{FunctionName, Function1, Function2}, {merge_info(AstInfo, BodyInfo), TreeWalker1, Ctx}}
                    catch
                        throw:Error ->
                            {error, {AstInfo, TreeWalker, ?ERR(Error, Ctx)}}
                    end
            end,
            {#ast_info{},
             init_treewalker(Context0),
             Context0},
            ParserResults),
    if length(Errors) == 0 ->
            Forms = custom_forms(Dir, Context1#dtl_context.module, Functions, AstInfo),
            compile_forms(Forms, Context1);
       true ->
            Context1
    end.

compile_to_binary(DjangoParseTree, CheckSum, Context) ->
    try body_ast(DjangoParseTree, Context, init_treewalker(Context)) of
        {{BodyAst, BodyInfo}, BodyTreeWalker} ->
            try custom_tags_ast(BodyInfo#ast_info.custom_tags, Context, BodyTreeWalker) of
                {{CustomTagsAst, CustomTagsInfo}, _} ->
                    Forms = forms(
                              Context#dtl_context.module,
                              {BodyAst, BodyInfo},
                              {CustomTagsAst, CustomTagsInfo},
                              CheckSum,
                              BodyTreeWalker,
                              Context),
                    compile_forms(Forms, Context)
            catch
                throw:Error -> ?ERR(Error, Context)
            end
    catch
        throw:Error -> ?ERR(Error, Context)
    end.

compile_forms(Forms, Context) ->
    maybe_debug_template(Forms, Context),
    Options = Context#dtl_context.compiler_options,
    case compile:forms(Forms, Options) of
        Compiled when element(1, Compiled) =:= ok ->
            [ok, Module, Bin|Info] = tuple_to_list(Compiled),
            lists:foldl(
              fun (F, C) -> F(Module, Bin, C) end,
              Context#dtl_context{ bin=Bin },
              [fun maybe_write/3,
               fun maybe_load/3,
               fun (_, _, C) ->
                       case Info of
                           [Ws] when length(Ws) > 0 ->
                               add_warnings(Ws, C);
                           _ -> C
                       end
               end
              ]);
        error ->
            ?ERR(compile_beam, Context);
        {error, Es, Ws} ->
            add_warnings(Ws, add_errors(Es, Context))
    end.

maybe_write(Module, Bin, Context) ->
    case proplists:get_value(out_dir, Context#dtl_context.all_options) of
        false -> Context;
        undefined ->
            ?WARN(no_out_dir, Context);
        OutDir ->
            BeamFile = filename:join([OutDir, [Module, ".beam"]]),
            print("Template module: ~w -> ~s\n", [Module, BeamFile], Context),
            case file:write_file(BeamFile, Bin) of
                ok -> Context;
                {error, Reason} ->
                    ?ERR({write_file, Reason}, Context)
            end
    end.

maybe_load(Module, Bin, Context) ->
    case proplists:get_bool(no_load, Context#dtl_context.all_options) of
        true -> Context;
        false -> load_code(Module, Bin, Context)
    end.

load_code(Module, Bin, Context) ->
    code:purge(Module),
    case code:load_binary(Module, atom_to_list(Module) ++ ".erl", Bin) of
        {module, Module} -> Context;
        Error -> ?WARN({load, Error}, Context)
    end.

maybe_debug_template(Forms, Context) ->
    %% undocumented option to debug the compiled template
    case proplists:get_bool(debug_info, Context#dtl_context.all_options) of
        false -> nop;
        true ->
            Options = Context#dtl_context.compiler_options,
            print("Compiler options: ~p~n", [Options], Context),
            try
                Source = erl_prettypr:format(erl_syntax:form_list(Forms)),
                File = lists:concat([proplists:get_value(source, Options), ".erl"]),
                io:format("Saving template source to: ~s.. ~p~n",
                          [File, file:write_file(File, Source)])
            catch
                error:Err ->
                    io:format("Pretty printing failed: ~p~n"
                              "Context: ~n~p~n"
                              "Forms: ~n~p~n",
                              [Err, Context, Forms])
            end
    end.

is_up_to_date(CheckSum, Context) ->
    Module = Context#dtl_context.module,
    {M, F} = Context#dtl_context.reader,
    case catch Module:source() of
        {_, CheckSum} ->
            case catch Module:dependencies() of
                L when is_list(L) ->
                    RecompileList = lists:foldl(
                                      fun ({XFile, XCheckSum}, Acc) ->
                                              case catch M:F(XFile) of
                                                  {ok, Data} ->
                                                      case binary_to_list(erlang:md5(Data)) of
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


%%====================================================================
%% AST functions
%%====================================================================

custom_tags_ast(CustomTags, Context, TreeWalker) ->
    %% avoid adding the render_tag/3 fun if it isn't used,
    %% since we can't add a -compile({nowarn_unused_function, render_tag/3}).
    %% attribute due to a bug in syntax_tools.
    case custom_tags_clauses_ast(CustomTags, Context, TreeWalker) of
        skip ->
            {{erl_syntax:comment(
                ["% render_tag/3 is not used in this template."]),
              #ast_info{}},
             TreeWalker};
        {{CustomTagsClauses, CustomTagsInfo}, TreeWalker1} ->
            {{erl_syntax:function(
                erl_syntax:atom(render_tag),
                CustomTagsClauses),
              CustomTagsInfo},
             TreeWalker1}
    end.

custom_tags_clauses_ast([], _Context, _TreeWalker) -> skip;
custom_tags_clauses_ast(CustomTags, Context, TreeWalker) ->
    custom_tags_clauses_ast1(CustomTags, [], [], #ast_info{}, Context, TreeWalker).

custom_tags_clauses_ast1([], _ExcludeTags, ClauseAcc, InfoAcc, Context, TreeWalker) ->
    {{DefaultAst, DefaultInfo}, TreeWalker1} =
        case call_extension(Context, custom_tag_ast, [Context, TreeWalker]) of
            undefined ->
                {{?Q("(_TagName, _, _) -> []"), InfoAcc}, TreeWalker};
            {{ExtAst, ExtInfo}, ExtTreeWalker} ->
                Clause = ?Q("(TagName, _Variables, RenderOptions) -> _@tag",
                            [{tag, options_match_ast(Context, ExtTreeWalker) ++ [ExtAst]}]),
                {{Clause, merge_info(ExtInfo, InfoAcc)}, ExtTreeWalker}
        end,
    {{lists:reverse([DefaultAst|ClauseAcc]), DefaultInfo}, TreeWalker1};
custom_tags_clauses_ast1([Tag|CustomTags], ExcludeTags, ClauseAcc, InfoAcc, Context, TreeWalker) ->
    case lists:member(Tag, ExcludeTags) of
        true ->
            custom_tags_clauses_ast1(CustomTags, ExcludeTags, ClauseAcc, InfoAcc, Context, TreeWalker);
        false ->
            CustomTagFile = full_path(Tag, Context#dtl_context.custom_tags_dir),
            case filelib:is_file(CustomTagFile) of
                true ->
                    case parse_file(CustomTagFile, Context) of
                        {ok, DjangoParseTree, CheckSum} ->
                            {{BodyAst, BodyAstInfo}, TreeWalker1} = with_dependency(
                                                                      {CustomTagFile, CheckSum},
                                                                      body_ast(DjangoParseTree, Context, TreeWalker)),
                            MatchAst = options_match_ast(Context, TreeWalker),
                            Clause = ?Q("(_@Tag@, _Variables, RenderOptions) -> _@MatchAst, _@BodyAst"),
                            custom_tags_clauses_ast1(
                              CustomTags, [Tag|ExcludeTags],
                              [Clause|ClauseAcc], merge_info(BodyAstInfo, InfoAcc),
                              Context, TreeWalker1);
                        {error, Reason} ->
                            throw(Reason)
                    end;
                false ->
                    case call_extension(Context, custom_tag_ast, [Tag, Context, TreeWalker]) of
                        undefined ->
                            custom_tags_clauses_ast1(
                              CustomTags, [Tag | ExcludeTags],
                              ClauseAcc, InfoAcc, Context, TreeWalker);
                        {{Ast, Info}, TW} ->
                            Clause = ?Q("(_@Tag@, _Variables, RenderOptions) -> _@match, _@Ast",
                                        [{match, options_match_ast(Context, TW)}]),
                            custom_tags_clauses_ast1(
                              CustomTags, [Tag | ExcludeTags],
                              [Clause|ClauseAcc], merge_info(Info, InfoAcc),
                              Context, TW)
                    end
            end
    end.

dependencies_function(Dependencies) ->
    ?Q("dependencies() -> _@Dependencies@.").

translatable_strings_function(TranslatableStrings) ->
    ?Q("translatable_strings() -> _@TranslatableStrings@.").

translated_blocks_function(TranslatedBlocks) ->
    ?Q("translated_blocks() -> _@TranslatedBlocks@.").

variables_function(Variables) ->
    ?Q("variables() -> _@vars.",
       [{vars, merl:term(lists:usort(Variables))}]).

custom_forms(Dir, Module, Functions, AstInfo) ->
    Exported = [erl_syntax:arity_qualifier(erl_syntax:atom(source_dir), erl_syntax:integer(0)),
                erl_syntax:arity_qualifier(erl_syntax:atom(dependencies), erl_syntax:integer(0)),
                erl_syntax:arity_qualifier(erl_syntax:atom(translatable_strings), erl_syntax:integer(0))
                | lists:foldl(
                    fun({FunctionName, _, _}, Acc) ->
                            [erl_syntax:arity_qualifier(erl_syntax:atom(FunctionName), erl_syntax:integer(1)),
                             erl_syntax:arity_qualifier(erl_syntax:atom(FunctionName), erl_syntax:integer(2))
                             |Acc]
                    end, [], Functions)
               ],
    ModuleAst = ?Q("-module('@Module@')."),
    ExportAst = ?Q("-export(['@_Exported'/1])"),

    SourceFunctionAst = ?Q("source_dir() -> _@Dir@."),

    DependenciesFunctionAst = dependencies_function(AstInfo#ast_info.dependencies),
    TranslatableStringsFunctionAst = translatable_strings_function(AstInfo#ast_info.translatable_strings),
    FunctionAsts = lists:foldl(fun({_, Function1, Function2}, Acc) -> [Function1, Function2 | Acc] end, [], Functions),

    [erl_syntax:revert(X)
     || X <- [ModuleAst, ExportAst, SourceFunctionAst, DependenciesFunctionAst, TranslatableStringsFunctionAst
              | FunctionAsts] ++ AstInfo#ast_info.pre_render_asts
    ].

stringify(BodyAst, #dtl_context{ binary_strings=BinaryStrings }) ->
    [?Q("erlydtl_runtime:stringify_final(_@BodyAst, '@BinaryStrings@')")].

forms(Module, {BodyAst, BodyInfo}, {CustomTagsFunctionAst, CustomTagsInfo}, CheckSum, TreeWalker,
      #dtl_context{ parse_trail=[File|_] }=Context) ->
    MergedInfo = merge_info(BodyInfo, CustomTagsInfo),

    Render0FunctionAst = ?Q("render() -> render([])."),
    Render1FunctionAst = ?Q("render(Variables) -> render(Variables, [])."),

    Render2FunctionAst = ?Q(["render(Variables, RenderOptions) ->",
                             "  try render_internal(Variables, RenderOptions) of",
                             "    Val -> {ok, Val}",
                             "  catch",
                             "    Err -> {error, Err}",
                             "end."
                            ]),

    SourceFunctionAst = ?Q("source() -> {_@File@, _@CheckSum@}."),

    DependenciesFunctionAst = dependencies_function(MergedInfo#ast_info.dependencies),

    TranslatableStringsAst = translatable_strings_function(MergedInfo#ast_info.translatable_strings),

    TranslatedBlocksAst = translated_blocks_function(MergedInfo#ast_info.translated_blocks),

    VariablesAst = variables_function(MergedInfo#ast_info.var_names),

    MatchAst = options_match_ast(Context, TreeWalker),
    BodyAstTmp = MatchAst ++ stringify(BodyAst, Context),
    RenderInternalFunctionAst = ?Q("render_internal(_Variables, RenderOptions) -> _@BodyAstTmp."),

    ModuleAst  = ?Q("-module('@Module@')."),

    ExportAst = erl_syntax:attribute(
                  erl_syntax:atom(export),
                  [erl_syntax:list(
                     [erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(0)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(1)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(2)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(source), erl_syntax:integer(0)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(dependencies), erl_syntax:integer(0)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(translatable_strings), erl_syntax:integer(0)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(translated_blocks), erl_syntax:integer(0)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(variables), erl_syntax:integer(0))
                     ])
                  ]),

    erl_syntax:revert_forms(
      erl_syntax:form_list(
        [ModuleAst, ExportAst, Render0FunctionAst, Render1FunctionAst, Render2FunctionAst,
         SourceFunctionAst, DependenciesFunctionAst, TranslatableStringsAst,
         TranslatedBlocksAst, VariablesAst, RenderInternalFunctionAst,
         CustomTagsFunctionAst
         |BodyInfo#ast_info.pre_render_asts
        ])).

options_match_ast(Context) -> options_match_ast(Context, undefined).
options_match_ast(Context, TreeWalker) ->
    [
     ?Q("_TranslationFun = proplists:get_value(translation_fun, RenderOptions, none)"),
     ?Q("_CurrentLocale = proplists:get_value(locale, RenderOptions, none)"),
     ?Q("_RecordInfo = _@info", [{info, merl:term(Context#dtl_context.record_info)}])
     | case call_extension(Context, setup_render_ast, [Context, TreeWalker]) of
           undefined -> [];
           Ast when is_list(Ast) -> Ast
       end].

%% child templates should only consist of blocks at the top level
body_ast([{'extends', {string_literal, _Pos, String}} | ThisParseTree], Context, TreeWalker) ->
    File = full_path(unescape_string_literal(String), Context#dtl_context.doc_root),
    case lists:member(File, Context#dtl_context.parse_trail) of
        true ->
            throw(circular_include);
        _ ->
            case parse_file(File, Context) of
                {ok, ParentParseTree, CheckSum} ->
                    BlockDict = lists:foldl(
                                  fun ({block, {identifier, _, Name}, Contents}, Dict) ->
                                          dict:store(Name, Contents, Dict);
                                      (_, Dict) ->
                                          Dict
                                  end, dict:new(), ThisParseTree),
                    with_dependency({File, CheckSum},
                                    body_ast(ParentParseTree,
                                             Context#dtl_context{
                                               block_dict = dict:merge(fun(_Key, _ParentVal, ChildVal) -> ChildVal end,
                                                                       BlockDict, Context#dtl_context.block_dict),
                                               parse_trail = [File | Context#dtl_context.parse_trail]},
                                             TreeWalker));
                {error, Reason} ->
                    throw(Reason)
            end
    end;


body_ast(DjangoParseTree, Context, TreeWalker) ->
    {AstInfoList, TreeWalker2} = lists:mapfoldl(
                                   fun
                                       ({'autoescape', {identifier, _, OnOrOff}, Contents}, TreeWalkerAcc) ->
                                                       body_ast(Contents, Context#dtl_context{auto_escape = OnOrOff},
                                                                TreeWalkerAcc);
                                       ({'block', {identifier, _, Name}, Contents}, TreeWalkerAcc) ->
                                                       Block = case dict:find(Name, Context#dtl_context.block_dict) of
                                                                   {ok, ChildBlock} -> ChildBlock;
                                                                   _ -> Contents
                                                               end,
                                                       body_ast(Block, Context, TreeWalkerAcc);
                                       ({'blocktrans', Args, Contents}, TreeWalkerAcc) ->
                                                       blocktrans_ast(Args, Contents, Context, TreeWalkerAcc);
                                       ({'call', {identifier, _, Name}}, TreeWalkerAcc) ->
                                                       call_ast(Name, TreeWalkerAcc);
                                       ({'call', {identifier, _, Name}, With}, TreeWalkerAcc) ->
                                                       call_with_ast(Name, With, Context, TreeWalkerAcc);
                                       ({'comment', _Contents}, TreeWalkerAcc) ->
                                                       empty_ast(TreeWalkerAcc);
                                       ({'cycle', Names}, TreeWalkerAcc) ->
                                                       cycle_ast(Names, Context, TreeWalkerAcc);
                                       ({'cycle_compat', Names}, TreeWalkerAcc) ->
                                                       cycle_compat_ast(Names, Context, TreeWalkerAcc);
                                       ({'date', 'now', {string_literal, _Pos, FormatString}}, TreeWalkerAcc) ->
                                                       now_ast(FormatString, Context, TreeWalkerAcc);
                                       ({'filter', FilterList, Contents}, TreeWalkerAcc) ->
                                                       filter_tag_ast(FilterList, Contents, Context, TreeWalkerAcc);
                                       ({'firstof', Vars}, TreeWalkerAcc) ->
                                                       firstof_ast(Vars, Context, TreeWalkerAcc);
                                       ({'for', {'in', IteratorList, Variable, Reversed}, Contents}, TreeWalkerAcc) ->
                                                       {EmptyAstInfo, TreeWalker1} = empty_ast(TreeWalkerAcc),
                                                       for_loop_ast(IteratorList, Variable, Reversed, Contents, EmptyAstInfo, Context, TreeWalker1);
                                       ({'for', {'in', IteratorList, Variable, Reversed}, Contents, EmptyPartContents}, TreeWalkerAcc) ->
                                                       {EmptyAstInfo, TreeWalker1} = body_ast(EmptyPartContents, Context, TreeWalkerAcc),
                                                       for_loop_ast(IteratorList, Variable, Reversed, Contents, EmptyAstInfo, Context, TreeWalker1);
                                       ({'if', Expression, Contents, Elif}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                                                       {ElifAstInfo, TreeWalker2} = body_ast(Elif, Context, TreeWalker1),
                                                       ifelse_ast(Expression, IfAstInfo, ElifAstInfo, Context, TreeWalker2);
                                       ({'if', Expression, Contents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                                                       ifelse_ast(Expression, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifchanged', '$undefined', Contents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                                                       ifchanged_contents_ast(Contents, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifchanged', Values, Contents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                                                       ifchanged_values_ast(Values, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifchangedelse', '$undefined', IfContents, ElseContents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context, TreeWalker1),
                                                       ifchanged_contents_ast(IfContents, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifchangedelse', Values, IfContents, ElseContents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context, TreeWalker1),
                                                       ifchanged_values_ast(Values, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifelse', Expression, IfContents, ElseContents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context, TreeWalker1),
                                                       ifelse_ast(Expression, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifequal', [Arg1, Arg2], Contents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                                                       ifelse_ast({'expr', "eq", Arg1, Arg2}, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifequalelse', [Arg1, Arg2], IfContents, ElseContents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context,TreeWalker1),
                                                       ifelse_ast({'expr', "eq", Arg1, Arg2}, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifnotequal', [Arg1, Arg2], Contents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                                                       ifelse_ast({'expr', "ne", Arg1, Arg2}, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifnotequalelse', [Arg1, Arg2], IfContents, ElseContents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context, TreeWalker1),
                                                       ifelse_ast({'expr', "ne", Arg1, Arg2}, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'include', {string_literal, _, File}, Args}, TreeWalkerAcc) ->
                                                       include_ast(unescape_string_literal(File), Args, Context#dtl_context.local_scopes, Context, TreeWalkerAcc);
                                       ({'include_only', {string_literal, _, File}, Args}, TreeWalkerAcc) ->
                                                       include_ast(unescape_string_literal(File), Args, [], Context, TreeWalkerAcc);
                                       ({'regroup', {ListVariable, Grouper, {identifier, _, NewVariable}}, Contents}, TreeWalkerAcc) ->
                                                       regroup_ast(ListVariable, Grouper, NewVariable, Contents, Context, TreeWalkerAcc);
                                       ({'spaceless', Contents}, TreeWalkerAcc) ->
                                                       spaceless_ast(Contents, Context, TreeWalkerAcc);
                                       ({'ssi', Arg}, TreeWalkerAcc) ->
                                                       ssi_ast(Arg, Context, TreeWalkerAcc);
                                       ({'ssi_parsed', {string_literal, _, FileName}}, TreeWalkerAcc) ->
                                                       include_ast(unescape_string_literal(FileName), [], Context#dtl_context.local_scopes, Context, TreeWalkerAcc);
                                       ({'string', _Pos, String}, TreeWalkerAcc) ->
                                                       string_ast(String, Context, TreeWalkerAcc);
                                       ({'tag', {identifier, _, Name}, Args}, TreeWalkerAcc) ->
                                                       tag_ast(Name, Args, Context, TreeWalkerAcc);
                                       ({'templatetag', {_, _, TagName}}, TreeWalkerAcc) ->
                                                       templatetag_ast(TagName, Context, TreeWalkerAcc);
                                       ({'trans', Value}, TreeWalkerAcc) ->
                                                       translated_ast(Value, Context, TreeWalkerAcc);
                                       ({'widthratio', Numerator, Denominator, Scale}, TreeWalkerAcc) ->
                                                       widthratio_ast(Numerator, Denominator, Scale, Context, TreeWalkerAcc);
                                       ({'with', Args, Contents}, TreeWalkerAcc) ->
                                                       with_ast(Args, Contents, Context, TreeWalkerAcc);
                                       ({'extension', Tag}, TreeWalkerAcc) ->
                                                       extension_ast(Tag, Context, TreeWalkerAcc);
                                       ({'extends', _}, _TreeWalkerAcc) ->
                                                       throw(unexpected_extends_tag);
                                       (ValueToken, TreeWalkerAcc) ->
                                                       {{ValueAst,ValueInfo},ValueTreeWalker} = value_ast(ValueToken, true, true, Context, TreeWalkerAcc),
                                                       {{format(ValueAst, Context, ValueTreeWalker),ValueInfo},ValueTreeWalker}
                                               end, TreeWalker, DjangoParseTree),
    {AstList, {Info, TreeWalker3}} = lists:mapfoldl(
                                       fun({Ast, Info}, {InfoAcc, TreeWalkerAcc}) ->
                                               PresetVars = lists:foldl(fun
                                                                            (X, Acc) ->
                                                                               case proplists:lookup(X, Context#dtl_context.vars) of
                                                                                   none -> Acc;
                                                                                   Val -> [Val|Acc]
                                                                               end
                                                                       end, [], Info#ast_info.var_names),
                                               case PresetVars of
                                                   [] ->
                                                       {Ast, {merge_info(Info, InfoAcc), TreeWalkerAcc}};
                                                   _ ->
                                                       Counter = TreeWalkerAcc#treewalker.counter,
                                                       Name = list_to_atom(lists:concat([pre_render, Counter])),
                                                       Ast1 = ?Q("'@Name@'(_@PresetVars@, RenderOptions)"),
                                                       PreRenderAst = ?Q("'@Name@'(_Variables, RenderOptions) -> _@match, _@Ast.",
                                                                         [{match, options_match_ast(Context, TreeWalkerAcc)}]),
                                                       PreRenderAsts = Info#ast_info.pre_render_asts,
                                                       Info1 = Info#ast_info{pre_render_asts = [PreRenderAst | PreRenderAsts]},
                                                       {Ast1, {merge_info(Info1, InfoAcc), TreeWalkerAcc#treewalker{counter = Counter + 1}}}
                                               end
                                       end, {#ast_info{}, TreeWalker2}, AstInfoList),
    {{erl_syntax:list(AstList), Info}, TreeWalker3}.


value_ast(ValueToken, AsString, EmptyIfUndefined, Context, TreeWalker) ->
    case ValueToken of
        {'expr', Operator, Value} ->
            {{ValueAst,InfoValue}, TreeWalker1} = value_ast(Value, false, EmptyIfUndefined, Context, TreeWalker),
            Op = list_to_atom(Operator),
            Ast = ?Q("erlydtl_runtime:_@Op@(_@ValueAst)"),
            {{Ast, InfoValue}, TreeWalker1};
        {'expr', Operator, Value1, Value2} ->
            {{Value1Ast,InfoValue1}, TreeWalker1} = value_ast(Value1, false, EmptyIfUndefined, Context, TreeWalker),
            {{Value2Ast,InfoValue2}, TreeWalker2} = value_ast(Value2, false, EmptyIfUndefined, Context, TreeWalker1),
            Op = list_to_atom(Operator),
            Ast = ?Q("erlydtl_runtime:_@Op@(_@Value1Ast, _@Value2Ast)"),
            {{Ast, merge_info(InfoValue1,InfoValue2)}, TreeWalker2};
        {'string_literal', _Pos, String} ->
            string_ast(unescape_string_literal(String), Context, TreeWalker);
        {'number_literal', _Pos, Number} ->
            case AsString of
                true  -> string_ast(Number, Context, TreeWalker);
                false -> {{erl_syntax:integer(Number), #ast_info{}}, TreeWalker}
            end;
        {'apply_filter', Variable, Filter} ->
            filter_ast(Variable, Filter, Context, TreeWalker);
        {'attribute', _} = Variable ->
            resolve_variable_ast(Variable, Context, TreeWalker, EmptyIfUndefined);
        {'variable', _} = Variable ->
            resolve_variable_ast(Variable, Context, TreeWalker, EmptyIfUndefined);
        {extension, Tag} ->
            extension_ast(Tag, Context, TreeWalker)
    end.

extension_ast(Tag, Context, TreeWalker) ->
    case call_extension(Context, compile_ast, [Tag, Context, TreeWalker]) of
        undefined ->
            throw({unknown_extension, Tag});
        Result ->
            Result
    end.


with_dependencies([], Args) ->
    Args;
with_dependencies([Dependency | Rest], Args) ->
    with_dependencies(Rest, with_dependency(Dependency, Args)).

with_dependency(FilePath, {{Ast, Info}, TreeWalker}) ->
    {{Ast, Info#ast_info{dependencies = [FilePath | Info#ast_info.dependencies]}}, TreeWalker}.


empty_ast(TreeWalker) ->
    {{erl_syntax:list([]), #ast_info{}}, TreeWalker}.

blocktrans_ast(ArgList, Contents, Context, TreeWalker) ->
    %% add new scope using 'with' values
    {NewScope, {ArgInfo, TreeWalker1}} = lists:mapfoldl(fun
                                                            ({{identifier, _, LocalVarName}, Value}, {AstInfo1, TreeWalker1}) ->
                                                               {{Ast, Info}, TreeWalker2} = value_ast(Value, false, false, Context, TreeWalker1),
                                                               {{LocalVarName, Ast}, {merge_info(AstInfo1, Info), TreeWalker2}}
                                                       end, {#ast_info{}, TreeWalker}, ArgList),
    NewContext = Context#dtl_context{ local_scopes = [NewScope|Context#dtl_context.local_scopes] },
    %% key for translation lookup
    SourceText = lists:flatten(erlydtl_unparser:unparse(Contents)),
    {{DefaultAst, AstInfo}, TreeWalker2} = body_ast(Contents, NewContext, TreeWalker1),
    MergedInfo = merge_info(AstInfo, ArgInfo),
    case Context#dtl_context.trans_fun of
        none ->
            %% translate in runtime
            blocktrans_runtime_ast({DefaultAst, MergedInfo}, TreeWalker2, SourceText, Contents, NewContext);
        BlockTransFun when is_function(BlockTransFun) ->
            %% translate in compile-time
            {FinalAstInfo, FinalTreeWalker, Clauses} = 
                lists:foldr(
                  fun(Locale, {AstInfoAcc, ThisTreeWalker, ClauseAcc}) ->
                          case BlockTransFun(SourceText, Locale) of
                              default ->
                                  {AstInfoAcc, ThisTreeWalker, ClauseAcc};
                              Body ->
                                  {ok, DjangoParseTree} = do_parse_template(Body, Context),
                                  {{ThisAst, ThisAstInfo}, TreeWalker3} = body_ast(DjangoParseTree, NewContext, ThisTreeWalker),
                                  {merge_info(ThisAstInfo, AstInfoAcc), TreeWalker3,
                                   [?Q("_@Locale@ -> _@ThisAst")|ClauseAcc]}
                          end
                  end, {MergedInfo, TreeWalker2, []}, Context#dtl_context.trans_locales),
            Ast = ?Q("case _CurrentLocale of _@_Clauses -> _; _ -> _@DefaultAst end"),
            {{Ast, FinalAstInfo#ast_info{ translated_blocks = [SourceText] }}, FinalTreeWalker}
    end.

blocktrans_runtime_ast({DefaultAst, Info}, Walker, SourceText, Contents, Context) ->
    %% Contents is flat - only strings and '{{var}}' allowed.
    %% build sorted list (orddict) of pre-resolved variables to pass to runtime translation function
    USortedVariables = lists:usort(fun({variable, {identifier, _, A}},
                                       {variable, {identifier, _, B}}) ->
                                           A =< B
                                   end, [Var || {variable, _}=Var <- Contents]),
    VarBuilder = fun({variable, {identifier, _, Name}}=Var, Walker1) ->
                         {{Ast2, _InfoIgn}, Walker2}  = resolve_variable_ast(Var, Context, Walker1, false),
                         {?Q("{_@name, _@Ast2}", [{name, merl:term(atom_to_list(Name))}]),
                          Walker2}
                 end,
    {VarAsts, Walker2} = lists:mapfoldl(VarBuilder, Walker, USortedVariables),
    VarListAst = erl_syntax:list(VarAsts),
    BlockTransAst = ?Q(["if _TranslationFun =:= none -> _@DefaultAst;",
                        "  true -> erlydtl_runtime:translate_block(",
                        "    _@SourceText@, _TranslationFun, _@VarListAst)",
                        "end"]),
    {{BlockTransAst, Info}, Walker2}.

translated_ast({string_literal, _, String}, Context, TreeWalker) ->
    UnescapedStr = unescape_string_literal(String),
    case call_extension(Context, translate_ast, [UnescapedStr, Context, TreeWalker]) of
        undefined ->
            AstInfo = #ast_info{translatable_strings = [UnescapedStr]},
            case Context#dtl_context.trans_fun of
                none -> runtime_trans_ast(erl_syntax:string(UnescapedStr), AstInfo, TreeWalker);
                _ -> compiletime_trans_ast(UnescapedStr, AstInfo, Context, TreeWalker)
            end;
        Translated ->
            Translated
    end;
translated_ast(ValueToken, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = value_ast(ValueToken, true, false, Context, TreeWalker),
    runtime_trans_ast(Ast, Info, TreeWalker1).

runtime_trans_ast(ValueAst, AstInfo, TreeWalker) ->
    {{?Q("erlydtl_runtime:translate(_@ValueAst, _TranslationFun)"),
      AstInfo},
     TreeWalker}.

compiletime_trans_ast(String, AstInfo,
                      #dtl_context{trans_fun=TFun,
                                   trans_locales=TLocales}=Context,
                      TreeWalker) ->
    ClAst = lists:foldl(
              fun(Locale, ClausesAcc) ->
                      [?Q("_@Locale@ -> _@translated",
                          [{translated, case TFun(String, Locale) of
                                            default -> string_ast(String, Context);
                                            Translated -> string_ast(Translated, Context)
                                        end}])
                       |ClausesAcc]
              end,
              [], TLocales),
    CaseAst = ?Q(["case _CurrentLocale of",
                  "  _@_ClAst -> _;",
                  " _ -> _@string",
                  "end"],
                 [{string, string_ast(String, Context)}]),
    {{CaseAst, AstInfo}, TreeWalker}.

%% Completely unnecessary in ErlyDTL (use {{ "{%" }} etc), but implemented for compatibility.
templatetag_ast("openblock", Context, TreeWalker) ->
    string_ast("{%", Context, TreeWalker);
templatetag_ast("closeblock", Context, TreeWalker) ->
    string_ast("%}", Context, TreeWalker);
templatetag_ast("openvariable", Context, TreeWalker) ->
    string_ast("{{", Context, TreeWalker);
templatetag_ast("closevariable", Context, TreeWalker) ->
    string_ast("}}", Context, TreeWalker);
templatetag_ast("openbrace", Context, TreeWalker) ->
    string_ast("{", Context, TreeWalker);
templatetag_ast("closebrace", Context, TreeWalker) ->
    string_ast("}", Context, TreeWalker);
templatetag_ast("opencomment", Context, TreeWalker) ->
    string_ast("{#", Context, TreeWalker);
templatetag_ast("closecomment", Context, TreeWalker) ->
    string_ast("#}", Context, TreeWalker).


widthratio_ast(Numerator, Denominator, Scale, Context, TreeWalker) ->
    {{NumAst, NumInfo}, TreeWalker1} = value_ast(Numerator, false, true, Context, TreeWalker),
    {{DenAst, DenInfo}, TreeWalker2} = value_ast(Denominator, false, true, Context, TreeWalker1),
    {{ScaleAst, ScaleInfo}, TreeWalker3} = value_ast(Scale, false, true, Context, TreeWalker2),
    {{format_number_ast(?Q("erlydtl_runtime:widthratio(_@NumAst, _@DenAst, _@ScaleAst)")),
      merge_info(ScaleInfo, merge_info(NumInfo, DenInfo))},
     TreeWalker3}.


string_ast(Arg, Context) ->
    merl:term(erlydtl_compiler_utils:to_string(Arg, Context)).
    
string_ast(Arg, Context, TreeWalker) ->
    {{string_ast(Arg, Context), #ast_info{}}, TreeWalker}.


include_ast(File, ArgList, Scopes, Context, TreeWalker) ->
    FilePath = full_path(File, Context#dtl_context.doc_root),
    case parse_file(FilePath, Context) of
        {ok, InclusionParseTree, CheckSum} ->
            {NewScope, {ArgInfo, TreeWalker1}}
                = lists:mapfoldl(
                    fun ({{identifier, _, LocalVarName}, Value}, {AstInfo1, TreeWalker1}) ->
                            {{Ast, Info}, TreeWalker2} = value_ast(Value, false, false, Context, TreeWalker1),
                            {{LocalVarName, Ast}, {merge_info(AstInfo1, Info), TreeWalker2}}
                    end, {#ast_info{}, TreeWalker}, ArgList),

            {{BodyAst, BodyInfo}, TreeWalker2} = with_dependency(
                                                   {FilePath, CheckSum},
                                                   body_ast(
                                                     InclusionParseTree,
                                                     Context#dtl_context{
                                                       parse_trail = [FilePath | Context#dtl_context.parse_trail],
                                                       local_scopes = [NewScope|Scopes]
                                                      }, TreeWalker1)),

            {{BodyAst, merge_info(BodyInfo, ArgInfo)}, TreeWalker2};
        {error, Reason} -> throw(Reason)
    end.

%% include at run-time
ssi_ast(FileName, Context, TreeWalker) ->
    {{FileAst, Info}, TreeWalker1} = value_ast(FileName, true, true, Context, TreeWalker),
    {Mod, Fun} = Context#dtl_context.reader,
    Dir = Context#dtl_context.doc_root,
    {{?Q("erlydtl_runtime:read_file(_@Mod@, _@Fun@, _@Dir@, _@FileAst)"), Info}, TreeWalker1}.

filter_tag_ast(FilterList, Contents, Context, TreeWalker) ->
    {{InnerAst, Info}, TreeWalker1} = body_ast(Contents, Context#dtl_context{auto_escape = did}, TreeWalker),
    {{FilteredAst, FilteredInfo}, TreeWalker2} =
        lists:foldl(
          fun ({{identifier, _, Name}, []}, {{AstAcc, InfoAcc}, TreeWalkerAcc})
                when Name =:= 'escape'; Name =:= 'safe'; Name =:= 'safeseq' ->
                  {{AstAcc, InfoAcc}, TreeWalkerAcc#treewalker{safe = true}};
              (Filter, {{AstAcc, InfoAcc}, TreeWalkerAcc}) ->
                  {{Ast, AstInfo}, TW} = filter_ast1(Filter, AstAcc, Context, TreeWalkerAcc),
                  {{Ast, merge_info(InfoAcc, AstInfo)}, TW}
          end,
          {{?Q("erlang:iolist_to_binary(_@InnerAst)"), Info}, TreeWalker1},
          FilterList),

    EscapedAst = case search_for_escape_filter(lists:reverse(FilterList), Context) of
                     on -> ?Q("erlydtl_filters:force_escape(_@FilteredAst)");
                     _ -> FilteredAst
                 end,
    {{EscapedAst, FilteredInfo}, TreeWalker2}.

search_for_escape_filter(FilterList, #dtl_context{auto_escape = on}) ->
    search_for_safe_filter(FilterList);
search_for_escape_filter(_, #dtl_context{auto_escape = did}) -> off;
search_for_escape_filter([{{identifier, _, 'escape'}, []}|Rest], _Context) ->
    search_for_safe_filter(Rest);
search_for_escape_filter([_|Rest], Context) ->
    search_for_escape_filter(Rest, Context);
search_for_escape_filter([], _Context) -> off.

search_for_safe_filter([{{identifier, _, Name}, []}|_])
  when Name =:= 'safe'; Name =:= 'safeseq' -> off;
search_for_safe_filter([_|Rest]) -> search_for_safe_filter(Rest);
search_for_safe_filter([]) -> on.

filter_ast(Variable, Filter, Context, TreeWalker) ->
    %% the escape filter is special; it is always applied last, so we have to go digging for it

    %% AutoEscape = 'did' means we (will have) decided whether to escape the current variable,
    %% so don't do any more escaping
    {{UnescapedAst, Info}, TreeWalker2} = filter_ast_noescape(
                                            Variable, Filter,
                                            Context#dtl_context{auto_escape = did},
                                            TreeWalker),

    EscapedAst = case search_for_escape_filter(Variable, Filter, Context) of
                     on -> ?Q("erlydtl_filters:force_escape(_@UnescapedAst)");
                     _ -> UnescapedAst
                 end,
    {{EscapedAst, Info}, TreeWalker2}.

filter_ast_noescape(Variable, {{identifier, _, Name}, []}, Context, TreeWalker)
  when Name =:= 'escape'; Name =:= 'safe'; Name =:= 'safeseq' ->
    value_ast(Variable, true, false, Context, TreeWalker#treewalker{safe = true});
filter_ast_noescape(Variable, Filter, Context, TreeWalker) ->
    {{ValueAst, Info1}, TreeWalker2} = value_ast(Variable, true, false, Context, TreeWalker),
    {{VarValue, Info2}, TreeWalker3} = filter_ast1(Filter, ValueAst, Context, TreeWalker2),
    {{VarValue, merge_info(Info1, Info2)}, TreeWalker3}.

filter_ast1({{identifier, _, Name}, Args}, ValueAst, Context, TreeWalker) ->
    {{ArgsAst, ArgsInfo}, TreeWalker2} =
        lists:foldr(
          fun (Arg, {{AccAst, AccInfo}, AccTreeWalker}) ->
                  {{ArgAst, ArgInfo}, ArgTreeWalker} = value_ast(Arg, false, false, Context, AccTreeWalker),
                  {{[ArgAst|AccAst], merge_info(ArgInfo, AccInfo)}, ArgTreeWalker}
          end,
          {{[], #ast_info{}}, TreeWalker},
          Args),
    FilterAst = filter_ast2(Name, [ValueAst|ArgsAst], Context),
    {{FilterAst, ArgsInfo}, TreeWalker2}.

filter_ast2(Name, Args, #dtl_context{ filter_modules = [Module|Rest] } = Context) ->
    case lists:member({Name, length(Args)}, Module:module_info(exports)) of
        true -> ?Q("'@Module@':'@Name@'(_@Args)");
        false ->
            filter_ast2(Name, Args, Context#dtl_context{ filter_modules = Rest })
    end;
filter_ast2(Name, Args, _) ->
    throw({unknown_filter, Name, length(Args)}).

search_for_escape_filter(Variable, Filter, #dtl_context{auto_escape = on}) ->
    search_for_safe_filter(Variable, Filter);
search_for_escape_filter(_, _, #dtl_context{auto_escape = did}) ->
    off;
search_for_escape_filter(Variable, {{identifier, _, 'escape'}, []} = Filter, _Context) ->
    search_for_safe_filter(Variable, Filter);
search_for_escape_filter({apply_filter, Variable, Filter}, _, Context) ->
    search_for_escape_filter(Variable, Filter, Context);
search_for_escape_filter(_Variable, _Filter, _Context) ->
    off.

search_for_safe_filter(_, {{identifier, _, 'safe'}, []}) ->
    off;
search_for_safe_filter(_, {{identifier, _, 'safeseq'}, []}) ->
    off;
search_for_safe_filter({apply_filter, Variable, Filter}, _) ->
    search_for_safe_filter(Variable, Filter);
search_for_safe_filter(_Variable, _Filter) ->
    on.

finder_function(true) -> {erlydtl_runtime, fetch_value};
finder_function(false) -> {erlydtl_runtime, find_value}.

finder_function(EmptyIfUndefined, Context) ->
    case call_extension(Context, finder_function, [EmptyIfUndefined]) of
        undefined -> finder_function(EmptyIfUndefined);
        Result -> Result
    end.

resolve_variable_ast({extension, Tag}, Context, TreeWalker, _) ->
    extension_ast(Tag, Context, TreeWalker);
resolve_variable_ast(VarTuple, Context, TreeWalker, EmptyIfUndefined)
  when is_boolean(EmptyIfUndefined) ->
    resolve_variable_ast(VarTuple, Context, TreeWalker, finder_function(EmptyIfUndefined, Context));
resolve_variable_ast(VarTuple, Context, TreeWalker, FinderFunction) ->
    resolve_variable_ast1(VarTuple, Context, TreeWalker, FinderFunction).

resolve_variable_ast1({attribute, {{_, Pos, Attr}, Variable}}, Context, TreeWalker, FinderFunction) ->
    {{VarAst, VarInfo}, TreeWalker1} = resolve_variable_ast(Variable, Context, TreeWalker, FinderFunction),
    FileName = case Context#dtl_context.parse_trail of
                   [] -> undefined;
                   [H|_] -> H
               end,
    {Runtime, Finder} = FinderFunction,
    {{?Q(["'@Runtime@':'@Finder@'(",
          "  _@Attr@, _@VarAst,",
          "  [{filename, _@FileName@},",
          "   {pos, _@Pos@},",
          "   {record_info, _RecordInfo},",
          "   {render_options, RenderOptions}])"]),
      VarInfo},
     TreeWalker1};

resolve_variable_ast1({variable, {identifier, Pos, VarName}}, Context, TreeWalker, FinderFunction) ->
    VarValue = case resolve_scoped_variable_ast(VarName, Context) of
                   undefined ->
                       FileName = case Context#dtl_context.parse_trail of
                                      [] -> undefined;
                                      [H|_] -> H
                                  end,
                       {Runtime, Finder} = FinderFunction,
                       ?Q(["'@Runtime@':'@Finder@'(",
                           "  _@VarName@, _Variables,",
                           "  [{filename, _@FileName@},",
                           "   {pos, _@Pos@},",
                           "   {record_info, _RecordInfo},",
                           "   {render_options, RenderOptions}])"]);
                   Val ->
                       Val
               end,
    {{VarValue, #ast_info{ var_names=[VarName] }}, TreeWalker}.

resolve_scoped_variable_ast(VarName, Context) ->
    resolve_scoped_variable_ast(VarName, Context, undefined).

resolve_scoped_variable_ast(VarName, Context, Default) ->
    lists:foldl(
      fun (Scope, Res) ->
              if Res =:= Default ->
                      proplists:get_value(VarName, Scope, Default);
                 true -> Res
              end
      end,
      Default,
      Context#dtl_context.local_scopes).

format(Ast, Context, TreeWalker) ->
    auto_escape(format_number_ast(Ast), Context, TreeWalker).

format_number_ast(Ast) ->
    ?Q("erlydtl_filters:format_number(_@Ast)").


auto_escape(Value, _, #treewalker{safe = true}) ->
    Value;
auto_escape(Value, #dtl_context{auto_escape = on}, _) ->
    ?Q("erlydtl_filters:force_escape(_@Value)");
auto_escape(Value, _, _) ->
    Value.

firstof_ast(Vars, Context, TreeWalker) ->
    body_ast([lists:foldr(fun
                              ({L, _, _}=Var, []) when L=:=string_literal;L=:=number_literal ->
                                 Var;
                              ({L, _, _}, _) when L=:=string_literal;L=:=number_literal ->
                                 erlang:error(errbadliteral);
                              (Var, []) ->
                                 {'ifelse', Var, [Var], []};
                              (Var, Acc) ->
                                 {'ifelse', Var, [Var], [Acc]} end,
                          [], Vars)], Context, TreeWalker).

ifelse_ast(Expression, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    {{Ast, ExpressionInfo}, TreeWalker1} = value_ast(Expression, false, false, Context, TreeWalker),
    {{?Q(["case erlydtl_runtime:is_true(_@Ast) of",
          "  true -> _@IfContentsAst;",
          "  _ -> _@ElseContentsAst",
          "end"]),
      merge_info(ExpressionInfo, Info)},
     TreeWalker1}.

with_ast(ArgList, Contents, Context, TreeWalker) ->
    {ArgAstList, {ArgInfo, TreeWalker1}} =
        lists:mapfoldl(
          fun ({{identifier, _, _LocalVarName}, Value}, {AstInfo1, TreeWalker1}) ->
                  {{Ast, Info}, TreeWalker2} = value_ast(Value, false, false, Context, TreeWalker1),
                  {Ast, {merge_info(AstInfo1, Info), TreeWalker2}}
          end, {#ast_info{}, TreeWalker}, ArgList),

    NewScope = lists:map(
                 fun({{identifier, _, LocalVarName}, _Value}) ->
                         {LocalVarName, erl_syntax:variable(lists:concat(["Var_", LocalVarName]))}
                 end, ArgList),

    {{InnerAst, InnerInfo}, TreeWalker2} =
        body_ast(
          Contents,
          Context#dtl_context{local_scopes = [NewScope|Context#dtl_context.local_scopes]},
          TreeWalker1),

    {{?Q("fun (_@args) -> _@InnerAst end (_@ArgAstList)",
         [{args, element(2, lists:unzip(NewScope))}]),
      merge_info(ArgInfo, InnerInfo)},
     TreeWalker2}.

regroup_ast(ListVariable, GrouperVariable, LocalVarName, Contents, Context, TreeWalker) ->
    {{ListAst, ListInfo}, TreeWalker1} = value_ast(ListVariable, false, true, Context, TreeWalker),
    LocalVarAst = erl_syntax:variable(lists:concat(["Var_", LocalVarName])),
    NewScope = [{LocalVarName, LocalVarAst}],

    {{InnerAst, InnerInfo}, TreeWalker2} = body_ast(Contents,
                                                    Context#dtl_context{
                                                      local_scopes = [NewScope|Context#dtl_context.local_scopes]
                                                     },
                                                    TreeWalker1),

    {{?Q(["fun (_@LocalVarAst) -> _@InnerAst end",
         "(erlydtl_runtime:regroup(_@ListAst, _@regroup))"],
        [{regroup, regroup_filter(GrouperVariable, [])}]),
      merge_info(ListInfo, InnerInfo)},
     TreeWalker2}.

regroup_filter({attribute,{{identifier,_,Ident},Next}},Acc) ->
    regroup_filter(Next,[erl_syntax:atom(Ident)|Acc]);
regroup_filter({variable,{identifier,_,Var}},Acc) ->
    erl_syntax:list([erl_syntax:atom(Var)|Acc]).

to_list_ast(Value, IsReversed) ->
    ?Q("erlydtl_runtime:to_list(_@Value, _@IsReversed)").

to_list_ast(Value, IsReversed, Context, TreeWalker) ->
    case call_extension(Context, to_list_ast, [Value, IsReversed, Context, TreeWalker]) of
        undefined -> to_list_ast(Value, IsReversed);
        Result -> Result
    end.

for_loop_ast(IteratorList, LoopValue, IsReversed, Contents, {EmptyContentsAst, EmptyContentsInfo}, Context, TreeWalker) ->
    %% create unique namespace for this instance
    Level = length(Context#dtl_context.local_scopes),
    {Row, Col} = element(2, hd(IteratorList)),
    ForId = lists:concat(["/", Level, "_", Row, ":", Col]),

    Counters = merl:var(lists:concat(["Counters", ForId])),
    Vars = merl:var(lists:concat(["Vars", ForId])),

    %% setup
    VarScope = lists:map(
                 fun({identifier, {R, C}, Iterator}) ->
                         {Iterator, erl_syntax:variable(
                                      lists:concat(["Var_", Iterator,
                                                    "/", Level, "_", R, ":", C
                                                   ]))}
                 end, IteratorList),
    {Iterators, IteratorVars} = lists:unzip(VarScope),
    IteratorCount = length(IteratorVars),

    {{LoopBodyAst, Info}, TreeWalker1} =
        body_ast(
          Contents,
          Context#dtl_context{
            local_scopes =
                [[{'forloop', Counters} | VarScope]
                 | Context#dtl_context.local_scopes]
           },
          TreeWalker),

    {{LoopValueAst, LoopValueInfo}, TreeWalker2} = value_ast(LoopValue, false, true, Context, TreeWalker1),

    LoopValueAst0 = to_list_ast(LoopValueAst, erl_syntax:atom(IsReversed), Context, TreeWalker2),

    ParentLoop = resolve_scoped_variable_ast('forloop', Context, erl_syntax:atom(undefined)),

    %% call for loop
    {{?Q(["case erlydtl_runtime:forloop(",
          "  fun (_@Vars, _@Counters) ->",
          "    {_@IteratorVars} = if is_tuple(_@Vars), size(_@Vars) == _@IteratorCount@ -> _@Vars;",
          "                          _@___ifclauses -> _",
          "                       end,",
          "    {_@LoopBodyAst, erlydtl_runtime:increment_counter_stats(_@Counters)}",
          "  end,",
          "  _@LoopValueAst0, _@ParentLoop)",
          "of",
          "  empty -> _@EmptyContentsAst;",
          "  {L, _} -> L",
          "end"],
         [{ifclauses, if IteratorCount > 1 ->
                              ?Q(["() when is_list(_@Vars), length(_@Vars) == _@IteratorCount@ ->",
                                  "  list_to_tuple(_@Vars);",
                                  "() when true -> throw({for_loop, _@Iterators@, _@Vars})"]);
                         true ->
                              ?Q("() when true -> {_@Vars}")
                      end}]),
      merge_info(merge_info(Info, EmptyContentsInfo), LoopValueInfo)},
     TreeWalker2}.

ifchanged_values_ast(Values, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    ValueAstFun = fun(Expr, {LTreeWalker, LInfo, Acc}) ->
                          {{EAst, EInfo}, ETw} = value_ast(Expr, false, true, Context, LTreeWalker),
                          {ETw, merge_info(LInfo, EInfo),
                           [?Q("{_@hash, _@EAst}",
                               [{hash, merl:term(erlang:phash2(Expr))}])
                            |Acc]}
                  end,
    {TreeWalker1, MergedInfo, Changed} = lists:foldl(ValueAstFun, {TreeWalker, Info, []}, Values),
    {{?Q(["case erlydtl_runtime:ifchanged([_@Changed]) of",
          "  true -> _@IfContentsAst;",
          "  _ -> _@ElseContentsAst",
          "end"]),
      MergedInfo},
     TreeWalker1}.

ifchanged_contents_ast(Contents, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, _Context, TreeWalker) ->
    {{?Q(["case erlydtl_runtime:ifchanged([{_@hash, _@IfContentsAst}]) of",
          "  true -> _@IfContentsAst;",
          "  _ -> _@ElseContentsAst",
          "end"],
         [{hash, merl:term(erlang:phash2(Contents))}]),
      merge_info(IfContentsInfo, ElseContentsInfo)},
     TreeWalker}.

cycle_ast(Names, Context, TreeWalker) ->
    {NamesTuple, VarNames}
        = lists:mapfoldl(
            fun ({string_literal, _, Str}, VarNamesAcc) ->
                    S = string_ast(unescape_string_literal(Str), Context),
                    {S, VarNamesAcc};
                ({variable, _}=Var, VarNamesAcc) ->
                    {{V, #ast_info{ var_names=[VarName] }}, _} = resolve_variable_ast(Var, Context, TreeWalker, true),
                    {V, [VarName|VarNamesAcc]};
                ({number_literal, _, Num}, VarNamesAcc) ->
                    {format(erl_syntax:integer(Num), Context, TreeWalker), VarNamesAcc};
                (_, VarNamesAcc) ->
                    {[], VarNamesAcc}
            end, [], Names),
    {{?Q("erlydtl_runtime:cycle({_@NamesTuple}, _@forloop)",
        [{forloop, resolve_scoped_variable_ast('forloop', Context)}]),
      #ast_info{ var_names = VarNames }},
     TreeWalker}.

%% Older Django templates treat cycle with comma-delimited elements as strings
cycle_compat_ast(Names, Context, TreeWalker) ->
    NamesTuple = lists:map(
                   fun ({identifier, _, X}) ->
                           string_ast(X, Context)
                   end, Names),
    {{?Q("erlydtl_runtime:cycle({_@NamesTuple}, _@forloop)",
        [{forloop, resolve_scoped_variable_ast('forloop', Context)}]),
      #ast_info{}},
     TreeWalker}.

now_ast(FormatString, Context, TreeWalker) ->
    %% Note: we can't use unescape_string_literal here
    %% because we want to allow escaping in the format string.
    %% We only want to remove the surrounding escapes,
    %% i.e. \"foo\" becomes "foo"
    UnescapeOuter = string:strip(FormatString, both, 34),
    {{StringAst, Info}, TreeWalker1} = string_ast(UnescapeOuter, Context, TreeWalker),
    {{?Q("erlydtl_dateformat:format(_@StringAst)"), Info}, TreeWalker1}.

spaceless_ast(Contents, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = body_ast(Contents, Context, TreeWalker),
    {{?Q("erlydtl_runtime:spaceless(_@Ast)"), Info}, TreeWalker1}.


%%-------------------------------------------------------------------
%% Custom tags
%%-------------------------------------------------------------------

interpret_value({trans, StringLiteral}, Context, TreeWalker) ->
    translated_ast(StringLiteral, Context, TreeWalker);
interpret_value(Value, Context, TreeWalker) ->
    value_ast(Value, false, false, Context, TreeWalker).

interpret_args(Args, Context, TreeWalker) ->
    lists:foldr(
      fun ({{identifier, _, Key}, Value}, {{ArgsAcc, AstInfoAcc}, TreeWalkerAcc}) ->
              {{Ast0, AstInfo0}, TreeWalker0} = interpret_value(Value, Context, TreeWalkerAcc),
              {{[?Q("{_@Key@, _@Ast0}")|ArgsAcc], merge_info(AstInfo0, AstInfoAcc)}, TreeWalker0};
          (Value, {{ArgsAcc, AstInfoAcc}, TreeWalkerAcc}) ->
              {{Ast0, AstInfo0}, TreeWalker0} = value_ast(Value, false, false, Context, TreeWalkerAcc),
              {{[Ast0|ArgsAcc], merge_info(AstInfo0, AstInfoAcc)}, TreeWalker0}
      end, {{[], #ast_info{}}, TreeWalker}, Args).

tag_ast(Name, Args, Context, TreeWalker) ->
    {{InterpretedArgs, AstInfo1}, TreeWalker1} = interpret_args(Args, Context, TreeWalker),
    {RenderAst, RenderInfo} = custom_tags_modules_ast(Name, InterpretedArgs, Context),
    {{RenderAst, merge_info(AstInfo1, RenderInfo)}, TreeWalker1}.

custom_tags_modules_ast(Name, InterpretedArgs, #dtl_context{ custom_tags_modules = [], is_compiling_dir = false }) ->
    {?Q("render_tag(_@Name@, [_@InterpretedArgs], RenderOptions)"),
     #ast_info{custom_tags = [Name]}};
custom_tags_modules_ast(Name, InterpretedArgs, #dtl_context{ custom_tags_modules = [], is_compiling_dir = true, module = Module }) ->
    {?Q("'@Module@':'@Name@'([_@InterpretedArgs], RenderOptions)"),
     #ast_info{ custom_tags = [Name] }};
custom_tags_modules_ast(Name, InterpretedArgs, #dtl_context{ custom_tags_modules = [Module|Rest] } = Context) ->
    try lists:max([I || {N,I} <- Module:module_info(exports), N =:= Name]) of
        2 ->
            {?Q("'@Module@':'@Name@'([_@InterpretedArgs], RenderOptions)"), #ast_info{}};
        1 ->
            {?Q("'@Module@':'@Name@'([_@InterpretedArgs])"), #ast_info{}};
        I ->
            throw({unsupported_custom_tag_fun, {Module, Name, I}})
    catch _:function_clause ->
            custom_tags_modules_ast(Name, InterpretedArgs,
                                    Context#dtl_context{ custom_tags_modules = Rest })
    end.

call_ast(Module, TreeWalkerAcc) ->
    call_ast(Module, merl:var("_Variables"), #ast_info{}, TreeWalkerAcc).

call_with_ast(Module, Variable, Context, TreeWalker) ->
    {{VarAst, VarInfo}, TreeWalker2} = resolve_variable_ast(Variable, Context, TreeWalker, false),
    call_ast(Module, VarAst, VarInfo, TreeWalker2).

call_ast(Module, Variable, AstInfo, TreeWalker) ->
    Ast = ?Q(["case '@Module@':render(_@Variable, RenderOptions) of",
              "  {ok, Rendered} -> Rendered;",
              "  {error, Reason} -> io_lib:format(\"error: ~p\", [Reason])",
              "end"]),
    with_dependencies(Module:dependencies(), {{Ast, AstInfo}, TreeWalker}).
