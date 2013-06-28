%%%-------------------------------------------------------------------
%%% File:      erlydtl_compiler.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @author    Andreas Stenius <kaos@astekk.se>
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
-author('Andreas Stenius <kaos@astekk.se>').

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-export([compile/2, compile/3, compile_dir/2, compile_dir/3, parse/1]).

%% exported for use by extension modules
-export([
         merge_info/2, 
         format/3, 
         value_ast/5,
         resolve_scoped_variable_ast/2
        ]).

-include("erlydtl_ext.hrl").

compile(Binary, Module) when is_binary(Binary) ->
    compile(Binary, Module, []);

compile(File, Module) ->
    compile(File, Module, []).

compile(Binary, Module, Options) when is_binary(Binary) ->
    File = "",
    CheckSum = "",
    Context = init_dtl_context(File, Module, Options),
    case parse(Binary, Context) of
        {ok, DjangoParseTree} ->
            case compile_to_binary(File, DjangoParseTree, Context, CheckSum) of
                {ok, Module1, _, _} ->
                    {ok, Module1};
                Err ->
                    Err
            end;
        Err ->
            Err
    end;

compile(File, Module, Options) ->  
    Context = init_dtl_context(File, Module, Options),
    case parse(File, Context) of  
        ok ->
            ok;
        {ok, DjangoParseTree, CheckSum} ->
            case compile_to_binary(File, DjangoParseTree, Context, CheckSum) of
                {ok, Module1, Bin, Warnings} ->
                    write_binary(Module1, Bin, Options, Warnings);
                Err ->
                    Err
            end;
        Err ->
            Err
    end.


compile_dir(Dir, Module) ->
    compile_dir(Dir, Module, []).

compile_dir(Dir, Module, Options) ->
    Context = init_dtl_context_dir(Dir, Module, Options),
    %% Find all files in Dir (recursively), matching the regex (no
    %% files ending in "~").
    Files = filelib:fold_files(Dir, ".+[^~]$", true, fun(F1,Acc1) -> [F1 | Acc1] end, []),
    {ParserResults, ParserErrors} = lists:foldl(fun
						    (File, {ResultAcc, ErrorAcc}) ->
						       case filename:basename(File) of
							   "."++_ ->
							       {ResultAcc, ErrorAcc};
							   _ ->
							       FilePath = filename:absname(File),
							       case filelib:is_dir(FilePath) of
								   true ->
								       {ResultAcc, ErrorAcc};
								   false ->
								       case parse(FilePath, Context) of
									   ok -> {ResultAcc, ErrorAcc};
									   {ok, DjangoParseTree, CheckSum} -> 
									       {[{File, DjangoParseTree, CheckSum}|ResultAcc], ErrorAcc};
									   Err -> {ResultAcc, [Err|ErrorAcc]}
								       end
							       end
						       end
					       end, {[], []}, Files),
    case ParserErrors of
        [] ->
            case compile_multiple_to_binary(Dir, ParserResults, Context) of
                {ok, Module1, Bin, Warnings} ->
                    write_binary(Module1, Bin, Options, Warnings);
                Err ->
                    Err
            end;
        [Error|_] ->
            Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

write_binary(Module1, Bin, Options, Warnings) ->
    Verbose = proplists:get_value(verbose, Options, false),
    case proplists:get_value(out_dir, Options) of
        undefined ->
            print(Verbose, "Template module: ~w not saved (no out_dir option)\n", [Module1]),
            ok;
        OutDir ->
            BeamFile = filename:join([OutDir, atom_to_list(Module1) ++ ".beam"]),

            print(Verbose, "Template module: ~w -> ~s~s\n",
		  [Module1, BeamFile,
		   case Warnings of
		       [] -> "";
		       _  -> io_lib:format("\n  Warnings: ~p", [Warnings])
		   end]),

            case file:write_file(BeamFile, Bin) of
                ok ->
                    ok;
                {error, Reason} ->
                    {error, lists:flatten(
			      io_lib:format("Beam generation of '~s' failed: ~p",
					    [BeamFile, file:format_error(Reason)]))}
            end
    end.

compile_multiple_to_binary(Dir, ParserResults, Context) ->
    MatchAst = options_match_ast(Context), 
    {Functions, {AstInfo, _}} = lists:mapfoldl(fun({File, DjangoParseTree, CheckSum}, {AstInfo, TreeWalker}) ->
						       FilePath = full_path(File, Context#dtl_context.doc_root),
						       {{BodyAst, BodyInfo}, TreeWalker1} = with_dependency({FilePath, CheckSum}, body_ast(DjangoParseTree, Context, TreeWalker)),
						       FunctionName = filename:rootname(filename:basename(File)),
						       Function1 = erl_syntax:function(erl_syntax:atom(FunctionName),
										       [erl_syntax:clause([erl_syntax:variable("_Variables")], none,
													  [erl_syntax:application(none, erl_syntax:atom(FunctionName), 
																  [erl_syntax:variable("_Variables"), erl_syntax:list([])])])]),
						       Function2 = erl_syntax:function(erl_syntax:atom(FunctionName), 
										       [erl_syntax:clause([erl_syntax:variable("_Variables"), erl_syntax:variable("RenderOptions")], none,
													  MatchAst ++ [BodyAst])]),
						       {{FunctionName, Function1, Function2}, {merge_info(AstInfo, BodyInfo), TreeWalker1}}
					       end, {#ast_info{}, init_treewalker(Context)}, ParserResults),
    Forms = custom_forms(Dir, Context#dtl_context.module, Functions, AstInfo),
    compile_forms_and_reload(Dir, Forms, Context).

compile_to_binary(File, DjangoParseTree, Context, CheckSum) ->
    try body_ast(DjangoParseTree, Context, init_treewalker(Context)) of
        {{BodyAst, BodyInfo}, BodyTreeWalker} ->
            try custom_tags_ast(BodyInfo#ast_info.custom_tags, Context, BodyTreeWalker) of
                {{CustomTagsAst, CustomTagsInfo}, _} ->
                    Forms = forms(File, Context#dtl_context.module, {BodyAst, BodyInfo}, 
				  {CustomTagsAst, CustomTagsInfo}, CheckSum, Context, BodyTreeWalker), 
                    compile_forms_and_reload(File, Forms, Context)
            catch 
                throw:Error -> Error
            end
    catch 
        throw:Error -> Error
    end.

compile_forms_and_reload(File, Forms, Context) ->
    case proplists:get_value(debug_compiler, Context#dtl_context.compiler_options) of
	true ->
	    io:format("template source:~n~s~n",
                      [try 
                           erl_prettypr:format(erl_syntax:form_list(Forms))
                       catch
                           error:Err ->
                               io_lib:format("Pretty printing failed: ~p~nContext: ~n~p~nForms: ~n~p~n",
                                             [Err, Context, Forms])
                       end
                      ]);
	_ -> nop
    end,
    case compile:forms(Forms, Context#dtl_context.compiler_options) of
        {ok, Module1, Bin} -> 
            load_code(Module1, Bin, []);
        {ok, Module1, Bin, Warnings} ->
            load_code(Module1, Bin, Warnings);
        error ->
            {error, lists:concat(["compilation failed: ", File])};
        OtherError ->
            OtherError
    end.

load_code(Module, Bin, Warnings) ->
    code:purge(Module),
    case code:load_binary(Module, atom_to_list(Module) ++ ".erl", Bin) of
        {module, _} -> {ok, Module, Bin, Warnings};
        _ -> {error, lists:concat(["code reload failed: ", Module])}
    end.

init_context(IsCompilingDir, ParseTrail, DefDir, Module, Options) ->
    Ctx = #dtl_context{},
    Context = #dtl_context{
		  parse_trail = ParseTrail,
		  module = Module,
		  doc_root = proplists:get_value(doc_root, Options, DefDir),
		  filter_modules = proplists:get_value(custom_filters_modules, Options, Ctx#dtl_context.filter_modules) ++ [erlydtl_filters],
		  custom_tags_dir = proplists:get_value(custom_tags_dir, Options, filename:join([erlydtl_deps:get_base_dir(), "priv", "custom_tags"])),
		  custom_tags_modules = proplists:get_value(custom_tags_modules, Options, Ctx#dtl_context.custom_tags_modules),
		  blocktrans_fun = proplists:get_value(blocktrans_fun, Options, Ctx#dtl_context.blocktrans_fun),
		  blocktrans_locales = proplists:get_value(blocktrans_locales, Options, Ctx#dtl_context.blocktrans_locales),
		  vars = proplists:get_value(vars, Options, Ctx#dtl_context.vars), 
		  reader = proplists:get_value(reader, Options, Ctx#dtl_context.reader),
		  compiler_options = proplists:get_value(compiler_options, Options, Ctx#dtl_context.compiler_options),
		  binary_strings = proplists:get_value(binary_strings, Options, Ctx#dtl_context.binary_strings),
		  force_recompile = proplists:get_value(force_recompile, Options, Ctx#dtl_context.force_recompile),
		  locale = proplists:get_value(locale, Options, Ctx#dtl_context.locale),
		  verbose = proplists:get_value(verbose, Options, Ctx#dtl_context.verbose),
		  is_compiling_dir = IsCompilingDir,
		  extension_module = proplists:get_value(extension_module, Options, Ctx#dtl_context.extension_module)
		},
    case call_extension(Context, init_context, [Context]) of
        {ok, C} when is_record(C, dtl_context) -> C;
        undefined -> Context
    end.

init_dtl_context(File, Module, Options) when is_list(Module) ->
    init_dtl_context(File, list_to_atom(Module), Options);
init_dtl_context(File, Module, Options) ->
    init_context(false, [File], filename:dirname(File), Module, Options).

init_dtl_context_dir(Dir, Module, Options) when is_list(Module) ->
    init_dtl_context_dir(Dir, list_to_atom(Module), Options);
init_dtl_context_dir(Dir, Module, Options) ->
    init_context(true, [], Dir, Module, Options).

init_treewalker(Context) ->
    TreeWalker = #treewalker{},
    case call_extension(Context, init_treewalker, [TreeWalker]) of
        {ok, TW} when is_record(TW, treewalker) -> TW;
        undefined -> TreeWalker
    end.

is_up_to_date(_, #dtl_context{force_recompile = true}) ->
    false;
is_up_to_date(CheckSum, Context) ->
    Module = Context#dtl_context.module,
    {M, F} = Context#dtl_context.reader,
    case catch Module:source() of
        {_, CheckSum} -> 
            case catch Module:dependencies() of
                L when is_list(L) ->
                    RecompileList = lists:foldl(fun
						    ({XFile, XCheckSum}, Acc) ->
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

parse(Data) ->
    parse(Data, #dtl_context{}).

parse(Data, Context) when is_binary(Data) ->
    check_scan(erlydtl_scanner:scan(binary_to_list(Data)), Context);
parse(File, Context) ->  
    {M, F} = Context#dtl_context.reader,
    case catch M:F(File) of
        {ok, Data} ->
            CheckSum = binary_to_list(erlang:md5(Data)),
            case parse(CheckSum, Data, Context) of
                {error, Msg} when is_list(Msg) ->
                    {error, File ++ ": " ++ Msg};
                {error, Msg} ->
                    {error, {File, [Msg]}};
                Result ->
                    Result
            end;
        _ ->
            {error, {File, [{0, Context#dtl_context.module, "Failed to read file"}]}}
    end.

parse(CheckSum, Data, Context) ->
    case is_up_to_date(CheckSum, Context) of
        true ->
            ok;
        _ ->
            case parse(Data, Context) of
                {ok, Val} ->
                    {ok, Val, CheckSum};
                Err ->
                    Err
            end
    end.

call_extension(#dtl_context{ extension_module=undefined }, _Fun, _Args) ->
    undefined;
call_extension(#dtl_context{ extension_module=Mod }, Fun, Args) 
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    M = case code:is_loaded(Mod) of
	    false ->
		case code:load_file(Mod) of
		    {module, Mod} ->
			Mod;
		    _ ->
			undefined
		end;
	    _ -> Mod
	end,
    if M /= undefined ->
	    case erlang:function_exported(M, Fun, length(Args)) of
		true ->
		    apply(M, Fun, Args);
		false ->
		    undefined
	    end;
       true ->
	    undefined
    end.

check_scan({ok, Tokens}, Context) ->
    Tokens1 = case call_extension(Context, post_scan, [Tokens]) of
                  undefined -> Tokens;
                  {ok, T} -> T
              end,
    check_parse(erlydtl_parser:parse(Tokens1), [], Context#dtl_context{ scanned_tokens=Tokens1 });
check_scan({error, Err, State}, Context) ->
    case call_extension(Context, scan, [State]) of
        undefined ->
            {error, Err};
        {ok, NewState} ->
            check_scan(erlydtl_scanner:resume(NewState), Context);
        ExtRes ->
            ExtRes
    end.

check_parse({ok, _}=Ok, [], _Context) -> Ok;
check_parse({ok, _, _}=Ok, [], _Context) -> Ok;
check_parse({ok, Parsed}, Acc, _Context) -> {ok, Acc ++ Parsed};
check_parse({ok, Parsed, C}, Acc, _Context) -> {ok, Acc ++ Parsed, C};
check_parse({error, _}=Err, _, _Context) -> Err;
check_parse({error, Err, State}, Acc, Context) ->
    {State1, Parsed} = reset_parse_state(State, Context),
    case call_extension(Context, parse, [State1]) of
        undefined ->
            {error, Err};
        {ok, ExtParsed} ->
            {ok, Acc ++ Parsed ++ ExtParsed};
        {error, ExtErr, ExtState} ->
            case reset_parse_state(ExtState, Context) of
                {_, []} ->
                    %% todo: see if this is indeed a sensible ext error,
                    %% or if we should rather present the original Err message
                    {error, ExtErr};
                {State2, ExtParsed} ->
                    check_parse(erlydtl_parser:resume(State2), Acc ++ Parsed ++ ExtParsed, Context)
            end;
        ExtRes ->
            ExtRes
    end.

%% backtrack up to the nearest opening tag, and keep the value stack parsed ok so far
reset_parse_state([[{Tag, _, _}|_]=Ts, Tzr, _, _, Stack], Context)
  when Tag==open_tag; Tag==open_var ->
    %% reached opening tag, so the stack should be sensible here
    {[reset_token_stream(Ts, Context#dtl_context.scanned_tokens),
      Tzr, 0, [], []], lists:flatten(Stack)};
reset_parse_state([_, _, 0, [], []]=State, _Context) ->
    %% top of (empty) stack
    {State, []};
reset_parse_state([Ts, Tzr, _, [0 | []], [Parsed | []]], Context)
  when is_list(Parsed) ->
    %% top of good stack
    {[reset_token_stream(Ts, Context#dtl_context.scanned_tokens),
      Tzr, 0, [], []], Parsed};
reset_parse_state([Ts, Tzr, _, [S | Ss], [T | Stack]], Context) ->
    %% backtrack...
    reset_parse_state([[T|Ts], Tzr, S, Ss, Stack], Context).

reset_token_stream([T|_], [T|Ts]) -> [T|Ts];
reset_token_stream(Ts, [_|S]) ->
    reset_token_stream(Ts, S).
%% we should find the next token in the list of scanned tokens, or something is real fishy


custom_tags_ast(CustomTags, Context, TreeWalker) ->
    {{CustomTagsClauses, CustomTagsInfo}, TreeWalker1} = custom_tags_clauses_ast(CustomTags, Context, TreeWalker),
    %% This doesn't work since erl_syntax:revert/1 chokes on the airity_qualifier in the -compile attribute..
    %% bug report sent to the erlang-bugs mailing list.
    %% {{erl_syntax:form_list(
    %%     [erl_syntax:attribute(
    %%        erl_syntax:atom(compile),
    %%        [erl_syntax:tuple(
    %%           [erl_syntax:atom(nowarn_unused_function),
    %%            erl_syntax:arity_qualifier(
    %%              erl_syntax:atom(render_tag),
    %%              erl_syntax:integer(3))])]),
    %%      erl_syntax:function(erl_syntax:atom(render_tag), CustomTagsClauses)]),
    {{erl_syntax:function(erl_syntax:atom(render_tag), CustomTagsClauses),
      CustomTagsInfo},
     TreeWalker1}.

custom_tags_clauses_ast(CustomTags, Context, TreeWalker) ->
    custom_tags_clauses_ast1(CustomTags, [], [], #ast_info{}, Context, TreeWalker).

custom_tags_clauses_ast1([], _ExcludeTags, ClauseAcc, InfoAcc, Context, TreeWalker) ->
    {{DefaultAst, DefaultInfo}, TreeWalker1} =
        case call_extension(Context, custom_tag_ast, [Context, TreeWalker]) of
            undefined ->
                {{erl_syntax:clause(
                    [erl_syntax:variable("_TagName"), erl_syntax:underscore(), erl_syntax:underscore()], 
                    none,
                    [erl_syntax:list([])]),
                  InfoAcc},
                 TreeWalker};
            {{ExtAst, ExtInfo}, ExtTreeWalker} ->
                Clause = erl_syntax:clause(
                           [erl_syntax:variable("TagName"), erl_syntax:variable("_Variables"), erl_syntax:variable("RenderOptions")],
                           none, options_match_ast(Context, ExtTreeWalker) ++ [ExtAst]),
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
                    case parse(CustomTagFile, Context) of
                        {ok, DjangoParseTree, CheckSum} ->
                            {{BodyAst, BodyAstInfo}, TreeWalker1} = with_dependency(
								      {CustomTagFile, CheckSum}, body_ast(DjangoParseTree, Context, TreeWalker)),
                            MatchAst = options_match_ast(Context, TreeWalker), 
                            Clause = erl_syntax:clause(
				       [erl_syntax:atom(Tag), erl_syntax:variable("_Variables"), erl_syntax:variable("RenderOptions")],
				       none, MatchAst ++ [BodyAst]),
                            custom_tags_clauses_ast1(CustomTags, [Tag|ExcludeTags],
						     [Clause|ClauseAcc], merge_info(BodyAstInfo, InfoAcc), 
						     Context, TreeWalker1);
                        Error ->
                            throw(Error)
                    end;
                false ->
                    case call_extension(Context, custom_tag_ast, [Tag, Context, TreeWalker]) of
                        undefined ->
                            custom_tags_clauses_ast1(
                              CustomTags, [Tag | ExcludeTags],
                              ClauseAcc, InfoAcc, Context, TreeWalker);
                        {{Ast, Info}, TW} ->
                            Clause = erl_syntax:clause(
                                       [erl_syntax:atom(Tag), erl_syntax:variable("_Variables"), erl_syntax:variable("RenderOptions")],
                                       none, options_match_ast(Context, TW) ++ [Ast]),
                            custom_tags_clauses_ast1(
                             CustomTags, [Tag | ExcludeTags],
                             [Clause|ClauseAcc], merge_info(Info, InfoAcc),
                             Context, TW)
                    end
            end
    end.

dependencies_function(Dependencies) ->
    erl_syntax:function(
      erl_syntax:atom(dependencies), [erl_syntax:clause([], none, 
							[erl_syntax:list(lists:map(fun 
										       ({XFile, XCheckSum}) -> 
											  erl_syntax:tuple([erl_syntax:string(XFile), erl_syntax:string(XCheckSum)])
										  end, Dependencies))])]).

translatable_strings_function(TranslatableStrings) ->
    erl_syntax:function(
      erl_syntax:atom(translatable_strings), [erl_syntax:clause([], none,
								[erl_syntax:list(lists:map(fun(String) -> 
												   erl_syntax:string(String) 
											   end,
											   TranslatableStrings))])]).

translated_blocks_function(TranslatedBlocks) ->
    erl_syntax:function(
      erl_syntax:atom(translated_blocks), [erl_syntax:clause([], none,
							     [erl_syntax:list(lists:map(fun(String) -> 
												erl_syntax:string(String) 
											end,
											TranslatedBlocks))])]).

variables_function(Variables) ->
    erl_syntax:function(
      erl_syntax:atom(variables), [erl_syntax:clause([], none,
						     [erl_syntax:list([erl_syntax:atom(S) || S <- lists:usort(Variables)])])]). 

custom_forms(Dir, Module, Functions, AstInfo) ->
    ModuleAst = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
				     [erl_syntax:list([
						       erl_syntax:arity_qualifier(erl_syntax:atom(source_dir), erl_syntax:integer(0)),
						       erl_syntax:arity_qualifier(erl_syntax:atom(dependencies), erl_syntax:integer(0)),
						       erl_syntax:arity_qualifier(erl_syntax:atom(translatable_strings), erl_syntax:integer(0))
						       | 
						       lists:foldl(fun({FunctionName, _, _}, Acc) ->
									   [erl_syntax:arity_qualifier(erl_syntax:atom(FunctionName), erl_syntax:integer(1)),
									    erl_syntax:arity_qualifier(erl_syntax:atom(FunctionName), erl_syntax:integer(2))|Acc]
								   end, [], Functions)]
						     )]),
    SourceFunctionAst = erl_syntax:function(
			  erl_syntax:atom(source_dir), [erl_syntax:clause([], none, [erl_syntax:string(Dir)])]),
    DependenciesFunctionAst = dependencies_function(AstInfo#ast_info.dependencies), 
    TranslatableStringsFunctionAst = translatable_strings_function(AstInfo#ast_info.translatable_strings),
    FunctionAsts = lists:foldl(fun({_, Function1, Function2}, Acc) -> [Function1, Function2 | Acc] end, [], Functions),

    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, SourceFunctionAst, DependenciesFunctionAst, TranslatableStringsFunctionAst
				   | FunctionAsts] ++ AstInfo#ast_info.pre_render_asts].

forms(File, Module, {BodyAst, BodyInfo}, {CustomTagsFunctionAst, CustomTagsInfo}, CheckSum, Context, TreeWalker) ->
    MergedInfo = merge_info(BodyInfo, CustomTagsInfo),
    Render0FunctionAst = erl_syntax:function(erl_syntax:atom(render),
					     [erl_syntax:clause([], none, [erl_syntax:application(none, 
												  erl_syntax:atom(render), [erl_syntax:list([])])])]),
    Render1FunctionAst = erl_syntax:function(erl_syntax:atom(render),
					     [erl_syntax:clause([erl_syntax:variable("_Variables")], none,
								[erl_syntax:application(none,
											erl_syntax:atom(render),
											[erl_syntax:variable("_Variables"), erl_syntax:list([])])])]),
    Function2 = erl_syntax:application(none, erl_syntax:atom(render_internal), 
				       [erl_syntax:variable("_Variables"), erl_syntax:variable("RenderOptions")]),
    ClauseOk = erl_syntax:clause([erl_syntax:variable("Val")], none,
				 [erl_syntax:tuple([erl_syntax:atom(ok), erl_syntax:variable("Val")])]),     
    ClauseCatch = erl_syntax:clause([erl_syntax:variable("Err")], none,
				    [erl_syntax:tuple([erl_syntax:atom(error), erl_syntax:variable("Err")])]),            
    Render2FunctionAst = erl_syntax:function(erl_syntax:atom(render),
					     [erl_syntax:clause([erl_syntax:variable("_Variables"),
								 erl_syntax:variable("RenderOptions")], none, 
								[erl_syntax:try_expr([Function2], [ClauseOk], [ClauseCatch])])]),  

    SourceFunctionTuple = erl_syntax:tuple(
			    [erl_syntax:string(File), erl_syntax:string(CheckSum)]),
    SourceFunctionAst = erl_syntax:function(
			  erl_syntax:atom(source),
			  [erl_syntax:clause([], none, [SourceFunctionTuple])]),

    DependenciesFunctionAst = dependencies_function(MergedInfo#ast_info.dependencies),

    TranslatableStringsAst = translatable_strings_function(MergedInfo#ast_info.translatable_strings),

    TranslatedBlocksAst = translated_blocks_function(MergedInfo#ast_info.translated_blocks),

    VariablesAst = variables_function(MergedInfo#ast_info.var_names),

    MatchAst = options_match_ast(Context, TreeWalker), 

    BodyAstTmp = MatchAst ++ [
			      erl_syntax:application(
				erl_syntax:atom(erlydtl_runtime),
				erl_syntax:atom(stringify_final),
				[BodyAst, erl_syntax:atom(Context#dtl_context.binary_strings)])
			     ],

    RenderInternalFunctionAst = erl_syntax:function(
				  erl_syntax:atom(render_internal), 
				  [erl_syntax:clause([
						      erl_syntax:variable("_Variables"),
						      erl_syntax:variable("RenderOptions")],
						     none, BodyAstTmp)]
				 ),   

    ModuleAst  = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),

    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
				     [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(0)),
						       erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(1)),
						       erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(2)),
						       erl_syntax:arity_qualifier(erl_syntax:atom(source), erl_syntax:integer(0)),
						       erl_syntax:arity_qualifier(erl_syntax:atom(dependencies), erl_syntax:integer(0)),
						       erl_syntax:arity_qualifier(erl_syntax:atom(translatable_strings), erl_syntax:integer(0)),
						       erl_syntax:arity_qualifier(erl_syntax:atom(translated_blocks), erl_syntax:integer(0)),
						       erl_syntax:arity_qualifier(erl_syntax:atom(variables), erl_syntax:integer(0))
						      ])]),

    erl_syntax:revert_forms(
      erl_syntax:form_list(
        [ModuleAst, ExportAst, Render0FunctionAst, Render1FunctionAst, Render2FunctionAst,
         SourceFunctionAst, DependenciesFunctionAst, TranslatableStringsAst,
         TranslatedBlocksAst, VariablesAst, RenderInternalFunctionAst, 
         CustomTagsFunctionAst
         |BodyInfo#ast_info.pre_render_asts])).

options_match_ast(Context) -> options_match_ast(Context, undefined).
options_match_ast(Context, TreeWalker) -> 
    [
     erl_syntax:match_expr(
       erl_syntax:variable("_TranslationFun"),
       erl_syntax:application(
	 erl_syntax:atom(proplists),
	 erl_syntax:atom(get_value),
	 [erl_syntax:atom(translation_fun), erl_syntax:variable("RenderOptions"), erl_syntax:atom(none)])),
     erl_syntax:match_expr(
       erl_syntax:variable("_CurrentLocale"),
       erl_syntax:application(
	 erl_syntax:atom(proplists),
	 erl_syntax:atom(get_value),
	 [erl_syntax:atom(locale), erl_syntax:variable("RenderOptions"), erl_syntax:atom(none)]))
     | case call_extension(Context, setup_render_ast, [Context, TreeWalker]) of
           undefined -> [];
           Ast when is_list(Ast) -> Ast
       end
    ].

						% child templates should only consist of blocks at the top level
body_ast([{'extends', {string_literal, _Pos, String}} | ThisParseTree], Context, TreeWalker) ->
    File = full_path(unescape_string_literal(String), Context#dtl_context.doc_root),
    case lists:member(File, Context#dtl_context.parse_trail) of
        true ->
            throw({error, "Circular file inclusion!"});
        _ ->
            case parse(File, Context) of
                {ok, ParentParseTree, CheckSum} ->
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
										  parse_trail = [File | Context#dtl_context.parse_trail]}, TreeWalker));
                Err ->
                    throw(Err)
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
				       (ValueToken, TreeWalkerAcc) -> 
						       {{ValueAst,ValueInfo},ValueTreeWalker} = value_ast(ValueToken, true, true, Context, TreeWalkerAcc),
						       {{format(ValueAst, Context, ValueTreeWalker),ValueInfo},ValueTreeWalker}
					       end, TreeWalker, DjangoParseTree),   
    {AstList, {Info, TreeWalker3}} = lists:mapfoldl(
				       fun({Ast, Info}, {InfoAcc, TreeWalkerAcc}) -> 
					       PresetVars = lists:foldl(fun
									    (X, Acc) ->
									       case proplists:lookup(X, Context#dtl_context.vars) of
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
											  [erl_syntax:clause([erl_syntax:variable("_Variables")], none, [Ast])]),
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
            Ast = erl_syntax:application(erl_syntax:atom(erlydtl_runtime), 
                                         erl_syntax:atom(Operator), 
                                         [ValueAst]),
            {{Ast, InfoValue}, TreeWalker1};
        {'expr', Operator, Value1, Value2} ->
            {{Value1Ast,InfoValue1}, TreeWalker1} = value_ast(Value1, false, EmptyIfUndefined, Context, TreeWalker),
            {{Value2Ast,InfoValue2}, TreeWalker2} = value_ast(Value2, false, EmptyIfUndefined, Context, TreeWalker1),
            Ast = erl_syntax:application(erl_syntax:atom(erlydtl_runtime), 
                                         erl_syntax:atom(Operator), 
                                         [Value1Ast, Value2Ast]),
            {{Ast, merge_info(InfoValue1,InfoValue2)}, TreeWalker2};
        {'string_literal', _Pos, String} ->
            string_ast(unescape_string_literal(String), Context, TreeWalker);
        {'number_literal', _Pos, Number} ->
            case AsString of
                true  -> string_ast(Number, Context, TreeWalker);
                false -> {{erl_syntax:integer(list_to_integer(Number)), #ast_info{}}, TreeWalker}
            end;
        {'apply_filter', Variable, Filter} ->
            filter_ast(Variable, Filter, Context, TreeWalker);
        {'attribute', _} = Variable ->
            {Ast, VarName} = resolve_variable_ast(Variable, Context, EmptyIfUndefined),
            {{Ast, #ast_info{var_names = [VarName]}}, TreeWalker};
        {'variable', _} = Variable ->
            {Ast, VarName} = resolve_variable_ast(Variable, Context, EmptyIfUndefined),
            {{Ast, #ast_info{var_names = [VarName]}}, TreeWalker};
        {extension, Tag} ->
            extension_ast(Tag, Context, TreeWalker)
    end.

extension_ast(Tag, Context, TreeWalker) ->
    case call_extension(Context, compile_ast, [Tag, Context, TreeWalker]) of
        undefined ->
            throw({error, {unknown_extension, Tag}});
        Result ->
            Result
    end.

merge_info(Info1, Info2) ->
    #ast_info{
	    dependencies = 
		lists:merge(
		  lists:sort(Info1#ast_info.dependencies), 
		  lists:sort(Info2#ast_info.dependencies)),
	    var_names = 
		lists:merge(
		  lists:sort(Info1#ast_info.var_names), 
		  lists:sort(Info2#ast_info.var_names)),
	    translatable_strings =
		lists:merge(
		  lists:sort(Info1#ast_info.translatable_strings),
		  lists:sort(Info2#ast_info.translatable_strings)),
	    translated_blocks =
		lists:merge(
		  lists:sort(Info1#ast_info.translated_blocks),
		  lists:sort(Info2#ast_info.translated_blocks)),
	    custom_tags = 
		lists:merge(
		  lists:sort(Info1#ast_info.custom_tags),
		  lists:sort(Info2#ast_info.custom_tags)),
	    pre_render_asts = 
		lists:merge(
		  Info1#ast_info.pre_render_asts,
		  Info2#ast_info.pre_render_asts)}.


with_dependencies([], Args) ->
    Args;
with_dependencies([Dependency | Rest], Args) ->
    with_dependencies(Rest, with_dependency(Dependency, Args)).

with_dependency(FilePath, {{Ast, Info}, TreeWalker}) ->
    {{Ast, Info#ast_info{dependencies = [FilePath | Info#ast_info.dependencies]}}, TreeWalker}.


empty_ast(TreeWalker) ->
    {{erl_syntax:list([]), #ast_info{}}, TreeWalker}.

blocktrans_ast(ArgList, Contents, Context, TreeWalker) ->
    {NewScope, {ArgInfo, TreeWalker1}} = lists:mapfoldl(fun
							    ({{identifier, _, LocalVarName}, Value}, {AstInfo1, TreeWalker1}) ->
							       {{Ast, Info}, TreeWalker2} = value_ast(Value, false, false, Context, TreeWalker1),
							       {{LocalVarName, Ast}, {merge_info(AstInfo1, Info), TreeWalker2}}
						       end, {#ast_info{}, TreeWalker}, ArgList),
    NewContext = Context#dtl_context{ local_scopes = [NewScope|Context#dtl_context.local_scopes] },
    SourceText = lists:flatten(erlydtl_unparser:unparse(Contents)),
    {{DefaultAst, AstInfo}, TreeWalker2} = body_ast(Contents, NewContext, TreeWalker1),
    MergedInfo = merge_info(AstInfo, ArgInfo),
    case Context#dtl_context.blocktrans_fun of
        none ->
            {{DefaultAst, MergedInfo}, TreeWalker2};
        BlockTransFun when is_function(BlockTransFun) ->
            {FinalAstInfo, FinalTreeWalker, Clauses} = lists:foldr(fun(Locale, {AstInfoAcc, ThisTreeWalker, ClauseAcc}) ->
									   case BlockTransFun(SourceText, Locale) of
									       default ->
										   {AstInfoAcc, ThisTreeWalker, ClauseAcc};
									       Body ->
										   {ok, DjangoParseTree} = parse(Body, Context),
										   {{ThisAst, ThisAstInfo}, TreeWalker3} = body_ast(DjangoParseTree, NewContext, ThisTreeWalker),
										   {merge_info(ThisAstInfo, AstInfoAcc), TreeWalker3, 
										    [erl_syntax:clause([erl_syntax:string(Locale)], none, [ThisAst])|ClauseAcc]}
									   end
								   end, {MergedInfo, TreeWalker2, []}, Context#dtl_context.blocktrans_locales),
            Ast = erl_syntax:case_expr(erl_syntax:variable("_CurrentLocale"),
				       Clauses ++ [erl_syntax:clause([erl_syntax:underscore()], none, [DefaultAst])]),
            {{Ast, FinalAstInfo#ast_info{ translated_blocks = [SourceText] }}, FinalTreeWalker}
    end.

translated_ast({string_literal, _, String}, Context, TreeWalker) ->
    NewStr = unescape_string_literal(String),
    DefaultString = case Context#dtl_context.locale of
			none -> NewStr;
			Locale -> erlydtl_i18n:translate(NewStr,Locale)
		    end,
    translated_ast2(erl_syntax:string(NewStr), erl_syntax:string(DefaultString), 
		    #ast_info{translatable_strings = [NewStr]}, TreeWalker);
translated_ast(ValueToken, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = value_ast(ValueToken, true, false, Context, TreeWalker),
    translated_ast2(Ast, Ast, Info, TreeWalker1).

translated_ast2(NewStrAst, DefaultStringAst, AstInfo, TreeWalker) ->
    StringLookupAst = erl_syntax:application(
			erl_syntax:atom(erlydtl_runtime),
			erl_syntax:atom(translate),
			[NewStrAst, erl_syntax:variable("_TranslationFun"), DefaultStringAst]),
    {{StringLookupAst, AstInfo}, TreeWalker}.

						% Completely unnecessary in ErlyDTL (use {{ "{%" }} etc), but implemented for compatibility.
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
    {{format_number_ast(erl_syntax:application(
			  erl_syntax:atom(erlydtl_runtime),
			  erl_syntax:atom(widthratio),
			  [NumAst, DenAst, ScaleAst])), merge_info(ScaleInfo, merge_info(NumInfo, DenInfo))},
     TreeWalker3}.

binary_string(String) ->
    erl_syntax:binary([erl_syntax:binary_field(erl_syntax:integer(X)) || X <- String]).

string_ast(String, #dtl_context{ binary_strings = true }, TreeWalker) when is_list(String) ->
    {{binary_string(String), #ast_info{}}, TreeWalker};
string_ast(String, #dtl_context{ binary_strings = false }, TreeWalker) when is_list(String) ->
    {{erl_syntax:string(String), #ast_info{}}, TreeWalker}; %% less verbose AST, better for development and debugging
string_ast(S, Context, TreeWalker) when is_atom(S) ->
    string_ast(atom_to_list(S), Context, TreeWalker).


include_ast(File, ArgList, Scopes, Context, TreeWalker) ->
    FilePath = full_path(File, Context#dtl_context.doc_root),
    case parse(FilePath, Context) of
        {ok, InclusionParseTree, CheckSum} ->
            {NewScope, {ArgInfo, TreeWalker1}} = lists:mapfoldl(fun
								    ({{identifier, _, LocalVarName}, Value}, {AstInfo1, TreeWalker1}) ->
								       {{Ast, Info}, TreeWalker2} = value_ast(Value, false, false, Context, TreeWalker1),
								       {{LocalVarName, Ast}, {merge_info(AstInfo1, Info), TreeWalker2}}
							       end, {#ast_info{}, TreeWalker}, ArgList),

            {{BodyAst, BodyInfo}, TreeWalker2} = with_dependency({FilePath, CheckSum}, 
								 body_ast(InclusionParseTree, Context#dtl_context{
												parse_trail = [FilePath | Context#dtl_context.parse_trail],
												local_scopes = [NewScope|Scopes]
											       }, TreeWalker1)),

            {{BodyAst, merge_info(BodyInfo, ArgInfo)}, TreeWalker2};
        Err ->
            throw(Err)
    end.

						% include at run-time
ssi_ast(FileName, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = value_ast(FileName, true, true, Context, TreeWalker),
    {Mod, Fun} = Context#dtl_context.reader,
    {{erl_syntax:application(
	erl_syntax:atom(erlydtl_runtime),
	erl_syntax:atom(read_file),
	[erl_syntax:atom(Mod), erl_syntax:atom(Fun), erl_syntax:string(Context#dtl_context.doc_root), Ast]), Info}, TreeWalker1}.

filter_tag_ast(FilterList, Contents, Context, TreeWalker) ->
    {{InnerAst, Info}, TreeWalker1} = body_ast(Contents, Context#dtl_context{auto_escape = did}, TreeWalker),
    {{FilteredAst, FilteredInfo}, TreeWalker2} = lists:foldl(fun
								 ([{identifier, _, 'escape'}], {{AstAcc, InfoAcc}, TreeWalkerAcc}) ->
								    {{AstAcc, InfoAcc}, TreeWalkerAcc#treewalker{safe = true}};
								 ([{identifier, _, 'safe'}], {{AstAcc, InfoAcc}, TreeWalkerAcc}) ->
								    {{AstAcc, InfoAcc}, TreeWalkerAcc#treewalker{safe = true}};
								 ([{identifier, _, 'safeseq'}], {{AstAcc, InfoAcc}, TreeWalkerAcc}) ->
								    {{AstAcc, InfoAcc}, TreeWalkerAcc#treewalker{safe = true}};
								 (Filter, {{AstAcc, InfoAcc}, TreeWalkerAcc}) ->
								    {Ast, AstInfo} = filter_ast1(Filter, AstAcc, Context),
								    {{Ast, merge_info(InfoAcc, AstInfo)}, TreeWalkerAcc}
							    end, {{erl_syntax:application(
								     erl_syntax:atom(erlang),
								     erl_syntax:atom(iolist_to_binary),
								     [InnerAst]), Info}, TreeWalker1}, FilterList),

    EscapedAst = case search_for_escape_filter(lists:reverse(FilterList), Context) of
		     on ->
			 erl_syntax:application(
			   erl_syntax:atom(erlydtl_filters), 
			   erl_syntax:atom(force_escape), 
			   [FilteredAst]);
		     _ ->
			 FilteredAst
		 end,
    {{EscapedAst, FilteredInfo}, TreeWalker2}.

search_for_escape_filter(FilterList, #dtl_context{auto_escape = on}) ->
    search_for_safe_filter(FilterList);
search_for_escape_filter(_, #dtl_context{auto_escape = did}) ->
    off;
search_for_escape_filter([[{identifier, _, 'escape'}]|Rest], _Context) ->
    search_for_safe_filter(Rest);
search_for_escape_filter([_|Rest], Context) ->
    search_for_escape_filter(Rest, Context);
search_for_escape_filter([], _Context) ->
    off.

search_for_safe_filter([[{identifier, _, 'safe'}]|_]) ->
    off;
search_for_safe_filter([[{identifier, _, 'safeseq'}]|_]) ->
    off;
search_for_safe_filter([_|Rest]) ->
    search_for_safe_filter(Rest);
search_for_safe_filter([]) ->
    on.

filter_ast(Variable, Filter, Context, TreeWalker) ->
						% the escape filter is special; it is always applied last, so we have to go digging for it

						% AutoEscape = 'did' means we (will have) decided whether to escape the current variable,
						% so don't do any more escaping
    {{UnescapedAst, Info}, TreeWalker2} = filter_ast_noescape(Variable, Filter, 
							      Context#dtl_context{auto_escape = did}, TreeWalker),
    EscapedAst = case search_for_escape_filter(Variable, Filter, Context) of
		     on ->
			 erl_syntax:application(
			   erl_syntax:atom(erlydtl_filters), 
			   erl_syntax:atom(force_escape), 
			   [UnescapedAst]);
		     _ -> 
			 UnescapedAst
		 end,
    {{EscapedAst, Info}, TreeWalker2}.

filter_ast_noescape(Variable, [{identifier, _, 'escape'}], Context, TreeWalker) ->
    value_ast(Variable, true, false, Context, TreeWalker#treewalker{safe = true});
filter_ast_noescape(Variable, [{identifier, _, 'safe'}], Context, TreeWalker) ->
    value_ast(Variable, true, false, Context, TreeWalker#treewalker{safe = true});
filter_ast_noescape(Variable, [{identifier, _, 'safeseq'}], Context, TreeWalker) ->
    value_ast(Variable, true, false, Context, TreeWalker#treewalker{safe = true});
filter_ast_noescape(Variable, Filter, Context, TreeWalker) ->
    {{VariableAst, Info1}, TreeWalker2} = value_ast(Variable, true, false, Context, TreeWalker),
    {VarValue, Info2} = filter_ast1(Filter, VariableAst, Context),
    {{VarValue, merge_info(Info1, Info2)}, TreeWalker2}.

filter_ast1([{identifier, _, Name}, {string_literal, _, ArgName}], VariableAst, #dtl_context{ binary_strings = true } = Context) ->
    filter_ast2(Name, VariableAst, [binary_string(unescape_string_literal(ArgName))], [], Context);
filter_ast1([{identifier, _, Name}, {string_literal, _, ArgName}], VariableAst, #dtl_context{ binary_strings = false } = Context) ->
    filter_ast2(Name, VariableAst, [erl_syntax:string(unescape_string_literal(ArgName))], [], Context);
filter_ast1([{identifier, _, Name}, {number_literal, _, ArgName}], VariableAst, Context) ->
    filter_ast2(Name, VariableAst, [erl_syntax:integer(list_to_integer(ArgName))], [], Context);
filter_ast1([{identifier, _, Name}, ArgVariable], VariableAst, Context) ->
    {ArgAst, ArgVarName} = resolve_variable_ast(ArgVariable, Context, false),
    filter_ast2(Name, VariableAst, [ArgAst], [ArgVarName], Context);
filter_ast1([{identifier, _, Name}], VariableAst, Context) ->
    filter_ast2(Name, VariableAst, [], [], Context).

filter_ast2(Name, VariableAst, [], VarNames, #dtl_context{ filter_modules = [Module|Rest] } = Context) ->
    case lists:member({Name, 1}, Module:module_info(exports)) of
        true ->
            {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Name), 
				    [VariableAst]), #ast_info{var_names = VarNames}};
        false ->
            filter_ast2(Name, VariableAst, [], VarNames, Context#dtl_context{ filter_modules = Rest })
    end;
filter_ast2(Name, VariableAst, [Arg], VarNames, #dtl_context{ filter_modules = [Module|Rest] } = Context) ->
    case lists:member({Name, 2}, Module:module_info(exports)) of
        true ->
            {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Name),
				    [VariableAst, Arg]), #ast_info{var_names = VarNames}};
        false ->
            filter_ast2(Name, VariableAst, [Arg], VarNames, Context#dtl_context{ filter_modules = Rest })
    end.

search_for_escape_filter(Variable, Filter, #dtl_context{auto_escape = on}) ->
    search_for_safe_filter(Variable, Filter);
search_for_escape_filter(_, _, #dtl_context{auto_escape = did}) ->
    off;
search_for_escape_filter(Variable, [{identifier, _, 'escape'}] = Filter, _Context) ->
    search_for_safe_filter(Variable, Filter);
search_for_escape_filter({apply_filter, Variable, Filter}, _, Context) ->
    search_for_escape_filter(Variable, Filter, Context);
search_for_escape_filter(_Variable, _Filter, _Context) ->
    off.

search_for_safe_filter(_, [{identifier, _, 'safe'}]) ->
    off;
search_for_safe_filter(_, [{identifier, _, 'safeseq'}]) ->
    off;
search_for_safe_filter({apply_filter, Variable, Filter}, _) ->
    search_for_safe_filter(Variable, Filter);
search_for_safe_filter(_Variable, _Filter) ->
    on.

resolve_variable_ast(VarTuple, Context, true) ->
    resolve_variable_ast1(VarTuple, Context, 'fetch_value');
resolve_variable_ast(VarTuple, Context, false) ->
    resolve_variable_ast1(VarTuple, Context, 'find_value').

resolve_variable_ast1({attribute, {{identifier, {Row, Col}, AttrName}, Variable}}, Context, FinderFunction) ->
    {VarAst, VarName} = resolve_variable_ast1(Variable, Context, FinderFunction),
    FileNameAst = case Context#dtl_context.parse_trail of 
		      [] -> erl_syntax:atom(undefined); 
		      [H|_] -> erl_syntax:string(H)
		  end,
    {erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(FinderFunction),
			    [erl_syntax:atom(AttrName), VarAst, FileNameAst,
			     erl_syntax:tuple([erl_syntax:integer(Row), erl_syntax:integer(Col)])
			    ]), VarName};

resolve_variable_ast1({variable, {identifier, {Row, Col}, VarName}}, Context, FinderFunction) ->
    VarValue = case resolve_scoped_variable_ast(VarName, Context) of
		   undefined ->
		       FileNameAst = case Context#dtl_context.parse_trail of 
					 [] -> erl_syntax:atom(undefined); 
					 [H|_] -> erl_syntax:string(H)
				     end,
		       erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(FinderFunction),
					      [erl_syntax:atom(VarName), erl_syntax:variable("_Variables"), FileNameAst,
					       erl_syntax:tuple([erl_syntax:integer(Row), erl_syntax:integer(Col)])
					      ]);
		   Val ->
		       Val
	       end,
    {VarValue, VarName};

resolve_variable_ast1(What, _Context, _FinderFunction) ->
    error_logger:error_msg("~p:resolve_variable_ast unhandled: ~p~n", [?MODULE, What]).

resolve_scoped_variable_ast(VarName, Context) ->
    lists:foldl(fun(Scope, Value) ->
			case Value of
			    undefined -> proplists:get_value(VarName, Scope);
			    _ -> Value
			end
		end, undefined, Context#dtl_context.local_scopes).

format(Ast, Context, TreeWalker) ->
    auto_escape(format_number_ast(Ast), Context, TreeWalker).

format_number_ast(Ast) ->
    erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(format_number),
			   [Ast]).


auto_escape(Value, _, #treewalker{safe = true}) ->
    Value;
auto_escape(Value, #dtl_context{auto_escape = on}, _) ->
    erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(force_escape), [Value]);
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
    {{erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(is_true), [Ast]),
			   [erl_syntax:clause([erl_syntax:atom(true)], none, 
					      [IfContentsAst]),
			    erl_syntax:clause([erl_syntax:underscore()], none,
					      [ElseContentsAst])
			   ]), merge_info(ExpressionInfo, Info)}, TreeWalker1}.

with_ast(ArgList, Contents, Context, TreeWalker) ->
    {ArgAstList, {ArgInfo, TreeWalker1}} = lists:mapfoldl(fun
							      ({{identifier, _, _LocalVarName}, Value}, {AstInfo1, TreeWalker1}) ->
								 {{Ast, Info}, TreeWalker2} = value_ast(Value, false, false, Context, TreeWalker1),
								 {Ast, {merge_info(AstInfo1, Info), TreeWalker2}}
							 end, {#ast_info{}, TreeWalker}, ArgList),

    NewScope = lists:map(fun({{identifier, _, LocalVarName}, _Value}) ->
				 {LocalVarName, erl_syntax:variable(lists:concat(["Var_", LocalVarName]))}
			 end, ArgList),

    {{InnerAst, InnerInfo}, TreeWalker2} = body_ast(Contents,
						    Context#dtl_context{local_scopes = [NewScope|Context#dtl_context.local_scopes]}, TreeWalker1),

    {{erl_syntax:application(
	erl_syntax:fun_expr([
			     erl_syntax:clause(lists:map(fun({_, Var}) -> Var end, NewScope), none,
					       [InnerAst])]), ArgAstList), merge_info(ArgInfo, InnerInfo)}, TreeWalker2}.

regroup_ast(ListVariable, GrouperVariable, LocalVarName, Contents, Context, TreeWalker) ->
    {{ListAst, ListInfo}, TreeWalker1} = value_ast(ListVariable, false, true, Context, TreeWalker),
    NewScope = [{LocalVarName, erl_syntax:variable(lists:concat(["Var_", LocalVarName]))}],

    {{InnerAst, InnerInfo}, TreeWalker2} = body_ast(Contents, 
						    Context#dtl_context{ local_scopes = [NewScope|Context#dtl_context.local_scopes] }, TreeWalker1),

    Ast = {erl_syntax:application(
	     erl_syntax:fun_expr([
				  erl_syntax:clause([erl_syntax:variable(lists:concat(["Var_", LocalVarName]))], none,
						    [InnerAst])]), 
	     [erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(regroup),
				     [ListAst, regroup_filter(GrouperVariable,[])])]), merge_info(ListInfo, InnerInfo)},
    {Ast,TreeWalker2}.

regroup_filter({attribute,{{identifier,_,Ident},Next}},Acc) ->
    regroup_filter(Next,[erl_syntax:atom(Ident)|Acc]);
regroup_filter({variable,{identifier,_,Var}},Acc) ->
    erl_syntax:list([erl_syntax:atom(Var)|Acc]).


for_loop_ast(IteratorList, LoopValue, IsReversed, Contents, {EmptyContentsAst, EmptyContentsInfo}, Context, TreeWalker) ->
    Vars = lists:map(fun({identifier, _, Iterator}) -> 
			     erl_syntax:variable(lists:concat(["Var_", Iterator])) 
		     end, IteratorList),
    {{InnerAst, Info}, TreeWalker1} = body_ast(Contents,
					       Context#dtl_context{local_scopes = [
										   [{'forloop', erl_syntax:variable("Counters")} | lists:map(
																     fun({identifier, _, Iterator}) ->
																	     {Iterator, erl_syntax:variable(lists:concat(["Var_", Iterator]))} 
																     end, IteratorList)] | Context#dtl_context.local_scopes]}, TreeWalker),
    CounterAst = erl_syntax:application(erl_syntax:atom(erlydtl_runtime), 
					erl_syntax:atom(increment_counter_stats), [erl_syntax:variable("Counters")]),

    {{LoopValueAst, LoopValueInfo}, TreeWalker2} = value_ast(LoopValue, false, true, Context, TreeWalker1),

    LoopValueAst0 = case IsReversed of
			true -> erl_syntax:application(erl_syntax:atom(lists), erl_syntax:atom(reverse), [LoopValueAst]);
			false -> LoopValueAst
		    end,

    CounterVars0 = case resolve_scoped_variable_ast('forloop', Context) of
		       undefined ->
			   erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(init_counter_stats), [LoopValueAst0]);
		       Value ->
			   erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(init_counter_stats), [LoopValueAst0, Value])
		   end,
    {{erl_syntax:case_expr(
	erl_syntax:application(
	  erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('forloop'),
	  [erl_syntax:fun_expr([
                                erl_syntax:clause([erl_syntax:tuple(Vars), erl_syntax:variable("Counters")], none, 
						  [erl_syntax:tuple([InnerAst, CounterAst])]),
                                erl_syntax:clause(case Vars of [H] -> [H, erl_syntax:variable("Counters")];
						      _ -> [erl_syntax:list(Vars), erl_syntax:variable("Counters")] end, none, 
						  [erl_syntax:tuple([InnerAst, CounterAst])])
			       ]),
	   CounterVars0, LoopValueAst0]),
	[erl_syntax:clause(
	   [erl_syntax:tuple([erl_syntax:underscore(), 
			      erl_syntax:list([erl_syntax:tuple([erl_syntax:atom(counter), erl_syntax:integer(1)])], 
					      erl_syntax:underscore())])],
	   none, [EmptyContentsAst]),
	 erl_syntax:clause(
	   [erl_syntax:tuple([erl_syntax:variable("L"), erl_syntax:underscore()])],
	   none, [erl_syntax:variable("L")])]
       ),
      merge_info(merge_info(Info, EmptyContentsInfo), LoopValueInfo)
     }, TreeWalker2}.

ifchanged_values_ast(Values, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    ValueAstFun = fun(Expr, {LTreeWalker, LInfo, Acc}) ->
                          {{EAst, EInfo}, ETw} = value_ast(Expr, false, true, Context, LTreeWalker),
                          {ETw, merge_info(LInfo, EInfo), [erl_syntax:tuple([erl_syntax:integer(erlang:phash2(Expr)), EAst])|Acc]} end,
    {TreeWalker1, MergedInfo, Changed} = lists:foldl(ValueAstFun, {TreeWalker, Info,  []}, Values),
    {{erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(ifchanged), [erl_syntax:list(Changed)]),
			   [erl_syntax:clause([erl_syntax:atom(true)], none,
					      [IfContentsAst]),
			    erl_syntax:clause([erl_syntax:underscore()], none,
					      [ElseContentsAst])
			   ]), MergedInfo}, TreeWalker1}.

ifchanged_contents_ast(Contents, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, _Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    Key = erl_syntax:integer(erlang:phash2(Contents)),
    {{erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(ifchanged), [erl_syntax:list([erl_syntax:tuple([Key, IfContentsAst])])]),
			   [erl_syntax:clause([erl_syntax:atom(true)], none,
					      [IfContentsAst]),
			    erl_syntax:clause([erl_syntax:underscore()], none,
					      [ElseContentsAst])
			   ]), Info}, TreeWalker}.


cycle_ast(Names, Context, TreeWalker) ->
    {NamesTuple, VarNames} = lists:mapfoldl(fun
						({string_literal, _, Str}, VarNamesAcc) ->
						   {{S, _}, _} = string_ast(unescape_string_literal(Str), Context, TreeWalker),
						   {S, VarNamesAcc};
						({variable, _}=Var, VarNamesAcc) ->
						   {V, VarName} = resolve_variable_ast(Var, Context, true),
						   {V, [VarName|VarNamesAcc]};
						({number_literal, _, Num}, VarNamesAcc) ->
						   {format(erl_syntax:integer(Num), Context, TreeWalker), VarNamesAcc};
						(_, VarNamesAcc) ->
						   {[], VarNamesAcc}
					   end, [], Names),
    {{erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [erl_syntax:tuple(NamesTuple), erl_syntax:variable("Counters")]), #ast_info{ var_names = VarNames }}, TreeWalker}.

%% Older Django templates treat cycle with comma-delimited elements as strings
cycle_compat_ast(Names, Context, TreeWalker) ->
    NamesTuple = lists:map(fun
			       ({identifier, _, X}) ->
				  {{S, _}, _} = string_ast(X, Context, TreeWalker),
				  S
			  end, Names),
    {{erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [erl_syntax:tuple(NamesTuple), erl_syntax:variable("Counters")]), #ast_info{}}, TreeWalker}.

now_ast(FormatString, Context, TreeWalker) ->
						% Note: we can't use unescape_string_literal here
						% because we want to allow escaping in the format string.
						% We only want to remove the surrounding escapes,
						% i.e. \"foo\" becomes "foo"
    UnescapeOuter = string:strip(FormatString, both, 34),
    {{StringAst, Info}, TreeWalker1} = string_ast(UnescapeOuter, Context, TreeWalker),
    {{erl_syntax:application(
        erl_syntax:atom(erlydtl_dateformat),
        erl_syntax:atom(format),
        [StringAst]), Info}, TreeWalker1}.

spaceless_ast(Contents, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = body_ast(Contents, Context, TreeWalker),
    {{erl_syntax:application(
	erl_syntax:atom(erlydtl_runtime),
	erl_syntax:atom(spaceless),
	[Ast]), Info}, TreeWalker1}.

unescape_string_literal(String) ->
    unescape_string_literal(string:strip(String, both, 34), [], noslash).

unescape_string_literal([], Acc, noslash) ->
    lists:reverse(Acc);
unescape_string_literal([$\\ | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, Acc, slash);
unescape_string_literal([C | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, [C | Acc], noslash);
unescape_string_literal("n" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\n | Acc], noslash);
unescape_string_literal("r" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\r | Acc], noslash);
unescape_string_literal("t" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\t | Acc], noslash);
unescape_string_literal([C | Rest], Acc, slash) ->
    unescape_string_literal(Rest, [C | Acc], noslash).


full_path(File, DocRoot) ->
    case filename:absname(File) of
        File -> File;
        _ -> filename:join([DocRoot, File])
    end.

%%-------------------------------------------------------------------
%% Custom tags
%%-------------------------------------------------------------------

tag_ast(Name, Args, Context, TreeWalker) ->
    {{InterpretedArgs, AstInfo1}, TreeWalker1} = 
        lists:foldr(
          fun ({{identifier, _, Key}, {trans, StringLiteral}}, {{ArgsAcc, AstInfoAcc}, TreeWalkerAcc}) ->
                  {{TransAst, TransAstInfo}, TreeWalker0} = translated_ast(StringLiteral, Context, TreeWalkerAcc),
                  {{[erl_syntax:tuple([erl_syntax:atom(Key), TransAst])|ArgsAcc], merge_info(TransAstInfo, AstInfoAcc)}, TreeWalker0};
              ({{identifier, _, Key}, Value}, {{ArgsAcc, AstInfoAcc}, TreeWalkerAcc}) ->
                  {{Ast0, AstInfo0}, TreeWalker0} = value_ast(Value, false, false, Context, TreeWalkerAcc),
                  {{[erl_syntax:tuple([erl_syntax:atom(Key), Ast0])|ArgsAcc], merge_info(AstInfo0, AstInfoAcc)}, TreeWalker0};
              ({extension, Tag}, {{ArgsAcc, AstInfoAcc}, TreeWalkerAcc}=Acc) ->
                  case call_extension(Context, compile_ast, [Tag, Context, TreeWalkerAcc]) of
                      undefined -> Acc;
                      {{ExtAst, ExtInfo}, ExtTreeWalker} ->
                          {{[ExtAst|ArgsAcc], merge_info(ExtInfo, AstInfoAcc)}, ExtTreeWalker}
                  end
          end, {{[], #ast_info{}}, TreeWalker}, Args),
    {RenderAst, RenderInfo} = custom_tags_modules_ast(Name, InterpretedArgs, Context),
    {{RenderAst, merge_info(AstInfo1, RenderInfo)}, TreeWalker1}.

custom_tags_modules_ast(Name, InterpretedArgs, #dtl_context{ custom_tags_modules = [], is_compiling_dir = false }) ->
    {erl_syntax:application(none, erl_syntax:atom(render_tag),
			    [erl_syntax:atom(Name), erl_syntax:list(InterpretedArgs),
			     erl_syntax:variable("RenderOptions")]),
     #ast_info{custom_tags = [Name]}};
custom_tags_modules_ast(Name, InterpretedArgs, #dtl_context{ custom_tags_modules = [], is_compiling_dir = true, module = Module }) ->
    {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Name),
			    [erl_syntax:list(InterpretedArgs), erl_syntax:variable("RenderOptions")]),
     #ast_info{ custom_tags = [Name] }};
custom_tags_modules_ast(Name, InterpretedArgs, #dtl_context{ custom_tags_modules = [Module|Rest] } = Context) ->
    try lists:max([I || {N,I} <- Module:module_info(exports), N =:= Name]) of
        2 ->
            {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Name),
				    [erl_syntax:list(InterpretedArgs),
				     erl_syntax:variable("RenderOptions")]), #ast_info{}};
        1 ->
            {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Name),
				    [erl_syntax:list(InterpretedArgs)]), #ast_info{}};
        I ->
            throw({unsupported_custom_tag_fun, {Module, Name, I}})
    catch _:function_clause ->
	    custom_tags_modules_ast(Name, InterpretedArgs,
				    Context#dtl_context{ custom_tags_modules = Rest })
    end.

print(true, Fmt, Args) ->
    io:format(Fmt, Args);
print(_, _Fmt, _Args) ->
    ok.

call_ast(Module, TreeWalkerAcc) ->
    call_ast(Module, erl_syntax:variable("_Variables"), #ast_info{}, TreeWalkerAcc).

call_with_ast(Module, Variable, Context, TreeWalker) ->
    {VarAst, VarName} = resolve_variable_ast(Variable, Context, false),
    call_ast(Module, VarAst, #ast_info{var_names=[VarName]}, TreeWalker).

call_ast(Module, Variable, AstInfo, TreeWalker) ->
    AppAst = erl_syntax:application(
	       erl_syntax:atom(Module),
	       erl_syntax:atom(render),
	       [Variable, erl_syntax:variable("RenderOptions")]),
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
    with_dependencies(Module:dependencies(), {{CallAst, AstInfo}, TreeWalker}).
