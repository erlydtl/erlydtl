%%%-------------------------------------------------------------------
%%% File:      erlydtl_compiler_utils.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @copyright 2014 Andreas Stenius
%%% @doc
%%% ErlyDTL template compiler utils.
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
-module(erlydtl_compiler_utils).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').
-author('Andreas Stenius <kaos@astekk.se>').

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

-export([
         add_error/3, add_errors/2,
         add_filters/2, add_tags/2,
         add_warning/3, add_warnings/2,
         call_extension/3,
         format_error/1,
         full_path/2,
         get_current_file/1,
         init_treewalker/1,
         load_library/2,
         load_library/3,
         load_library/4,
         merge_info/2,
         print/3,
         to_string/2,
         unescape_string_literal/1,
         reset_parse_trail/2,
         resolve_variable/2,
         resolve_variable/3,
         push_scope/2,
         restore_scope/2,
         begin_scope/1,
         begin_scope/2,
         end_scope/4,
         empty_scope/0
        ]).

-include("erlydtl_ext.hrl").


%% --------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------

init_treewalker(Context) ->
    TreeWalker = #treewalker{ context=Context },
    case call_extension(Context, init_treewalker, [TreeWalker]) of
        {ok, TW} when is_record(TW, treewalker) -> TW;
        undefined -> TreeWalker
    end.

to_string(Arg, #dtl_context{ binary_strings = true }) ->
    to_binary_string(Arg);
to_string(Arg, #dtl_context{ binary_strings = false }) ->
    to_list_string(Arg).

unescape_string_literal(String) ->
    unescape_string_literal(string:strip(String, both, 34), [], noslash).

full_path(File, DocRoot) ->
    case filename:absname(File) of
        File -> File;
        _ -> filename:join([DocRoot, File])
    end.

print(Fmt, Args, #dtl_context{ verbose = true }) -> io:format(Fmt, Args);
print(_Fmt, _Args, _Context) -> ok.

get_current_file(#treewalker{ context=Context }) ->
    get_current_file(Context);
get_current_file(#dtl_context{ parse_trail=[File|_] }) -> File;
get_current_file(#dtl_context{ doc_root=Root }) -> Root.

add_error(Module, Error, #treewalker{ context=Context }=TreeWalker) ->
    TreeWalker#treewalker{ context=add_error(Module, Error, Context) };
add_error(Module, Error, #dtl_context{
                    errors=#error_info{ report=Report, list=Es }=Ei
                   }=Context) ->
    Item = get_error_item(
             Report, "",
             get_current_file(Context),
             Error, Module),
    Context#dtl_context{
      errors=Ei#error_info{ list=[Item|Es] }
     }.

add_errors(Errors, #treewalker{ context=Context }=TreeWalker) ->
    TreeWalker#treewalker{ context=add_errors(Errors, Context) };
add_errors(Errors, Context) ->
    lists:foldl(
      fun (E, C) -> add_error(?MODULE, E, C) end,
      Context, Errors).

add_warning(Module, Warning, #treewalker{ context=Context }=TreeWalker) ->
    TreeWalker#treewalker{ context=add_warning(Module, Warning, Context) };
add_warning(Module, Warning, #dtl_context{ warnings=warnings_as_errors }=Context) ->
    add_error(Module, Warning, Context);
add_warning(Module, Warning, #dtl_context{
                        warnings=#error_info{ report=Report, list=Ws }=Wi
                       }=Context) ->
    Item = get_error_item(
             Report, "Warning: ",
             get_current_file(Context),
             Warning, Module),
    Context#dtl_context{
      warnings=Wi#error_info{ list=[Item|Ws] }
     }.

add_warnings(Warnings, #treewalker{ context=Context }=TreeWalker) ->
    TreeWalker#treewalker{ context=add_warnings(Warnings, Context) };
add_warnings(Warnings, Context) ->
    lists:foldl(
      fun (W, C) -> add_warning(?MODULE, W, C) end,
      Context, Warnings).

call_extension(#treewalker{ context=Context }, Fun, Args) ->
    call_extension(Context, Fun, Args);
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

resolve_variable(VarName, TreeWalker) ->
    resolve_variable(VarName, undefined, TreeWalker).

resolve_variable(VarName, Default, #treewalker{ context=Context }) ->
    resolve_variable1(Context#dtl_context.local_scopes, VarName, Default).

push_scope(Scope, #treewalker{ context=Context }=TreeWalker) ->
    TreeWalker#treewalker{ context=push_scope(Scope, Context) };
push_scope(Scope, #dtl_context{ local_scopes=Scopes }=Context) ->
    Context#dtl_context{ local_scopes=[Scope|Scopes] }.

restore_scope(Target, #treewalker{ context=Context }=TreeWalker) ->
    TreeWalker#treewalker{ context=restore_scope(Target, Context) };
restore_scope(#treewalker{ context=Target }, Context) ->
    restore_scope(Target, Context);
restore_scope(#dtl_context{ local_scopes=Scopes }, Context) ->
    Context#dtl_context{ local_scopes=Scopes }.

begin_scope(TreeWalker) -> begin_scope(empty_scope(), TreeWalker).

begin_scope({Scope, Values}, TreeWalker) ->
    Id = make_ref(),
    {Id, push_scope({Id, Scope, Values}, TreeWalker)}.

end_scope(Fun, Id, AstList, TreeWalker) ->
    close_scope(Fun, Id, AstList, TreeWalker).

empty_scope() -> {[], []}.

reset_parse_trail(ParseTrail, #treewalker{ context=Context }=TreeWalker) ->
    TreeWalker#treewalker{ context=reset_parse_trail(ParseTrail, Context) };
reset_parse_trail(ParseTrail, Context) ->
    Context#dtl_context{ parse_trail=ParseTrail }.

load_library(Lib, Context) -> load_library(none, Lib, [], Context).
load_library(Pos, Lib, Context) -> load_library(Pos, Lib, [], Context).

load_library(Pos, Lib, Accept, #treewalker{ context=Context }=TreeWalker) ->
    TreeWalker#treewalker{ context=load_library(Pos, Lib, Accept, Context) };
load_library(Pos, Lib, Accept, Context) ->
    case lib_module(Lib, Context) of
        {ok, Mod} ->
            add_filters(
              [{Name, lib_function(Mod, Filter)}
               || {Name, Filter} <- Mod:inventory(filters),
                  Accept =:= [] orelse lists:member(Name, Accept)
              ],
              add_tags(
                [{Name, lib_function(Mod, Tag)}
                 || {Name, Tag} <- Mod:inventory(tags),
                    Accept =:= [] orelse lists:member(Name, Accept)
                ],
                Context));
        Error ->
            ?WARN({Pos, Error}, Context)
    end.

add_filters(Load, #dtl_context{ filters=Filters }=Context) ->
    Context#dtl_context{ filters=Load ++ Filters }.

add_tags(Load, #dtl_context{ tags=Tags }=Context) ->
    Context#dtl_context{ tags=Load ++ Tags }.

format_error({load_library, Name, Mod, Reason}) ->
    io_lib:format("Failed to load library '~s' from '~s' (~s)", [Name, Mod, Reason]);
format_error({unknown_extension, Tag}) ->
    io_lib:format("Unhandled extension: ~p", [Tag]);
format_error(Other) ->
    io_lib:format("## Error description for ~p not implemented.", [Other]).


%%====================================================================
%% Internal functions
%%====================================================================

to_binary_string(Arg) when is_binary(Arg) -> Arg;
to_binary_string(Arg) when is_list(Arg) -> list_to_binary(Arg);
to_binary_string(Arg) when is_integer(Arg) ->
    case erlang:function_exported(erlang, integer_to_binary, 1) of
        true -> erlang:integer_to_binary(Arg);
        false -> list_to_binary(integer_to_list(Arg))
    end;
to_binary_string(Arg) when is_atom(Arg) -> atom_to_binary(Arg, latin1).

to_list_string(Arg) when is_list(Arg) -> Arg;
to_list_string(Arg) when is_binary(Arg) -> binary_to_list(Arg);
to_list_string(Arg) when is_integer(Arg) -> integer_to_list(Arg);
to_list_string(Arg) when is_atom(Arg) -> atom_to_list(Arg).

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

get_error_item(Report, Prefix, File, Error, DefaultModule) ->
    case compose_error_desc(Error, DefaultModule) of
        {Pos, Module, ErrorDesc} ->
            new_error_item(Report, Prefix, File, Pos, Module, ErrorDesc);
        ErrorItem ->
            ErrorItem
    end.

compose_error_desc({{Line, Col}=Pos, ErrorDesc}, Module)
  when is_integer(Line), is_integer(Col), is_atom(Module) ->
    {Pos, Module, ErrorDesc};
compose_error_desc({Line, ErrorDesc}, Module)
  when is_integer(Line) ->
    {Line, Module, ErrorDesc};
compose_error_desc({{Line, Col}, Module, _}=ErrorDesc, _)
  when is_integer(Line), is_integer(Col), is_atom(Module) ->
    ErrorDesc;
compose_error_desc({Line, Module, _}=ErrorDesc, _)
  when is_integer(Line), is_atom(Module) ->
    ErrorDesc;
compose_error_desc({_, InfoList}=ErrorDesc, _)
  when is_list(InfoList) -> ErrorDesc;
compose_error_desc(ErrorDesc, Module) ->
    {none, Module, ErrorDesc}.

new_error_item(Report, Prefix, File, Pos, Module, ErrorDesc) ->
    if Report  ->
            io:format("~s:~s~s~s~n",
                      [File, pos_info(Pos), Prefix,
                       Module:format_error(ErrorDesc)]);
       true -> nop
    end,
    {File, [{Pos, Module, ErrorDesc}]}.

pos_info(none) -> " ";
pos_info(Line) when is_integer(Line) ->
    io_lib:format("~b: ", [Line]);
pos_info({Line, Col}) when is_integer(Line), is_integer(Col) ->
    io_lib:format("~b:~b ", [Line, Col]).

resolve_variable1([], _VarName, Default) -> Default;
resolve_variable1([Scope|Scopes], VarName, Default) ->
    case proplists:get_value(VarName, get_scope(Scope), Default) of
        Default ->
            resolve_variable1(Scopes, VarName, Default);
        Value -> Value
    end.

get_scope({_Id, Scope, _Values}) -> Scope;
get_scope(Scope) -> Scope.

close_scope(Fun, Id, AstList, TreeWalker) ->
    case merge_scopes(Id, TreeWalker) of
        {[], TreeWalker1} -> {AstList, TreeWalker1};
        {Values, TreeWalker1} ->
            {lists:foldl(
               fun ({ScopeId, ScopeValues}, AstAcc) ->
                       {Pre, Target, Post} = split_ast(ScopeId, AstAcc),
                       Pre ++ Fun(ScopeValues ++ Target) ++ Post
               end,
               AstList, Values),
             TreeWalker1}
    end.

merge_scopes(Id, #treewalker{ context=Context }=TreeWalker) ->
    {Values, Scopes} = merge_scopes(Id, Context#dtl_context.local_scopes, []),
    {lists:reverse(Values),
     TreeWalker#treewalker{
       context=Context#dtl_context{
                 local_scopes = Scopes
                } }}.

merge_scopes(Id, [{Id, _Scope, []}|Scopes], Acc) -> {Acc, Scopes};
merge_scopes(Id, [{Id, _Scope, Values}|Scopes], Acc) -> {[{Id, Values}|Acc], Scopes};
merge_scopes(Id, [{_ScopeId, _Scope, []}|Scopes], Acc) ->
    merge_scopes(Id, Scopes, Acc);
merge_scopes(Id, [{ScopeId, _Scope, Values}|Scopes], Acc) ->
    merge_scopes(Id, Scopes, [{ScopeId, Values}|Acc]);
merge_scopes(Id, [_PlainScope|Scopes], Acc) ->
    merge_scopes(Id, Scopes, Acc).


split_ast(Id, AstList) ->
    split_ast(Id, AstList, []).

split_ast(_Split, [], {Pre, Acc}) ->
    {Pre, lists:reverse(Acc), []};
split_ast(_Split, [], Acc) ->
    {[], lists:reverse(Acc), []};
split_ast(Split, [Split|Rest], {Pre, Acc}) ->
    {Pre, lists:reverse(Acc), Rest};
split_ast(Split, [Split|Rest], Acc) ->
    split_ast(end_scope, Rest, {lists:reverse(Acc), []});
split_ast(Split, [Ast|Rest], {Pre, Acc}) ->
    split_ast(Split, Rest, {Pre, [Ast|Acc]});
split_ast(Split, [Ast|Rest], Acc) ->
    split_ast(Split, Rest, [Ast|Acc]).

lib_module(Name, #dtl_context{ libraries=Libs }) ->
    Mod = proplists:get_value(Name, Libs, Name),
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            IsLib = case proplists:get_value(behaviour, Mod:module_info(attributes)) of
                        Behaviours when is_list(Behaviours) ->
                            lists:member(erlydtl_library, Behaviours);
                        _ -> false
                    end,
            if IsLib -> {ok, Mod};
               true -> {load_library, Name, Mod, "not a library"}
            end;
        {error, Reason} ->
            {load_library, Name, Mod, Reason}
    end.

lib_function(_, {Mod, Fun}) ->
    lib_function(Mod, Fun);
lib_function(Mod, Fun) ->
    %% TODO: we can check for lib function availability here.. (sanity check)
    {Mod, Fun}.
