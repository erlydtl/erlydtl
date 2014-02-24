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
         add_warning/3, add_warnings/2,
         call_extension/3,
         format_error/1,
         full_path/2,
         get_current_file/1,
         init_treewalker/1,
         merge_info/2,
         print/3,
         to_string/2,
         unescape_string_literal/1
        ]).

-include("erlydtl_ext.hrl").


%% --------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------

init_treewalker(Context) ->
    TreeWalker = #treewalker{},
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

get_current_file(#dtl_context{ parse_trail=[File|_] }) -> File;
get_current_file(#dtl_context{ doc_root=Root }) -> Root.

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

add_errors(Errors, Context) ->
    lists:foldl(
      fun (E, C) -> add_error(?MODULE, E, C) end,
      Context, Errors).

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

add_warnings(Warnings, Context) ->
    lists:foldl(
      fun (W, C) -> add_warning(?MODULE, W, C) end,
      Context, Warnings).

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
