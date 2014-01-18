%% Copyright (c) 2006, Torbjorn Tornkvist, tobbe@tornkvist.org
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without 
%% modification, are permitted provided that the following conditions are met:
%% 
%%     * Redistributions of source code must retain the above copyright 
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright 
%%       notice, this list of conditions and the following disclaimer in the 
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of "Torbjorn Tornkvist" nor the names of any other
%%       contributors may be used to endorse or promote products derived from
%%       this software without specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
%% POSSIBILITY OF SUCH DAMAGE.

%%%----------------------------------------------------------------------
%%% Created:  27 Oct 2003 by tobbe@bluetail.com
%%% Function: Tools for multi-lingual capabilities,
%%%           similar to GNU gettext.
%%%----------------------------------------------------------------------
%%% Modified: 2010-05-18 by marc@worrell.nl
%%%
%%% Adaptations for Zotonic. 
%%% Original code is at http://github.com/noss/erlang-gettext
%%%----------------------------------------------------------------------
%%% Adapted for erlydtl, 2014-01-18 by Andreas Stenius <git@astekk.se>
%%% From: https://raw.github.com/zotonic/zotonic/4b2ec1486aaa67d758f43fbf38fd7da58e7cdfdc/src/i18n/z_gettext.erl
%%%----------------------------------------------------------------------
-module(po_scanner).

-export([parse_po_file/1, parse_po/1, test/0]).


-define(GETTEXT_HEADER_INFO, header).


%%% --------------------------------------------------------------------
%%% Parse a PO-file
%%% --------------------------------------------------------------------

parse_po_file(Fname) ->
    case file:read_file(Fname) of
        {ok, Bin} -> 
            parse_po(Bin);
        {error, Reason} ->
            io:format(
              "Error reading po file ~s: ~s~n",
              [Fname, file:format_error(Reason)]),
            []
    end.

parse_po(Bin) when is_binary(Bin) ->
    parse_po(to_list(Bin));
parse_po(List) when is_list(List) ->
    lists:reverse(
      lists:foldl(fun ({"", R}, AccIn) ->
                          [{?GETTEXT_HEADER_INFO, R}|AccIn];
                      ({_, ""}, AccIn) ->
                          AccIn;
                      (R, AccIn) ->
                          [R|AccIn]
                  end,
                  [],
                  parse_po_list(List))).

parse_po_list("msgid" ++ T) ->
    {Key, R0} = get_po_string(T),
    {Val, Rest} = get_msgstr(R0),
    [{Key,Val} | parse_po_list(Rest)];
parse_po_list([_ | T]) ->
    parse_po_list(T);
parse_po_list([]) ->
    [].

get_msgstr("msgstr" ++ T) ->
    get_po_string(T);
get_msgstr([_ | T]) ->
    get_msgstr(T).

%%%
%%% A PO-string has the same syntax as a C character string.
%%% For example:
%%%
%%%   msgstr ""
%%%     "Hello "
%%%
%%%     "\\World\n"
%%%
%%% Is parsed as: "Hello \World\n"
%%%
get_po_string([$\s|T]) -> get_po_string(T);
get_po_string([$\r|T]) -> get_po_string(T);
get_po_string([$\n|T]) -> get_po_string(T);
get_po_string([$\t|T]) -> get_po_string(T);
get_po_string([$"|T])  -> eat_string(T).

eat_string(S) ->
    eat_string(S,[]).

eat_string([$\\,$"|T], Acc)   -> eat_string(T, [$"|Acc]);   % unescape !
eat_string([$\\,$\\ |T], Acc) -> eat_string(T, [$\\|Acc]);  % unescape !
eat_string([$\\,$n |T], Acc)  -> eat_string(T, [$\n|Acc]);  % unescape !
eat_string([$"|T], Acc)       -> eat_more(T,Acc);
eat_string([H|T], Acc)        -> eat_string(T, [H|Acc]).

eat_more([$\s|T], Acc) -> eat_more(T, Acc);
eat_more([$\n|T], Acc) -> eat_more(T, Acc);
eat_more([$\r|T], Acc) -> eat_more(T, Acc);
eat_more([$\t|T], Acc) -> eat_more(T, Acc);
eat_more([$"|T], Acc)  -> eat_string(T, Acc);
eat_more(T, Acc)       -> {lists:reverse(Acc), T}.


to_list(A) when is_atom(A)    -> atom_to_list(A);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(B) when is_binary(B)  -> binary_to_list(B);
to_list(L) when is_list(L)    -> L.

test() ->

    X = parse_po(<<"msgid \"\"
msgstr \"header value\"">>),

[{header, "header value"}] = X,

    X2 = parse_po(<<"msgid \"\"
msgstr \"header value\"

msgid \"en\"
msgstr \"nl\"
">>),

[{header, "header value"}, {"en", "nl"}] = X2,

    X3 = parse_po(<<"msgid \"\"
msgstr \"header value\"

msgid \"en\"
msgstr \"nl\"

msgid \"empty trans\"
msgstr \"\"
">>),

[{header, "header value"}, {"en", "nl"}, {"empty trans", "empty trans"}] = X3.
