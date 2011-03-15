%%%-------------------------------------------------------------------
%%% File:      erlydtl_filters.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc 
%%% Template filters
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
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(erlydtl_filters).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').
-author('drew dot gulino at google dot com').
 
-define(TEST,"").
%-define(NOTEST,1).
-define(NODEBUG,1).
-include_lib("eunit/include/eunit.hrl").
 
-ifdef(TEST).
        -export([cast_to_float/1,cast_to_integer/1,stringformat_io/7,round/2,unjoin/2]).
-endif.
 

-export([add/2, 
        addslashes/1,
        capfirst/1, 
        center/2, 
        cut/2,
        date/2, 
        default/2,
        default_if_none/2, 
        %dictsort/,
        %dictsortreversed/,
        divisibleby/2,
        %escape/,
        escapejs/1,
        filesizeformat/1,
        first/1, 
        fix_ampersands/1, 
        floatformat/2,
        force_escape/1, 
        format_integer/1, 
        format_number/1,
        get_digit/2,
        %iriencode/1,
        join/2, 
        last/1, 
        length/1, 
        length_is/2, 
        linebreaks/1,
        linebreaksbr/1, 
        linenumbers/1,
        ljust/2,
        lower/1, 
        make_list/1, 
        phone2numeric/1,
        pluralize/1,
        pluralize/2,
        %pprint/,
        random/1,
        random_num/1,
        random_range/1,
        removetags/2,
        rjust/2, 
        %safe/,
        %safeseq/,
        slice/2,
        slugify/1,
        stringformat/2,
        %striptags/,
        time/2,
        %timesince/,
        %timeuntil/,
        title/1,
        truncatewords/2, 
        %truncatewords_html/,
        %unordered_list/,
        upper/1, 
        urlencode/1,
        %urlize/,
        %urlizetrunc/,
        wordcount/1,
        wordwrap/2,
        yesno/2]).

-define(NO_ENCODE(C), ((C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse
        (C =:= $\. orelse C =:= $- 
        orelse C =:= $~ orelse C =:= $_))).

-define(KILOBYTE, 1024).
-define(MEGABYTE, (1024 * ?KILOBYTE)).
-define(GIGABYTE, (1024 * ?MEGABYTE)).

%% @doc Adds a number to the value.
add(Input, Number) when is_binary(Input) ->
    list_to_binary(add(binary_to_list(Input), Number));
add(Input, Number) when is_list(Input) ->
    integer_to_list(add(list_to_integer(Input), Number));
add(Input, Number) when is_integer(Input) ->
    Input + Number.

%% @doc Adds slashes before quotes.
addslashes(Input) when is_binary(Input) ->
    addslashes(binary_to_list(Input));
addslashes(Input) when is_list(Input) ->
    addslashes(Input, []).

%% @doc Capitalizes the first character of the value.
capfirst([H|T]) when H >= $a andalso H =< $z ->
    [H + $A - $a | T];
capfirst(<<Byte:8/integer, Binary/binary>>) when Byte >= $a andalso Byte =< $z ->
    [(Byte + $A - $a)|binary_to_list(Binary)].

%% @doc Centers the value in a field of a given width.
center(Input, Number) when is_binary(Input) ->
    list_to_binary(center(binary_to_list(Input), Number));
center(Input, Number) when is_list(Input) ->
    string:centre(Input, Number).

%% @doc Removes all values of arg from the given string.
cut(Input, Arg) when is_binary(Input) ->
    cut(binary_to_list(Input), Arg);
cut(Input, [Char]) when is_list(Input) ->
    cut(Input, Char, []).

%% @doc Formats a date according to the given format.
date(Input, FormatStr) when is_binary(Input) ->
    list_to_binary(date(binary_to_list(Input), FormatStr));
date(Input, "") ->
    date(Input, "F j, Y");
date({{_,_,_} = Date,{_,_,_} = Time}, FormatStr) ->
    erlydtl_dateformat:format({Date, Time}, FormatStr);
date({_,_,_} = Date, FormatStr) ->
    erlydtl_dateformat:format(Date, FormatStr);
date(Input, _FormatStr) when is_list(Input) ->
    io:format("Unexpected date parameter : ~p~n", [Input]),
    "".

%% @doc If value evaluates to `false', use given default. Otherwise, use the value.
default(Input, Default) ->
    case erlydtl_runtime:is_false(Input) of
        true -> Default;
        false -> Input
    end.

%% @doc If (and only if) value is `undefined', use given default. Otherwise, use the value.
default_if_none(undefined, Default) ->
    Default;
default_if_none(Input, _) ->
    Input.

%% @doc Returns `true' if the value is divisible by the argument.
divisibleby(Input, Divisor) when is_binary(Input) ->
    divisibleby(binary_to_list(Input), Divisor);
divisibleby(Input, Divisor) when is_list(Input) ->
    divisibleby(list_to_integer(Input), Divisor);
divisibleby(Input, Divisor) when is_list(Divisor) ->
    divisibleby(Input, list_to_integer(Divisor));
divisibleby(Input, Divisor) when is_integer(Input), is_integer(Divisor) ->
    Input rem Divisor =:= 0.

%% @doc Escapes 
 
%% @doc Escapes characters for use in JavaScript strings.
escapejs(Input) when is_binary(Input) ->
    escapejs(binary_to_list(Input));
escapejs(Input) when is_list(Input) ->
    escapejs(Input, []).

%% @doc Format the value like a human-readable file size.
filesizeformat(Input) when is_binary(Input) ->
    filesizeformat(binary_to_list(Input));
filesizeformat(Input) when is_list(Input) ->
    filesizeformat(list_to_integer(Input));
filesizeformat(Bytes) when is_integer(Bytes), Bytes >= ?GIGABYTE->
    filesizeformat(Bytes / ?GIGABYTE, "GB");
filesizeformat(Bytes) when is_integer(Bytes), Bytes >= ?MEGABYTE ->
    filesizeformat(Bytes / ?MEGABYTE, "MB");
filesizeformat(Bytes) when is_integer(Bytes), Bytes >= ?KILOBYTE ->
    filesizeformat(Bytes / ?KILOBYTE, "KB");
filesizeformat(Bytes) when is_integer(Bytes) ->
    integer_to_list(Bytes) ++ " bytes".

%% @doc Returns the first item in a list.
first([First|_Rest]) ->
    [First];
first(<<First, _/binary>>) ->
    <<First>>.

%% @doc Replaces ampersands with &amp; entities.
fix_ampersands(Input) when is_binary(Input) ->
    fix_ampersands(Input, 0);
fix_ampersands(Input) when is_list(Input) ->
    fix_ampersands(Input, []).

%% @doc When used without an argument, rounds a floating-point number to one decimal place
%% @doc -- but only if there's a decimal part to be displayed
floatformat(Number, Place) when is_binary(Number) ->
    floatformat(binary_to_list(Number), Place);
floatformat(Number, Place) ->
    floatformat_io(Number, Place).

floatformat_io(Number, []) ->
    floatformat_io(Number, -1);
floatformat_io(Number, Precision) when Precision > 0 ->
    Format = lists:flatten(io_lib:format("~~.~Bf",[Precision])),
    [Result] = io_lib:format(Format,[Number]),
    Result;
floatformat_io(Number, Precision) when Precision < 0 ->   
    Round = erlang:round(Number),
    RoundPrecision = round(Number, -Precision),
    case RoundPrecision == Round of
        true ->
            %Format = lists:flatten(io_lib:format("~~~BB",[-Precision])),
            [Result] = io_lib:format("~B",[Round]);
        false ->
            Format = lists:flatten(io_lib:format("~~.~Bf",[-Precision])),
            [Result] = io_lib:format(Format,[RoundPrecision])
    end,
    Result.

round(Number, Precision) ->
    P = math:pow(10, Precision),
    round(Number * P) / P.
 
%% @doc Applies HTML escaping to a string.
force_escape(Input) when is_list(Input) ->
    escape(Input, []);
force_escape(Input) when is_binary(Input) ->
    escape(Input, 0).

format_integer(Input) when is_integer(Input) ->
    integer_to_list(Input);
format_integer(Input) ->
    Input.

format_number(Input) when is_integer(Input) ->
    integer_to_list(Input);
format_number(Input) when is_float(Input) ->
    io_lib:format("~p", [Input]);
format_number(Input) when is_function(Input, 0) ->
    format_number(Input());
format_number(Input) ->
    Input.

%% @doc Given a whole number, returns the requested digit, where 1 is the right-most digit.
get_digit(Input, Digit) when is_binary(Input) ->
    get_digit(binary_to_list(Input), Digit);
get_digit(Input, Digit) when is_integer(Input) ->
    get_digit(integer_to_list(Input), Digit);
get_digit(Input, Digit) when is_list(Digit) ->
    get_digit(Input, list_to_integer(Digit));
get_digit(Input, Digit) when Digit > erlang:length(Input) ->
    0;
get_digit(Input, Digit) when Digit > 0 ->
    lists:nth(Digit, lists:reverse(Input)) - $0;
get_digit(Input, _) ->
    Input.

%% @doc Joins a list with a given separator.
join(Input, Separator) when is_list(Input) ->
    join_io(Input, Separator).

%% @doc Returns the last item in a list.
last(Input) when is_binary(Input) ->
    case size(Input) of
        0 -> Input;
        N ->
            Offset = N - 1,
            <<_:Offset/binary, Byte/binary>> = Input,
            Byte
    end;
last(Input) when is_list(Input) ->
    [lists:last(Input)].

%% @doc Returns the length of the value.
length(Input) when is_list(Input) ->
    integer_to_list(erlang:length(Input));
length(Input) when is_binary(Input) ->
    integer_to_list(size(Input)).

%% @doc Returns True iff the value's length is the argument.
length_is(Input, Number) when is_list(Input), is_integer(Number) ->
    length_is(Input, integer_to_list(Number));
length_is(Input, Number) when is_list(Input), is_list(Number) ->
    ?MODULE:length(Input) =:= Number.

%% @doc Replaces line breaks in plain text with appropriate HTML
linebreaks(Input) when is_binary(Input) ->
    linebreaks(binary_to_list(Input));
% a bit of a hack; unlikely, but could replace "</p>\z\z<p>" literal by mistake 
linebreaks(Input) ->
    Input1 = re:replace(Input,"\r\n"       ,"<br />", [global,{return,list}]),
    Input2 = re:replace(Input1, "\n[\n\r]+"  ,"</p>\z\z<p>", [global,{return,list}]),
    Input3 = re:replace(Input2,"\r[\n\r]+"  ,"</p>\z\z<p>", [global,{return,list}]),
    Input4 = re:replace(Input3,"\n"         ,"<br />", [global,{return,list}]),
    Input5 = re:replace(Input4,"\r"         ,"<br />", [global,{return,list}]),
    Input6 = re:replace(Input5,"</p>\z\z<p>","</p>\n\n<p>", [global,{return,list}]),
    lists:flatten(["<p>", Input6,"</p>"]).

%% @doc Converts all newlines to HTML line breaks.
linebreaksbr(Input) when is_binary(Input) ->
    linebreaksbr(Input, 0);
linebreaksbr(Input) ->
    linebreaksbr(Input, []).

%% @doc Displays text with line numbers.
%% probably slow implementation
linenumbers(Input) when is_binary(Input) ->
         linenumbers(binary_to_list(Input));
linenumbers(Input) when is_list(Input) ->
         linenumbers_io(Input, [], 1).
 
%linenumbers_io(Input) ->
%         Lines = string:tokens(Input, "\n"),
%         Numbers = lists:seq(1, erlang:length(Lines)),
%         lists:concat(lists:zipwith(fun(Number, Line) -> erlang:integer_to_list(Number) ++ ". " ++ Line ++ "\n" end, Numbers, Lines)).

%linenumbers(Input) ->
%   linenumbers_io(Input, [], 1).

linenumbers_io([], Acc, _) ->
    lists:reverse(Acc);
linenumbers_io(Input, [], LineNumber) ->
    linenumbers_io(Input,
    lists:reverse(integer_to_list(LineNumber)++". "), LineNumber + 1);
linenumbers_io("\n"++Rest, Acc, LineNumber) ->
    linenumbers_io(Rest, lists:reverse("\n" ++ integer_to_list(LineNumber) ++ ". ") ++ Acc, LineNumber + 1);
linenumbers_io([H|T], Acc, LineNumber) ->
   linenumbers_io(T, [H|Acc], LineNumber).
 
%% @doc Displays text with line numbers.
%% tail recursive implementation
%% linenumbers(Input) when is_binary(Input) ->
%%       linenumbers_io(binary_to_list(Input), [], 0);
%% linenumbers(Input) when is_list(Input) ->
%%       linenumbers_io(Input, [], 0).
%%
%% linenumbers([], Acc, ) ->
%%     lists:reverse(Acc);
%% linenumbers([Head|Rest], Acc, CountAcc) ->
%%       Count = CountAcc + 1,
%%       LineNumber = integer_to_list(Count) ++ ". ",
%%     linenumbers(Rest, lists:append(LineNumber, Acc));
 
%% @doc Left-aligns the value in a field of a given width.
ljust(Input, Number) when is_binary(Input) ->
    list_to_binary(ljust(binary_to_list(Input), Number));
ljust(Input, Number) when is_list(Input) ->
    string:left(Input, Number).

%% @doc Converts a string into all lowercase.
lower(Input) when is_binary(Input) ->
    lower(Input, 0);
lower(Input) ->
    string:to_lower(Input).

%% @doc Returns the value turned into a list. For an integer, it's a list of digits. 
%% @doc For a string, it's a list of characters.
%% Added this for DTL compatibility, but since strings are lists in Erlang, no need for this.
make_list(Input) when is_binary(Input) ->
    make_list(binary_to_list(Input));
make_list(Input) ->
    unjoin(Input,"").
 
%% @doc Converts a phone number (possibly containing letters) to its numerical equivalent.
phone2numeric(Input) when is_binary(Input) ->
    phone2numeric(binary_to_list(Input));
phone2numeric(Input) when is_list(Input) ->
    phone2numeric(Input, []).

%% @doc Returns a plural suffix if the value is not 1. By default, this suffix is 's'.
pluralize(Number, Suffix) when is_binary(Suffix) ->
        pluralize_io(Number, binary_to_list(Suffix) );
pluralize(Number, Suffix) when is_list(Suffix) ->
        pluralize_io(Number, Suffix).
 
pluralize(Number) ->
        pluralize(Number, "s").
       
pluralize_io(Number, Suffix) ->
%%       io:format("Number, Suffix: ~p, ~p ~n", [Number, Suffix]),
        case lists:member($, , Suffix) of
                true ->
                        [Singular, Plural] = string:tokens(Suffix,","),
                        case Number > 1 of
                                false ->
                                        Singular;
                                true  ->
                                        Plural
                        end;
                false ->
                        case Number > 1 of
                                false ->
                                        [];
                                true  ->
                                        Suffix
                        end
        end.
 
%% @doc Returns a random item from the given list.
random(Input) when is_list(Input) ->
    lists:nth(random:uniform(erlang:length(Input)), Input);
random(_) ->
    "".

random_num(Value) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Rand = random:uniform(Value),
    Rand.

%% random tags to be used when using erlydtl in testing
random_range(Range) ->
    [Start, End] = string:tokens(Range,","),
    %?debugFmt("Start, End: ~p,~p~n",[Start,End]),
    random_range(cast_to_integer(Start),cast_to_integer(End)).

random_range(Start, End) when End >= Start ->
    %?debugFmt("Input, Start, End: ~p,~p,~p~n",[Input,Start,End]),
    Range = End - Start,
    Rand = random:uniform(Range),
    Num = Rand + Start,
    lists:flatten(io_lib:format("~B",[Num])).

removetags(Input, Tags) when is_binary(Input), is_binary(Tags) ->
    removetags(binary_to_list(Input), binary_to_list(Tags));
removetags(Input, Tags) ->
    TagList = string:tokens(Tags," "),
    TagListString = string:join(TagList,"|"),
    Regex = lists:flatten(io_lib:format("</?(~s)( |\n)?>",[TagListString])),
    Result = re:replace(Input,Regex,"", [global,{return,list}]),
    Result.
 
%% @doc Right-aligns the value in a field of a given width.
rjust(Input, Number) when is_binary(Input) ->
    list_to_binary(rjust(binary_to_list(Input), Number));
rjust(Input, Number) ->
    string:right(Input, Number).

%% @doc Returns a slice of the list.
slice(Input, Index) when is_binary(Input) ->
        erlydtl_slice:slice(binary_to_list(Input), Index);
slice(Input, Index) when is_list(Input) ->
        erlydtl_slice:slice(Input, Index).

%% regex  " ^([#0-\s+].)([0-9\*]+)(\.[0-9]+)([diouxXeEfFgGcrs]) " matches ALL of "-10.0f"
%% ([#0-\s+]?)([0-9\*]+)?(\.?)([0-9]?)([diouxXeEfFgGcrs])
%% @doc Returns a formatted string
stringformat(Input, Conversion) when is_binary(Input) ->
        stringformat(binary_to_list(Input), Conversion);
stringformat(Input, Conversion) ->
        ParsedConversion = re:replace(Conversion, "([\-#\+ ]?)([0-9\*]+)?(\.?)([0-9]?)([diouxXeEfFgGcrs])", "\\1 ,\\2 ,\\3 ,\\4 ,\\5 ", [{return,list}]),
        ?debugFmt("ParsedConversion: ~p~n", [ParsedConversion]),
        ParsedConversion1 = lists:map(fun(X) -> string:strip(X) end, string:tokens(ParsedConversion, ",")),
         [ConversionFlag, MinFieldWidth, Precision, PrecisionLength, ConversionType] = ParsedConversion1,
        ?debugFmt("ConversionFlag, MinFieldWidth, Precision, PrecisionLength, ConversionType: ~p, ~p, ~p, ~p, ~p ~n", [ConversionFlag, cast_to_integer(MinFieldWidth), Precision, cast_to_integer(PrecisionLength), ConversionType]),
        [String] = stringformat_io(Input, Conversion, ConversionFlag, cast_to_integer(MinFieldWidth), Precision, cast_to_integer(PrecisionLength), ConversionType),
        lists:flatten(String).

%% @doc
%% A conversion specifier contains two or more characters and has the following components, which must occur in this order:
%% 
%%    1. The "%" character, which marks the start of the specifier.
%%    2. Mapping key (optional), consisting of a parenthesised sequence of characters (for example, (somename)).
%%    3. Conversion flags (optional), which affect the result of some conversion types.
%%    4. Minimum field width (optional). If specified as an "*" (asterisk), the actual width is read from the next element of the tuple in values, and the object to convert comes after the minimum field width and optional precision.
%%    5. Precision (optional), given as a "." (dot) followed by the precision. If specified as "*" (an asterisk), the actual width is read from the next element of the tuple in values, and the value to convert comes after the precision.
%%    6. Length modifier (optional).
%%    7. Conversion type.
 
stringformat_io(Input, _Conversion, _ConversionFlag, [],
                                _Precision, _PrecisionLength, "s") when is_list(Input) ->
        Format = lists:flatten(io_lib:format("~~s", [])),
       io_lib:format(Format, [Input]);
stringformat_io(Input, _Conversion, ConversionFlag, MinFieldWidth,
                                _Precision, _PrecisionLength, "s") when is_list(Input) ->
        %Conversion = [ConversionFlag, MinFieldWidth, Precision, PrecisionLength, ConversionType],
        InputLength = erlang:length(Input),
        case erlang:abs(MinFieldWidth) < InputLength of
            true ->
                MFW = InputLength;
            false ->
                MFW = MinFieldWidth
        end,
        Format = lists:flatten(io_lib:format("~~~s~ps", [ConversionFlag,MFW])),
        io_lib:format(Format, [Input]);
stringformat_io(Input, _Conversion, _ConversionFlag, MinFieldWidth,
                                Precision, PrecisionLength, "f") when Precision == ".", MinFieldWidth == 0 ->
    Conversion1 = lists:concat(["","",Precision,PrecisionLength,"f"]),
    stringformat_io(Input, Conversion1, [], [], Precision, PrecisionLength, "f");
stringformat_io(Input, Conversion, ConversionFlag, MinFieldWidth,
                                Precision, "", "f") when Precision == "." ->
    Format = re:replace(Conversion, "f", "d", [{return, list}] ),
    stringformat_io(Input, Format, ConversionFlag, MinFieldWidth,
                                    Precision, "", "d");
stringformat_io(Input, Conversion, _ConversionFlag, _MinFieldWidth,
                                _Precision, _PrecisionLength, "f")->
        %Conversion = [ConversionFlag, MinFieldWidth, Precision, PrecisionLength, ConversionType],
        Format = "~" ++ Conversion,
        io_lib:format(Format, [cast_to_float(Input)]);
stringformat_io(Input, Conversion, _ConversionFlag, _MinFieldWidth,
                                [], [], "d")->
        %?debugMsg("plain d"),
        %Conversion = [ConversionFlag, MinFieldWidth, Precision, PrecisionLength, ConversionType],
        Format = "~" ++ re:replace(Conversion, "d", "B", [{return, list}] ),
        io_lib:format(Format, [cast_to_integer(Input)]);
stringformat_io(Input, _Conversion, "-", MinFieldWidth,
                                _Precision, PrecisionLength, "d") when MinFieldWidth > 0 ->
        %Format = "~" ++ re:replace(Conversion, "d", "B", [{return, list}] ),
        DecimalFormat = "~" ++ integer_to_list(PrecisionLength) ++ "..0B", 
        Decimal = lists:flatten( io_lib:format(DecimalFormat, [cast_to_integer(Input)]) ),
        SpaceFormat = "~" ++ integer_to_list(MinFieldWidth - erlang:length(Decimal)) ++ "s",
        Spaces = io_lib:format(SpaceFormat,[""]),
        ?debugFmt("Spaces: |~s|", [Spaces]),
        ?debugFmt("Decimal: ~s", [Decimal]),
        [lists:flatten(Decimal  ++ Spaces)];
stringformat_io(Input, _Conversion, _ConversionFlag, MinFieldWidth,
                                _Precision, PrecisionLength, "d") when MinFieldWidth > 0 ->
        %Format = "~" ++ re:replace(Conversion, "d", "B", [{return, list}] ),
        DecimalFormat = "~" ++ integer_to_list(PrecisionLength) ++ "..0B", 
        Decimal = lists:flatten( io_lib:format(DecimalFormat, [cast_to_integer(Input)]) ),
        SpaceFormat = "~" ++ integer_to_list(MinFieldWidth - erlang:length(Decimal)) ++ "s",
        Spaces = io_lib:format(SpaceFormat,[""]),
        ?debugFmt("Spaces: |~s|", [Spaces]),
        ?debugFmt("Decimal: ~s", [Decimal]),
        [lists:flatten(Spaces ++ Decimal)];
stringformat_io(Input, _Conversion, _ConversionFlag, _MinFieldWidth,
                                _Precision, PrecisionLength, "d") ->
        %Conversion = [ConversionFlag, MinFieldWidth, Precision, PrecisionLength, ConversionType],
        %Format = "~" ++ PrecisionLength ++ "..0" ++ re:replace(Conversion, "d", "B", [{return, list}] ),
        ?debugFmt("precision d, Conversion: ~p~n", [Conversion]),
        Format = lists:flatten("~" ++ io_lib:format("~B..0B",[PrecisionLength])), 
        ?debugFmt("Format: ~p~n",[Format]),
        io_lib:format(Format, [cast_to_integer(Input)]);
stringformat_io(Input, Conversion, _ConversionFlag, _MinFieldWidth,
                                _Precision, _PrecisionLength, "i")->
        Format = "~" ++ re:replace(Conversion, "i", "B", [{return, list}] ),
        io_lib:format(Format, [cast_to_integer(Input)]);
stringformat_io(Input, Conversion, _ConversionFlag, _MinFieldWidth,
                                _Precision, _PrecisionLength, "X")->
        Format = "~" ++ re:replace(Conversion, "X", ".16B", [{return, list}] ),
        io_lib:format(Format, [cast_to_integer(Input)]);
stringformat_io(Input, Conversion, _ConversionFlag, _MinFieldWidth,
                                _Precision, _PrecisionLength, "x")->
        Format = "~" ++ re:replace(Conversion, "x", ".16b", [{return, list}] ),
        io_lib:format(Format, [cast_to_integer(Input)]);
stringformat_io(Input, Conversion, _ConversionFlag, _MinFieldWidth,
                                _Precision, _PrecisionLength, "o")->
        Format = "~" ++ re:replace(Conversion, "o", ".8b", [{return, list}] ),
        io_lib:format(Format, [cast_to_integer(Input)]);
stringformat_io(Input, _Conversion, _ConversionFlag, _MinFieldWidth,
                                Precision, PrecisionLength, "e") when is_integer(PrecisionLength), PrecisionLength >= 2->
        ?debugFmt("PrecisionLength ~p~n", [PrecisionLength]),
        Conversion1 = lists:concat(["","",Precision,PrecisionLength + 1,"e"]),
        Format = "~" ++ Conversion1,
        io_lib:format(Format, [cast_to_float(Input)]);
stringformat_io(Input, Conversion, ConversionFlag, MinFieldWidth,
                                "", [], "e")->
        Format = "~" ++ re:replace(Conversion, "e", ".6e", [{return, list}] ),
        Raw = lists:flatten(stringformat_io(Input, Format, ConversionFlag, MinFieldWidth,
                                ".", 6, "e")
                           ),
        %io:format("Raw: ~p~n", [Raw]),
        Elocate = string:rstr(Raw,"e+"),
        %io:format("Elocate: ~p~n", [Elocate]),
        String = [string:substr(Raw,1,Elocate-1) ++ "e+" 
                 ++ io_lib:format("~2..0B",[list_to_integer(string:substr(Raw,Elocate+2))])], %works till +99, then outputs "**"
        %io:format("String: ~p~n", [String]),
        String;
stringformat_io(Input, Conversion, ConversionFlag, MinFieldWidth,
                                Precision, PrecisionLength, "E")->
        Format = re:replace(Conversion, "E", "e", [{return, list}] ),
        [Str] = stringformat_io(Input, Format, ConversionFlag, MinFieldWidth,
                                Precision, PrecisionLength, "e"),
        [string:to_upper(Str)].
 
cast_to_float([]) ->
    [];
cast_to_float(Input) when is_float(Input) ->
        Input;
cast_to_float(Input) when is_integer(Input) ->
        Input + 0.0;
cast_to_float(Input) ->
        try list_to_float(Input)
        catch
                error:_Reason ->
                        list_to_integer(Input) + 0.0
        end.

cast_to_integer([]) ->
    [];
cast_to_integer(Input) when is_integer(Input) ->
        Input;
cast_to_integer(Input) when is_float(Input) ->
        erlang:round(Input);
cast_to_integer(Input) when is_list(Input)->
        case lists:member($., Input) of
                true ->
                        erlang:round(erlang:list_to_float(Input));
                false ->       
                        erlang:list_to_integer(Input)
        end.
       
%% @doc Converts to lowercase, removes non-word characters (alphanumerics and underscores) and converts spaces to hyphens.
slugify(Input) when is_binary(Input) ->
    slugify(binary_to_list(Input));
slugify(Input) when is_list(Input) ->
    slugify(Input, []).

%% @doc Formats a time according to the given format.
time(Input, "") ->
    date(Input, "f a");
time(Input, FormatStr) ->
    date(Input, FormatStr).
 
%% @doc Converts a string into titlecase.
title(Input) when is_binary(Input) ->
    title(binary_to_list(Input));
title(Input) when is_list(Input) ->
    title(Input, [], capnext).

%% @doc Truncates a string after a certain number of words.
truncatewords(Input, Max) when is_binary(Input) ->
    list_to_binary(truncatewords(binary_to_list(Input), Max));
truncatewords(_Input, Max) when Max =< 0 ->
    "";
truncatewords(Input, Max) ->
    truncatewords(Input, Max, []).

%% @doc Converts a string into all uppercase.
upper(Input) when is_binary(Input) ->
    list_to_binary(upper(binary_to_list(Input)));
upper(Input) ->
    string:to_upper(Input).

%% @doc Escapes a value for use in a URL.
urlencode(Input) when is_binary(Input) ->
    urlencode(Input, 0);
urlencode(Input) when is_list(Input) ->
    urlencode(Input, []).

%% @doc Returns the number of words.
wordcount(Input) when is_binary(Input) ->
    wordcount(binary_to_list(Input));
wordcount(Input) when is_list(Input) ->
    wordcount(Input, 0).

%% @doc Wraps words at specified line length, uses <BR/> html tag to delimit lines
wordwrap(Input, Number) when is_binary(Input) ->
    wordwrap_io(binary_to_list(Input), Number);
wordwrap(Input, Number) when is_list(Input) ->
    wordwrap_io(Input, Number).
 
%% @doc Given a string mapping values for true, false and (optionally) undefined, returns one of those strings according to the value.
yesno(Bool, Choices) when is_binary(Choices) ->
        yesno_io(binary_to_list(Bool), Choices);
yesno(Bool, Choices) when is_list(Choices) ->
        yesno_io(Bool, Choices).

% internal

addslashes([], Acc) ->
    lists:reverse(Acc);
addslashes([H|T], Acc) when H =:= $"; H =:= $' -> 
    addslashes(T, [H, $\\|Acc]);
addslashes([H|T], Acc) ->
    addslashes(T, [H|Acc]).

cut([], _, Acc) ->
    lists:reverse(Acc);
cut([H|T], H, Acc) ->
    cut(T, H, Acc);
cut([H|T], Char, Acc) ->
    cut(T, Char, [H|Acc]).

escape(Binary, Index) when is_binary(Binary) ->
    case Binary of
        <<Pre:Index/binary, $<, Post/binary>> ->
            process_binary_match(Pre, <<"&lt;">>, size(Post), escape(Post, 0));
        <<Pre:Index/binary, $>, Post/binary>> ->
            process_binary_match(Pre, <<"&gt;">>, size(Post), escape(Post, 0));
        <<Pre:Index/binary, $&, Post/binary>> ->
            process_binary_match(Pre, <<"&amp;">>, size(Post), escape(Post, 0));
        <<Pre:Index/binary, 34, Post/binary>> ->
            process_binary_match(Pre, <<"&quot;">>, size(Post), escape(Post, 0));
        <<Pre:Index/binary, 39, Post/binary>> ->
            process_binary_match(Pre, <<"&#039;">>, size(Post), escape(Post, 0));
        <<_:Index/binary, _, _/binary>> ->
            escape(Binary, Index + 1);
        Binary ->
            Binary
    end;
escape([], Acc) ->
    lists:reverse(Acc);
escape("<" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&lt;", Acc));
escape(">" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&gt;", Acc));
escape("&" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&amp;", Acc));
escape("\"" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&quot;", Acc));
escape("'" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&#039;", Acc));
escape([C | Rest], Acc) ->
    escape(Rest, [C | Acc]).


escapejs([], Acc) ->
    lists:reverse(Acc);
escapejs([C | Rest], Acc) when C < 32; C =:= $"; C =:= $'; C =:= $\\; C =:= $<;
                               C =:= $>; C =:= $&; C =:= $=; C =:= $-; C =:= $;; 
                               C =:= 8232; C =:= 8233 -> % just following django here...
    escapejs(Rest, lists:reverse(lists:flatten(io_lib:format("\\u~4.16.0B", [C])), Acc));
escapejs([C | Rest], Acc) ->
    escapejs(Rest, [C | Acc]).

filesizeformat(Bytes, UnitStr) ->
    lists:flatten(io_lib:format("~.1f ~s", [Bytes, UnitStr])).

fix_ampersands(Input, Index) when is_binary(Input) ->
    case Input of
        <<Pre:Index/binary, $&, Post/binary>> ->
            process_binary_match(Pre, <<"&amp;">>, size(Post), Post);
        <<_:Index/binary, _/binary>> ->
            fix_ampersands(Input, Index + 1);
        _ ->
            Input
    end;
fix_ampersands([], Acc) ->
    lists:reverse(Acc);
fix_ampersands("&" ++ Rest, Acc) ->
    fix_ampersands(Rest, lists:reverse("&amp;", Acc));
fix_ampersands([C | Rest], Acc) ->
    fix_ampersands(Rest, [C | Acc]).

join_io([], _Sep) -> [];
join_io([_] = X, _Sep) -> X;
join_io([X|T], Sep) -> [X,Sep] ++ join_io(T, Sep).

linebreaksbr(Input, Index) when is_binary(Input) ->
    Break = <<"<br />">>,
    case Input of
        <<Pre:Index/binary, $\r, $\n, Post/binary>> ->
            process_binary_match(Pre, Break, size(Post), linebreaksbr(Post, 0));
        <<Pre:Index/binary, $\n, Post/binary>> ->
            process_binary_match(Pre, Break, size(Post), linebreaksbr(Post, 0));
        <<_:Index/binary, _/binary>> ->
            linebreaksbr(Input, Index + 1);
        _ ->
            Input
    end;
linebreaksbr([], Acc) ->
    lists:reverse(Acc);
linebreaksbr("\r\n" ++ Rest, Acc) ->
    linebreaksbr(Rest, lists:reverse("<br />", Acc));
linebreaksbr("\n" ++ Rest, Acc) ->
    linebreaksbr(Rest, lists:reverse("<br />", Acc));
linebreaksbr([C | Rest], Acc) ->
    linebreaksbr(Rest, [C | Acc]).

lower(Input, Index) ->
    case Input of
        <<Pre:Index/binary, Byte, Post/binary>> when Byte >= $A andalso Byte =< $Z ->
            process_binary_match(Pre, <<(Byte - $A + $a)>>, size(Post), lower(Post, 0));
        <<_:Index/binary, _/binary>> ->
            lower(Input, Index + 1);
        _ ->
            Input
    end.

phone2numeric([], Acc) ->
    lists:reverse(Acc);
phone2numeric([H|T], Acc) when H >= $a, H =< $c; H >= $A, H =< $C ->
    phone2numeric(T, [$2|Acc]);
phone2numeric([H|T], Acc) when H >= $d, H =< $f; H >= $D, H =< $F ->
    phone2numeric(T, [$3|Acc]);
phone2numeric([H|T], Acc) when H >= $g, H =< $i; H >= $G, H =< $I ->
    phone2numeric(T, [$4|Acc]);
phone2numeric([H|T], Acc) when H >= $j, H =< $l; H >= $J, H =< $L ->
    phone2numeric(T, [$5|Acc]);
phone2numeric([H|T], Acc) when H >= $m, H =< $o; H >= $M, H =< $O ->
    phone2numeric(T, [$6|Acc]);
phone2numeric([H|T], Acc) when H >= $p, H =< $s; H >= $P, H =< $S ->
    phone2numeric(T, [$7|Acc]);
phone2numeric([H|T], Acc) when H >= $t, H =< $v; H >= $T, H =< $V ->
    phone2numeric(T, [$8|Acc]);
phone2numeric([H|T], Acc) when H >= $w, H =< $z; H >= $W, H =< $Z ->
    phone2numeric(T, [$9|Acc]);
phone2numeric([H|T], Acc) ->
    phone2numeric(T, [H|Acc]).

 
 
slugify([], Acc) ->
    lists:reverse(Acc);
slugify([H|T], Acc) when H >= $A, H =< $Z ->
    slugify(T, [H-$A+$a|Acc]);
slugify([$\ |T], Acc) ->
    slugify(T, [$-|Acc]);
slugify([H|T], Acc) when H >= $a, H =< $z; H >= $0, H =< $9; H =:= $_ ->
    slugify(T, [H|Acc]);
slugify([_|T], Acc) ->
    slugify(T, Acc).

title([], Acc, _) ->
    lists:reverse(Acc);
title([$\  = H|T], Acc, capnext) ->
    title(T, [H|Acc], capnext);
title([H|T], Acc, capnext) when H >= $a, H =< $z ->
    title(T, [H - $a + $A|Acc], dont);
title([H|T], Acc, capnext) ->
    title(T, [H|Acc], dont);
title([$\  = H|T], Acc, dont) ->
    title(T, [H|Acc], capnext);
title([H|T], Acc, dont) ->
    title(T, [H|Acc], dont).

truncatewords([], _WordsLeft, Acc) ->
    lists:reverse(Acc);
truncatewords(_Input, 0, Acc) ->
    lists:reverse("..." ++ Acc);
truncatewords([C1, C2|Rest], WordsLeft, Acc) when C1 =/= $\  andalso C2 =:= $\  ->
    truncatewords([C2|Rest], WordsLeft - 1, [C1|Acc]);
truncatewords([C1|Rest], WordsLeft, Acc) ->
    truncatewords(Rest, WordsLeft, [C1|Acc]).

wordcount([], Count) ->
    Count;
wordcount([C1], Count) when C1 =/= $\  ->
    Count+1;
wordcount([C1, C2|Rest], Count) when C1 =/= $\  andalso C2 =:= $\  ->
    wordcount([C2|Rest], Count + 1);
wordcount([_|Rest], Count) ->
    wordcount(Rest, Count).

% based on: http://pragdave.pragprog.com/pragdave/2007/04/testfirst_word_.html 
% This is the exported function: it passes the initial
% result set to the internal versions
wordwrap_io(Input, Number) ->
        Words = string:tokens(Input, " "),
    string:join(lists:reverse(wordwrap_io(Words, [""], Number)),"").
% When we run out of words, we're done 
wordwrap_io([], Result, _Number) ->
    Result;
% Adding a word to an empty line
wordwrap_io([ NextWord | Rest ], [ "" | PreviousLines ], Number) ->
    wordwrap_io(Rest, [ NextWord | PreviousLines ], Number);
% Or to a line that's already partially full. There are two cases:
% 1. The word fits
wordwrap_io([ NextWord | Rest ], [ CurrentLine | PreviousLines ], Number)
  when erlang:length(NextWord) + erlang:length(CurrentLine) < Number ->
    %wordwrap_io(Rest, [ NextWord, " ", CurrentLine | PreviousLines ], Number);
    wordwrap_io(Rest, [ lists:flatten([CurrentLine," " ,NextWord]) | PreviousLines ], Number);    
% 2. The word doesn't fit, so we create a new line 
wordwrap_io([ NextWord | Rest], [ CurrentLine | PreviousLines ], Number) ->
        wordwrap_io(Rest, [ NextWord, "\n", CurrentLine | PreviousLines ], Number).


% Taken from quote_plus of mochiweb_util

urlencode(Input, Index) when is_binary(Input) ->
    case Input of
        <<_:Index/binary, Byte, _/binary>> when ?NO_ENCODE(Byte) ->
            urlencode(Input, Index + 1);
        <<Pre:Index/binary, $\s, Post/binary>> ->
            process_binary_match(Pre, <<"+">>, size(Post), urlencode(Post, 0));
        <<Pre:Index/binary, Hi:4, Lo:4, Post/binary>> ->
            HiDigit = hexdigit(Hi),
            LoDigit = hexdigit(Lo),
            Code = <<$\%, HiDigit, LoDigit>>,
            process_binary_match(Pre, Code, size(Post), urlencode(Post, 0));
        Input ->
            Input
    end;
urlencode([], Acc) ->
    lists:reverse(Acc);
urlencode([C | Rest], Acc) when ?NO_ENCODE(C) ->
    urlencode(Rest, [C | Acc]);
urlencode([$\s | Rest], Acc) ->
    urlencode(Rest, [$+ | Acc]);
urlencode([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    urlencode(Rest, [hexdigit(Lo), hexdigit(Hi), $\% | Acc]).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

process_binary_match(Pre, Insertion, SizePost, Post) ->
    case {size(Pre), SizePost} of
        {0, 0} -> Insertion;
        {0, _} -> [Insertion, Post];
        {_, 0} -> [Pre, Insertion];
        _ -> [Pre, Insertion, Post]
    end.
 
yesno_io(Bool, Choices) ->
%%       io:format("Bool, Choices: ~p, ~p ~n",[Bool, Choices]),
        Terms = string:tokens(Choices, ","),
        case Bool of
                true ->
                        lists:nth(1, Terms);
                false ->
                        lists:nth(2, Terms);
                undefined when erlang:length(Terms) == 2 -> % (converts undefined to false if no mapping for undefined is given)
                        lists:nth(2, Terms);
                undefined when erlang:length(Terms) == 3 ->
                        lists:nth(3, Terms);
                _ ->
                        error
        end.

%% unjoin == split in other languages; inverse of join
%% from http://www.erlang.org/pipermail/erlang-questions/2008-October/038896.html
unjoin(String, []) ->
     unjoin0(String);
unjoin(String, [Sep]) when is_integer(Sep) ->
     unjoin1(String, Sep);
unjoin(String, [C1,C2|L]) when is_integer(C1), is_integer(C2) ->
     unjoin2(String, C1, C2, L).

%% Split a string at "", which is deemed to occur _between_
%% adjacent characters, but queerly, not at the beginning
%% or the end.

unjoin0([C|Cs]) ->
     [[C] | unjoin0(Cs)];
unjoin0([]) ->
     [].

%% Split a string at a single character separator.

unjoin1(String, Sep) ->
     unjoin1_loop(String, Sep, "").

unjoin1_loop([Sep|String], Sep, Rev) ->
     [lists:reverse(Rev) | unjoin1(String, Sep)];
unjoin1_loop([Chr|String], Sep, Rev) ->
     unjoin1_loop(String, Sep, [Chr|Rev]);
unjoin1_loop([], _, Rev) ->
     [lists:reverse(Rev)].

%% Split a string at a multi-character separator
%% [C1,C2|L].  These components are split out for
%% a fast match.

unjoin2(String, C1, C2, L) ->
     unjoin2_loop(String, C1, C2, L, "").

unjoin2_loop([C1|S = [C2|String]], C1, C2, L, Rev) ->
     case unjoin_prefix(L, String)
       of no   -> unjoin2_loop(S, C1, C2, L, [C1|Rev])
        ; Rest -> [lists:reverse(Rev) | unjoin2(Rest, C1, C2, L)]
     end;
unjoin2_loop([Chr|String], C1, C2, L, Rev) ->
     unjoin2_loop(String, C1, C2, L, [Chr|Rev]);
unjoin2_loop([], _, _, _, Rev) ->
     [lists:reverse(Rev)].

unjoin_prefix([C|L], [C|S]) -> unjoin_prefix(L, S);
unjoin_prefix([],    S)     -> S;
unjoin_prefix(_,     _)     -> no.
