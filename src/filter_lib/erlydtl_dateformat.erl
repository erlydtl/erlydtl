-module(erlydtl_dateformat).
-export([format/1, format/2, format/3, format/4]).

-define(TAG_SUPPORTED(C),
        C =:= $a orelse
        C =:= $A orelse
        C =:= $b orelse
        C =:= $B orelse
        C =:= $c orelse
        C =:= $d orelse
        C =:= $D orelse
        C =:= $E orelse
        C =:= $f orelse
        C =:= $F orelse
        C =:= $g orelse
        C =:= $G orelse
        C =:= $h orelse
        C =:= $H orelse
        C =:= $i orelse
        C =:= $I orelse
        C =:= $j orelse
        C =:= $l orelse
        C =:= $L orelse
        C =:= $m orelse
        C =:= $M orelse
        C =:= $n orelse
        C =:= $N orelse
        C =:= $O orelse
        C =:= $P orelse
        C =:= $r orelse
        C =:= $s orelse
        C =:= $S orelse
        C =:= $t orelse
        C =:= $T orelse
        C =:= $U orelse
        C =:= $w orelse
        C =:= $W orelse
        C =:= $y orelse
        C =:= $Y orelse
        C =:= $z orelse
        C =:= $Z orelse
        C =:= $o
       ).

%%
%% Format the current date/time
%%
format(FormatString) when is_binary(FormatString) ->
    format(binary_to_list(FormatString), fun stub_tran/2, <<>>);
format(FormatString) ->
    {Date, Time} = erlang:localtime(),
    replace_tags(Date, Time, FormatString, fun stub_tran/2, <<>>).

%%
%% Format a tuple of the form {{Y,M,D},{H,M,S}}
%% This is the format returned by erlang:localtime()
%% and other standard date/time BIFs
%%
format(DateTime, FormatString) when is_binary(FormatString) ->
    format(DateTime, binary_to_list(FormatString),fun stub_tran/2, <<>>);
format({{_,_,_} = Date,{_,_,_} = Time}, FormatString) ->
    replace_tags(Date, Time, FormatString, fun stub_tran/2, <<>> );
%%
%% Format a tuple of the form {Y,M,D}
%%
format({_,_,_} = Date, FormatString) ->
    replace_tags(Date, {0,0,0}, FormatString, fun stub_tran/2, <<>> );
format(DateTime, FormatString) ->
    io:format("Unrecognised date parameter : ~p~n", [DateTime]),
    FormatString.

%% The same set of functions with TranslationFunction and Locale args
%% Translation function may be 'none' atom - handle this separately
%% replacing atom with a stub function (it's easier to do it this way)
format(FormatString, TransFun, Locale) when is_binary(FormatString) ->
    format(binary_to_list(FormatString), TransFun, Locale);
format(FormatString, none, _Locale) -> 
    format(FormatString, fun stub_tran/2, <<>>);
format(FormatString, TransFun, Locale) ->
    {Date, Time} = erlang:localtime(),
    replace_tags(Date, Time, FormatString, TransFun, Locale).

format(DateTime, FormatString, TransFun, Locale) when is_binary(FormatString) ->
    format(DateTime, binary_to_list(FormatString), TransFun, Locale);
format(DateTime, FormatString, none, _Locale) ->
    format(DateTime, FormatString, fun stub_tran/2, <<>>);
format({{_,_,_} = Date,{_,_,_} = Time}, FormatString, TransFun, Locale) ->
    replace_tags(Date, Time, FormatString, TransFun, Locale );

format({_,_,_} = Date, FormatString, none, _Locale) ->
    replace_tags(Date, {0,0,0}, FormatString, fun stub_tran/2, <<>>);
format({_,_,_} = Date, FormatString, TransFun, Locale) ->
    replace_tags(Date, {0,0,0}, FormatString, TransFun, Locale);
format(DateTime, FormatString, _TransFun, _Locale) ->
    io:format("Unrecognised date parameter : ~p~n", [DateTime]),
    FormatString.

replace_tags(Date, Time, Input, TransFun, Locale) ->
    replace_tags(Date, Time, Input, [], noslash, TransFun, Locale).
replace_tags(_Date, _Time, [], Out, _State, _TransFun, _Locale) ->
    lists:reverse(Out);
replace_tags(Date, Time, [C|Rest], Out, noslash, TransFun, Locale) when ?TAG_SUPPORTED(C) ->
    case tag_to_value(C, Date, Time, TransFun, Locale) of
        V when is_binary(V) -> replace_tags(Date, Time, Rest, 
                                            [V] ++ Out, noslash, 
                                            TransFun, Locale);
        V when is_list(V) ->   replace_tags(Date, Time, Rest, 
                                            lists:reverse(V) ++ Out, 
                                            noslash, TransFun, Locale)
    end;
replace_tags(Date, Time, [$\\|Rest], Out, noslash, TransFun, Locale) ->
    replace_tags(Date, Time, Rest, Out, slash, TransFun, Locale);
replace_tags(Date, Time, [C|Rest], Out, slash, TransFun, Locale) ->
    replace_tags(Date, Time, Rest, [C|Out], noslash, TransFun, Locale);
replace_tags(Date, Time, [C|Rest], Out, _State, TransFun, Locale) ->
    replace_tags(Date, Time, Rest, [C|Out], noslash, TransFun, Locale).


%%-----------------------------------------------------------
%% Time formatting
%%-----------------------------------------------------------

%% 'a.m.' or 'p.m.'
tag_to_value($a, _, {H, _, _}, TransFun, Locale) when H > 11 -> 
    TransFun("p.m.", Locale);
tag_to_value($a, _, _, TransFun, Locale) -> 
    TransFun("a.m.", Locale);

%% 'AM' or 'PM'
tag_to_value($A, _, {H, _, _}, TransFun, Locale) when H > 11 -> 
    TransFun("PM", Locale);
tag_to_value($A, _, _, TransFun, Locale) -> 
    TransFun("AM", Locale);

%% Swatch Internet time
tag_to_value($B, _, _, _TransFun, _Locale) ->
    ""; %% NotImplementedError

%% ISO 8601 Format.
tag_to_value($c, Date, Time, TransFun, Locale) ->
    tag_to_value($Y, Date, Time, TransFun, Locale) ++
        "-" ++ tag_to_value($m, Date, Time, TransFun, Locale) ++
        "-" ++ tag_to_value($d, Date, Time, TransFun, Locale) ++
        "T" ++ tag_to_value($H, Date, Time, TransFun, Locale) ++
        ":" ++ tag_to_value($i, Date, Time, TransFun, Locale) ++
        ":" ++ tag_to_value($s, Date, Time, TransFun, Locale);

%%
%% Time, in 12-hour hours and minutes, with minutes
%% left off if they're zero.
%%
%% Examples: '1', '1:30', '2:05', '2'
%%
%% Proprietary extension.
%%
tag_to_value($f, Date, {H, 0, S}, TransFun, Locale) ->
    %% If min is zero then return the hour only
    tag_to_value($g, Date, {H, 0, S}, TransFun, Locale);
tag_to_value($f, Date, Time, TransFun, Locale) ->
    %% Otherwise return hours and mins
    tag_to_value($g, Date, Time, TransFun, Locale)
        ++ ":" ++ tag_to_value($i, Date, Time, TransFun, Locale);

%% Hour, 12-hour format without leading zeros; i.e. '1' to '12'
tag_to_value($g, _, {H,_,_}, _TransFun, _Locale) ->
    integer_to_list(hour_24to12(H));

%% Hour, 24-hour format without leading zeros; i.e. '0' to '23'
tag_to_value($G, _, {H,_,_}, _TransFun, _Locale) ->
    integer_to_list(H);

%% Hour, 12-hour format; i.e. '01' to '12'
tag_to_value($h, _, {H,_,_}, _TransFun, _Locale) ->
    integer_to_list_zerofill(hour_24to12(H));

%% Hour, 24-hour format; i.e. '00' to '23'
tag_to_value($H, _, {H,_,_}, _TransFun, _Locale) ->
    integer_to_list_zerofill(H);

%% Minutes; i.e. '00' to '59'
tag_to_value($i, _, {_,M,_}, _TransFun, _Locale) ->
    integer_to_list_zerofill(M);

%% Time, in 12-hour hours, minutes and 'a.m.'/'p.m.', with minutes left off
%% if they're zero and the strings 'midnight' and 'noon' if appropriate.
%% Examples: '1 a.m.', '1:30 p.m.', 'midnight', 'noon', '12:30 p.m.'
%% Proprietary extension.
tag_to_value($P, _, {0,  0, _}, TransFun, Locale) -> 
    TransFun("midnight", Locale);
tag_to_value($P, _, {12, 0, _}, TransFun, Locale) -> 
    TransFun("noon", Locale);
tag_to_value($P, Date, Time, TransFun, Locale) ->
    tag_to_value($f, Date, Time, TransFun, Locale)
        ++ " " ++ tag_to_value($a, Date, Time, TransFun, Locale);

%% Seconds; i.e. '00' to '59'
tag_to_value($s, _, {_,_,S}, _TransFun, _Locale) ->
    integer_to_list_zerofill(S);

%%-----------------------------------------------------------
%% Date formatting
%%-----------------------------------------------------------

%% Month, textual, 3 letters, lowercase; e.g. 'jan'
tag_to_value($b, {_,M,_}, _, TransFun, Locale) ->
    TransFun(string:sub_string(monthname(M), 1, 3), Locale);

%% Day of the month, 2 digits with leading zeros; i.e. '01' to '31'
tag_to_value($d, {_, _, D}, _, _TransFun, _Locale) ->
    integer_to_list_zerofill(D);

%% Day of the week, textual, 3 letters; e.g. 'Fri'
tag_to_value($D, Date, _, TransFun, Locale) ->
    Dow = calendar:day_of_the_week(Date),
    TransFun(ucfirst(string:sub_string(dayname(Dow), 1, 3)), Locale);

%% Month, textual, long, alternative; e.g. 'Listopada'
tag_to_value($E, {_,M,_}, _, TransFun, Locale) ->
    TransFun(ucfirst(monthname(M)), {Locale, <<"alt. month">>});


%% Month, textual, long; e.g. 'January'
tag_to_value($F, {_,M,_}, _, TransFun, Locale) ->
    TransFun(ucfirst(monthname(M)), Locale);

%% '1' if Daylight Savings Time, '0' otherwise.
tag_to_value($I, _, _, _TransFun, _Locale) ->
    "TODO";

%% Day of the month without leading zeros; i.e. '1' to '31'
tag_to_value($j, {_, _, D}, _, _TransFun, _Locale) ->
    integer_to_list(D);

%% Day of the week, textual, long; e.g. 'Friday'
tag_to_value($l, Date, _, TransFun, Locale) ->
    TransFun(ucfirst(dayname(calendar:day_of_the_week(Date))), Locale);

%% Boolean for whether it is a leap year; i.e. True or False
tag_to_value($L, {Y,_,_}, _, _TransFun, _Locale) ->
    case calendar:is_leap_year(Y) of
        true -> "True";
        _ -> "False"
    end;

%% Month; i.e. '01' to '12'
tag_to_value($m, {_, M, _}, _, _TransFun, _Locale) ->
    integer_to_list_zerofill(M);

%% Month, textual, 3 letters; e.g. 'Jan'
tag_to_value($M, {_,M,_}, _, TransFun, Locale) ->
    TransFun(ucfirst(string:sub_string(monthname(M), 1, 3)), Locale);

%% Month without leading zeros; i.e. '1' to '12'
tag_to_value($n, {_, M, _}, _, _TransFun, _Locale) ->
    integer_to_list(M);

%% Month abbreviation in Associated Press style. Proprietary extension.
tag_to_value($N, {_,M,_}, _, TransFun, Locale) when M =:= 9 ->
    %% Special case - "Sept."
    TransFun(ucfirst(string:sub_string(monthname(M), 1, 4)) ++ ".",
             {Locale, <<"abbrev. month">>});
tag_to_value($N, {_,M,_}, _, TransFun, Locale) when M < 3 orelse M > 7 ->
    %% Jan, Feb, Aug, Oct, Nov, Dec are all
    %% abbreviated with a full-stop appended.
    TransFun(ucfirst(string:sub_string(monthname(M), 1, 3)) ++ ".",
                 {Locale, <<"abbrev. month">>});
tag_to_value($N, {_,M,_}, _, TransFun, Locale) ->
    %% The rest are the fullname.
    TransFun(ucfirst(monthname(M)), {Locale, <<"abbrev. month">>});

%% Difference to Greenwich time in hours; e.g. '+0200'
tag_to_value($O, Date, Time, _TransFun, _Locale) ->
    Diff = utc_diff(Date, Time),
    Offset = if
                 Diff < 0 ->
                     io_lib:format("-~4..0w", [abs(Diff)]);
                 true ->
                     io_lib:format("+~4..0w", [Diff])
             end,
    lists:flatten(Offset);

%% RFC 2822 formatted date; e.g. 'Thu, 21 Dec 2000 16:01:07 +0200'
tag_to_value($r, Date, Time, _TransFun, _Locale) ->
    % afaik, date should not be translated in case RFC format is specified.
    replace_tags(Date, Time, "D, j M Y H:i:s O", fun stub_tran/2, <<>> );

%% English ordinal suffix for the day of the month, 2 characters;
%% i.e. 'st', 'nd', 'rd' or 'th'
tag_to_value($S, {_, _, D}, _, _TransFun, _Locale) when
      D rem 100 =:= 11 orelse
      D rem 100 =:= 12 orelse
      D rem 100 =:= 13 -> "th";
tag_to_value($S, {_, _, D}, _, _TransFun, _Locale) when D rem 10 =:= 1 -> "st";
tag_to_value($S, {_, _, D}, _, _TransFun, _Locale) when D rem 10 =:= 2 -> "nd";
tag_to_value($S, {_, _, D}, _, _TransFun, _Locale) when D rem 10 =:= 3 -> "rd";
tag_to_value($S, _, _, _TransFun, _Locale) -> "th";

%% Number of days in the given month; i.e. '28' to '31'
tag_to_value($t, {Y,M,_}, _, _TransFun, _Locale) ->
    integer_to_list(calendar:last_day_of_the_month(Y,M));

%% Time zone of this machine; e.g. 'EST' or 'MDT'
tag_to_value($T, _, _, _TransFun, _Locale) ->
    "TODO";

%% Seconds since the Unix epoch (January 1 1970 00:00:00 GMT)
tag_to_value($U, Date, Time, _TransFun, _Locale) ->
    EpochSecs = calendar:datetime_to_gregorian_seconds({Date, Time})
        - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    integer_to_list(EpochSecs);

%% Day of the week, numeric, i.e. '0' (Sunday) to '6' (Saturday)
tag_to_value($w, Date, _, _TransFun, _Locale) ->
    %% Note: calendar:day_of_the_week returns
    %%   1 | .. | 7. Monday = 1, Tuesday = 2, ..., Sunday = 7
    integer_to_list(calendar:day_of_the_week(Date) rem 7);

%% ISO-8601 week number of year, weeks starting on Monday
tag_to_value($W, {Y,M,D}, _, _TransFun, _Locale) ->
    integer_to_list(year_weeknum(Y,M,D));

%% Year, 2 digits; e.g. '99'
tag_to_value($y, {Y, _, _}, _, _TransFun, _Locale) ->
    string:sub_string(integer_to_list(Y), 3);

%% Year, 4 digits; e.g. '1999'
tag_to_value($Y, {Y, _, _}, _, _TransFun, _Locale) ->
    integer_to_list(Y);

%% Day of the year; i.e. '0' to '365'
tag_to_value($z, {Y,M,D}, _, _TransFun, _Locale) ->
    integer_to_list(day_of_year(Y,M,D));

%% Time zone offset in seconds (i.e. '-43200' to '43200'). The offset for
%% timezones west of UTC is always negative, and for those east of UTC is
%% always positive.
tag_to_value($Z, _, _, _TransFun, _Locale) ->
    "TODO";

%% o – the ISO 8601 year number
tag_to_value($o, {Y,M,D}, _, _TransFun, _Locale) ->
    integer_to_list(weeknum_year(Y,M,D));

tag_to_value(C, Date, Time, _TransFun, _Locale) ->
    io:format("Unimplemented tag : ~p [Date : ~p] [Time : ~p]",
              [C, Date, Time]),
    "".

%% Date helper functions
day_of_year(Y,M,D) ->
    day_of_year(Y,M,D,0).
day_of_year(_Y,M,D,Count) when M =< 1 ->
    D + Count;
day_of_year(Y,M,D,Count) when M =< 12 ->
    day_of_year(Y, M - 1, D, Count + calendar:last_day_of_the_month(Y,M));
day_of_year(Y,_M,D,_Count) ->
    day_of_year(Y, 12, D, 0).

hour_24to12(0) -> 12;
hour_24to12(H) when H < 13 -> H;
hour_24to12(H) when H < 24 -> H - 12;
hour_24to12(H) -> H.

year_weeknum(Y,M,D) ->
    First = (calendar:day_of_the_week(Y, 1, 1) rem 7) - 1,
    Wk = ((((calendar:date_to_gregorian_days(Y, M, D) -
                 calendar:date_to_gregorian_days(Y, 1, 1)) + First) div 7)
          + (case First < 4 of true -> 1; _ -> 0 end)),
    case Wk of
        0 -> weeks_in_year(Y - 1);
        _ -> case weeks_in_year(Y) of
                 WksInThisYear when Wk > WksInThisYear -> 1;
                 _ -> Wk
             end
    end.

weeknum_year(Y,M,D) ->
    WeekNum = year_weeknum(Y,M,D),
    case {M, WeekNum} of
        {1, 53} -> Y - 1;
        {1, 52} -> Y - 1;
        {12, 1} -> Y + 1;
        {12, 2} -> Y + 1;
        _ -> Y
    end.

weeks_in_year(Y) ->
    D1 = calendar:day_of_the_week(Y, 1, 1),
    D2 = calendar:day_of_the_week(Y, 12, 31),
    if (D1 =:= 4 orelse D2 =:= 4) -> 53; true -> 52 end.

utc_diff({Y, M, D}, Time) when Y < 1970->
    utc_diff({1970, M, D}, Time);
utc_diff(Date, Time) ->
    LTime = {Date, Time},
    UTime = erlang:localtime_to_universaltime(LTime),
    DiffSecs = calendar:datetime_to_gregorian_seconds(LTime) -
        calendar:datetime_to_gregorian_seconds(UTime),
    trunc((DiffSecs / 3600) * 100).

dayname(1) -> "monday";
dayname(2) -> "tuesday";
dayname(3) -> "wednesday";
dayname(4) -> "thursday";
dayname(5) -> "friday";
dayname(6) -> "saturday";
dayname(7) -> "sunday".

monthname(1) ->  "january";
monthname(2) ->  "february";
monthname(3) ->  "march";
monthname(4) ->  "april";
monthname(5) ->  "may";
monthname(6) ->  "june";
monthname(7) ->  "july";
monthname(8) ->  "august";
monthname(9) ->  "september";
monthname(10) -> "october";
monthname(11) -> "november";
monthname(12) -> "december".

%% Utility functions
integer_to_list_zerofill(N) when is_float(N) ->
    integer_to_list_zerofill(erlang:round(N));
integer_to_list_zerofill(N) when N < 10 ->
    lists:flatten(io_lib:format("~2..0B", [N]));
integer_to_list_zerofill(N) when is_integer(N) ->
    integer_to_list(N).

ucfirst([First | Rest]) when First >= $a, First =< $z ->
    [First-($a-$A) | Rest];
ucfirst(Other) ->
    Other.

stub_tran(A,_) -> 
    % useful for test debuggging
    % io:format("calling stub translation!!!",[]),
    A.
