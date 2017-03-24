%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc Provide string datetime manipulation functions
%%%
%%% @end
%%% Created : 17. Nov 2016 11:23 AM
%%%-------------------------------------------------------------------
-module(datetime_x_fin).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([
  now/0
  , now/1
  , today/0
  , yesterday/0
  , today/1
  , yesterday/1
  , prefix_yyyy_2_dtime/1
  , prefix_yyyy_2_dtime/2
  , prefix_yyyy_2_settle_date/1
  , prefix_yyyy_2_settle_date/2
  , nextday/1                     %%返回下一天
  , inc_days/2                    %%返回指定天数之后的日期
  , dec_days/2                    %%返回指定天数之前的日期
]).

-export([
  diff/2

]).

-define(SEC_PER_DAY, 86400).
-define(SEC_PER_HOUR, 3600).
-define(SEC_PER_MIN, 60).

%%%===================================================================
%%% Types
%%%===================================================================
-type byte4() :: <<_:32>>.
-type byte6() :: <<_:48>>.
-type byte8() :: <<_:64>>.
-type byte12() :: <<_:96>>.
-type byte14() :: <<_:112>>.

-type date_yyyymmdd() :: byte8().
-type date_yymmdd() :: byte6().
-type date_mmdd() :: byte4().
-type time_hhmmss() :: byte6().
-type datetime_yyyymmddhhmmss() :: byte14().
-type datetime_yymmddhhmmss() :: byte12().

-type datetime_type() :: date_yyyymmdd() | date_yymmdd() | time_hhmmss() | datetime_yyyymmddhhmmss() | datetime_yymmddhhmmss().
-type time_in_secs() :: integer().

%%====================================================================
%% API functions
%%====================================================================


%%--------------------------------------------------------
now() ->
  now(local).

now(utc) ->
  list_to_binary(datetime_x:now_to_utc_string(erlang:timestamp()));
now(epoch) ->
  list_to_binary(erlang:system_time(milli_seconds));
now(local) ->
  list_to_binary(datetime_x:now_to_local_string(erlang:timestamp()));
now(utc) ->
  list_to_binary(datetime_x:now_to_utc_string(erlang:timestamp()));
%% used for up txn req packet
now(txn) ->
  list_to_binary(datetime_x:now_to_local_txn_string(erlang:timestamp()));
now(ts) ->
  list_to_binary(datetime_x:now_to_local_ts_string(erlang:timestamp())).

now_test() ->
  ?assertEqual(true, is_binary(now(txn))),
  ok.

%%--------------------------------------------------------
-spec today() -> Date when
  Date :: types:date_format_yyyymmdd().

today() ->
  datetime_x:localtime_to_yyyymmdd(datetime_x:localtime()).

-spec today(Fmt) -> Date when
  Fmt :: types:today_format(),
  Date :: types:date_format_yyyymmdd().

today(mmdd) ->
  YYYYMMDD = today(),
  binary:part(YYYYMMDD, 4, 4).
%%--------------------------------------------------------
-spec yesterday() -> Date when
  Date :: types:date_format_yyyymmdd().

yesterday() ->
  Seconds = datetime_x:localtime_to_seconds(datetime_x:localtime()),
  YesterdayTime = calendar:gregorian_seconds_to_datetime(Seconds - 86400),
  datetime_x:localtime_to_yyyymmdd(YesterdayTime).

-spec yesterday(Fmt) -> Date when
  Fmt :: types:today_format(),
  Date :: types:date_format_yyyymmdd().

yesterday(mmdd) ->
  YYYYMMDD = yesterday(),
  binary:part(YYYYMMDD, 4, 4).

%%--------------------------------------------------------
prefix_yyyy_2_dtime(DTime) when is_binary(DTime) ->
  prefix_yyyy_2_dtime(DTime, today()).

prefix_yyyy_2_dtime(DTime, Today) when is_binary(Today) ->
  <<ThisYear:4/bytes, MMDD_IN_TODAY:4/bytes, _/binary>> = Today,
  <<MMDD:4/bytes, _/binary>> = DTime,
  prefix_yyyy_2_dtime(DTime, MMDD, ThisYear, MMDD_IN_TODAY).

prefix_yyyy_2_dtime(DTime, <<"1231">>, ThisYear, MMDD_IN_TODAY)
  when is_binary(DTime), is_binary(ThisYear), is_binary(MMDD_IN_TODAY) ->

  case binary_to_integer(MMDD_IN_TODAY) < 1231 of
    true ->
      %% should use last year
      %% DTime = 1231 xx:xx  , curr time = YYYY 0101 xx:xx
      %% last year = YYYY-1
      LastYear = integer_to_binary(binary_to_integer(ThisYear) - 1);
    false ->
      LastYear = ThisYear
  end,
  list_to_binary([LastYear, DTime]);

prefix_yyyy_2_dtime(DTime, _, ThisYear, _) when is_binary(DTime), is_binary(ThisYear) ->
  list_to_binary([ThisYear, DTime]).

%%--------------------------------------------------------
prefix_yyyy_2_settle_date(MMDD) when is_binary(MMDD) ->
  prefix_yyyy_2_settle_date(MMDD, today()).

prefix_yyyy_2_settle_date(<<>>, _) ->
  %% incase orig settle date is empty
  <<>>;
prefix_yyyy_2_settle_date(MMDD, Today) when is_binary(MMDD), is_binary(Today) ->
  <<Year_IN_TODAY:4/bytes, MMDD_IN_TODAY:4/bytes, _/binary>> = Today,
  4 = byte_size(MMDD),

  prefix_yyyy_2_settle_date(MMDD, Year_IN_TODAY, MMDD_IN_TODAY).

prefix_yyyy_2_settle_date(MMDD, Year_IN_TODAY, MMDD_IN_TODAY)
  when is_binary(MMDD), is_binary(Year_IN_TODAY), is_binary(MMDD_IN_TODAY) ->

  SettleYear = case binary_to_integer(MMDD) < binary_to_integer(MMDD_IN_TODAY) of
                 true ->
                   %% settle date less than today, year should be next year
                   integer_to_binary(binary_to_integer(Year_IN_TODAY) + 1);
                 false ->
                   %% settle date large than today, year should be same as today's year
                   Year_IN_TODAY
               end,

  <<SettleYear/binary, MMDD/binary>>.


%%--------------------------------------------------------
prefix_yyyy_2_dtime_test() ->
  DTime = <<"1231">>,

  ?assertEqual(prefix_yyyy_2_dtime(DTime, <<"20160101">>),
    list_to_binary([<<"2015">>, DTime])),
  ?assertEqual(prefix_yyyy_2_dtime(DTime, <<"20161231">>),
    list_to_binary([<<"2016">>, DTime])),

  DTime1 = <<"0301">>,
  ?assertEqual(prefix_yyyy_2_dtime(DTime1, <<"20160301">>),
    list_to_binary([<<"2016">>, DTime1])),
  ?assertEqual(prefix_yyyy_2_dtime(DTime1, <<"20160302">>),
    list_to_binary([<<"2016">>, DTime1])),
  ok.

prefix_yyyy_2_settle_date_test() ->
  ?assertEqual(prefix_yyyy_2_settle_date(<<"0101">>, <<"20161231">>), <<"20170101">>),
  ?assertEqual(prefix_yyyy_2_settle_date(<<"0101">>, <<"20161230">>), <<"20170101">>),
  ?assertEqual(prefix_yyyy_2_settle_date(<<"0101">>, <<"20170101">>), <<"20170101">>),
  ?assertEqual(prefix_yyyy_2_settle_date(<<"0102">>, <<"20170101">>), <<"20170102">>),
  ?assertEqual(prefix_yyyy_2_settle_date(<<"1231">>, <<"20171230">>), <<"20171231">>),
  ?assertEqual(prefix_yyyy_2_settle_date(<<"1230">>, <<"20171230">>), <<"20171230">>),
  ?assertEqual(prefix_yyyy_2_settle_date(<<"1230">>, <<"20171231">>), <<"20181230">>),
  ?assertEqual(prefix_yyyy_2_settle_date(<<"">>, <<"20171231">>), <<"">>),
  ok.
%%--------------------------------------------------------
%%  @doc
%%    calculate datetime difference value in seconds for DT1/DT2
%%
%%  @end
%%--------------------------------------------------------
-spec diff(DT1, DT2) -> Delta when
  DT1 :: datetime_type(),
  DT2 :: datetime_type(),
  Delta :: time_in_secs().

diff(DT1, DT2) when is_list(DT1) ->
  diff(list_to_binary(DT1), DT2);
diff(DT1, DT2) when is_list(DT2) ->
  diff(DT1, list_to_binary(DT2));
diff(DT1, DT2) when is_binary(DT1), is_binary(DT2) ->
  %% the length of DT1/DT2 must be same
  true = byte_size(DT1) =:= byte_size(DT2),

  do_diff(DT1, DT2).

%%--------------------------------------------------------
diff_test() ->
  ?assertEqual(0, diff(<<"20170101">>, <<"20170101">>)),
  ?assertEqual(?SEC_PER_DAY, diff(<<"20170101">>, <<"20170102">>)),

  ?assertEqual(0, diff(<<"20170101101010">>, <<"20170101101010">>)),
  ?assertEqual(?SEC_PER_DAY, diff(<<"20170101101010">>, <<"20170102101010">>)),
  ?assertEqual(?SEC_PER_DAY + 1, diff(<<"20170101101010">>, <<"20170102101011">>)),

  ?assertEqual(0, diff(<<"170101101010">>, <<"170101101010">>)),
  ?assertEqual(?SEC_PER_DAY, diff(<<"170101101010">>, <<"170102101010">>)),
  ?assertEqual(?SEC_PER_DAY + 1, diff(<<"170101101010">>, <<"170102101011">>)),

  ?assertEqual(0, diff(<<"101010">>, <<"101010">>)),
  ?assertEqual(?SEC_PER_HOUR, diff(<<"101010">>, <<"111010">>)),
  ?assertEqual(?SEC_PER_HOUR + 10, diff(<<"101010">>, <<"111020">>)),
  ok.
%%--------------------------------------------------------
do_diff(DT1, DT2) when byte_size(DT1) =:= 8 ->
  %% YYYYMMDD format
  diff_yyyymmdd(DT1, DT2);
do_diff(DT1, DT2) when byte_size(DT1) =:= 6 ->
  %% hhmmss format
  diff_hhmmss(DT1, DT2);
do_diff(DT1, DT2) when byte_size(DT1) =:= 6 + 8 ->
  %% YYYYMMDDhhmmss format
  diff_yyyymmddhhmmss(DT1, DT2);
do_diff(DT1, DT2) when byte_size(DT1) =:= 6 + 6 ->
  %% YYMMDDhhmmss format
  diff_yymmddhhmmss(DT1, DT2).

%%--------------------------------------------------------

diff_yyyymmdd(DT1, DT2) when byte_size(DT1) =:= 8 ->
  Days1 = yyyymmdd_2_days(DT1),
  Days2 = yyyymmdd_2_days(DT2),

  ?SEC_PER_DAY * (Days2 - Days1).

diff_yyyymmdd_test() ->
  ?assertEqual(0, diff_yyyymmdd(<<"20101010">>, <<"20101010">>)),
  ?assertEqual(86400, diff_yyyymmdd(<<"20101010">>, <<"20101011">>)),
  ?assertEqual(-86400, diff_yyyymmdd(<<"20101012">>, <<"20101011">>)),
  ok.




yyyymmdd_2_days(<<Year:4/bytes, Month:2/bytes, Day:2/bytes>> = YYYYMMDD)
  when is_binary(YYYYMMDD), byte_size(YYYYMMDD) =:= 8 ->
  calendar:date_to_gregorian_days(binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)).

yyyymmdd_2_days_test() ->
  ?assertEqual(734503, yyyymmdd_2_days(<<"20110101">>)),
  ?assertEqual(734504, yyyymmdd_2_days(<<"20110102">>)),
  ok.


%%--------------------------------------------------------
diff_yyyymmddhhmmss(DT1, DT2) when byte_size(DT1) =:= 14 ->
  DiffInTuple = calendar:time_difference(
    yyyymmddhhmmss_2_datetime_tuple(DT1)
    , yyyymmddhhmmss_2_datetime_tuple(DT2)
  ),
  calc_diff_in_secs(DiffInTuple).

yyyymmddhhmmss_2_datetime_tuple(<<Year:4/bytes, Month:2/bytes, Day:2/bytes, HH:2/bytes, MM:2/bytes, SS:2/bytes>> = _DT) ->
  {
    {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}
    , {binary_to_integer(HH), binary_to_integer(MM), binary_to_integer(SS)}
  }.

yyyymmddhhmmss_2_datetime_tuple_test() ->
  ?assertEqual({{2010, 10, 10}, {12, 12, 12}}, yyyymmddhhmmss_2_datetime_tuple(<<"20101010121212">>)),
  ok.

diff_yyyymmddhhmmss_test() ->
  ?assertEqual(0, diff_yyyymmddhhmmss(<<"20101010101010">>, <<"20101010101010">>)),
  ?assertEqual(?SEC_PER_DAY, diff_yyyymmddhhmmss(<<"20101010121212">>, <<"20101011121212">>)),
  ?assertEqual(- ?SEC_PER_DAY, diff_yyyymmddhhmmss(<<"20101010121212">>, <<"20101009121212">>)),
  ok.

%%--------------------------------------------------------
diff_hhmmss(DT1, DT2) when byte_size(DT1) =:= 6 ->

  BaseDate = <<"20100101">>,
  diff_yyyymmddhhmmss(<<BaseDate/binary, DT1/binary>>, <<BaseDate/binary, DT2/binary>>).

diff_hhmmss_test() ->
  ?assertEqual(0, diff_hhmmss(<<"101010">>, <<"101010">>)),
  ?assertEqual(1, diff_hhmmss(<<"101010">>, <<"101011">>)),
  ?assertEqual(-1, diff_hhmmss(<<"101010">>, <<"101009">>)),
  ?assertEqual(3600, diff_hhmmss(<<"101010">>, <<"111010">>)),
  ok.
%%--------------------------------------------------------
-define(YEAR_BASE, 2000).

diff_yymmddhhmmss(DT1, DT2) when byte_size(DT1) =:= 12 ->
  BaseCentery = <<"20">>,

  diff_yyyymmddhhmmss(<<BaseCentery/binary, DT1/binary>>, <<BaseCentery/binary, DT2/binary>>).

diff_yymmddhhmmss_test() ->
  ?assertEqual(0, diff_yymmddhhmmss(<<"101010101010">>, <<"101010101010">>)),
  ?assertEqual(?SEC_PER_DAY, diff_yymmddhhmmss(<<"101010121212">>, <<"101011121212">>)),
  ?assertEqual(- ?SEC_PER_DAY, diff_yymmddhhmmss(<<"101010121212">>, <<"101009121212">>)),
  ok.

%%--------------------------------------------------------
calc_diff_in_secs({Days, {H, M, S}}) when is_integer(Days), is_integer(H), is_integer(M), is_integer(S) ->
  Secs = Days * ?SEC_PER_DAY + H * 3600 + M * 60 + S,
  Secs.

calc_diff_in_secs_test() ->
  ?assertEqual(0, calc_diff_in_secs({0, {0, 0, 0}})),
  ?assertEqual(86400 - 1, calc_diff_in_secs({0, {23, 59, 59}})),
  ?assertEqual(-86400 + 1, calc_diff_in_secs({-1, {0, 0, 1}})),
  ok.


%%====================================================================
%% Internal functions
%%====================================================================



-spec nextday(Date) -> NextDate when
  Date :: types:date_yyyymmdd()|types:date_mmdd(),
  NextDate :: types:byte8().

%%返回下一天
nextday(Date) when byte_size(Date) =:= 8 ->
  case calendar:valid_date(datetime_x:yyyymmdd_to_tuple(Date)) of
      true ->
        Days = calendar:date_to_gregorian_days(datetime_x:yyyymmdd_to_tuple(Date)),
        datetime_x:tuple_to_yyyymmdd(calendar:gregorian_days_to_date(Days+1));
      false->
        errortime

end;
nextday(<<Month:2/bytes, Day:2/bytes>>) ->
  <<Year:4/bytes, _Rest/binary>> = today(),
  case calendar:valid_date({binary_to_integer(Year),binary_to_integer(Month),binary_to_integer(Day)}) of
      true->
        Days = calendar:date_to_gregorian_days({binary_to_integer(Year),binary_to_integer(Month),binary_to_integer(Day)}),
        {_,MM,DD}=calendar:gregorian_days_to_date(Days+1),
        list_to_binary(io_lib:format("~2..0w~2..0w", [MM, DD]));
      false->
        errortime
end.
%%nextday函数测试
nextday_test()->
  ?assertEqual(<<"20170101">>,nextday(<<"20161231">>)), %% 年末 月末
  ?assertEqual(<<"0101">>,nextday(<<"1231">>)), %% 年末 月末

  ?assertEqual(<<"20170102">>,nextday(<<"20170101">>)), %% 元旦 月初
  ?assertEqual(<<"0102">>,nextday(<<"0101">>)), %% 元旦 月初

  ?assertEqual(<<"20170301">>,nextday(<<"20170228">>)),  %% 闰月 月末
  ?assertEqual(<<"0301">>,nextday(<<"0228">>)),  %% 闰月 月末

  ?assertEqual(errortime,nextday(<<"20170229">>)),  %% 闰月 29日
  ok
  .


%%返回指定天数之后的日期
-spec inc_days(Yyyymmdd,Days) -> Date when
  Yyyymmdd :: types:date_yyyymmdd(),
  Days :: integer(),
  Date :: types:date_yyyymmdd().

inc_days(Date , Days) when byte_size(Date) =:= 8  ->
  case calendar:valid_date(datetime_x:yyyymmdd_to_tuple(Date)) of
      true->
        Day = calendar:date_to_gregorian_days(datetime_x:yyyymmdd_to_tuple(Date)),
        datetime_x:tuple_to_yyyymmdd(calendar:gregorian_days_to_date(Day+Days));
      false->
        errortime
  end.

%%inc_day函数测试
inc_days_test()->
  ?assertEqual(<<"20170131">>,inc_days(<<"20161231">>,31)), %% 年末 月末
  ?assertEqual(<<"20170106">>,inc_days(<<"20170101">>,5)), %% 元旦 月初
  ?assertEqual(<<"20170310">>,inc_days(<<"20170228">>,10)),  %% 闰月 月末
  ?assertEqual(errortime,inc_days(<<"20170229">>,10)),  %% 闰月 29日

  ok
.


%%返回指定天数之前的日期
-spec dec_days(Yyyymmdd,Days) -> Date when
  Yyyymmdd :: types:date_yyyymmdd(),
  Days :: integer(),
  Date :: types:date_yyyymmdd().

dec_days(Date , Days) when byte_size(Date) =:= 8 ->
  case calendar:valid_date(datetime_x:yyyymmdd_to_tuple(Date)) of
      true->
        Day = calendar:date_to_gregorian_days(datetime_x:yyyymmdd_to_tuple(Date)),
        datetime_x:tuple_to_yyyymmdd(calendar:gregorian_days_to_date(Day-Days));
      false->
        errortime
  end.

%%dec_days函数测试
dec_days_test()->
  ?assertEqual(<<"20161130">>,dec_days(<<"20161231">>,31)), %% 年末 月末
  ?assertEqual(<<"20161227">>,dec_days(<<"20170101">>,5)), %% 元旦 月初
  ?assertEqual(<<"20170218">>,dec_days(<<"20170228">>,10)),  %% 闰月 月末
  ?assertEqual(errortime,dec_days(<<"20170229">>,10)),  %% 闰月 29日
  ok
.