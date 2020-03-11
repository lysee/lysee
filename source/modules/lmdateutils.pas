{==============================================================================}
{     PROJECT: lysee_datautils                                                 }
{ DESCRIPTION: datatime utilities                                              }
{   COPYRIGHT: Copyright (c) 2012, Li Yun Jie. All Rights Reserved.            }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2012/03/07                                                      }
{    MODIFIED: 2013/09/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmdateutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, lysee;

function encodeGMT(Date: TDateTime): string;
function decodeGMT(const GMT: string): TDateTime;

  function EncodeDate(Year, Month, Day :word): TDateTime;
  function EncodeTime(Hour, Minute, Second, MilliSecond:word): TDateTime;
  function ComposeDateTime(Date,Time : TDateTime) : TDateTime;
  function DayOfWeek(DateTime: TDateTime): integer;
  function Date: TDateTime;
  function Time: TDateTime;
  function Now: TDateTime;
  function IncMonth(const DateTime: TDateTime; NumberOfMonths: integer): TDateTime;
  function IsLeapYear(Year: Word): boolean;
  function DateToStr(Date: TDateTime): string;
  function TimeToStr(Time: TDateTime): string;
  function DateTimeToStr(DateTime: TDateTime): string;
  function StrToDate(const S: Ansistring): TDateTime;
  function StrToTime(const S: Ansistring): TDateTime;
  function StrToDateTime(const S: string): TDateTime;
  function FormatDateTime(const FormatStr: string; DateTime: TDateTime):string;
  function DateTimeToFileDate(DateTime : TDateTime) : Longint;
  function FileDateToDateTime (Filedate : Longint) :TDateTime;
  function StrToDateDef(const S: AnsiString; const Defvalue : TDateTime): TDateTime;
  function StrToTimeDef(const S: AnsiString; const Defvalue : TDateTime): TDateTime;
  function StrToDateTimeDef(const S: AnsiString; const Defvalue : TDateTime): TDateTime;
  function CurrentYear:Word;

  function DateOf(const AValue: TDateTime): TDateTime;
  function TimeOf(const AValue: TDateTime): TDateTime;
  // Identification functions.
  function IsInLeapYear(const AValue: TDateTime): Boolean;
  function IsPM(const AValue: TDateTime): Boolean;
  function IsValidDate(const AYear, AMonth, ADay: Word): Boolean;
  function IsValidTime(const AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
  function IsValidDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
  function IsValidDateDay(const AYear, ADayOfYear: Word): Boolean;
  function IsValidDateWeek(const AYear, AWeekOfYear, ADayOfWeek: Word): Boolean;
  function IsValidDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word): Boolean;
  // Enumeration functions.
  function WeeksInYear(const AValue: TDateTime): Word;
  function WeeksInAYear(const AYear: Word): Word;
  function DaysInYear(const AValue: TDateTime): Word;
  function DaysInAYear(const AYear: Word): Word;
  function DaysInMonth(const AValue: TDateTime): Word;
  function DaysInAMonth(const AYear, AMonth: Word): Word;
  // Variations on current date/time.
  function Today: TDateTime;
  function Yesterday: TDateTime;
  function Tomorrow: TDateTime;
  function IsToday(const AValue: TDateTime): Boolean;
  function IsSameDay(const AValue, ABasis: TDateTime): Boolean;
  function PreviousDayOfWeek (DayOfWeek : Word) : Word;
  // Extraction functions.
  function YearOf(const AValue: TDateTime): Word;
  function MonthOf(const AValue: TDateTime): Word;
  function WeekOf(const AValue: TDateTime): Word;
  function DayOf(const AValue: TDateTime): Word;
  function HourOf(const AValue: TDateTime): Word;
  function MinuteOf(const AValue: TDateTime): Word;
  function SecondOf(const AValue: TDateTime): Word;
  function MilliSecondOf(const AValue: TDateTime): Word;
  // Start/End of year functions.
  function StartOfTheYear(const AValue: TDateTime): TDateTime;
  function EndOfTheYear(const AValue: TDateTime): TDateTime;
  function StartOfAYear(const AYear: Word): TDateTime;
  function EndOfAYear(const AYear: Word): TDateTime;
  // Start/End of month functions.
  function StartOfTheMonth(const AValue: TDateTime): TDateTime;
  function EndOfTheMonth(const AValue: TDateTime): TDateTime;
  function StartOfAMonth(const AYear, AMonth: Word): TDateTime;
  function EndOfAMonth(const AYear, AMonth: Word): TDateTime;
  // Start/End of week functions.
  function StartOfTheWeek(const AValue: TDateTime): TDateTime;
  function EndOfTheWeek(const AValue: TDateTime): TDateTime;
  function StartOfAWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
  function EndOfAWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
  // Start/End of day functions.
  function StartOfTheDay(const AValue: TDateTime): TDateTime;
  function EndOfTheDay(const AValue: TDateTime): TDateTime;
  function StartOfADay(const AYear, AMonth, ADay: Word): TDateTime;
  function EndOfADay(const AYear, AMonth, ADay: Word): TDateTime;
  // Part of year functions.
  function MonthOfTheYear(const AValue: TDateTime): Word;
  function WeekOfTheYear(const AValue: TDateTime): Word;
  function DayOfTheYear(const AValue: TDateTime): Word;
  function HourOfTheYear(const AValue: TDateTime): Word;
  function MinuteOfTheYear(const AValue: TDateTime): LongWord;
  function SecondOfTheYear(const AValue: TDateTime): LongWord;
  function MilliSecondOfTheYear(const AValue: TDateTime): Int64;
  // Part of month functions.
  function WeekOfTheMonth(const AValue: TDateTime): Word;
  function DayOfTheMonth(const AValue: TDateTime): Word;
  function HourOfTheMonth(const AValue: TDateTime): Word;
  function MinuteOfTheMonth(const AValue: TDateTime): Word;
  function SecondOfTheMonth(const AValue: TDateTime): LongWord;
  function MilliSecondOfTheMonth(const AValue: TDateTime): LongWord;
  // Part of week functions.
  function DayOfTheWeek(const AValue: TDateTime): Word;
  function HourOfTheWeek(const AValue: TDateTime): Word;
  function MinuteOfTheWeek(const AValue: TDateTime): Word;
  function SecondOfTheWeek(const AValue: TDateTime): LongWord;
  function MilliSecondOfTheWeek(const AValue: TDateTime): LongWord;
  // Part of day functions.
  function HourOfTheDay(const AValue: TDateTime): Word;
  function MinuteOfTheDay(const AValue: TDateTime): Word;
  function SecondOfTheDay(const AValue: TDateTime): LongWord;
  function MilliSecondOfTheDay(const AValue: TDateTime): LongWord;
  // Part of hour functions.
  function MinuteOfTheHour(const AValue: TDateTime): Word;
  function SecondOfTheHour(const AValue: TDateTime): Word;
  function MilliSecondOfTheHour(const AValue: TDateTime): LongWord;
  // Part of minute functions.
  function SecondOfTheMinute(const AValue: TDateTime): Word;
  function MilliSecondOfTheMinute(const AValue: TDateTime): LongWord;
  // Part of second functions.
  function MilliSecondOfTheSecond(const AValue: TDateTime): Word;
  // Range checking functions.
  function WithinPastYears(const ANow, AThen: TDateTime; const AYears: Integer): Boolean;
  function WithinPastMonths(const ANow, AThen: TDateTime; const AMonths: Integer): Boolean;
  function WithinPastWeeks(const ANow, AThen: TDateTime; const AWeeks: Integer): Boolean;
  function WithinPastDays(const ANow, AThen: TDateTime; const ADays: Integer): Boolean;
  function WithinPastHours(const ANow, AThen: TDateTime; const AHours: Int64): Boolean;
  function WithinPastMinutes(const ANow, AThen: TDateTime; const AMinutes: Int64): Boolean;
  function WithinPastSeconds(const ANow, AThen: TDateTime; const ASeconds: Int64): Boolean;
  function WithinPastMilliSeconds(const ANow, AThen: TDateTime; const AMilliSeconds: Int64): Boolean;
  // Period functions.
  function YearsBetween(const ANow, AThen: TDateTime): Integer;
  function MonthsBetween(const ANow, AThen: TDateTime): Integer;
  function WeeksBetween(const ANow, AThen: TDateTime): Integer;
  function DaysBetween(const ANow, AThen: TDateTime): Integer;
  function HoursBetween(const ANow, AThen: TDateTime): Int64;
  function MinutesBetween(const ANow, AThen: TDateTime): Int64;
  function SecondsBetween(const ANow, AThen: TDateTime): Int64;
  function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64;
  // YearSpan and MonthSpan are approximate values
  function YearSpan(const ANow, AThen: TDateTime): Double;
  function MonthSpan(const ANow, AThen: TDateTime): Double;
  function WeekSpan(const ANow, AThen: TDateTime): Double;
  function DaySpan(const ANow, AThen: TDateTime): Double;
  function HourSpan(const ANow, AThen: TDateTime): Double;
  function MinuteSpan(const ANow, AThen: TDateTime): Double;
  function SecondSpan(const ANow, AThen: TDateTime): Double;
  function MilliSecondSpan(const ANow, AThen: TDateTime): Double;
  // Increment/decrement functions.
  function IncYear(const AValue: TDateTime; const ANumberOfYears: Integer ): TDateTime;
  function IncWeek(const AValue: TDateTime; const ANumberOfWeeks: Integer): TDateTime;
  function IncDay(const AValue: TDateTime; const ANumberOfDays: Integer): TDateTime;
  function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64): TDateTime;
  function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime;
  function IncSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64): TDateTime;
  function IncMilliSecond(const AValue: TDateTime; const ANumberOfMilliSeconds: Int64): TDateTime;
  // Encode/Decode of complete timestamp
  function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
  // Encode/decode date, specifying week of year and day of week
  function EncodeDateWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
  // Encode/decode date, specifying day of year
  function EncodeDateDay(const AYear, ADayOfYear: Word): TDateTime;
  // Encode/decode date, specifying week of month
  function EncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word): TDateTime;
  // Replace given element with supplied value.
  function RecodeYear(const AValue: TDateTime; const AYear: Word): TDateTime;
  function RecodeMonth(const AValue: TDateTime; const AMonth: Word): TDateTime;
  function RecodeDay(const AValue: TDateTime; const ADay: Word): TDateTime;
  function RecodeHour(const AValue: TDateTime; const AHour: Word): TDateTime;
  function RecodeMinute(const AValue: TDateTime; const AMinute: Word): TDateTime;
  function RecodeSecond(const AValue: TDateTime; const ASecond: Word): TDateTime;
  function RecodeMilliSecond(const AValue: TDateTime; const AMilliSecond: Word): TDateTime;
  function RecodeDate(const AValue: TDateTime; const AYear, AMonth, ADay: Word): TDateTime;
  function RecodeTime(const AValue: TDateTime; const AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
  function RecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
  // Comparision of date/time
  function CompareDateTime(const A, B: TDateTime): integer;
  function CompareDate(const A, B: TDateTime): integer;
  function CompareTime(const A, B: TDateTime): integer;
  function SameDateTime(const A, B: TDateTime): Boolean;
  function SameDate(const A, B: TDateTime): Boolean;
  function SameTime(const A, B: TDateTime): Boolean;
  // For a given date these Functions tell you the which day of the week of the
  // month (or year).  If its a Thursday, they will tell you if its the first,
  // second, etc Thursday of the month (or year).  Remember, even though its
  // the first Thursday of the year it doesn't mean its the first week of the
  // year.  See ISO 8601 above for more information.
  function NthDayOfWeek(const AValue: TDateTime): Word;
  function EncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek,  ADayOfWeek: Word): TDateTime;
  // Exception throwing routines
  procedure InvalidDateTimeError(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word);
  procedure InvalidDateWeekError(const AYear, AWeekOfYear, ADayOfWeek: Word);
  procedure InvalidDateDayError(const AYear, ADayOfYear: Word);
  procedure InvalidDateMonthWeekError(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word);
  procedure InvalidDayOfWeekInMonthError(const AYear, AMonth, ANthDayOfWeek,  ADayOfWeek: Word);
  // Julian and Modified Julian Date conversion support
  function DateTimeToJulianDate(const AValue: TDateTime): Double;
  function JulianDateToDateTime(const AValue: Double): TDateTime;
  function DateTimeToModifiedJulianDate(const AValue: TDateTime): Double;
  function ModifiedJulianDateToDateTime(const AValue: Double): TDateTime;
  // Unix timestamp support.
  function DateTimeToUnix(const AValue: TDateTime): Int64;
  function UnixToDateTime(const AValue: Int64): TDateTime;
  function UnixTimeStampToMac(const AValue: Int64): Int64;
  // Mac timestamp support.
  function DateTimeToMac(const AValue: TDateTime): Int64;
  function MacToDateTime(const AValue: Int64): TDateTime;
  function MacTimeStampToUnix(const AValue: Int64): Int64;
  // Dos <-> Delphi datetime support
  function DateTimeToDosDateTime(const AValue: TDateTime): longint;
  function DosDateTimeToDateTime( AValue: longint): TDateTime;
  // ScanDateTime is a limited inverse of formatdatetime
  function ScanDateTime(const Pattern:string;const s:string;startpos:integer=1) : tdatetime;

implementation

{ GMT }

const
  GMT_WEEKDAY: array[1..7] of string = (
    'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'
  );

  GMT_MONTH: array[1..12] of string = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
  );

  GMT_FORMAT = '"%s", dd "%s" yyyy hh:nn:ss "GMT"';

function encodeGMT(Date: TDateTime): string;
var
  week, month: string;

  function weekday_str(DateTime: TDateTime): string;
  begin
    Result := GMT_WEEKDAY[DayOfWeek(DateTime)];
  end;

  function month_str(DateTime: TDateTime): string;
  var
    year, month, day: word;
  begin
    DecodeDate(DateTime, year, month, day);
    Result := GMT_MONTH[month];
  end;

begin
  if Date > 0 then
  begin
    week := weekday_str(Date);
    month := month_str(Date);
    Result := Format(FormatDateTime(GMT_FORMAT, Date), [week, month])
  end
  else Result := '';
end;

function decodeGMT(const GMT: string): TDateTime;
var
  yy, mm, dd, hh, nn, ss: Integer;
  line: string;

  function get_token(var token: string): boolean;
  const
    TOKENCH = ['A'..'Z', 'a'..'z', '0'..'9'];
  var
    index, count: integer;
  begin
    count := Length(line);
    index := 1;
    while (index < count) and (line[index] in TOKENCH) do Inc(index);
    Result := (index < count) and (index > 1);
    if Result then
    begin
      token := Copy(line, 1, index - 1);
      while (index < count) and not (line[index] in TOKENCH) do Inc(index);
      line := Copy(line, index, MaxInt);
    end;
  end;

  function skip_weekday: boolean;
  var
    token: string;
    index: integer;
  begin
    Result := get_token(token);
    if Result then
    begin
      token := Copy(token, 1, 3);
      for index := Low(GMT_WEEKDAY) to High(GMT_WEEKDAY) do
        if LySameText(token, GMT_WEEKDAY[index]) then
          Exit;
      Result := false;
    end;
  end;

  function get_month(var month: integer): boolean;
  var
    token: string;
    index: integer;
  begin
    Result := get_token(token);
    if Result then
    begin
      token := Copy(token, 1, 3);
      for index := Low(GMT_MONTH) to High(GMT_MONTH) do
        if LySameText(token, GMT_MONTH[index]) then
        begin
          month := index;
          Exit;
        end;
      Result := false;
    end;
  end;

  function get_int(var value: integer; min, max: integer): boolean;
  var
    token: string;
  begin
    Result := get_token(token);
    if Result then
    begin
      Result := TryStrToInt(token, value);
      if Result then
        Result := (value >= min) and (value <= max);
    end;
  end;

begin
  { Sun, 20 Jan 2008 14:58:45 GMT }
  Result := -1;
  line := Trim(GMT);
  if not (skip_weekday and get_int(dd, 1, 31) and get_month(mm) and
          get_int(yy, 1, 9999) and get_int(hh, 0, 24) and
          get_int(nn, 0, 60) and get_int(ss, 0, 60) and
          TryEncodeDateTime(yy, mm, dd, hh, nn, ss, 0, Result)) then
    Result := -1;
end;

end.

