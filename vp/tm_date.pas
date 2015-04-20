Unit TM_DATE;

Interface

uses Use32;

Const
     MonthStr : array[1..12] of string[12] = ('January', 'February',
                'March', 'April', 'May', 'June', 'July', 'August',
                'September', 'October', 'November', 'December');
     DayStr : array[0..6] of string[15] = ('Sunday', 'Monday',
              'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
     AMPMStr  : array[FALSE..TRUE] of string[2] = ('am','pm');
     CDaysInMonth : Array [1..12] of Byte =
                    (31,28,31,30,31,30,31,31,30,31,30,31);

     DaysInWeek      = 7;
     SecondsInMinute = 60;
     MinutesInHour   = 60;
     HoursInDay      = 24;
     SecondsInHour   = SecondsInMinute*MinutesInHour;
     SecondsInDay    = SecondsInMinute*MinutesInHour*HoursInDay;


function IsLeapYear(y : Integer) : Boolean;
function Julian (Y,M,D : Word)   : LongInt; { Days  since 0000 }
Function JulianDay(y,m,d : word) : integer;
Function DaysInMonth(M,Y : word) : byte;
function TodayJulian : longint;

Function IsPM       : Boolean;
Function TodayDay   : byte;
Function TodayMonth : byte;
Function TodayYear  : Word;
Function TodayDOW   : byte;
Function HourNow    : byte;
Function Hour12Now  : byte;
Function MinuteNow  : byte;
Function SecondNow  : byte;
Function HundredthNow:byte;

Function SecondsToday: word;
Function HundredthSecondsToday: longint;


{**********************************************************************}
Implementation
uses dos;

function IsLeapYear(y : Integer) : Boolean;
begin
     IsLeapYear:=(y mod 4 = 0) and (y mod 4000 <> 0)
               and((y mod 400 <> 0) or (y mod 100 = 0));
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function TodayDay   : byte;
var
    Y, M, D, DOW : Word;
begin
    GetDate(Y, M, D, DOW);
    TodayDay := D;
end;
{----------------------------------------------------------------------}
Function TodayMonth : byte;
var
    Y, M, D, DOW : Word;
begin
    GetDate(Y, M, D, DOW);
    TodayMonth:=M;
end;
{----------------------------------------------------------------------}
Function TodayYear  : Word;
var
    Y, M, D, DOW : Word;
begin
    GetDate(Y, M, D, DOW);
    TodayYear := Y;
end;
{----------------------------------------------------------------------}
Function TodayDOW   : byte;
var
    Y, M, D, DOW : Word;
begin
    GetDate(Y, M, D, DOW);
    TodayDow := Dow;
end;
{----------------------------------------------------------------------}
Function IsPM : Boolean;
var H, M, S, D : word;
begin
     if HourNow>=12 then IsPm:=TRUE else IsPM:=FALSE;
end;
{----------------------------------------------------------------------}
Function HourNow    : byte;
var H, M, S, D : word;
begin
     GetTime(H,M,S,D);
     HourNow:=H;
end;
{----------------------------------------------------------------------}
Function Hour12Now    : byte;
var H, M, S, D : word;
begin
     GetTime(H,M,S,D);
     If H>12 then dec(h,12);
     if H=0 then H:=12;
     Hour12Now:=H;
end;
{----------------------------------------------------------------------}
Function MinuteNow  : byte;
var H, M, S, D : word;
begin
     GetTime(H,M,S,D);
     MinuteNow := M;
end;
{----------------------------------------------------------------------}
Function SecondNow  : byte;
var H, M, S, D : word;
begin
     GetTime(H,M,S,D);
     SecondNow:=S;
end;
{----------------------------------------------------------------------}
Function HundredthNow  : byte;
var H, M, S, D : word;
begin
     GetTime(H,M,S,D);
     HundredthNow:=D;
end;
{----------------------------------------------------------------------}
{--                         JULIAN                                   --}
{----------------------------------------------------------------------}
function Julian (Y,M,D : Word) : LongInt; { days since 0000 }
Begin
  Dec(y);
  Julian := (LongInt(Y)*365) + (Y div 4) - (Y div 100) +
               (Y div 400) + JulianDay(succ(Y),M,D);
End;
{----------------------------------------------------------------------}
{--                      Days in Month                               --}
{----------------------------------------------------------------------}
Function DaysInMonth(M,Y : word) : byte;
begin
     DaysInMonth:=CdaysInMonth[M];
     if (y<>0)and(M=2)and(IsLeapyear(Y)) then DaysInMonth:=29;
end;
{----------------------------------------------------------------------}
{--                  Julian Day                                      --}
{----------------------------------------------------------------------}
Function JulianDay(y,m,d : word) : integer;
var X : Word;
    C : Byte;
Begin
  X := 0;
  For c:= 1 to pred(m) do begin
      X := X + DaysInMonth(c,y);
  End;
  JulianDay := X + D;  (* add the days of the month *)
End;
{----------------------------------------------------------------------}
{--                    Today Julian                                  --}
{----------------------------------------------------------------------}
function TodayJulian : longint;
var
    Y, M, D, DOW : Word;
begin
    GetDate(Y, M, D, DOW);
    TodayJulian := Julian(Y,M,D);
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function SecondsToday: word;
var H, M, S, D : word;
begin
     GetTime(H,M,S,D);
     SecondsToday:=(H*SecondsInHour)+(M*SecondsInMinute)+S;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function HundredthSecondsToday: longint;
var H, M, S, D : word;
begin
     GetTime(H,M,S,D);
     HundredthSecondsToday:=(((H*SecondsInHour)+(M*SecondsInMinute)+S)*100)+D;
end;

END.
