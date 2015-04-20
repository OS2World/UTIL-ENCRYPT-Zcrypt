Unit TM_Log; {For Log File functions}

interface
uses tobjs, use32;

type
    Ltype = (LInfo, Lwarn, LError, LFatal);
    side  = (left, right);

    PLogObj = ^TLogObj;
    TLogObj = Object(Tobject)
         Ofm  : byte;           {Old Filemode holder}
         Fmode: byte;           {File Mode for opening}
         LMode: byte;           {Log mode (future use)}
         Open : boolean;        {Is log file open?}
         ErrC : byte;           {error code}
         F    : Text;           {actual log file}
         fn   : Pstring;        {path\filename of logfile}
         MaxLines : Word;       {maximum lines in log file}
         Brack : array[Linfo..Lfatal] of array[left..right] of char;
         ds   : char;           {date seperator}
         hms  : char;           {hour:minute seperator}
         mss  : char;           {minutes.seconds seperator}

         Constructor init(const nFn : string);
         Destructor done; virtual;
         Function OpenFile              : byte;
         Function CloseFile             : byte;
         Function EraseFile             : byte;
         Function SetFMode(b : byte)    : byte;
         Function getdatetime(b : Ltype): string; virtual;
         Function Log(b: Ltype; const s : string) : byte;
         Function LogW(b: Ltype; const s : string) : byte;
         Function Err(b : byte; s : string; Die : boolean) : byte;

    end;

{**********************************************************************}
{----------------------------------------------------------------------}
implementation
uses TM_STR, {TM_OBJS,} TM_DOS, DOS;
{----------------------------------------------------------------------}
Constructor TLogObj.Init(const nfn:string);
begin
     OfM  := Filemode;
     Fmode:= fmDenyNone+fmReadWrite;
     ds := '-';
     hms:= ':';
     mss:= '.';
     ErrC := 0;
     MaxLines:=0;
     LMode:=0;
     Assign(f,nfn);
     fn := Newstr(nfn);
     Brack[Linfo,left] :='[';Brack[Linfo,right] :=']';
     Brack[Lwarn,left] :='(';Brack[Lwarn,right] :=')';
     Brack[Lerror,left]:='{';Brack[Lerror,right]:='}';
     Brack[Lfatal,left]:='<';Brack[Lfatal,right]:='>';
end;
{----------------------------------------------------------------------}
Destructor TLogObj.Done;
begin
     If Open then CloseFile;
     DisposeStr(fn);
end;
{----------------------------------------------------------------------}
Function TlogObj.OpenFile : byte;
begin
     {$I-}append(f);{$I+}
     if IoResult>0 then begin
        {$I-}rewrite(F);{$I+}
        if IoResult>0 then begin
           OpenFile:=Ioresult;
           Open := FALSE;
           exit;
        end;
     end;
     Open:=TRUE;
     OpenFile:=0;
end;
{----------------------------------------------------------------------}
Function TlogObj.CloseFile : byte;
begin
     closefile:=$FF;
     if Open then begin
        {$I-}close(f);{$I+}
        CloseFile:=IoResult;
        Open:=FALSE;
     end;
end;
{----------------------------------------------------------------------}
Function TlogObj.EraseFile : byte;
begin
     If Open then closeFile;
     {$I-}erase(f);{$I+}
     if IoResult=0 then begin
        Open:=FALSE;
        EraseFile:=0;
     end else EraseFile:=IoResult;
end;
{----------------------------------------------------------------------}
Function TlogObj.SetFMode( b: byte) : byte;
begin
end;
{----------------------------------------------------------------------}
Function TlogObj.Log(b : Ltype; const s : string) : byte;
var x : byte;
begin
     If (not Open) then x:=OpenFile;
     if x>0 then begin Log:=x; exit; end;
     {$I-}Writeln(f, GetDateTime(b)+'  '+s);{$I+}
     x:=IOResult;
     if x>0 then begin Log:=x; exit; end;
     Log:=CloseFile;
end;
{----------------------------------------------------------------------}
Function TlogObj.LogW(b : Ltype; const s : string) : byte;
begin
     writeln(s);
     LogW:=Log(b,s);
end;
{----------------------------------------------------------------------}
Function Tlogobj.GetDateTime(b : Ltype) : string;
var
   y,m,d,dow,h,n,s,r : word;
begin
     GetDate(y,m,d,dow);
     GetTime(h,n,s,r);
     dec(y,1800); while y>=100 do dec(y,100);
     GetDateTime:=Brack[b][Left]+Leadzero(y,2)+ds+Leadzero(m,2)+ds+Leadzero(d,2)+' '
       +Leadzero(h,2)+hms+Leadzero(n,2)+mss+Leadzero(s,2)+Brack[b][right];
end;
{----------------------------------------------------------------------}
Function TlogObj.Err(b : byte; s : string; Die : boolean) : byte;
var
   stmp : string[5];
begin
     Writeln;
     If Die=TRUE then Write('Terminal ') else write('Warning ');
     Writeln('Error #',b,': '+S);
     str(b,stmp);
     s:= '['+stmp+'] '+s;
     if Die=TRue then Err:=Log(LFatal,s) else Err:=Log(LWARN,s);
     If DIE=TRUE then begin
        done;
        Halt(b);
     end;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
END.
