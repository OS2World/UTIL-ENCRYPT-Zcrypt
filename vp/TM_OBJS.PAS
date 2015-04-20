UNIT TM_OBJS;

Interface
uses dos;

{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
Type

    Tobject = Object
            Constructor Init;
            Destructor Done; Virtual;
    end;

    Pstring = ^String;


    PStrObj = ^TStrObj;
    TStrObj = Object(Tobject)
            S : Pstring;
            Constructor Init(NS : String);
            Destructor Done; virtual;
    end;

    PTextCollection = ^TTextCollection;                    {** ONE based! **}
    TTextCollection = Object({TCollection}Tobject)                   {not zero based}
            Function  Load(fn : string) : byte;
            Function  Save(fn : string; OvrWrt : boolean) : byte;
            Function  Get(i : integer) : string;
            Procedure Add(s : String);
            Procedure Replace(i : integer; s : String);
            Procedure Del(i : integer);
            Procedure Empty;
    end;

    PTextColScroll = ^TTextColScroll;
    TTextColScroll = Object(Tobject)
            Constructor init (nX, nY, nW, nH, nAt : byte; nC : PTextCollection);
            Procedure DrawLine(I : integer; Row : byte);
            Procedure DrawList{(i : integer)}; {starting at item number}
            Procedure DrawTopLine;
            Procedure DrawBotLine;
            Procedure ScrollUp;
            Procedure ScrollDn;
            Procedure PageUp;
            Procedure PageDn;
            Procedure GoHome;
            Procedure GoEnd;
            Procedure ScrollRt;
            Procedure ScrollLt;
            Procedure HardLeft;
            Procedure HandleKeys; Virtual;
            Procedure MoreKeys(mk : word); Virtual;
            Destructor Done; Virtual;
            Private
            C : PTextCollection;
            At : byte; {attribuate}
            X,Y,W,H : byte;
            CTL     : Word; {current Top Line}
            Indent  : byte; {offset to the right}
            Stmp    : string;
    end;

    P4Desc = ^T4Desc;
    T4Desc = object(Tobject)
           C : PTextCollection;
           B : array[1..1024] of byte;
           ps: Pstring;
           st: string;
           path: pathstr;
           Changed : boolean;
           Loaded : Boolean;
           f : text;
           Constructor Init(s : string);
           Procedure NewFile(s : string);
           Function FindFile(fn : pathstr): integer;
           Function GetDesc(num : integer) : string;
           Procedure PutDesc(const fn, s : string);
           Procedure WriteDescs;
           Destructor Done; virtual;
    end;

    Function NewStr(s : string) : pointer;
    Procedure DisposeStr(p : pointer);
    Procedure DispoStr(var P : Pstring);

{----------------------------------------------------------------------}
{**********************************************************************}
{----------------------------------------------------------------------}
Implementation
Uses TM_STR, TM_SCR, tm_crt;

Constructor Tobject.init;
begin
end;

Destructor Tobject.done;
begin
end;

Function Newstr( s: string) : pointer;
var p : pointer;
begin
     Getmem(p, succ(length(s)));
     if p <> nil then begin
        move(s,p^,succ(length(s)));
        NewStr:=p;
     end;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Constructor TStrObj.init(NS : String);
begin
     S := NewStr(NS);
end;
Destructor TStrObj.Done;
begin
     if s^<>'' then dispostr(S);
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function  TTextCollection.Get(i : integer) : string;
var ps : pstrobj;
begin
{     Get:='';
     if (i>0)and(i<=Count) then begin
        ps:=at(pred(i));
        if ps^.s<>nil then Get:=ps^.s^;
     end;}
end;
{----------------------------------------------------------------------}
Procedure TTextCollection.Add(s : String);
begin
{     insert(new(PStrObj, init(s)));}
end;
{----------------------------------------------------------------------}
Procedure TTextCollection.Replace(i : integer; s : String);
begin
{     if (i>0)and(i<=Count) then begin
        atput(pred(i),new(PStrObj, init(s)));
     end;}
end;
{----------------------------------------------------------------------}
Procedure TTextCollection.Del(i : integer);
begin
{     if (i>0)and(i<=Count) then begin
        atFree(pred(i));
     end;                             }
end;
{----------------------------------------------------------------------}
Procedure TTextCollection.Empty;
begin;
      while count>0 do atfree(0);
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function  TTextCollection.Load(fn : string) : byte;
var F : text;
    s : string;
begin
     Assign(f,fn); {$I-}reset(f);{$I+}
     if IoResult=0 then begin
        while not EOF(f) do begin
              readln(f,s);
              Add(s);
        end;
        load:=0;
        Close(F);
     end else Load:=IoResult;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function  TTextCollection.Save(fn : string; OvrWrt : boolean) : byte;
var F : text;
    x : word;
begin
     Assign(f,fn); {$I-}reset(f);{$I+}
     if IoResult=0 then begin
        if OvrWrt=TRUE then {$I-}rewrite(f){$I+}
           else {$I-}append(f);{$I+}
        if IoResult=0 then begin
           for x:=1 to count do Writeln(f,Get(x));
           Save:=0;
           close(f);
        end else Save:=IoResult;
     end else Save:=IoResult;
end;
{**********************************************************************}
{----------------------------------------------------------------------}
{--                     Scroller                                     --}
{----------------------------------------------------------------------}
Constructor TTextColScroll.init(nX, nY, nW, nH, nAt : byte; nC : PTextCollection);
begin
     x:=nx;y:=ny;h:=nh;w:=nw;at:=nat;C:=nc;
     if (C=nil)or(C^.Count=0) then fail;
     CTL:=1; Indent := 1;
     DrawList{(CTL)};
end;
{----------------------------------------------------------------------}
Destructor TTextColScroll.Done;
begin
     inherited Done;
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.DrawLine(i : integer; row : byte);
begin
     if (i)<=C^.Count then begin
        Stmp:=C^.Get(i);
        Stmp:=Copy(stmp,Indent,w);
     end else stmp:='';
     Qwrite(x,row,Rpad(stmp,w,' '),at);
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.DrawList{(i : integer)};
var l : byte;
begin
     for l := 0 to pred(h) do begin
         DrawLine(ctl+l,y+l);
     end;
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.DrawTopLine;
begin
     DrawLine(CTL,y);
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.DrawBotLine;
begin
     DrawLine(CTL+Pred(h),y+pred(h));
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.PageUp;
begin
     if Ctl>1 then begin
        if CTL<=H then CTL:=1 else CTL:=CTL-H;
        DrawList{(CTL)};
     end;
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.PageDn;
begin
     if Ctl<=(C^.Count-H) then begin
        CTL:=CTL+H;
        if CTL>C^.COUNT-H then CTL:=Succ(C^.Count-H);
        DrawList{(CTL)};
     end;
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.ScrollDn;
begin
     if Ctl>1 then begin
        Dec(CTL);
        DScroll(x,y,pred(x+w),Pred(y+h),1,at);
        DrawTopLine;
     end;
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.ScrollUp;
begin
     if Ctl<C^.count then begin
        Inc(CTL);
        UScroll(x,y,pred(x+w),Pred(y+h),1,at);
        DrawBotLine;
     end;
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.GoHome;
begin
     if Ctl>1 then begin
        InDent:=1;
        CTL:=1;
        DrawList{(CTL)};
     end;
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.GoEnd;
begin
     if Ctl<=C^.Count-H then begin
        CTL:=succ(C^.Count-h);
        DrawList{(CTL)};
     end;
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.ScrollRt;
begin
     if Indent>1 then begin
        dec(Indent);
        DrawList{(CTL)};
     end;
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.ScrollLt;
begin
     inc(Indent);
     DrawList{(CTL)};
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.HardLeft;
begin
     if Indent>1 then begin
        Indent:=1;
        DrawList{(CTL)};
     end;
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.HandleKeys;
var ch : word;
begin
    repeat
          getkey(ch);
          case ch of
               kb_pgup : pageup;
               kb_pgdown: pagedn;
               kb_up   : ScrollDn;
               kb_down : ScrollUp;
               kb_home : GoHome;
               kb_End  : GoEnd;
               Kb_Right: ScrollLt;
               Kb_Left : ScrollRt;
               Kb_CTRLLeft: HardLeft;
          end;
          MoreKeys(ch);
    until ch=kb_esc;
end;
{----------------------------------------------------------------------}
Procedure TTextColScroll.MoreKeys(mk : word);
begin
end;
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
Constructor T4Desc.Init(s : string);
begin
end;
{----------------------------------------------------------------------}
Procedure   T4Desc.NewFile(s : string);
begin
end;
{----------------------------------------------------------------------}
Function    T4Desc.FindFile(fn : PathStr) : integer;
begin
end;
{----------------------------------------------------------------------}
Function    T4Desc.GetDesc(num : integer) : string;
begin
end;
{----------------------------------------------------------------------}
Procedure   T4Desc.PutDesc(const fn, s : string);
begin
end;
{----------------------------------------------------------------------}
Procedure   T4Desc.WriteDescs;
begin
end;
{----------------------------------------------------------------------}
Destructor  T4Desc.Done;
begin
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure DispoStr(var P : Pstring);
begin
     DisposeStr(P);
     P:=nil;
end;
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{**********************************************************************}
END.
