Unit TM_SCR;

Interface
{uses use32;}
uses os2base;
Const
     VidSeg : Word = $B800;
     DefaultAttr : byte = $07;
     FrameChar : array[1..8] of char = ('Ú','Ä','¿','³','³','À','Ä','Ù');
     SnglFrame : array[1..8] of char = ('Ú','Ä','¿','³','³','À','Ä','Ù');
     DblFrame  : array[1..8] of char = ('É','Í','»','º','º','È','Í','¼');
     DblVFrame : array[1..8] of char = ('Ö','Ä','·','º','º','Ó','Ä','½');
     DblHFrame : array[1..8] of char = ('Õ','Í','¸','³','³','Ô','Í','¾');
     Sld1Frame : array[1..8] of char = ('Û','Û','Û','Û','Û','Û','Û','Û');
     Sld2Frame : array[1..8] of char = ('Ü','Ü','Ü','Û','Û','ß','ß','ß');
     Sld3Frame : array[1..8] of char = ('Û','ß','Û','Û','Û','Û','Ü','Û');
     Sld4Frame : array[1..8] of char = ('ß','ß','ß','Þ','Ý','Ü','Ü','Ü');
     Chr1Frame : array[1..8] of char = ('+','-','+','|','|','+','-','+');
     Chr2Frame : array[1..8] of char = (',','-','.','|','|','`','-','''');
     Chr3Frame : array[1..8] of char = ('(','-',')','(',')','(','-',')');

Type
    TWordArray = Array[0..$FFF8 div 2] of Word;
    PWordArray = ^TWordArray;
    TbyteArray = Array[0..$FFF8] of byte;
    PByteArray = ^TByteArray;

Var
   CurrVioMode : VioModeInfo;
   MaxCol : WORD;
   GetRows: word;
   {VMode  : BYTE ABSOLUTE $0040:$0049;} { Video mode: Mono=7, Color=0-3 }

{----------------------------------------------------------------------}
 procedure qwrite(x, y: word; s: string; at: byte); {Quick Write /w attrib}
 Procedure GotoXY(X,Y : word);                      {fast goto x,y}
 Function  WhereX : word;                           {fast wherey}
 Function  WhereY : word;                           {fast wherex}
 Function  VidOfs(x,y : word) : word;               {calculate video offset}
 Function  getcRows : word;                          {Max Rows on screen}
 procedure BlinkOn(IsOn:boolean);                   {allow blink colours?}
 Procedure ScreenOff;                               {turn video outout off}
 Procedure ScreenOn;                                {turn it back on again}

 Function  GetScrChar(x,y : word) : char;           {get Char from screen}
 Procedure PutScrChar(x,y : word; c: char);         {put char to screen}
 Function  GetScrAttr(x,y : word) : byte;           {get attribute from vid}
 Procedure PutScrAttr(x,y : word; b : byte);        {put attribute to screen}

 Procedure UScroll(x1,y1,x2,y2,Lines: word;at : Byte);    {scroll up screen area}
 Procedure DScroll(x1,y1,x2,y2,Lines: word;at : Byte);    {scroll down scr area}

 Function  SaveWindow(x1,y1,w1,h1 : word; var P1: Pointer; SAlloc : Boolean) : Boolean;
 Function  RestoreWindow(x1,y1,w1,h1 : word; var p : Pointer; DeAlloc : boolean) : boolean;
 Procedure FrameWindow(x1,y1,w1,h1: word; at : byte);
 Procedure FrameOWindow(x1,y1,w1,h1: word; at : byte);
 Procedure ShadowWindow(x1,y1,w1,h1, kind, pos : byte);
 Procedure SetFrameChar(c1,c2,c3,c4,c5,c6,c7,c8 : char);
 Procedure ClearWindow(x1,y1,w1,h1: word; at : byte);
 Procedure ClearWindowChar(x1,y1,w1,h1: word; at : byte; ch : char);
 Procedure ClearScreen(at: byte);

 Procedure MeltWindow(x1,y1,w1,h1 : byte);
 Procedure MeltScreen;
 Procedure MeltRotWindow(x1,y1,w1,h1 : byte);
 Procedure MeltRotScreen;

{**********************************************************************}
{----------------------------------------------------------------------}
{**********************************************************************}
Implementation
Uses Tm_str;
{----------------------------------------------------------------------}
Procedure GotoXY(X,Y : Word);
begin
     VioSetCurPos(pred(y),pred(x),0);
end;
{----------------------------------------------------------------------}
Function  WhereX : word;
var row,col : word;
begin
     VioGetCurPos(Row,Col,0);
     WhereX:=pred(Col);
end;
{----------------------------------------------------------------------}
Function WhereY : word;
var row,col : word;
begin
     VioGetCurPos(Row,Col,0);
     WhereY:=pred(Row);
end;
{----------------------------------------------------------------------}
procedure BlinkOn(IsOn:boolean);
var Intensity : VioIntensity;
begin
  Intensity.cb := SizeOf(VioIntensity);
  Intensity.rType := 2;
  VioSetState(Intensity, word(IsOn));
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function VidOfs(x,y: word) : word;
begin
{     VidOfs:=(((pred(y) * MaxCol) + pred(x)) * 2);}
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure ScreenOn;
Begin
{  Port[$3C4] := 1;
  Port[$3C5] := $00;}
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure ScreenOff;
Begin
{  Port[$3C4] := 1;
  Port[$3C5] := Port[$3C5] or $20;}
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function getcRows : word;
var VioMode : VioModeInfo;
begin
     VioMode.cb := sizeof(VioModeInfo);
     VioGetMode(VioMode,0);
     GetRows := VioMode.Row;
end;
{----------------------------------------------------------------------}
procedure qwrite(x, y: word; s: string; at: byte);
begin
     VioWrtCharStrAtt(@s[1],length(s),pred(y),pred(x),at,0);
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure UScroll(x1,y1,x2,y2,Lines: word; at : Byte);
begin
end;
{----------------------------------------------------------------------}
Procedure DScroll(x1,y1,x2,y2,Lines: word; at : Byte);
begin
end; { DScroll }
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function  SaveWindow(x1,y1,w1,h1: word; var P1: Pointer; SAlloc : Boolean) : Boolean;
var x,y: byte;
    c  : word;
    s  : word;
begin
     s := sizeof(s);
     SaveWindow:=False;
     if SAlloc then begin
        c:=(w1*h1)*2;
        if maxavail<c then exit;
        Getmem(p1,c);
     end;
     c:=0;
     for y:=0 to pred(h1) do begin
         for x := 0 to pred(w1) do begin
{             PWordArray(p1)^[c]:=MemW[vidseg:vidOfs(x1+x,y1+y)];}
             VioReadCellStr(PWordArray(p1)^[c],s, pred(y),pred(x),0);
             inc(c);
         end;
     end;
     SaveWindow:=TRUE;
end;
{----------------------------------------------------------------------}
Function  RestoreWindow(x1,y1,w1,h1 : word; var p : Pointer; DeAlloc : boolean) : boolean;
var x,y: byte;
    c  : word;
begin
     if p=nil then exit;
     c:=0;
     for y:=0 to pred(h1) do begin
         for x := 0 to pred(w1) do begin
{             MemW[vidseg:vidOfs(x1+x,y1+y)]:=PWordArray(P)^[c];}
              VioWrtNCell(PWordArray(P)^[c],1,pred(y),pred(x),0);
             inc(c);
         end;
     end;
     if DeAlloc then Freemem(p,(w1*h1)*2);
end;
{----------------------------------------------------------------------}
Procedure FrameWindow(x1,y1,w1,h1: word; at : byte);
begin
end;
{----------------------------------------------------------------------}
Procedure FrameOWindow(x1,y1,w1,h1: word; at : byte);
begin
end;
{----------------------------------------------------------------------}
Procedure ShadowWindow(x1,y1,w1,h1, kind, pos : byte);
begin
end;
{----------------------------------------------------------------------}
Procedure SetFrameChar(c1,c2,c3,c4,c5,c6,c7,c8 : char);
begin
end;
{----------------------------------------------------------------------}
Procedure ClearWindow(x1,y1,w1,h1: word; at : byte);
begin
     ClearWindowChar(x1,y1,w1,h1,at,' ');
end;
{----------------------------------------------------------------------}
Procedure ClearWindowChar(x1,y1,w1,h1: word; at : byte; ch : char);
var w  : word;
    x,y:byte;
begin
     W:=(byte(ch) shl 8) or at;
     for y:=0 to pred(h1) do begin
         for x := 0 to pred(w1) do begin
              VioWrtNCell(w,1,pred(y),pred(x),0);
{             MemW[vidseg:vidOfs(x1+x,y1+y)]:=w;}
         end;
     end;
end;
{----------------------------------------------------------------------}
Procedure ClearScreen(at : byte);
begin
     ClearWindowChar(1,1,Maxcol,getrows,at,' ');
end;
{----------------------------------------------------------------------}
Function GetScrChar(x,y : word) : char;
var c : char;
    s : word;
begin
     s := sizeof(c);
     VioReadCharStr(c,s, pred(y),pred(x),0);
     GetScrChar:=c;
end;
{----------------------------------------------------------------------}
Procedure PutScrChar(x,y : word; c: char);
begin
     VioWrtNChar(byte(C),1,pred(y),pred(x),0);
end;
{----------------------------------------------------------------------}
Function GetScrAttr(x,y : word) : byte;
var c, s : word;
begin
     s := sizeof(c);
     VioReadCellStr(c,s,pred(y),pred(x),0);
     getScrAttr:=hi(s);
end;
{----------------------------------------------------------------------}
Procedure PutScrAttr(x,y: word; b : byte);
begin
     VioWrtNAttr(b,1,pred(y),pred(x),0);
end;
{----------------------------------------------------------------------}
Procedure MeltWindow(x1,y1,w1,h1 : byte);
var done : boolean;
    x,y  : byte;
    ch   : char;
begin
     repeat
           Done:=True;
           for x:=0 to pred(w1) do begin
               for y:=0 to Pred(h1) do begin
                   ch:=GetScrChar(x1+x,y1+y);
                   if ch<>' ' then begin
                      Done:=FALSE;
                      PutScrChar(x1+x,y1+y,pred(ch));
                   end;
               end;
           end;
     until done=True;
end;
{----------------------------------------------------------------------}
Procedure MeltScreen;
begin
     meltWindow(1,1,maxcol,getrows);
end;
{----------------------------------------------------------------------}
Procedure MeltRotWindow(x1,y1,w1,h1 : byte);
var done : boolean;
    x,y  : byte;
    r    : byte;
    ch   : char;
begin

     For R:=1 to 128 do begin
           for x:=0 to pred(w1) do begin
               for y:=0 to Pred(h1) do begin
                   ch:=GetScrChar(x1+x,y1+y);
                   if ch<>#32 then dec(ch,2);
                   PutScrChar(x1+x,y1+y,ch);
               end;
           end;
     end;
end;
{----------------------------------------------------------------------}
Procedure MeltRotScreen;
begin
     meltRotWindow(1,1,maxcol,getrows);
end;
{----------------------------------------------------------------------}
PROCEDURE SetCursor( nTop, nBottom : INTEGER ); ASSEMBLER;
ASM
     MOV  AH, 1
     MOV  CH, BYTE PTR nTop
     MOV  CL, BYTE PTR nBottom
     INT  10h
END;
{----------------------------------------------------------------------}
Procedure UpdateVioModeInfo;
begin
     CurrVioMode.cb := sizeof(VioModeInfo);
     VioGetMode(CurrVioMode,0);
     GetRows := CurrVioMode.Row;
     MaxCol := CurrVioMode.Col;
end;
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{**********************************************************************}
Begin
     UpdateVioModeInfo;
END.
