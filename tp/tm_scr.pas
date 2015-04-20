Unit TM_SCR;

Interface

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
    TWordArray = Array[0..$FFF8 div 2] of word;
    PWordArray = ^TWordArray;
    TbyteArray = Array[0..$FFF8] of byte;
    PByteArray = ^TByteArray;

Var
   VMode  : BYTE ABSOLUTE $0040 : $0049; { Video mode: Mono=7, Color=0-3 }
   MaxCol : WORD ABSOLUTE $0040 : $004A; { Number of CRT columns (1-based) }

{----------------------------------------------------------------------}
 procedure qwrite(x, y: byte; s: string; at: byte); {Quick Write /w attrib}
 Procedure GotoXY(X,Y : Byte);                      {fast goto x,y}
 Function  WhereX : Byte;                           {fast wherey}
 Function  WhereY : Byte;                           {fast wherex}
 Function  VidOfs(x,y : word) : word;               {calculate video offset}
 Function  getRows : Byte;                          {Max Rows on screen}
 procedure BlinkOn(IsOn:boolean);                   {allow blink colours?}
 Procedure ScreenOff;                               {turn video outout off}
 Procedure ScreenOn;                                {turn it back on again}

 Function  GetScrChar(x,y : byte) : char;           {get Char from screen}
 Procedure PutScrChar(x,y : byte; c: char);         {put char to screen}
 Function  GetScrAttr(x,y : byte) : byte;           {get attribute from vid}
 Procedure PutScrAttr(x,y,b : byte);                {put attribute to screen}

 Procedure UScroll(x1,y1,x2,y2,Lines,at : Byte);    {scroll up screen area}
 Procedure DScroll(x1,y1,x2,y2,Lines,at : Byte);    {scroll down scr area}

 Function  SaveWindow(x1,y1,w1,h1 : byte; var P1: Pointer; SAlloc : Boolean) : Boolean;
 Function  RestoreWindow(x1,y1,w1,h1 : byte; var p : Pointer; DeAlloc : boolean) : boolean;
 Procedure FrameWindow(x1,y1,w1,h1, at : byte);
 Procedure FrameOWindow(x1,y1,w1,h1, at : byte);
 Procedure ShadowWindow(x1,y1,w1,h1, kind, pos : byte);
 Procedure SetFrameChar(c1,c2,c3,c4,c5,c6,c7,c8 : char);
 Procedure ClearWindow(x1,y1,w1,h1,at : byte);
 Procedure ClearWindowChar(x1,y1,w1,h1,at : byte; ch : char);
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
Procedure GotoXY(X,Y : Byte); Assembler; Asm
  MOV DH, Y    { DH = Row (Y) }
  MOV DL, X    { DL = Column (X) }
  DEC DH       { Adjust For Zero-based Bios routines }
  DEC DL       { Turbo Crt.GotoXY is 1-based }
  MOV BH,0     { Display page 0 } {XOR BH,BH}
  MOV AH,2     { Call For SET CURSOR POSITION }
  INT 10h
end;
{----------------------------------------------------------------------}
Function  WhereX : Byte;  Assembler;
Asm
  MOV     AH,3      {Ask For current cursor position}
  MOV     BH,0      { On page 0 }
  INT     10h       { Return inFormation in DX }
  INC     DL        { Bios Assumes Zero-based. Crt.WhereX Uses 1 based }
  MOV     AL, DL    { Return X position in AL For use in Byte Result }
end;
{----------------------------------------------------------------------}
Function WhereY : Byte; Assembler;
Asm
  MOV     AH,3     {Ask For current cursor position}
  MOV     BH,0     { On page 0 }
  INT     10h      { Return inFormation in DX }
  INC     DH       { Bios Assumes Zero-based. Crt.WhereY Uses 1 based }
  MOV     AL, DH   { Return Y position in AL For use in Byte Result }
end;
{----------------------------------------------------------------------}
procedure BlinkOn(IsOn:boolean); assembler;
asm
  mov ax,1003h
  mov bl,IsOn
  int 10h
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function VidOfs(x,y: word) : word;
begin
     VidOfs:=(((pred(y) * MaxCol) + pred(x)) * 2);
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure ScreenOn;
Begin
  Port[$3C4] := 1;
  Port[$3C5] := $00;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure ScreenOff;
Begin
  Port[$3C4] := 1;
  Port[$3C5] := Port[$3C5] or $20;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function getRows : Byte; Assembler;
Asm
  mov ax, $1130
  xor dx, dx
  int $10
  or  dx, dx
  jnz @S   {cga/mda don't have this fn}
  mov dx, 24
 @S:
  inc dx
  mov al, dl
end;
{----------------------------------------------------------------------}
procedure qwrite(x, y: byte; s: string; at: byte);
begin
  asm
    mov dh, y         { move X and Y into DL and DH }
    mov dl, x
    xor al, al
    mov ah, at        { load background into AH }
    push ax           { PUSH color combo onto the stack }
    mov ax, VIDSEG    { it's color: use segment B800h }
    push ax           { PUSH video segment onto stack }
    mov bx, 0040h     { look at 0040h:0049h to get video mode }
    mov es, bx
    mov bx, 004ah     { check 0040h:0049h to get number of screen columns }
    xor ch, ch
    mov cl, es:[bx]
    xor ah, ah        { move Y into AL; decrement to convert Pascal coords }
    mov al, dh
    dec al
    xor bh, bh        { shift X over into BL; decrement again }
    mov bl, dl
    dec bl
    cmp cl, $50       { see if we're in 80-column mode }
    je @eighty_column
    mul cx            { multiply Y by the number of columns }
    jmp @multiplied
    @eighty_column:   { 80-column mode: it may be faster to perform the }
    mov cl, 4         {   multiplication via shifts and adds: remember  }
    shl ax, cl        {   that 80d = 1010000b , so one can SHL 4, copy  }
    mov dx, ax        {   the result to DX, SHL 2, and add DX in.       }
    mov cl, 2
    shl ax, cl
    add ax, dx
    @multiplied:
    add ax, bx        { add X in }
    shl ax, 1         { multiply by 2 to get offset into video segment }
    mov di, ax        { video pointer is in DI }
    lea si, s         { string pointer is in SI }
    SEGSS lodsb
    cmp al, 00h       { if zero-length string, jump to end }
    je @done
    mov cl, al
    xor ch, ch        { string length is in CX }
    pop es            { get video segment back from stack; put in ES }
    pop ax            { get color back from stack; put in AX (AH = color) }
    @write_loop:
    SEGSS lodsb       { get character to write }
    mov es:[di], ax   { write AX to video memory }
    inc di            { increment video pointer }
    inc di
    loop @write_loop  { if CX > 0, go back to top of loop }
    @done:            { end }
  end;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure UScroll(x1,y1,x2,y2,Lines,at : Byte);
begin
  Asm
    mov ah, 06h
    mov al, Lines
    mov bh, At
    mov ch, y1
    dec ch
    mov cl, x1
    dec cl
    mov dh, y2
    dec dh
    mov dl, x2
    dec dl
    int 10h
  end
end;
{----------------------------------------------------------------------}
Procedure DScroll(x1,y1,x2,y2,Lines,at : Byte);
begin
  Asm
    mov ah, 07h
    mov al, Lines
    mov bh, At
    mov ch, y1
    dec ch
    mov cl, x1
    dec cl
    mov dh, y2
    dec dh
    mov dl, x2
    dec dl
    int 10h
  end
end; { DScroll }
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function  SaveWindow(x1,y1,w1,h1 : byte; var P1: Pointer; SAlloc : Boolean) : Boolean;
var x,y: byte;
    c  : word;
begin
     SaveWindow:=False;
     if SAlloc then begin
        c:=(w1*h1)*2;
        if maxavail<c then exit;
        Getmem(p1,c);
     end;
     c:=0;
     for y:=0 to pred(h1) do begin
         for x := 0 to pred(w1) do begin
             PWordArray(p1)^[c]:=MemW[vidseg:vidOfs(x1+x,y1+y)];
             inc(c);
         end;
     end;
     SaveWindow:=TRUE;
end;
{----------------------------------------------------------------------}
Function  RestoreWindow(x1,y1,w1,h1 : byte; var p : Pointer; DeAlloc : boolean) : boolean;
var x,y: byte;
    c  : word;
begin
     if p=nil then exit;
     c:=0;
     for y:=0 to pred(h1) do begin
         for x := 0 to pred(w1) do begin
             MemW[vidseg:vidOfs(x1+x,y1+y)]:=PWordArray(P)^[c];
             inc(c);
         end;
     end;
     if DeAlloc then Freemem(p,(w1*h1)*2);
end;
{----------------------------------------------------------------------}
Procedure FrameWindow(x1,y1,w1,h1, at : byte);
begin
end;
{----------------------------------------------------------------------}
Procedure FrameOWindow(x1,y1,w1,h1, at : byte);
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
Procedure ClearWindow(x1,y1,w1,h1,at : byte);
begin
     ClearWindowChar(x1,y1,w1,h1,at,' ');
end;
{----------------------------------------------------------------------}
Procedure ClearWindowChar(x1,y1,w1,h1,at : byte; ch : char);
var w  : word;
    x,y:byte;
begin
     W:=(byte(ch) shl 8) or at;
     for y:=0 to pred(h1) do begin
         for x := 0 to pred(w1) do begin
             MemW[vidseg:vidOfs(x1+x,y1+y)]:=w;
         end;
     end;
end;
{----------------------------------------------------------------------}
Procedure ClearScreen(at : byte);
begin
     ClearWindowChar(1,1,maxcol,getrows,at,' ');
end;
{----------------------------------------------------------------------}
Function GetScrChar(x,y : byte) : char;
begin
     getScrChar:=char(mem[vidseg:vidofs(x,y)]);
end;
{----------------------------------------------------------------------}
Procedure PutScrChar(x,y : byte; c: char);
begin
     mem[vidseg:vidofs(x,y)]:=byte(c);
end;
{----------------------------------------------------------------------}
Function GetScrAttr(x,y : byte) : byte;
begin
     getScrAttr:=mem[vidseg:vidofs(x,y)+1];
end;
{----------------------------------------------------------------------}
Procedure PutScrAttr(x,y,b : byte);
begin
     mem[vidseg:vidofs(x,y)+1]:=b;
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
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{**********************************************************************}
Begin
     {If VMode in [0,2,7] THEN VidSeg := $B000;}
     If VMode=7 THEN VidSeg := $B000;
END.
