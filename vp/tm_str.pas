Unit Tm_Str;

Interface
{uses Use32;}

CONST
     HexList :ARRAY[0..15] OF CHAR ='0123456789ABCDEF';
     cTab = #9;
     cBS  = #8;
     cCR  = #13;
     cLF  = #10;
     cBSlash = '\';

Type
    {Pstring = ^string;}
    AnArray = array[0..$FFF8] of byte;
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
 FUNCTION  LTrim(Const s: STRING; Const c: CHAR ): STRING;      {Strip all Char from Left side of string}
 FUNCTION  RTrim(s: STRING; Const c: CHAR ): STRING;            {Strip all Char from Right side of string}
 FUNCTION  ATrim(s: STRING; Const c: CHAR ): STRING;            {Strip all Char from both sides of string}
 FUNCTION  LPAD (Const s: STRING; n: BYTE; c: CHAR ): STRING;   {pad string on left side with Char to max N length}
 FUNCTION  RPAD (Const s: STRING; n: BYTE; c: CHAR ): STRING;   {pad string on right side with Char to max N length}
 FUNCTION  LoStr(const s:string):string;                        {simple lower case string a-z}
 FUNCTION  UpStr(s:string):string;                        {simple upper case string a-z}
 FUNCTION  LoCase(c: CHAR ): Char;                              {simple lower case of character A-Z}
 FUNCTION  StripChar(s: STRING; c: CHAR ): STRING;              {strip all occurance of Char from String}
 Function  CharStr(c : char; b : byte) : string;                {return string of C chars of B length}
 Procedure ExpandTabs(var S : string; b : byte);                {convert tabs in string to B spaces}
 function  WordsInStr(const s : string) : byte;                 {NUMBER of "words" in string with space/tab between}
 Function  WordARound(S : string; B : byte) : string;           {returns the word from a string around position B}
 Function  IsCharOrNum ( c : char) : boolean;
 Function  ReverseString (S : string) : string;                 {flips string to reverse character order}

 function  Long2Str(L : LongInt) : string;                      {longint, word, integer, byte to string}
 Function  LeadZero(L : Longint; b : byte) : String;            {same as above but with leading zeros to width b}
 function  Real2Str(R : Real; Width : Byte; Places : ShortInt) : string;
 Function  Nth(l : longint) : string;                           {1 = 1st, 2 = 2nd, 3 = 3rd, etc}


 function  Xwild(SearchStr,NameStr:string):boolean;             {test if extended wildcard is in string (see impl.)}
 Function  Roman(Number: Integer): String;                      {Convert number to roman numeral}

 Procedure Arr2Str(var v; sv : byte; c : char; var s : string); {Array of Char, sized SV, padded with C, to String}
 Function  Arr2Long(var v; sv : byte; c : char) : longint;      {Array of Char, sized SV, padded with C, to Longint}
 Procedure Str2Arr(s : string; var v; sv : byte; c : char);     {String to Array of Char, sized SV, padded with C}

 FUNCTION  CmdLine: STRING;                                     {the entire commandline in one string}
 Function  ForceExt(S: string; const E: string): string;
 FUNCTION  BackSlash(Const S:string): string;                   {adds \ to end of path if needed}
 FUNCTION  NoBackSlash(Const S:string): string;                 {remove \ from end of path if needed}
 FUNCTION  ExepathBS: string;                                   {path of current EXE with \}
 Function  Quoted(const s : string) : string;                   {returns outer double quoted part of string if any}
 Function  FNonly(const s : string) : string;                   {returns only Filename.ext from path}
 Function  PathOnly(const s : string) : string;                 {returns path part of path\filename (no \)}
 Function  PathOnlyBS(const s : string) : string;               {returns path part of path\filename (with \)}

 FUNCTION  BHex( V :BYTE ) :STRING;                             {byte to 2 digit Hex string}
 FUNCTION  WHex( V :WORD ) :STRING;                             {word to 4 digit Hex String}
 FUNCTION  LHex( Long :LONGINT ) :STRING;                       {Long to 8 digit Hex String}
 Function  HexB( Const S : string) : byte;                      {2 digit Hex Str to Byte (upper case)}
 Function  HexLong( S : string) : Longint;                         {convert 1 to 8 hex-digit number to whatever}

 Procedure Terr(B : byte; S : String; HLT : boolean);           {report error message/number, optionally HALT}

{**********************************************************************}
{-----------------------I M P L E M E N T A T I O N--------------------}
{**********************************************************************}
Implementation
uses Strings;
{----------------------------------------------------------------------}
{--                         CMD LINE                                 --}
{----------------------------------------------------------------------}
FUNCTION CmdLine : STRING;
var x : byte;
    s : string;
BEGIN
     s:='';
     for x:=1 to paramcount do begin
         s:=s+paramstr(x);
         if x<paramcount then s:=s+' ';
     end;
     cmdLine:=s;
{     CmdLine:=string(PTR(PREFIXSEG,$0080)^);}
END;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function  ForceExt(S: string; Const E: string): string;
var x : byte;
begin
     x:=pos('.',S);
     if x<(byte(s[0])-3) then x:=0;
     if x<>0 then begin
          system.Delete(S,x,255);
          ForceExt:=S
     end;
     ForceExt:=S+'.'+E;
end;
{----------------------------------------------------------------------}
{--                    HEX stuff                                     --}
{----------------------------------------------------------------------}
FUNCTION HiWord( Long :LONGINT ) :WORD; ASSEMBLER;
ASM
  Mov AX,Long.WORD[2]              { Move High word into AX.           }
END;
FUNCTION LoWord( Long :LONGINT ) :WORD; ASSEMBLER;
ASM
  Mov AX,Long.WORD[0]              { Move low word into AX.            }
END;
{----------------------------------------------------------------------}
FUNCTION BHex( V :BYTE ) :STRING;
BEGIN
  BHex := HexList[V Shr 4] + HexList[V Mod 16];
END;
{----------------------------------------------------------------------}
FUNCTION WHex( V :WORD ) :STRING;
BEGIN
  WHex := Bhex(Hi(V)) + BHex(Lo(V));
END;
{----------------------------------------------------------------------}
FUNCTION LHex( Long :LONGINT ) :STRING;
BEGIN
  LHex := WHex(HiWord(Long))+WHex(LoWord(Long));
END;
{----------------------------------------------------------------------}
Function HexC(c : Char) : byte;
begin
     HexC:=0;
     C:=Upcase(c);
     if (c>='0')and(c<='9') then HexC:=Byte(c)-48;
     if (c>='A')and(c<='F') then HexC:=Byte(c)-55;
end;
{----------------------------------------------------------------------}
Function  HexB( Const S : string) : byte;
begin
     HexB:=0;
     if S[0]=#1 then HexB:=HexC(s[1]);
     if S[0]=#2 then HexB:=(HexC(s[1])*16)+HexC(s[2]);
end;
{----------------------------------------------------------------------}
Function  HexLong( S : string) : Longint;
var
   l : Longint;
begin
     s:=ReverseString(s); l:=0;
     if s[0]>#8 then exit;
     if S[0]>#0 then l:=HexC(s[1]);
     if S[0]>#1 then l:=(HexC(s[2])*longint(16))+l;
     if S[0]>#2 then l:=(HexC(s[3])*longint(256))+l;
     if S[0]>#3 then l:=(HexC(s[4])*longint(4096))+l;
     if S[0]>#4 then l:=(HexC(s[5])*longint(65536))+l;
     if S[0]>#5 then l:=(HexC(s[6])*longint(1048576))+l;
     if S[0]>#6 then l:=(HexC(s[7])*longint(16777216))+l;
     if S[0]>#7 then l:=(HexC(s[8])*longint(268435456))+l;
     HexLong:=l;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function ReverseString (S : string) : string;
var c : char;
    x : byte;
begin
        for x:=1 to (byte(s[0]) div 2) do begin
            c:=s[x];
            s[x]:=s[byte(s[0])-pred(x)];
            s[byte(s[0])-pred(x)]:=c;
        end;
        ReverseString:=S;
end;

{----------------------------------------------------------------------}
{--                     Left Trim Char                               --}
{----------------------------------------------------------------------}
FUNCTION LTrim(Const s: STRING; Const c: CHAR ): STRING; Assembler;
ASM
      PUSH   DS
      LDS    SI, s
      XOR    AX, AX
      LODSB
      XCHG   AX, CX
      LES    DI, @Result
      INC    DI
      JCXZ   @@2
      MOV    BL, c
      CLD
@@1:  LODSB
      CMP    AL, BL
      LOOPE  @@1
      DEC    SI
      INC    CX
      REP    MOVSB

@@2:  XCHG   AX, DI
      MOV    DI, WORD PTR @Result
      SUB    AX, DI
      DEC    AX
      STOSB
      POP    DS
END;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
FUNCTION RTrim( s: STRING; const c: CHAR ): STRING;
BEGIN
      WHILE (byte(s[0]) > 0) AND (s[byte(s[0])] = c) DO DEC(s[0]);
      RTrim := s;
END;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
FUNCTION  ATrim(s: STRING; Const c: CHAR ): STRING;            {Strip all Char from both sides of string}
begin
     if s[1]=c then s:=Ltrim(s,c);
     if s[byte(s[0])]=c then Atrim:=Rtrim(s,c) else Atrim:=S;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
function LoStr(const s:string):string; assembler;
  asm
    push ds
    lds  si,s
    les  di,@result
    lodsb            { load and store length of string }
    stosb
    xor  ch,ch
    mov  cl,al
    jcxz @empty      { FIX for null string }
  @LowerLoop:
    lodsb
    cmp  al,'A'
    jb   @cont
    cmp  al,'Z'
    ja   @cont
    add  al,' '
  @cont:
    stosb
    loop @LowerLoop
  @empty:
    pop  ds
end;  { LoStr }
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
function UpStr(s:string):string;
var x : byte;
begin
     for x:=1 to length(s) do begin
         s[x]:=upCase(s[x]);
     end;
     Upstr:=S;
end;  { UpStr }
{----------------------------------------------------------------------}
{--             EXTENDED WILD CARD evaluation                        --}
{----------------------------------------------------------------------}
function Xwild(SearchStr,NameStr:string):boolean; assembler;
{
 The following wildcards are allowed:
 *ABC*        matches everything which contains ABC
 [A-C]*       matches everything that starts with either A,B or C
 [ADEF-JW-Z]  matches A,D,E,F,G,H,I,J,W,V,X,Y or Z
 ABC?         matches ABC, ABC1, ABC2, ABCA, ABCB etc.
 ABC[?]       matches ABC1, ABC2, ABCA, ABCB etc. (but not ABC)
 ABC*         matches everything starting with ABC
}
var
 LastW:word;
asm
 cld
 push ds
 lds si,SearchStr
 les di,NameStr
 xor ah,ah
 lodsb
 mov cx,ax
 mov al,es:[di]
 inc di
 mov bx,ax
 or cx,cx
 jnz @ChkChr
 or bx,bx
 jz @ChrAOk
 jmp @ChrNOk
 xor dh,dh
@ChkChr:
 lodsb
 cmp al,'*'
 jne @ChkQues
 dec cx
 jz @ChrAOk
 mov dh,1
 mov LastW,cx
 jmp @ChkChr
@ChkQues:
 cmp al,'?'
 jnz @NormChr
 inc di
 or bx,bx
 je @ChrOk
 dec bx
 jmp @ChrOk
@NormChr:
 or bx,bx
 je @ChrNOk
{From here to @No4DosChr is used for [0-9]/[?]/[!0-9] 4DOS wildcards...}
 cmp al,'['
 jne @No4DosChr
 cmp word ptr [si],']?'
 je @SkipRange
 mov ah,byte ptr es:[di]
 xor dl,dl
 cmp byte ptr [si],'!'
 jnz @ChkRange
 inc si
 dec cx
 jz @ChrNOk
 inc dx
@ChkRange:
 lodsb
 dec cx
 jz @ChrNOk
 cmp al,']'
 je @NChrNOk
 cmp ah,al
 je @NChrOk
 cmp byte ptr [si],'-'
 jne @ChkRange
 inc si
 dec cx
 jz @ChrNOk
 cmp ah,al
 jae @ChkR2
 inc si              {Throw a-Z < away}
 dec cx
 jz @ChrNOk
 jmp @ChkRange
@ChkR2:
 lodsb
 dec cx
 jz @ChrNOk
 cmp ah,al
 ja @ChkRange        {= jbe @NChrOk; jmp @ChkRange}
@NChrOk:
 or dl,dl
 jnz @ChrNOk
 inc dx
@NChrNOk:
 or dl,dl
 jz @ChrNOk
@NNChrOk:
 cmp al,']'
 je @NNNChrOk
@SkipRange:
 lodsb
 cmp al,']'
 loopne @SkipRange
 jne @ChrNOk
@NNNChrOk:
 dec bx
 inc di
 jmp @ChrOk
@No4DosChr:
 cmp es:[di],al
 jne @ChrNOk
 inc di
 dec bx
@ChrOk:
 xor dh,dh
 dec cx
 jnz @ChkChr        { Can't use loop, distance >128 bytes }
 or bx,bx
 jnz @ChrNOk
@ChrAOk:
 mov al,1
 jmp @EndR
@ChrNOk:
 or dh,dh
 jz @IChrNOk
 jcxz @IChrNOk
 or bx,bx
 jz @IChrNOk
 inc di
 dec bx
 jz @IChrNOk
 mov ax,[LastW]
 sub ax,cx
 add cx,ax
 sub si,ax
 dec si
 jmp @ChkChr
@IChrNOk:
 mov al,0
@EndR:
 pop ds
end;
{----------------------------------------------------------------------}
Function Roman (Number: Integer): String;
Var
  TempStr : String;   { Temporary storage for the result string }
Begin
  TempStr := '';
  If (Number > 0) And (Number < 4000) Then
  Begin
    { One 'M' for every 1000 }
    TempStr := Copy ('MMM', 1, Number Div 1000);
    Number := Number MOD 1000;
    If Number >= 900 Then
    { Number >= 900, so append 'CM' }
    Begin
      TempStr := TempStr + 'CM';
      Number := Number - 900;
    End
    Else
    { Number < 900 }
    Begin
      If Number >= 500 Then
      { Number >= 500, so append 'D' }
      Begin
        TempStr := TempStr + 'D';
        Number := Number - 500;
      End
      Else
        If Number >= 400 Then
        { 400 <= Number < 500, so append 'CD' }
        Begin
          TempStr := TempStr + 'CD';
          Number := Number - 400;
        End;
      { Now Number < 400!!! One 'C' for every 100 }
      TempStr := TempStr + Copy ('CCC', 1, Number Div 100);
      Number := Number Mod 100;
    End;
    If Number >= 90 Then
    { Number >= 90, so append 'XC' }
    Begin
      TempStr := TempStr + 'XC';
      Number := Number - 90;
    End
    Else
    { Number < 90 }
    Begin
      If Number >= 50 Then
      { Number >= 50, so append 'L'}
      Begin
        TempStr := TempStr + 'L';
        Number := Number - 50;
      End
      Else
        If Number >= 40 Then
        { 40 <= Number < 50, so append 'XL' }
        Begin
          TempStr := TempStr + 'XL';
          Number := Number - 40;
        End;
      { Now Number < 40!!! One 'X' for every 10 }
      TempStr := TempStr + Copy ('XXX', 1, Number Div 10);
      Number := Number Mod 10;
    End;
    If Number = 9 Then
    { Number = 9, so append 'IX' }
    Begin
      TempStr := TempStr + 'IX';
    End
    Else
    { Number < 9 }
    Begin
      If Number >= 5 Then
      { Number >= 5, so append 'V' }
      Begin
        TempStr := TempStr + 'V';
        Number := Number - 5;
      End
      Else
        If Number = 4 Then
        { Number = 4, so append 'IV' }
        Begin
          TempStr := TempStr + 'IV';
          Number := Number - 4;
        End;
      { Now Number < 4!!! One 'I' for every 1 }
      TempStr := TempStr + Copy ('III', 1, Number);
    End;
  End;
  Roman := TempStr;
End;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
FUNCTION  BackSlash(Const S:string): string;
begin
     if s[byte(s[0])]<>'\' then BackSlash:=S+'\' else BackSlash:=S;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
FUNCTION  NoBackSlash(Const S:string): string;
begin
     If s[byte(s[0])]='\' then NoBackSlash:=copy(S,1,pred(byte(s[0])))
        else NoBackSlash:=S;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
FUNCTION  exepathBS: string;
Var S : string[127];
begin
     s:=paramstr(0);
     while (s[byte(s[0])]<>'\')and(s[0]>#0) do dec(s[0]);
     exepathbS:=s;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
FUNCTION LPAD(Const s: STRING; n: BYTE; c: CHAR ): STRING; ASSEMBLER;
ASM
      PUSH   DS
      CLD

      LES    DI, @Result
      INC    DI
      LDS    SI, s
      XOR    AX, AX
      LODSB
      PUSH   AX

      XOR    CX, CX
      MOV    CL, n
      SUB    CL, AL

      CMP    CX, 0
      JNB    @@1
      XOR    CX, CX

@@1:  MOV    AL, c
      REP    STOSB

      POP    CX
      REP    MOVSB

      MOV    DI, WORD PTR @Result
      MOV    AL, n
      MOV    BYTE PTR ES:[DI], AL
      POP    DS
END;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
FUNCTION RPAD(Const s: STRING; n: BYTE; c: CHAR ): STRING; ASSEMBLER;
ASM
      PUSH   DS
      CLD
      LDS    SI, s
      XOR    AX, AX
      LODSB
      MOV    CX, AX

      LES    DI, @Result
      INC    DI
      REP    MOVSB

      MOV    CL, n
      SUB    CL, AL

      CMP    CX, 0
      JNB    @@1
      XOR    CX, CX

@@1:  MOV    AL, c
      REP    STOSB

      MOV    DI, WORD PTR @Result
      MOV    AL, n
      MOV    BYTE PTR ES:[DI], AL

      POP    DS
END;

{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function Quoted(const s : string) : string;
var b,e : byte;
begin
     b:=0; e:=succ(byte(s[0])); Quoted:='';
     repeat inc(b) until (s[b]='"')or(b=e);
     if b=e then exit;
     repeat dec(e) until (s[e]='"')or(e=0);
     if (b=e)or(succ(b)=e) then exit;
     Quoted:=copy(s,succ(b),pred(e-b));
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function  FNonly(const s : string) : string;
var b,x : byte;
begin
     B:=0;FnOnly:='';
     for x:=1 to byte(s[0]) do if s[x]='\' then b:=x;
     FnOnly:=copy(s,succ(b),255);
end;
{----------------------------------------------------------------------}
Function  PathOnly(const s : string) : string;
var b,x : byte;
begin
     B:=0; PathOnly:='';
     for x:=1 to byte(s[0]) do if s[x]='\' then b:=x;
     if b>0 then PathOnly:=copy(s,1,pred(b));
end;
{----------------------------------------------------------------------}
Function  PathOnlyBS(const s : string) : string;
var b,x : byte;
begin
     B:=0; PathONlyBS:='';
     for x:=1 to byte(s[0]) do if s[x]='\' then b:=x;
     if b>0 then PathOnlyBS:=copy(s,1,b);
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure Arr2Str(var v; sv : byte; c : char; var s : string);
var b : byte;
Begin
     if sv>$FF then b:=$FF else B:=sv;
     move(v,s[1],b);
     s[0]:=char(b);
     s:=Rtrim(s,c);
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure Str2Arr(s : string; var v; sv: byte; c : char);
var b : byte;
Begin
     fillchar(v,sv,c);
     if sv<byte(s[0]) then b:=sv else b:=byte(s[0]);
     move(s[1],v,b);
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function  Arr2Long(var v; sv : byte; c : char) : longint;
var s : string;
    i : longint;
    l : longint;
begin
     Arr2Str(v,sv,c,s);
     val(s,l,i);
     Arr2Long:=l;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
FUNCTION StripChar( s: STRING; c: CHAR ): STRING; Assembler;
ASM
      PUSH   DS
      CLD
      LDS    SI, s
      XOR    AX, AX
      LODSB
      XCHG   AX, CX
      LES    DI, @Result
      INC    DI
      JCXZ   @@3
      MOV    BL, c

@@1:  LODSB
      CMP    AL, BL
      JE     @@2
      STOSB

@@2:  LOOP   @@1

@@3:  XCHG   AX, DI
      MOV    DI, WORD PTR @Result
      SUB    AX, DI
      DEC    AX
      STOSB
      POP    DS
END;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function charstr(C : char; b : byte) : string;
var s : string;
begin
     s[0]:=char(b);
     fillchar(s[1],b,c);
     charstr:=S;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure ExpandTabs(var S : string; b : byte);
var p : byte;
begin
     repeat
           p:=pos(#9,S);
           if p>0 then begin
              system.delete(S,p,1);
              system.insert(charstr(' ',b),S,p);
           end;
     until P=0;
end;
{----------------------------------------------------------------------}
{--                      Is Char or Number                           --}
{----------------------------------------------------------------------}
Function IsCharOrNum ( c : char) : boolean;
begin
     IsCharOrNum:=False;
     if (((c>='a')and(c<='z'))or((c>='A')and(c<='Z'))or((c>='0')and(c<='9')))
        then IsCharOrNum := TRUE;
end;
{----------------------------------------------------------------------}
{--                 Is Char or Number with International             --}
{----------------------------------------------------------------------}



{----------------------------------------------------------------------}
{--                   Words in String (space, tab deliniated)        --}
{----------------------------------------------------------------------}
Function WordsInStr(const s : string) : byte;
var
   IsWhite    : Boolean;
   x          : byte;
   c          : byte;
begin
     c:=0; IsWhite:=TRUE;
     for x := 1 to byte(s[0]) do begin
         if ((s[x]<>#9)and(s[x]<>#32)) then begin
            if isWhite=TRUE then inc(c);
            IsWhite:=FALSE;
         end else IsWhite:=TRUE;
     end;
     WordsInStr:=C;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function WordARound(S : string; B : byte) : string;
var x : byte;
Begin
     WordAround:='';
     if (s[0]=#0)or(b=0)or(b>byte(s[0]))or(s[b]=' ') then exit;
     x:=b;
     while (x>0)and(s[x]<>' ') do dec(x);
     if x>0 then system.delete(s,1,x);
     dec(b,x); x:=b;
     while (x<=byte(s[0]))and(s[x]<>' ')and(x<>0) do inc(x);
     if (X<>0)and(X<=byte(s[0])) then S[0]:=CHAR(PRED(X));
     wORDAround:=s;
end;
{----------------------------------------------------------------------}
{--                     T-Error                                      --}
{----------------------------------------------------------------------}
Procedure Terr(b : byte; S : string; HLT : boolean);
begin
     Writeln;
     If HLT=TRUE then Write('Terminal ') else write('Warning ');
     Writeln('Error #',b,': '+S);
     If HLT=TRUE then Halt(b);
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function  Nth(l : longint) : string;                           {1 = 1st, 2 = 2nd, 3 = 3rd, etc}
var s : string[30];
begin
     str(l,s);
     if s[0]>#0 then begin
        case s[byte(s[0])] of
             '1' : s:=s+'st';
             '2' : s:=s+'nd';
             '3' : s:=s+'rd';
             else s:=s+'th';
        end;
     end;
     nth:=s;
end;
{----------------------------------------------------------------------}
function Long2Str(L : LongInt) : string;
var S : string[30];
begin
    Str(L, S);
    Long2Str := S;
end;
{----------------------------------------------------------------------}
Function  LeadZero(L : Longint; b : byte) : String;            {same as above but with leading zeros to width b}
var S : string[30];
begin
     Str(L,S);
     While byte(S[0])<b do system.insert('0',s,1);
     LeadZero := s;
end;
{----------------------------------------------------------------------}
function Real2Str(R : Real; Width : Byte; Places : ShortInt) : string;
var S : string[50];
begin
     Str(R:Width:Places, S);
     Real2Str := S;
end;
{----------------------------------------------------------------------}
FUNCTION  LoCase(c: CHAR ): Char;                              {simple lower case of character A-Z}
begin
     if (c>='A')or(c<='Z') then Locase:=char(byte(c)+32)
     else LoCase:=c;
end;
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{**********************************************************************}
END.
