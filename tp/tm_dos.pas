Unit Tm_DOS;

Interface
Uses DOS;

Const

  fmReadOnly = 0;          {FileMode constants}
  fmWriteOnly = 1;
  fmReadWrite = 2;
  fmDenyAll = 16;
  fmDenyWrite = 32;
  fmDenyRead = 48;
  fmDenyNone = 64;
  fmNoInherit = 128;


 Function DosDelete (FN : PathStr) : Word;
 Function DirExist(DN : Pathstr) : Boolean;
 Function NameExist(DN : Pathstr) : Boolean;
 Function DosAttr(FN : PathStr; At : byte) : Boolean;
 Function QueryPath(Const PN : PathStr) : PathStr;

 FUNCTION DrivesAvail : STRING;
 FUNCTION DrivesAvailStartingAt( c : char) : STRING;
 function DriveValid(Drive: Char): Boolean;

 Function CmdLineStr(C : Char) : string;
 Function CmdLineVal(C : Char; dft : longint) : Longint;
 Function CmdLineTog(C : Char) : Boolean;
 Function CmdLineNoTogStr(B: byte) : string;
 Function CmdLineNoTogsStr(const s : string; B: byte) : string;

 Function AppendTextFile(const fnOrig, fnAppend : string) : byte;

 function SearchBuffer(var Buffer; BufLength : Word;
                       var Match; MatLength : Word) : Word; { $FFFF if not found}
 Function FindInOpenFile(var f: file; var Target; TargetSize : word) : longint;


Implementation
uses TM_str, search;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function DirExist(DN : Pathstr) : Boolean;
var Sr : searchrec;
begin
     findfirst(NobackSlash(DN),Directory,sr);
     if (DosError=0)and((sr.attr and directory)=Directory)
        then DirExist:=TRUE else Direxist:=False;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function NameExist(DN : Pathstr) : Boolean;
var Sr : searchrec;
begin
     findfirst(NobackSlash(DN),AnyFile,sr);
     if (DosError=0) then NameExist:=TRUE else Nameexist:=False;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function DosDelete (FN : PathStr) : Word; {returns error if any}
Var Regs : Registers;
begin
  FN:=FN+#0;                  {make asciiz}
  Regs.DS := Seg(FN[1]);      {segment to String}
  Regs.DX := Ofs(FN[1]);
  Regs.AH := $41;
  Regs.AL := 0;               {Initialize}
  Intr ($21, Regs);
  if Regs.AL <> 0 then DosDelete := Regs.AX else DosDelete := 0;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function DosAttr (FN : PathStr; At : byte) : Boolean; {returns error if any}
Var Regs : Registers;
begin
  FN:=FN+#0;                  {make asciiz}
  Regs.DS := Seg(FN[1]);      {segment to String}
  Regs.DX := Ofs(FN[1]);
  Regs.AH := $43;
  Regs.AL := $01;               {Initialize}
  Regs.CL := At;
  Intr ($21, Regs);
  if (Regs.flags and Fcarry) <> 0 then DosAttr:=False else DosAttr:=True;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
procedure Deldirs;
var
  sr  :  searchrec;
begin
  findfirst('*.*',AnyFile xor VolumeId,sr);
  repeat
    if boolean(sr.Attr and Directory) and (sr.name[1]<>'.') then
    begin
      chdir(sr.name);
      Deldirs;
      chdir('..');
      rmdir(sr.name)
    end
    else if (sr.name[1]<>'.') then
    begin
      DosAttr(sr.name,Archive);
      DosDelete(sr.name)
    end;
    findnext(sr);
  until DOSError<>0;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function QueryPath(Const PN : Pathstr) : PathStr;
var b : byte;
begin
     b:=2;
     While b<=byte(pn[0]) do begin
           While (b<=byte(pn[0]))and(pn[b]<>'\') do inc(b);
           if pn[pred(b)]=':' then inc(b)
           else begin
                if DirExist(copy(pn,1,pred(b))) then QueryPath:=(copy(pn,1,pred(b)));
           end;
     end;
     if DirExist(copy(pn,1,b)) then QueryPath:=(copy(pn,1,b));
end;
{----------------------------------------------------------------------}
{--                   Command Line String                            --}
{----------------------------------------------------------------------}
Function CmdLineStr( C: char) : string;
var stmp : string;
    x    : byte;
begin
     CmdLineStr:=''; C := UPcase(c);
     if Paramcount=0 then exit;
     for x := 1 to paramcount do begin
         stmp:=Paramstr(x);
         if (stmp[1]='-')or(stmp[1]='/') then begin
            if upcase(stmp[2])=C then begin
               if stmp[0]>#2 then begin
                  if stmp[3]=':' then CmdLineStr:= copy(stmp,4,255)
                     else CmdLineStr:= copy(stmp,3,255);
               end;
            end;
         end;
     end;
end;
{----------------------------------------------------------------------}
{--                    Commandline Numeric Value                     --}
{----------------------------------------------------------------------}
Function CmdLineVal(C : Char; dft : longint) : Longint;
var s : string[30];
    L : Longint;
    i : integer;
begin
     CmdLineVal:=dft;
     s:=CmdLineStr(C);
     if s<>'' then begin
        val(s,L,i);
        if i=0 then CmdLineVal:=L;
     end;
end;
{----------------------------------------------------------------------}
{--                   Command Line Toggle                            --}
{----------------------------------------------------------------------}
Function CmdLineTog( C: char) : Boolean;
var stmp : string[2];
    x    : byte;
begin
     CmdLineTog:=FALSE; C := UPcase(c);
     if Paramcount=0 then exit;
     for x := 1 to paramcount do begin
         stmp:=Paramstr(x);
         if (stmp[1]='-')or(stmp[1]='/') then begin
            if upcase(stmp[2])=C then CMDLineTog:=TRUE;
         end;
     end;
end;
{----------------------------------------------------------------------}
{--                   Command Line Param not a Toggle                --}
{----------------------------------------------------------------------}
Function CmdLineNoTogStr(B: byte) : string;
var stmp : string;
    x,c  : byte;
begin
     c:=0;
     CmdLineNoTogStr:='';
     if (Paramcount=0)or(b>paramcount) then exit;
     for x := 1 to paramcount do begin
         stmp:=Paramstr(x);
         if (stmp[1]<>'-')and(stmp[1]<>'/') then begin
            inc(c);
         end;
         if (c=b) then break;
     end;
     if (c=b) then CmdLineNoTogStr:=stmp;
end;
{----------------------------------------------------------------------}
{--           Command Line Param not switched by s                   --}
{----------------------------------------------------------------------}
Function CmdLineNoTogsStr(Const S : string; B: byte) : string;
var stmp : string;
    x,c  : byte;
begin
     c:=0;
     CmdLineNoTogsStr:='';
     if (Paramcount=0)or(b>paramcount) then exit;
     for x := 1 to paramcount do begin
         stmp:=Paramstr(x);
         if ((stmp[1]<>'-')and(stmp[1]<>'/'))
            or(pos(upcase(stmp[2]),s)=0) then inc(c);
         if (c=b) then break;
     end;
     if (c=b) then CmdLineNoTogsStr:=stmp;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
FUNCTION DrivesAvail : STRING; ASSEMBLER;
VAR
  DriveInfo:  ARRAY[1..2] OF CHAR;
  Buffer: ARRAY[1..40] OF CHAR;
  DriveString: ARRAY[1..25] OF CHAR;
ASM
 PUSH  SI { Save Important Registers }
 PUSH  DI
 PUSH  ES
 PUSH  DS

 MOV SI, SS { The Stack Segment (SS) points to the }
 MOV DS, SI { VAR's above. Point DS to it... }
 PUSH  DS
 POP ES { ...and ES as well. }

 LEA SI, DriveInfo { DS:SI - Where we test each drive letter }
 LEA DI, Buffer { ES:DI - FCB Buffer }
 LEA BX, DriveString{ DS:BX - Our resultant string }

 MOV BYTE PTR [SI], 'A' { The character before 'A' }
{ MOV SI, FD}
 XOR CX, CX { Zero out CX }

@Scan:
 INC BYTE PTR [SI] { Next Drive Letter }
 MOV BYTE PTR [SI+1], ':'
 MOV AX, $2906 { DOS Function 29h - Parse Filename }
 INT 21h {  DS:SI - String to be parsed }
  {  ES:DI - FCB }
 LEA SI, DriveInfo { DS:SI }
 CMP AL, $FF{ AL = FFh if function fails (invalid }
 JE @NotValid { drive letter) }

 INC CX { Add one more to our string length... }
 PUSH  CX { ...and save it. }
 MOV CL, BYTE PTR DS:[SI]  { Grab the valid drive letter... }
 MOV [BX], CL  { ...and stuff it into our result }
 INC BX { Next position in result string }
 POP CX { Get our length counter back }

@NotValid:
 CMP BYTE PTR [SI], 'Z' { Did we go through all letters? }
 JNE @Scan { Nope, so next letter }

 LEA SI, DriveString{ Store DriveString to #Result }
 LES DI, @Result
 INC DI
 REP MOVSB

 XCHG  AX, DI { This is the only way to store the }
 MOV DI, WORD PTR @Result  {  length that I can get to work. }
 SUB AX, DI
 DEC AX
 STOSB

 POP DS { Restore Important Registers }
 POP ES
 POP DI
 POP SI
END;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
FUNCTION DrivesAvailStartingAt( c : char) : STRING;
var s : string;
    t : char;
begin
     c := upcase(c);
     s := drivesavail;
     while (s[1]<c)and(s[0]>#0) do system.delete(s,1,1);
     DrivesAvailStartingAt := s;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
function DriveValid(Drive: Char): Boolean; assembler;
asm
mov  ah, 19h { Select DOS function 19h }
int  21h { Call DOS for current disk drive }
mov  bl, al { Save drive code in bl }
mov  al, Drive  { Assign requested drive to al }
sub  al, 'A' { Adjust so A:=0, B:=1, etc. }
mov  dl, al { Save adjusted result in dl }
mov  ah, 0eh { Select DOS function 0eh }
int  21h { Call DOS to set default drive }
mov  ah, 19h { Select DOS function 19h }
int  21h { Get current drive again }
mov  cx, 0  { Preset result to False }
cmp  al, dl { Check if drives match }
jne  @@1 { Jump if not--drive not valid }
mov  cx, 1  { Preset result to True }
@@1:
mov  dl, bl { Restore original default drive }
mov  ah, 0eh { Select DOS function 0eh }
int  21h { Call DOS to set default drive }
xchg ax, cx { Return function result in ax }
end;


{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function AppendTextFile(const fnOrig, fnAppend : string) : byte;
begin
end;
{----------------------------------------------------------------------}
{--                    SearchBuffer                                  --}
{----------------------------------------------------------------------}
function SearchBuffer(var Buffer; BufLength : Word;
                  var Match; MatLength : Word) : Word;
   {-Search through Buffer for Match. BufLength is length of range to search.
     MatLength is length of string to match. Returns number of bytes searched
     to find Match, $FFFF if not found.}
  begin
    inline(
      $1E/                   {PUSH DS                 ;Save DS}
      $FC/                   {CLD                     ;Go forward}
      $C4/$7E/<Buffer/       {LES  DI,[BP+<Buffer]    ;ES:DI => Buffer}
      $89/$FB/               {MOV  BX,DI              ;BX = Ofs(Buffer)}
      $8B/$4E/<BufLength/    {MOV  CX,[BP+<BufLength] ;CX = Length of range to scan}
      $8B/$56/<MatLength/    {MOV  DX,[BP+<MatLength] ;DX = Length of match string}
      $85/$D2/               {TEST DX,DX              ;Length(Match) = 0?}
      $74/$24/               {JZ   Error              ;If so, we're done}
      $C5/$76/<Match/        {LDS  SI,[BP+<Match]     ;DS:SI => Match buffer}
      $AC/                   {LODSB                   ;AL = Match[1]; DS:SI => Match[2]}
      $4A/                   {DEC  DX                 ;DX = MatLength-1}
      $29/$D1/               {SUB  CX,DX              ;CX = BufLength-(MatLength-1)}
      $76/$1B/               {JBE  Error              ;Error if BufLength is less}
                             {;Search for first character in Match}
                             {Next:}
      $F2/$AE/               {REPNE SCASB             ;Search forward for Match[1]}
      $75/$17/               {JNE  Error              ;Done if not found}
      $85/$D2/               {TEST DX,DX              ;If Length = 1 (DX = 0) ...}
      $74/$0C/               {JZ   Found              ; the "string" was found}
                             {;Search for remainder of Match}
      $51/                   {PUSH CX                 ;Save CX}
      $57/                   {PUSH DI                 ;Save DI}
      $56/                   {PUSH SI                 ;Save SI}
      $89/$D1/               {MOV  CX,DX              ;CX = Length(Match) - 1}
      $F3/$A6/               {REPE CMPSB              ;Does rest of string match?}
      $5E/                   {POP  SI                 ;Restore SI}
      $5F/                   {POP  DI                 ;Restore DI}
      $59/                   {POP  CX                 ;Restore CX}
      $75/$EC/               {JNE  Next               ;Try again if no match}
                             {;Calculate number of bytes searched and return}
                             {Found:}
      $4F/                   {DEC  DI                 ;DX = Offset where found}
      $89/$F8/               {MOV  AX,DI              ;AX = Offset where found}
      $29/$D8/               {SUB  AX,BX              ;Subtract starting offset}
      $EB/$03/               {JMP  SHORT SDone        ;Done}
                             {;Match was not found}
                             {Error:}
      $31/$C0/               {XOR  AX,AX              ;Return $FFFF}
      $48/                   {DEC  AX}
                             {SDone:}
      $1F/                    {POP  DS                 ;Restore DS}
      $89/$46/<SearchBuffer); {MOV [BP+<Search],AX     ;Set func result}
  end;

Function FindInOpenFile(var f: file; var Target; TargetSize : word) : longint;
type
    arr = array[1..$FFF0] of byte;
    parr= ^arr;
var
   buff     : pointer;
   buffsize : word;
   buffp    : word;
   r        : word;
   tmp      : word;
   fpos     : longint;
   found    : boolean;
begin
     buffsize:=$FFF0; buffP:=1; fpos:=0; Found := false;
     FindInOpenFile:=-1; {not found}
     if MaxAvail<BuffSize then buffsize:=MaxAvail;
     if (BuffSize>TargetSize) then begin
        reset(f,1);
        getMem(buff, buffsize);
        while (not Eof(f)) and (not found) do begin
              fpos:=FilePos(f);
              BlockRead(f, Buff^, Buffsize, r);
              tmp:=SearchBuffer(buff^,r,target,targetsize);
              if tmp=$FFFF then begin
                 seek(f, filepos(f)-pred(TargetSize));
              end else begin Found:=TRUE; FindInOpenFile:=Fpos+tmp; end;
        end;
        freemem(buff, buffsize);
     end else FindINOpenFile:=-2 {not enough mem}
end;

{**********************************************************************}
{**********************************************************************}
end.