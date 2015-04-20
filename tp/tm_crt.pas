Unit TM_CRT;

Interface

{$I TM_SCANC.INC}

{----------------------------------------------------------------------}
 PROCEDURE DosRedirect;                        {enable DOS redirections}
 PROCEDURE MSDelay(MS: Word);                  {Milliseconds to pause}
 FUNCTION  GetAKey : word;                     {Gets a scan code and waits}
 FUNCTION  EnhancedKeyBoard :BOOLEAN;          {is enhanced keybaord?}
 Procedure getkey(Var key:word);
 Procedure egetkey(Var key:word);
 Function IsKeyPressed : boolean;

 Procedure AtWrite(s : string; r : byte);

{**********************************************************************}
{----------------------------------------------------------------------}
{**********************************************************************}
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Implementation
uses CRT;

Procedure AtWrite(s : string; r : byte);
var
   b      : byte;
   c1,c2  : char;
begin
     Repeat
           b:=pos('@X',s);
           if (b>0)and(b<(byte(s[0])-2)) then begin
              c1:=s[b+2]; c2:=s[b+3];
              if (((c1>='0')and(c1<='9'))or((c1>='A')and(c1<='F')))and
                 (((c2>='0')and(c2<='9'))or((c2>='A')and(c2<='F')))
                   then begin
                        if b>1 then System.Write(copy(s,1,pred(b)));
                        System.Delete(s,1,b+3);
                        if (c1>='0')and(c1<='9')then
                           TextBackground(byte(c1)-48) else
                           Textbackground(byte(c1)-55);
                        if (c2>='0')and(c2<='9')then
                           TextColor(byte(c2)-48) else
                           TextColor(byte(c2)-55);
              end else begin
                 System.Write(copy(s,1,b));
                 System.Delete(s,1,b);
              end;
           end else begin
               System.Write(s);
               b:=0;
           end;
     Until (b=0);
     For b:=1 to r do Writeln;
end;

{----------------------------------------------------------------------}
PROCEDURE DosRedirect;
BEGIN
     ASSIGN(Input,'');RESET(Input);
     ASSIGN(Output,'');REWRITE(Output);
END;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
FUNCTION EnhancedKeyBoard :BOOLEAN;
BEGIN
  EnhancedKeyBoard := (Mem[$40:$96] AND $10) = $10;
END;{EnhancedKeyBoard}
{----------------------------------------------------------------------}
FUNCTION GetAKey : word;
                {  Routine to Read Key Presses. Including F11 and F12         }
BEGIN
  ASM
    Call EnhancedKeyBoard       { Test for an Enhanced keyboard.        }
    Cmp AL,1                    { If AL=1 THEN Enhanced Keyboard= TRUE. }
    Je @Enhanced                { If it was TRUE then Get Enhanced key. }
    Mov AH,0                    { If not TRUE get normal key.           }
    Jmp @ReadKeyb
@Enhanced:
    Mov AH,$10                  { Function to get key from enhanced board.}
@ReadKeyb:
    Int $16                     { Call Int keyboard INT.                }
    Mov @result,AX              { Load both Ascii code and scan code    }
  END;
end;
{----------------------------------------------------------------------}
Procedure egetkey(Var key:word);
Var tmp:word;
Begin
 asm; mov ah,10h; int 16h; mov tmp,ax; end;
 key:=tmp;
end;

Procedure getkey(Var key:word);
Var tmp:word;
Begin
 asm; xor ah,ah; int 16h; mov tmp,ax; end;
 key:=tmp;
end;
{----------------------------------------------------------------------}
PROCEDURE MSDelay(MS: Word); Assembler;
ASM
   MOV Ax, 1000;
   MUL MS;
   MOV Cx, Dx;
   MOV Dx, Ax;
   MOV Ah, $86;
   INT $15;
END;
{----------------------------------------------------------------------}
Function IsKeyPressed : boolean; Assembler;
var
   tmpb : boolean;
ASM
   MOV AH, $01;
   INT $16;
   JNZ @NK;
   mov tmpb,TRUE;
   JMP @END;
   @NK:
   mov tmpb,FALSE;
   @END:
END;
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
{**********************************************************************}
END.