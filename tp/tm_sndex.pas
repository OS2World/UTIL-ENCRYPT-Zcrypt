Unit TM_SNDEX;

Interface

{----------------------------------------------------------------------}
{----------------------------------------------------------------------}

Function Sounds_Like (Name1,Name2 : string) : Boolean;

{----------------------------------------------------------------------}
{**********************************************************************}
{----------------------------------------------------------------------}
Implementation
Uses TM_Str;

{ remove all occurances of double letters like wie oo,tt,ee, etc. }
Procedure eliminate_doubles (Var str : string);
Var
   I,J : Integer;
Begin
   For I := 1 to Length (str) do
      Begin
      If str [I] = str [I + 1] then
         Begin
         For J := I + 1 to Length (str)-1 do
            str [J] := str [J + 1];
         End
      End
End  {  eliminate_doubles  };

{ Code 'Code' for soundex comparison }
Procedure Sound_Ex (var Code : string);
Var
   I : Integer;
   Sndex : string;
Begin
   Sndex := '';
   Sndex := Sndex + Code [1];
   For I := 2 to Length (Code) do
      Begin
      Case Code [I] of
         'B','F','P','V'                 : Sndex := Sndex +  '1';
         'C','G','J','K','Q','S','S','Z' : Sndex := Sndex +  '2';
         'D','T'                         : Sndex := Sndex +  '3';
         'L'                             : Sndex := Sndex +  '4';
         'M','N'                         : Sndex := Sndex +  '5';
         'R'                             : Sndex := Sndex +  '6';
      End { case };
      End { For };
   If Length (Sndex) > 4 then Sndex := Copy (Sndex,1,4);
   If Length (Sndex) < 4 then
      For I := Length (Sndex) to 3 do Sndex := Sndex + '0';
   Code := Sndex;
End  {  Sound_Ex  };

{**************************************************
 * returns TRUE, if Name1 in Soundexcode          *
 * ressembles to Name2, returns falsch, if not    *
 **************************************************}

Function Sounds_Like (Name1,Name2 : string) : Boolean;
Var
   Tnam1,Tnam2 : string;
Begin
   Tnam1 := Upstr(Name1);
   Tnam2 := UpStr(Name2);
   eliminate_doubles (Tnam1);
   eliminate_doubles (Tnam2);
   Sound_Ex (Tnam1);
   Sound_Ex (Tnam2);
   Writeln;
   Writeln ('> ',Tnam1,' <> ',tnam2,' <');
   If Tnam1 = Tnam2 then
      Sounds_Like := TRUE
   Else
      Sounds_Like := FALSE;
End  {  Sounds_Like  };

End.


