{$R-}
Program z_crypt;

uses dos, crc32, tm_str, Tm_dos, tm_crt, tm_log ,use32, crt, tm_scr;

const
     id1         = 'Z';
     id2         = 'C';
     Progver     : string[3] = '0.3';
     EncVer      : byte = 1;
     MaxKeyLen   = 255;
     MaxBlockLen = $B000;
     ResBytes    = 2;
     mEncrypt    = 0;
     mDecrypt    = 1;

     crc         : longint = $ffff;


     ModeIdent   : boolean = False;
     ModeTest    : boolean = False;
     keyPtr      : byte = 1;
     blkPtr      : word = 1;
     KeyLen      : byte = MaxKeyLen;
     BlockLen    : word = 0;
     PassLen     : byte = 0;
     PassChk     : byte = 0;
     RndSeed     : byte = 0;
     fnPrefix    : string[8] = '';
     fnOut       : string[8] = '';
     fnLog       : string[50] = '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@';
     fnSpec      : string[80] = '';
     passStr     : string[32] = '';
     EncMode     : byte = mEncrypt;
     vowels      : array[0..7] of byte = (ord('A'),ord('E'),ord('A'),ord('E'),
                                          ord('I'),ord('O'),ord('U'),ord('Y'));
type
    Ar = array[1..maxBlockLen] of byte;
    tHed = Record
        fn : string[12];
        fa : byte;
        fd : longint;
        fs : longint;
        fc : longint; {unecryped 32 bit file crc}
        b1 : byte; {first byte}
        rs : byte; {rndseed}
        pv : array[1..5] of char;
        ev : byte; {encryption Version}
    end;
    tkey = array[1..MaxKeyLen] of byte;

    TSeqBar = Object
            Procedure init(nx,ny : byte; min, max, len : longint);
            Procedure Update(v : longint);
            Procedure SetFill(c : char);
            private
            BarMax : longint;
            BarMin : Longint;
            BarLen : longint;
            fillchar : char;
            Backchar : char;
            FillClr: byte;
            BackClr: byte;
            AbsLen : longint;
            Drawn  : Longint;
            barx   : byte;
            bary   : byte;
    end;

var
   blk : array[1..MaxBlockLen] of byte;
   Hed : thed;
   key : tkey;
   res : array[1..ResBytes] of byte;
   sr  : SearchRec;
   stmp : string[253];
   lg  : TLogObj;
   lmode: Ltype;
   lattr: byte;

Procedure Tseqbar.init (nx,ny : byte; min, max, len : longint);
var x : byte;
begin
     BarMin:=Min;
     BarMax:=Max;
     AbsLen:=Max-Min;
     BarLen:=Len;
     drawn := 0;
     FillClr:=$0A;
     FillChar:='¯';
     BackClr:=$04;
     BackChar:='-';
     barX:=nx;
     bary:=ny;
     Qwrite(barx,bary, charstr(Backchar,BarLen),Backclr);
end;

Procedure Tseqbar.update (v : longint);
var x : longint;
    z : longint;
begin
     x:=((100 * (V - BarMin)) div AbsLen);
     x:=(x*barlen) div 100;
     while drawn<x do begin Qwrite(succ(drawn),bary,fillchar,fillclr); inc(drawn); end;
{     while drawn<x do begin write(fillchar); inc(drawn); end;}
end;

Procedure Tseqbar.SetFill (c : char);
begin
     fillchar:=c;
end;
{----------------------------------------------------------------------}
{----------------------------------------------------------------------}
procedure outkey;
var x: byte;
begin
           for x:=1 to Keylen do write(char(key[x]));
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function CheckSum(var A; Len : byte) : byte;
var x : byte;
    z : byte;
begin
     z:=RndSeed;
     for x:=1 to len do z:=z+ar(A)[x];
     CheckSum:=z;
end;

{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure MorphPass(seedy : byte);
var z : byte;
    x : byte;
    d : boolean;
    l : longint;
begin
     keyLen:=MaxKeyLen-(seedy div 10);
     fillchar(key,sizeof(key),#0);
     move(Seedy, Key[1], 1);
     move(PassStr, Key[2], Length(passStr));
     for x:=1 to pred(KeyLen) do begin
         dec(seedy);
         key[succ(x)]:=(Key[x] xor key[succ(x)])+(key[x] xor seedy);
     end;
     move(key[1],CRC,sizeof(crc));
     for x:=1 to (KeyLen-sizeof(crc)) do begin
         crc:=updc32(key[x],crc);
         move(key[x],l,sizeof(l));
         l:=l xor crc;
         move(l,key[x],sizeof(l));
     end;
end;

{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure Help;
begin
     writeln;
     Writeln('Any Order: ZC <filespec> /P<password> [/F<prefix>] [/L<logfile>] [/I] [/T]');
     Writeln('  Ordered: ZC <filespec> <password> [<prefix>] [/L<logfile>] [/I] [/T]');
     writeln('');
     writeln('/I = Identify mode (report true names of encrypted files with passowrd)');
     writeln('/L = LogFile (to change default hex edit @@@ field in EXE)');
     writeln('/T = Test CRC mode (buggy - *might* rarely report bad when crc is good');
     writeln('<prefex> = first character patern of file to be encrypted');
     writeln('<password> = don''t forget this! can''t even /I without it. No Spaces.');
     writeln('<filespec> = wildcards accepted (don''t use same letters for <prefix>).');
     writeln;
end;

{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure ParseCmdLine;
begin
     if (ParamCount=0) or CmdLineTog('?') then help;
     if CmdLineTog('P') then PassStr:=CmdLineStr('P')
        else if CmdLineNoTogStr(2)<>'' then PassStr:=CmdLineNoTogStr(2)
        else Terr(2,'No Password on commandline',TRUE);
     if CmdLineTog('F') then fnPrefix:=Upstr(CmdLineStr('F'))
        else if CmdLineNoTogStr(3)<>'' then fnPrefix:=UpStr(CmdLineNoTogStr(3));
     if CmdLineTog('L') then fnLog:=CmdLineStr('L');
     if CmdLineNoTogStr(1)<>'' then fnSpec:=CmdLineNoTogStr(1)
        else Terr(1,'No Filename or Filespec on commandline',TRUE);
     if CmdLineTog('I') then ModeIdent:=true;
     if CmdLineTog('T') then ModeTest:=True;
end;

{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function JazzBlk(var V; len : word; keypos : byte; var cr : longint) : byte;
var x : word;
begin
     for x:=1 to len do begin
         if EncMode=mEncrypt then cr:=updc32(ar(v)[x],cr);
         ar(v)[x]:=ar(V)[x] xor key[keypos];
         if Keypos<>KeyLen then inc(keypos) else Keypos:=1;
         if EncMode=mDecrypt then cr:=updc32(ar(v)[x],cr);
     end;
     JazzBlk:=Keypos;
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Function RevAr( var V; len : word; pos : word) : char;
begin
     RevAr:=char(Ar(V)[len-pred(pos)]);
end;
{----------------------------------------------------------------------}
{--                                                                  --}
{----------------------------------------------------------------------}
Procedure MakeHed(var r);
var x : byte;
begin
        hed.fa := sr.attr;
        hed.fd := sr.time;
        hed.fs := sr.size;
        hed.fn := sr.name;
        hed.fc := 0;
        hed.rs := rndseed; {rndseed}
        hed.ev := EncVer;
        hed.pv[1] := id1;
        hed.pv[5] := id2;
        move(progver[1],hed.pv[2],3);

        fnOut:=FnPrefix;
        for x:= succ(length(fnOUt)) to 8 do begin
            if random(4)<>1 then fnout:=fnout+char(65+random(26))
            else fnout:=fnout+char(vowels[random(8)]);
        end;
end;
Procedure SpecialRename(const fnFrom, FnTo : string);
var f : file;
begin
     assign(f , fnFrom);
     {$I-}reset(f,1);{$I+}
     if IoResult>0 then write('%');
     {$I-}rename(f,'1'+fnTo);{$I+}
     if IoResult>0 then write('$');
     {$I-}close(f);{$I+}
     if IoResult>0 then write('+');
end;
{**********************************************************************}
{----------------------------------------------------------------------}
{**********************************************************************}
var
   fin : file;
   fout: file;
   flog: text;
   x   : byte;
   first: boolean;
   LogStr: string;
   Pos   : longint;
   r     : word;
   bg    : TseqBar;
   cx,cy : word;

BEGIN
     Lattr:=TextAttr;
     TextColor(blue);
     Write('*');
     TextColor(lightblue);
     Write('Z');
     TextColor(blue);
     Write('*');
     TextColor(lightblue);
     Write('Crypt');
     TextColor(Blue);
     Write(' v');
     TextColor(lightblue);
     Writeln(ProgVer);
     CheckBreak:=FALSE;
     DosRedirect;
     Randomize;
     ParseCmdLine;
     fnLog:=rtrim(fnlog,'@');
     if (fnLog[0]>#0) then Lg.Init(fnLog);
     FindFirst(fnSpec,AnyFile-Directory,sr);
     IF DosError>0 then begin writeln('No files matching filespec: ',fnSpec); halt; end;
     while DosError=0 do begin
           Filemode:=2;
           assign(fin,{pathOnlybs(fnSpec)+}sr.name); {$I-}reset(Fin,1);{$I+}
           if IoResult=0 then begin
              blockRead(fin,rndseed,sizeof(rndseed));
              MorphPass(rndSeed);
              seek(fin,filesize(fin)-sizeof(hed));
              BlockRead(fin,hed,sizeof(hed));
              JazzBlk(hed,sizeof(hed),1,crc);
              {If (revar(hed,sizeof(hed),6)='Z') and (revar(hed,sizeof(hed),2)='C') then begin}
              If (hed.pv[1]='Z') and (hed.pv[5]='C') then begin
                 if hed.ev<>EncVer then begin
                    Writeln(pathOnlybs(fnSpec)+sr.name+' appears to be encrypted with *Z*Crypt version '+
                            {RevAr(hed,sizeof(hed),5)+RevAr(hed,sizeof(hed),4)+RevAr(hed,sizeof(hed),3)+',');}
                            hed.pv[2]+hed.pv[3]+hed.pv[4]+',');
                    Writeln('which uses *Z*Crypt encryption version '+Long2Str(byte(RevAr(hed,sizeof(hed),1)))+
                            '. The current version of');
                    Writeln('*Z*Crypt you are using is incompatable. Aborting...');
                    Close(fin); Halt(250);
                 end;
                 EncMOde:=mDecrypt;
                 if (not ModeIdent)and(not ModeTest) then begin
                    seek(fin,filesize(fin)-sizeof(hed));
                    Truncate(fin);
                 end;
                 LogStr:='Decrypting: '+sr.name+' -> '+hed.fn+' ('+Long2Str(hed.fs div 1024)+'k)';
              end else begin
                  rndSeed:=Random(256);
                  Morphpass(rndseed);
                  MakeHed(sr);
                  LogStr:='Encrypting: '+sr.name+' -> '+fnOUt+' ('+Long2Str(hed.fs div 1024)+'k)';
              end;
              if ModeTest then begin
                 if (enCmode=mEncrypt) then begin
                    Writeln(sr.name,' not *Z*crypted, can not be tested.');
                    close(fin);
                    findnext(sr);
                    continue;
                 end else LogStr:=LogStr+' [test mode]';
              end;
              if ModeIdent then begin
                 if EncMode=Mdecrypt then writeln(sr.name,' => ',hed.fn)
                    else Writeln(sr.name,' not *Z*crypted');
                 close(fin);
                 findnext(sr);
                 continue;
              end;
              Writeln(LogStr);
              Lmode:=Linfo;
              fIRST:=TRUE;
              cx:=wherex; cy:=wherey;
              keyptr:=1;
              crc:=$ffffffff;
              seek(fin,0);
              BG.init(1,1,1,filesize(fin),80);
              While not eof(fin) do begin
                    pos:=FilePos(fin);
                    BlockRead(fin,blk,sizeof(blk),r);
                    if modeTest then begin
                       if (FileSize(fin)-FilePos(fin))<Sizeof(hed) then
                          r:=r-(Sizeof(Hed)-(FileSize(fin)-FilePos(fin)));
                    end;
                    if First and (encmode=mdecrypt) then begin
                       first:=FALSE;
                       blk[1]:=hed.b1;
                    end;
                    keyptr:=JazzBlk(blk,r,keyptr,crc);
                    if First and (encmode=mEncrypt) then begin
                       first:=FALSE;
                       hed.b1:=blk[1];
                       blk[1]:=hed.rs;
                    end;
                    if not modeTest then begin
                       seek(fin,pos);
                       BlockWrite(fin,blk,r);
                    end;
                    bg.update(filepos(fin));
              end;
              gotoxy(cx,cy);
              TextColor(Lattr);
              if ModeTest then begin
                 Write('*Z*crypt file CRC is ');
                 if crc=hed.fc then Writeln('good.') else writeln('bad!');
                 close(fin);
                 findnext(sr);
                 continue;
              end;
              if EncMode=Mencrypt then begin
                 hed.fc := crc;
                 JazzBlk(hed, sizeof(hed), 1, crc);
                 BlockWrite(fin,hed,sizeof(hed));
                 Write('File *Z*Crypted');
                 close(fin);
                 {$I-}Rename(fin, FnOut);{$I+}
                 if IoResult>0 then begin
                    Writeln(', but could not be renamed.');
                    Lmode:=Lwarn; logstr:=logstr+' *';
                 end else writeln('.');
                 if (fnLog[0]>#0) then lg.log(Lmode, logstr);
              end else begin
                 close(fin);
(*                 SpecialRename(sr.name, hed.fn);*)
                 {$I-}rename(fin, hed.fn);{$I+}
                  x:=IOresult;
                  if x>0 then exec(getenv('COMSPEC'),'/C REN '+sr.name+' '+hed.fn);
                  if crc=hed.fc then begin
                     Write('Decrypted file CRC is good');
                     if x>0 then begin
(*                        Writeln(', but could not be renamed (',hed.fn,').');*)
                        Writeln(', but had to FORCE rename.');
                        Lmode:=Lwarn; logstr:=logstr+' *';
                     end else writeln('.');
                  end else begin
                      write('CRC Error: this file did not decrypt properly');
                      Lmode:=Lerror; logstr:=logstr+' **';
                      if x>0 then begin
(*                         Writeln(', and could not be renamed.');*)
                         Writeln(', and had to FORCE rename.');
                         Lmode:=Lfatal; logstr:=logstr+' *';
                      end else writeln('.');
                  end;
                  if (fnLog[0]>#0) then lg.log(Lmode, logstr);
              end;
              {close(fin);}
           end else Writeln('Can not access filename: ',Pathonlybs(FnSpec)+sr.Name);
           findnext(sr);
     end;
     if (fnLog[0]>#0) then lg.done;
END.
{
 ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
 ³ Invalid file handle ³
 ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

This error is reported if an invalid file
handle is passed to a DOS system call.

This error should never occur; if it does, you
know that the file variable is somehow
trashed.
}
