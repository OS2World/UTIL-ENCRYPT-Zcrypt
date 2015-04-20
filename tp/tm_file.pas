Unit TM_File;

INTERFACE


Type
  FileBuffer = Array[0..$fff8] of byte;


  BufFileObj = Object
    BufFile : File;
    Buf : ^FileBuffer;
    IOResult : Integer;
    Filename : String;
    Open,
    Dirty : Boolean;
    BufFilePos : Longint;
    RecordSize,
    BufSize,
    BufPos,
    BufTop : Word;
    Constructor Init(BufferSize : Word);
    Destructor Done;

    Procedure Assign(Fname : String);
    Procedure Reset(RecSize : Word);
    Procedure Rewrite(RecSize : Word);
    Procedure Close;
    Procedure Rename(Fname : String);
    Procedure Erase;

    Function EOF : Boolean;
    Function FilePos : Longint;
    Function FileSize : Longint;
    Procedure Seek(Pos : Longint);

    Procedure Read(Var V);
    Procedure Write(Var V);
    Procedure BlockRead(Var V; Count : Word; var Result : Word);
    Procedure BlockWrite(Var V; Count : Word; var Result : Word);

    Procedure FillBuffer;
    Procedure FlushBuffer;
  End;
  BufferedFile = ^BufFileObj;


IMPLEMENTATION


Constructor BufFileObj.Init(BufferSize : Word);
  Begin
    Filename:= '';
    Open:= false;
    Dirty:= false;
    BufFilePos:= 0;
    RecordSize:= 0;
    BufSize:= BufferSize;
    BufPos:= 0;
    BufTop:= 0;
    Getmem(Buf, BufSize);
    if Buf = nil then Fail;
  End;


Destructor BufFileObj.Done;
  Begin
    If Open then Close;
    if Buf <> nil then Freemem(Buf, BufSize);
  End;


Procedure BufFileObj.Assign(Fname : String);
  Begin
    if Open then Close;
    Filename:= FName;
    {$I-}system.Assign(BufFile, Fname);{$I+}
    IOResult:= system.IOResult;
  End;


Procedure BufFileObj.Reset(RecSize : Word);
  Begin
    if Open then Close;
    if RecSize = 0
    then RecordSize:= 128
    else RecordSize:= RecSize;
    {$I-}system.Reset(BufFile, 1);{$I+}
    IOResult:= system.IOResult;
    BufFilePos:= 0;
    BufPos:= 0;
    FillBuffer;
    if IOResult = 0
    then Open:= true
    else Open:= false;
  End;


Procedure BufFileObj.Rewrite(RecSize : Word);
  Begin
    if Open then Close;
    if RecSize = 0
    then RecordSize:= 128
    else RecordSize:= RecSize;
    BufTop:= 0;
    {$I-}system.Rewrite(BufFile, 1);{$I+}
    IOResult:= system.IOResult;
    if IOResult = 0
    then Open:= true
    else Open:= false;
  End;


Procedure BufFileObj.Close;
  Begin
    If Dirty then FlushBuffer;
    {$I-}system.Close(BufFile);{$I+}
    IOResult:= system.IoResult;
    Open:= false;
  End;


Procedure BufFileObj.Rename(Fname : String);
  Begin
    {$I-}system.Rename(BufFile, Fname);{$I+}
    IOResult:= system.IOResult;
    if IOResult = 0 then Filename:= FName;
  End;


Procedure BufFileObj.Erase;
  Begin
    if Open then Close;
    {$I-}system.Erase(BufFile);{$I+}
    IOResult:= system.IOResult;
  End;


Function BufFileObj.EOF : Boolean;
  Begin
    EOF:= BufFileObj.FilePos >= BufFileObj.FileSize;
  End;


Function BufFileObj.FileSize : Longint;
  Begin
    if system.FileSize(BufFile)>=(BufFilePos+BufTop)
       then FileSize:=system.FileSize(BufFile) div RecordSize
       else FileSize:=(BufFilePos+BufTop) div RecordSize;
  End;


Function BufFileObj.FilePos : Longint;
  Begin
    FilePos:= (BufFilePos + BufPos) div RecordSize;
  End;


Procedure BufFileObj.Seek(Pos : Longint);
  Begin
    Pos:= Pos * RecordSize;
    if (Pos >= BufFilePos + BufSize) or
       (Pos < BufFilePos)
    then begin
      if Dirty then FlushBuffer;
      {$I-}system.seek(BufFile, Pos);{$I+}
      IOResult:= system.IOResult;
      BufFilePos:= Pos;
      BufPos:= 0;
      FillBuffer;
    end
    else BufPos:= Pos - BufFilePos;
    IoResult:= system.IoResult;
  End;


Procedure BufFileObj.Read(Var V);
  Var
    Overflow : Word;
  Begin
    If BufPos + RecordSize > BufTop
    then begin
      Overflow:= BufSize - BufPos;
      Move(Buf^[BufPos], V, Overflow);
      inc(BufFilePos, BufSize);
      BufPos:= 0;
      FillBuffer;
      Move(Buf^[BufPos], FileBuffer(V)[Overflow], RecordSize - Overflow);
      BufPos:= BufPos + RecordSize - Overflow;
    end
    else begin
      Move(Buf^[BufPos], V, RecordSize);
      inc(BufPos, RecordSize);
    end;
    IoResult:= system.IoResult;
  End;


Procedure BufFileObj.BlockRead(Var V; Count : Word; var Result : Word);
  Begin
    Result:= 0;
    While (Result < Count) and
          (IoResult = 0) and
          not EOF
    do begin
      {$I-}Read(FileBuffer(V)[Result * RecordSize]);{$I+}
      Inc(Result);
    end;
  End;


Procedure BufFileObj.Write(Var V);
  Var
    Overflow : Word;
  Begin
    Dirty:= true;
    If BufPos + RecordSize > BufSize
    then begin
      Overflow:= BufSize - BufPos;
      if Overflow > 0
      then Move(V, Buf^[BufPos], Overflow);
      BufTop:= BufSize;
      FlushBuffer;
      inc(BufFilePos, BufSize);
      BufPos:= 0;
      FillBuffer;
      Move(FileBuffer(V)[Overflow], Buf^[BufPos], RecordSize - Overflow);
      if OverFlow>0 theN Dirty:=True;
      BufPos:= BufPos + RecordSize - Overflow;
    end
    else begin
      Move(V, Buf^[BufPos], RecordSize);
      inc(BufPos, RecordSize);
    end;
    if BufTop < BufPos
    then BufTop:= BufPos;
    IoResult:= system.IoResult;
  End;


Procedure BufFileObj.BlockWrite(Var V; Count : Word; var Result : Word);
  Begin
    Result:= 0;
    While (Result < Count) and
          (IoResult = 0)
    do begin
      {$I-}Write(FileBuffer(V)[Result * RecordSize]);{$I+}
      Inc(Result);
    end;
  End;


Procedure BufFileObj.FillBuffer;
  Begin
    system.Seek(BufFile, BufFilePos);
    system.BlockRead(BufFile, Buf^, BufSize, BufTop);
    IoResult:= system.IoResult;
    if IoResult = 0 then Dirty:= false;
  End;


Procedure BufFileObj.FlushBuffer;
  Begin
    {$I-}system.Seek(BufFile, BufFilePos);{$I+}
    {$I-}system.BlockWrite(BufFile, Buf^, BufTop, BufTop);{$I+}
    IoResult:= system.IoResult;
  End;


Begin
End.
(*
{ -------------------------   DEMO ----------------- }
Program TestFile;

Uses
  FastFile;

Var
  InFile,
  OutFile : BufferedFile;
  Data : Array[1..120] of byte;

Begin
  New(InFile, Init($8000));            { Allocate a 32K buffer };
  New(OutFile, Init($4000));           { 16K Buffer }
  InFile^.Assign(Parameter(1));        { Assign input to a filename }
  InFile^.Reset(sizeof(data));        
  OutFile^.Assign(Parameter(2));
  OutFile^.Rewrite(sizeof(data));
  While not InFile^.EOF do begin 
    InFile^.Read(Data);
    if Data[1] <> 0 then OutFile^.Write(Data);
  end;
  InFile^.Close;
  OutFile^.Close;
  InFile^.Erase;
  OutFile^.Rename(Parameter(1)); 
  Dispose(InFile);                     { Close the file and free the memory }
  Dispose(OutFile);
End.

  As you see, this implements a very silly program which removes the original
after stripping unwanted records from it.  Without the large RAM buffers, this
would be an extremely slow, but since all READs and WRITEs and SEEKs and 
everything else are done to RAM, few actual disk accesses result.  I used this
in a USEnet mail processing program and the speed went from 15K/sec to
130K/sec.

  The easiest way to use it is to simply change structures like:

Assign(Filehandle, name);   --->   Filehandle^.Assign(name);
 
  ...in many cases a couple of Search..Replace passes are all that you need to
modify your software to use this.  I've done it will all of my own and am very
pleased.  

  Again, if you run into any problems, please let me know!  I can guarantee
you only that it works fine for me.  :-)
*)