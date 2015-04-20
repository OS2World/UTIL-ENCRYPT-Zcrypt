unit TM_LINK;

Interface


Type
    POneLink = ^TOneLink;
    TOneLink = Object
             Obj : pointer;
             Next: pointer;
             Prev: Pointer;
             size: longint;
             constructor init( o : pointer; s : longint);
    end;

    end

    TLinkObj = Object
             Size  : longint;
             Pos   : longint;
             Count : longint;

             pFirst: PoneLink;
             pLast : POneLink;
             pCurr : PoneLInk;
             pTemp : PoneLink;

             Constructor Init;
             Destructor Done; Virtual;
             Procedure Clear;
             Procedure RemoveObj(p : pointer);
             Procedure Append (p : pointer; s : longint);
             Procedure Insert (p : pointer; s : longint);
             Procedure Insertat(l : longint; p : pointer; s : longint);
             Procedure Add    (p : pointer; s : longint);

             Procedure CreateFirst(p : pointer; s : longint);
             Procedure DestoryLast(p : pointer; s : longint);
    end;

Implementation

Constructor TOneLink(o : pointer; s: longint);
begin
     Obj := o;
     Suc := nil;
     Pre := nil;
     size:= s;
end;

Constructor TLinkObj.Init
begin
     Size:=0;
     Pos :=0;
     First:=Nil;
     Last:=Nil;
     Curr:=Nil;
     Temp:=Nil;
end;

Destructor TLinkObj.Done;
begin
     If Size>0 then Clear;
end;

Procedure TlinkObj.Clear;
begin
     While Size>0 do RemoveObj(First);
end;

Procedure TlinkObj.CreateFirst;
begin
     pCurr  := Temp;
     pFirst := pTemp;
     pLast  := pTemp;
     Size   :=pTemp^.size;
     pos    :=1;
     Count  :=1;
end;

Procedure TlinkObj.Add(o : pointer; s : longint);
begin
     pTemp := New(TOneLink, init(o,s));
     if Count=0 then CreateFirst else begin
         pTemp^.Next := pFirst;
         pFirst^.Prev := pTemp;
         pFirst := pTemp;
         Size:=Size+s;
         inc(pos);
         Inc(count);
         if (pos<count) then Pcurr := Pcurr^.next;
     end;
end;

Procedure TlinkObj.Append(o : pointer; s : longint);
begin
     pTemp := New(TOneLink, init(o,s));
     if Count=0 then CreateFirst else begin
         pTemp^.Prev := pLast;
         pLast^.Next := pTemp;
         pLast := pTemp;
         Size:=Size+s;
         Inc(count);
     end;
end;

Procedure TlinkObj.Insert(o : pointer; s : longint);
begin
     pTemp := New(TOneLink, init(o,s));
     if Count=0 then CreateFirst
     else if (count=Pos) then Append(o, s)
     else begin
          pTemp^.Next:= pCurr^.next;
          pCurr^.NExt:= Ptemp;
          PTemp^.Prev:= Pcurr;
          Size:=Size+s;
          inc(pos);
          Inc(count);
     end;
     pCurr := Ptemp;
end;

Procedure TlinkObj.InsertBefore(o : pointer; s : longint);
begin
     pTemp := New(TOneLink, init(o,s));
     if Count=0 then CreateFirst
     else begin
          pTemp^.Prev:= pCurr^.Prev;
          pCurr^.Prev:= Ptemp;
          PTemp^.Next:= Pcurr;
          Size:=Size+s;
          pred(pos);
          Inc(count);
          pCurr := pCurr^.Prev;
     end;
end;

END.
