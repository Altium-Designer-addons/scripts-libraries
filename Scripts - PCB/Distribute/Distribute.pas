{..............................................................................}
{                                                                              }
{ Summary   This Script distributes selected tracks, by equalizing their       }
{           distance. Selected tracks need to be parallel. They can be         }
{           distributed 4 ways:                                                }
{                                                                              }
{                                                                              }
{           - Distribute by Centerlines: Tracks will be distributed by their   }
{             centers. This way their clearance will only be the same if they  }
{             have same width.                                                 }
{           - Distribute by Clearance: If tracks do not have same width, in    }
{             this mode they will distribute in a way their clearance is the   }
{             same.                                                            }
{                                                                              }
{           First two modes will not move first and last line, and will        }
{           distribute tracks between this two lines.                          }
{                                                                              }
{           Last two modes will fix only one line and distibut by value in the }
{           form.                                                              }
{                                                                              }
{                                                                              }
{ Created by:    Matija Markovic, Petar Perisin                                }
{ Edited by:    Ryan Rutledge                                                  }
{..............................................................................}

{..............................................................................}



var
   Board : IPCB_Board;

Procedure SetupDataFromTrack(var Prim1 : IPCB_Track, out IsVertical : Boolean, out X1 : TCoord, out Y1 : TCoord, out X2 : TCoord : Out Y2 : TCoord, out k : Double, out c : TCoord);
var
   a, b : Integer;
begin
   if Prim1.x1 = Prim1.x2 then
   begin
      IsVertical := True;
      X1 := Prim1.x1;
      X2 := Prim1.x2;
      if Prim1.y1 < Prim1.y2 then
      begin
         Y1 := Prim1.y1;
         Y2 := Prim1.y2;
      end
      else
      begin
         Y1 := Prim1.y2;
         Y2 := Prim1.y1;
      end;
      k := 0;
      c := Prim1.x1;
   end
   else
   begin
      if Prim1.x1 < Prim1.x2 then
      begin
         X1 := Prim1.x1;
         Y1 := Prim1.y1;
         X2 := Prim1.x2;
         Y2 := Prim1.y2;
      end
      else
      begin
         X1 := Prim1.x2;
         Y1 := Prim1.y2;
         X2 := Prim1.x1;
         Y2 := Prim1.y1;
      end;
      k  := (Y2 - Y1)/(X2 - X1);

      // This is Vertical if k > 20
      if (Abs(k) > 20) then
      begin
         X1 := Prim1.x1;
         X2 := Prim1.x2;

        repeat
           a := X1 mod 10;
           b := X2 mod 10;
           X1 := X1 div 10;
           X2 := X2 div 10;
        until ((a <> 0) or (b <> 0));

        if a = 0 then
        begin
           X1 := Prim1.x1;
           X2 := Prim1.x1;
        end
        else
        begin
           X1 := Prim1.x2;
           X2 := Prim1.x2;
        end;

        if Prim1.y1 < Prim1.y2 then
        begin
           Y1 := Prim1.y1;
           Y2 := Prim1.y2;
        end
        else
        begin
           Y1 := Prim1.y2;
           Y2 := Prim1.y1;
        end;

        c := X1;
        IsVertical := True;
      end
      else
      begin
         c := Y1 - k * X1;
         IsVertical := False;
      end;
   end;
end;


Function GetIntersection(k1 : Double, c1 : TCoord, IsPrim1Vert : boolean, k2 : Double, c2 : TCoord, IsPrim2Vert : Boolean, out X : TCoord, out Y : TCoord) : Boolean;
begin
   Result := True;
   if (IsPrim1Vert and IsPrim2Vert) or ((not IsPrim1Vert) and (not IsPrim2Vert) and (abs(k1 - k2) < 0.01)) then
   begin
      // Parallel tracks
      Result := False;
   end
   else if IsPrim1Vert or IsPrim2Vert then
   begin
      if IsPrim1Vert then
      begin
         X := c1;
         Y := k2*c1 + c2;
      end
      else
      begin
         X := c2;
         Y := k1*c2 + c1;
      end;
   end
   else
   begin
      X := (c2 - c1)/(k1 - k2);
      Y := k1*X + c1;
   end;
end;

// Function that calculates point to point distance
function PointToPointDistance(X1, Y1, X2, Y2) : double;
begin
   Result := sqrt(sqr(X2 - X1) + sqr(Y2 - Y1));
end;


Function GetAnotherTrackInPoint(Prim1 : IPCB_Track, X : TCoord, Y : TCoord, out OnFirstPoint : Boolean) : IPCB_Primitive;
var
   SIter : IPCB_SpatialIterator;
   Prim2 : IPCB_Track;
begin
   Result := nil;
   OnFirstPoint := False;

   // Check if there is another track/arc in hotspot
   SIter := Board.SpatialIterator_Create;
   SIter.AddFilter_ObjectSet(MkSet(eTrackObject));
   SIter.AddFilter_LayerSet(MkSet(Prim1.Layer));
   SIter.AddFilter_Area(X - 1, Y - 1, X + 1, Y + 1);

   Prim2 := SIter.FirstPCBObject;
   While (Prim2 <> nil) do
   begin
      if (Prim2.InNet) and (Prim2.Net.Name = Prim1.Net.Name) and (Prim1.I_ObjectAddress <> Prim2.I_ObjectAddress) and (not Prim2.TearDrop) then
      begin
         if (PointToPointDistance(Prim2.x1, Prim2.y1, X, Y) <= 100) then
         begin
            Result := Prim2;
            OnFirstPoint := True;
            Board.SpatialIterator_Destroy(SIter);
            exit;
         end;

         if (PointToPointDistance(Prim2.x2, Prim2.y2, X, Y) <= 100) then
         begin
            Result := Prim2;
            Board.SpatialIterator_Destroy(SIter);
            exit;
         end;
      end;

      Prim2 := SIter.NextPCBObject;
   end;
   Board.SpatialIterator_Destroy(SIter);
end;

              // pcetak programa


Procedure InitialCheck(Var status : Integer);
var
   i : integer;
   Prim1 : IPCB_Primitive;
   k1, k2  : Double;
   C1, c2, : tCoord;
   IsVert1 : Boolean;
   IsVert2 : Boolean;
   x11, x12, y11,y12 : TCoord;       // altium zapis koordinata
   x21, x22, y21,y22 : TCoord;
begin
   status := 0;    // clear result status
      // Checks if current document is a PCB kind if not, exit.
   Board := PCBServer.GetCurrentPCBBoard;
   If Board = Nil Then Exit;
      // testiramo da li otvren trenutb aktivan PCB doc

   If Board.SelectecObjectCount < 2 then
   begin
      Showmessage('Select at least 2 tracks');
      status := 1;
      exit;
   end;


   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      Prim1 := Board.SelectecObject[i];

      if (Prim1.ObjectID <> eTrackObject) then
      begin
         showmessage ('Select only tracks');
         status := 1;
         Exit;
      end;
   end;

   Prim1 := Board.SelectecObject[0];
   SetupDataFromTrack(Prim1,IsVert1,x11,y11,x12,y12,k1,c1);

          // provjera da li je sve mparalelno
   for i := 1 to Board.SelectecObjectCount - 1 do
   begin
      Prim1 := Board.SelectecObject[i];
      SetupDataFromTrack(Prim1,IsVert2,x21,y21,x22,y22,k2,c2);


      if ((IsVert1 <> IsVert2) or (abs(k1 - k2) > 0.01)) then
      begin
         showmessage ('Selected tracks have to be parallel.');
         status := 1;
         Exit;
      end;
   end;
end;


procedure calculate(dummy : Integer = 0);
var
   i, j : Integer;
   k1, k2 : Double;
   C1, c2, minc, maxc, stepc,cFromWidths : tCoord;
   IsVert1 : Boolean;
   IsVert2 : Boolean;
   IsFirstPoint : Boolean;
   x11, x12, y11,y12 : TCoord;       // altium zapis koordinata
   x21, x22, y21,y22 : TCoord;
   Prim1 : IPCB_Primitive;
   Prim2 : IPCB_Primitive;
   LastAdded : IPCB_Primitive;
   SortedTracks : TStringList;
   MaxNumOfChar : Integer;          //najvci broj znamenki u stringu
   NumOfChar : Integer;              //broj znamenki
   TempString : string;
   X,Y       : TCoord;
   coef      : Double;

begin
   if (Board <> PCBServer.GetCurrentPCBBoard) then
   begin
      ShowMessage('Please start script using START procedure');
      close;
      exit;
   end;
                      // trazi min i max C tj grnice sirenja   vodova
   //Board.NewUndo;
   // Start undo
   PCBServer.PreProcess;
   Prim1 := Board.SelectecObject[0];
   SetupDataFromTrack(Prim1,IsVert1,x11,y11,x12,y12,k1,c1);

   minc := c1;
   maxc := c1;
   coef := cos(arctan(k1));
   cFromWidths := Prim1.Width / (2 * coef);

   // moramo srediti trackove po redu jer su izavrani random
   for i := 1 to Board.SelectecObjectCount - 1 do
   begin
      Prim1 := Board.SelectecObject[i];
      SetupDataFromTrack(Prim1,IsVert2,x21,y21,x22,y22,k2,c2);

      if (minc > c2) then minc := c2;
      if (maxc < c2) then maxc := c2;

      cFromWidths := cFromWidths + Prim1.Width / coef;
   end;

   cFromWidths := cFromWidths - Prim1.Width / (2 * coef);


   SortedTracks := TStringList.Create;

 //  SortedTracks.LoadFromFile('C:\');
 //  SortedTracks.Count


   // punjenje pocetne liste
   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      Prim1 := Board.SelectecObject[i];
      SetupDataFromTrack(Prim1,IsVert2,x21,y21,x22,y22,k2,c2);

      TempString := IntToStr(c2);

      if c2 > 0 then TempString := '+' + TempString;

      SortedTracks.AddObject(TempString,Prim1);
   end;

   // Punjenje stringova nulama   radi jednakosti
   MaxNumOfChar := 0;
   for i := 0 to SortedTracks.Count - 1 do
   begin
      NumOfChar := Length(SortedTracks.Get(i));
      if (MaxNumOfChar < NumOfChar) then MaxNumOfChar := NumOfChar;
   end;


   for i := 0 to SortedTracks.Count - 1 do
   begin
      TempString := SortedTracks[i];

      while (Length(tempString) < maxNumofChar) do
          Insert('0', TempString, 2);

      SortedTracks[i] := TempString;
   end;

   SortedTracks.Sort;
      //   ShowMessage (SortedTracks[0][1]);   prikazuje prvi clan "0" i string na njegovom 1 mjestu
   if (SortedTracks[SortedTracks.Count - 1][1] = '-') then
   begin
      i := 0;

      While(SortedTracks[i][1] = '+') do Inc(i);

      j := 0;

      While(i < SortedTracks.Count) do
      begin
         SortedTracks.Move(SortedTracks.Count - 1, j);
         Inc(j);
         Inc(i);
      end;

   end;


   {
   // Test case if all is good until now
   for i := 0 to SortedTracks.Count - 1 do
   begin
      Prim1 := SortedTracks.GetObject(i);
      Prim1.Selected := False;

      ShowMessage(SortedTracks[i]);

      Prim1.Selected := True;
   end;
   }

   //Dio koji puni object listu sa susjednim trackovima
   // ako je track onda u listu ide YES a noj stavlja NO
   i := 0;
   ResetParameters;
   AddStringParameter('Scope','All');
   RunProcess('PCB:DeSelect');
   while i < SortedTracks.Count do
   begin
      Prim1 := SortedTracks.getObject(i);
      SetupDataFromTrack(Prim1,IsVert1,x11,y11,x12,y12,k1,c1);


            // provjera za prvu tocku i umece 1a liniju ispod 1 linije tracka
      Prim2 := GetAnotherTrackInPoint(Prim1,Prim1.x1,Prim1.y1,IsFirstPoint);


      if (Prim2 = nil) then
         SortedTracks.Insert(i+1,'0')
      else
      begin
         SetupDataFromTrack(Prim2,IsVert2,x21,y21,x22,y22,k2,c2);
                  // test da li ima nekih problema na tracku npr paralelna dve u nastavku
                  // tj mora biti jedan jedini track
         if ((IsVert1 = IsVert2) and (abs(k1 - k2) < 0.01)) then
         begin
            Prim1.Selected := True;
            Prim2.Selected := True;
            Prim1.GraphicallyInvalidate;
            Prim2.GraphicallyInvalidate;

            ShowMessage('Problem on selected tracks');
         end;

         if IsFirstPoint then
            SortedTracks.InsertObject(i+1,'1',Prim2)
         else
            SortedTracks.InsertObject(i+1,'2',Prim2);
      end;
            // Provjera za drugu tocku i umece 1b ispod linije 1a. Ukupno 3 linije
      Prim2 := GetAnotherTrackInPoint(Prim1,Prim1.x2,Prim1.y2,IsFirstPoint);

      if (Prim2 = nil) then
         SortedTracks.Insert(i+2,'0')
      else
      begin
         SetupDataFromTrack(Prim2,IsVert2,x21,y21,x22,y22,k2,c2);

         if ((IsVert1 = IsVert2) and (abs(k1 - k2) < 0.01)) then
         begin
            Prim1.Selected := True;
            Prim2.Selected := True;
            Prim1.GraphicallyInvalidate;
            Prim2.GraphicallyInvalidate;

            ShowMessage('Problem on selected tracks');
         end;

         if IsFirstPoint then
            SortedTracks.InsertObject(i+2,'1',Prim2)
         else
            SortedTracks.InsertObject(i+2,'2',Prim2);
      end;

      i := i + 3;
   end;

   If Board.SelectecObjectCount <> 0 then exit;

   if RadioButtonCenters.Checked then
      stepc := (maxc - minc) / ((SortedTracks.Count / 3) - 1)
   else if RadioButtonClearance.Checked then
      stepc := (maxc - minc - cFromWidths ) / ((SortedTracks.Count / 3) - 1)
   else
   begin
      TempString := EditDistance.Text;
      if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

      if (ButtonUnits.Caption = 'mm') then stepc := mmsToCoord(StrToFloat(TempString)) / (coef)
      else                                 stepc := milsToCoord(StrToFloat(TempString)) / (coef);

   end;

   i := 0;
   while i < SortedTracks.Count do
   begin
      Prim1 := SortedTracks.GetObject(i);
      Prim1.Selected := True;
      SetupDataFromTrack(Prim1,IsVert1,x11,y11,x12,y12,k1,c1);

      if i = 0 then
          c1 := minc
      else
      begin
         if RadioButtonCenters.Checked or RadioButtonCentersVal.Checked then
            c1 := (i / 3) * stepc + minc
         else
            c1 := c2 + stepc + Prim1.width / (2 * coef);
      end;


      inc(i);
      Prim1.BeginModify;

      Prim2 := SortedTracks.GetObject(i);
      if (SortedTracks[i] <> '0') then
      begin
         SetupDataFromTrack(Prim2,IsVert2,x21,y21,x22,y22,k2,c2);
         if GetIntersection(k1,c1,IsVert1,k2,c2,IsVert2,X,Y) then
         begin
            Prim1.x1 := X;
            Prim1.y1 := y;

            Prim2.BeginModify;
            if (SortedTracks[i] = '1') then
            begin
               Prim2.x1 := X;
               Prim2.y1 := y;
            end
            else
            begin
               Prim2.x2 := X;
               Prim2.y2 := y;
            end;
            Prim2.EndModify;
            Prim2.GraphicallyInvalidate;
            Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim2.I_ObjectAddress);
         end;
      end
      else
      begin
         if IsVert1 then Prim1.x1 := c1
         else            Prim1.y1 := k1*Prim1.x1 + c1;
      end;


      inc(i);

      Prim2 := SortedTracks.GetObject(i);

      if (SortedTracks[i] <> '0') then
      begin
         SetupDataFromTrack(Prim2,IsVert2,x21,y21,x22,y22,k2,c2);

         if GetIntersection(k1,c1,IsVert1,k2,c2,IsVert2,X,Y) then
         begin
            Prim1.x2 := X;
            Prim1.y2 := y;

            Prim2.BeginModify;
            if (SortedTracks[i] = '1') then
            begin
               Prim2.x1 := X;
               Prim2.y1 := y;
            end
            else
            begin
               Prim2.x2 := X;
               Prim2.y2 := y;
            end;
            Prim2.EndModify;
            Prim2.GraphicallyInvalidate;
            Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim2.I_ObjectAddress);
         end;
      end
      else
      begin
         if IsVert1 then Prim1.x2 := c1
         else            Prim1.y2 := k1*Prim1.x2 + c1;
      end;

      Prim1.EndModify;
      Prim1.GraphicallyInvalidate;
      Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim1.I_ObjectAddress);

      if Prim1.InNet then
      begin
         Prim1.Net.ConnectivelyInValidate;
      end;

      c2 := c1 + (Prim1.Width / (2 * coef));

      Inc(i);
   end;
   // Stop undo
   PCBServer.PostProcess;
   close;
end;



function IsStringANum(Text : String) : Boolean;
var
   i : Integer;
   dotCount : Integer;
begin
   Result := True;

   if Text = '' then Result := False;

   // Test weather we have number, dot or comma
   for i := 1 to Length(Text) do
      if not(((ord(Text[i]) > 47) and (ord(Text[i]) < 58)) or (ord(Text[i]) = 44) or (ord(Text[i]) = 46)) then
         Result := False;

   // Test if we have more than one dot or comma
   dotCount := 0;
   for i := 1 to Length(Text) do
      if ((ord(Text[i]) = 44) or (ord(Text[i]) = 46)) then
      begin
         Inc(dotCount);
         if (i = 1) or (i = Length(Text)) then Result := False;
      end;

   if dotCount > 1 then Result := False;
end;



procedure TFormDistribute.ButtonUnitsClick(Sender: TObject);
var
   TempString : String;
begin
   TempString := EditDistance.Text;
   if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

   If ButtonUnits.Caption = 'mil' then
   begin
      ButtonUnits.Caption := 'mm';
      EditDistance.Text := CoordToMMs(MilsTocoord(StrToFloat(TempString)));
   end
   else
   begin
      ButtonUnits.Caption := 'mil';
      EditDistance.Text := CoordToMils(MmsTocoord(StrToFloat(TempString)));
   end;
   Editdistance.SetFocus;
end;


procedure TFormDistribute.EditDistanceChange(Sender: TObject);
begin

   If IsStringANum(EditDistance.Text) then
   begin
      EditDistance.Font.Color := clWindowText;
      ButtonOK.Enabled := True;
   end
   else
   begin
      ButtonOK.Enabled := False;
      EditDistance.Font.Color := clRed;
   end;
end;

procedure TFormDistribute.ButtonCancelClick(Sender: TObject);
begin
   close;
end;


procedure TFormDistribute.RadioButtonCentersClick(Sender: TObject);
begin
   EditDistance.Enabled := False;
   ButtonUnits.Enabled  := False;
end;

procedure TFormDistribute.RadioButtonClearanceClick(Sender: TObject);
begin
   EditDistance.Enabled := False;
   ButtonUnits.Enabled  := False;
end;

procedure TFormDistribute.RadioButtonCentersValClick(Sender: TObject);
begin
   EditDistance.Enabled := True;
   EditDistance.SetFocus;
   ButtonUnits.Enabled  := True;
end;

procedure TFormDistribute.RadioButtonClearanceValClick(Sender: TObject);
begin
   EditDistance.Enabled := True;
   EditDistance.SetFocus;
   ButtonUnits.Enabled  := True;
end;

Procedure FastDistributeByCenterline;
var
   status : integer;
begin
   InitialCheck(status);
   if status = 0 then
   begin
      calculate;
   end
   else exit;
end;

Procedure FastDistributeByClearance;
var
   status : integer;
begin
   InitialCheck(status);
   if status = 0 then
   begin
      RadioButtonClearance.Checked := True;        
      calculate;
   end
   else exit;
end;

Procedure Start;
var
   status : integer;
begin
   InitialCheck(status);
   if status = 0 then
   begin
      FormDistribute.ShowModal;
   end
   else exit;
end;

procedure TFormDistribute.ButtonOKClick(Sender: TObject);
begin
   calculate;
end;

procedure TFormDistribute.FormDistributeShow(Sender: TObject);
begin
   if Board.SelectecObjectCount = 2 then
   begin
         RadioButtonClearance.Enabled := False;
         RadioButtonCenters.Enabled := False;
         RadioButtonClearanceVal.Checked := True;
   end;
end;

