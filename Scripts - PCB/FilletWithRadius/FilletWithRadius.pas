{..............................................................................}
{                                                                              }
{ Summary   This Script rounds connections on selected tracks, by fixed radius }
{           value. It supports only horizontal and vertical lines              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}


var
   Board   : IPCB_Board;
   MaxRadi : Double;



function HorizOrVert(Track : IPCB_Track) : String ;
begin
   Result := 'None';

   if (abs(Track.x1 - Track.x2) < 100) then
       Result := 'Vert'
   else if (abs(Track.y1 - Track.y2) < 100) then
       Result := 'Horiz';
end;

Procedure NumOfConnections(Prim : IPCB_Primitive, X : TCoord, Y : TCoord, out NumConn : Integer, out ConnectedPrim : IPCB_Primitive);
var
   SpIter : IPCB_SpatialIterator;
   Prim1 : IPCB_Primitive;
begin
   NumConn := 0;

   SpIter := Board.SpatialIterator_Create;
   SpIter.AddFilter_ObjectSet(MkSet(eTrackObject));
   SpIter.AddFilter_LayerSet(MkSet(Prim.Layer));
   SpIter.AddFilter_Area(X,Y,X+1,Y+1);

   Prim1 := SpIter.FirstPCBObject;

   While (Prim1 <> nil) do
   begin
      if Prim1.Selected and (Prim1.I_ObjectAddress <> Prim.I_ObjectAddress) and
        (((abs(Prim1.x1 - X) < 100) and (abs(Prim1.y1 - Y) < 100)) or
         ((abs(Prim1.x2 - X) < 100) and (abs(Prim1.y2 - Y) < 100))) then
         begin
            if ((HorizOrVert(Prim1) = 'Vert')  and (HorizOrVert(Prim) = 'Horiz')) or
               ((HorizOrVert(Prim1) = 'Horiz') and (HorizOrVert(Prim) = 'Vert')) then
               begin
                  Inc(NumConn);
                  ConnectedPrim := Prim1;
               end;
         end;

      Prim1 := SpIter.NextPCBObject;
   end;
   Board.SpatialIterator_Destroy(SpIter);
end;


Procedure Start;
var
   i       : Integer;
   Prim    : IPCB_Primitive;
   Prim1   : IPCB_Primitive;
   SpIter  : IPCB_SpatialIterator;
   NumConn : Integer;
   Length  : Double;
   HasFirst: Boolean;
begin
   Board := PCBServer.GetCurrentPCBBoard;

   if Board = nil then exit;

   if Board.SelectecObjectCount = 0 then exit;

   MaxRadi := -1;

   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      Prim := Board.SelectecObject[i];
      if Prim.ObjectID = eTrackObject then
      begin
         If HorizOrvert(Prim) = 'None' then
         begin
            ShowMessage('Only Horizontal and vertical tracks are supported');
            exit;
         end;

         HasFirst := False;

         NumOfConnections(Prim,Prim.x1,Prim.y1, NumConn,Prim1);

         if NumConn > 1 then
         begin
            ShowMessage('Multiple connections in point are not supported.');
            exit;
         end
         else if NumConn = 1 then HasFirst := True;

         NumOfConnections(Prim,Prim.x2,Prim.y2,NumConn,Prim1);

         if NumConn > 1 then
         begin
            ShowMessage('Multiple connections in point are not supported.');
            exit;
         end;

         if (NumConn = 1) and HasFirst then
         begin
            Length := (sqrt(sqr(Prim.x2 - Prim.x1) + sqr(Prim.y2 - Prim.y1))) / 2;

            if (Length < MaxRadi) or (MaxRadi = -1) then MaxRadi := Length;
         end
         else if (NumConn = 1) or HasFirst then
         begin
            Length := sqrt(sqr(Prim.x2 - Prim.x1) + sqr(Prim.y2 - Prim.y1));

            if (Length < MaxRadi) or (MaxRadi = -1) then MaxRadi := Length;
         end;
      end;
   end;

   if MaxRadi = -1 then exit;

   EditRadius.Text := FloatToStr(CoordToMils(MaxRadi));

   FormFilletWithRadius.ShowModal;
end;


function IsStringANum(Tekst : String) : Boolean;
var
   i : Integer;
   dotCount : Integer;
begin
   Result := True;

   if Tekst = '' then Result := False;

   // Test weather we have number, dot or comma
   for i := 1 to Length(Tekst) do
      if not(((ord(Tekst[i]) > 47) and (ord(Tekst[i]) < 58)) or (ord(Tekst[i]) = 44) or (ord(Tekst[i]) = 46)) then
         Result := False;

   // Test if we have more than one dot or comma
   dotCount := 0;
   for i := 1 to Length(Tekst) do
      if ((ord(Tekst[i]) = 44) or (ord(Tekst[i]) = 46)) then
      begin
         Inc(dotCount);
         if (i = 1) or (i = Length(Tekst)) then Result := False;
      end;

   if dotCount > 1 then Result := False;
end;


procedure TFormFilletWithRadius.ButtonUnitsClick(Sender: TObject);
var
   TempString : String;
begin
   TempString := EditRadius.Text;
   if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

   If ButtonUnits.Caption = 'mil' then
   begin
      ButtonUnits.Caption := 'mm';
      EditRadius.Text := CoordToMMs(MilsTocoord(StrToFloat(TempString)));
   end
   else
   begin
      ButtonUnits.Caption := 'mil';
      EditRadius.Text := CoordToMils(MmsTocoord(StrToFloat(TempString)));
   end;
end;

procedure TFormFilletWithRadius.EditRadiusChange(Sender: TObject);
begin

   If IsStringANum(EditRadius.Text) then
   begin
      EditRadius.Font.Color := clWindowText;
      ButtonOK.Enabled := True;
   end
   else
   begin
      ButtonOK.Enabled := False;
      EditRadius.Font.Color := clRed;
   end;
end;

procedure TFormFilletWithRadius.ButtonCancelClick(Sender: TObject);
begin
   close;
end;


procedure TFormFilletWithRadius.ButtonOKClick(Sender: TObject);
var
   TempString : String;
   Radius     : Integer;
   i          : Integer;
   Prim       : IPCB_Primitive;
   Prim1      : IPCB_Primitive;
   NumConn    : Integer;
   X, Y       : TCoord;
   Arc        : IPCB_Arc;
   ModifyList   : TStringList;
begin
   Board.NewUndo;

   TempString := EditRadius.Text;
   if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

   if (ButtonUnits.Caption = 'mm') then Radius := mmsToCoord(StrToFloat(TempString))
   else                                 Radius := milsToCoord(StrToFloat(TempString));

   if Radius > MaxRadi then
   begin
      ShowMessage('Radius Larger than maximum. Please modify.');

      if (ButtonUnits.Caption = 'mm') then EditRadius.Text := StrToFloat(CoordToMMs(MaxRadi))
      else                                 EditRadius.Text := StrToFloat(CoordToMils(MaxRadi));

      exit;
   end;

   ModifyList := TStringList.Create;

   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      Prim := Board.SelectecObject[i];
      if Prim.ObjectID = eTrackObject then
      begin
         if HorizOrVert(Prim) = 'Vert' then
         begin
            if Prim.y1 > Prim.y2 then
            begin
               Y := Prim.y1;
               Prim.y1 := Prim.y2;
               Prim.y2 := Y;
            end;

            NumOfConnections(Prim,Prim.x1,Prim.y1, NumConn, Prim1);

            if (NumConn = 1) then
            begin
               // Prim is vertical, and Prim1 is horizontal
               if Prim1.x1 > Prim1.x2 then
               begin
                  X := Prim1.x1;
                  Prim1.x1 := Prim1.x2;
                  Prim1.x2 := X;
               end;

               X := Prim.x1;
               Y := Prim.y1;

               Arc := PCBServer.PCBObjectFactory(eArcObject,eNodimension,eCreate_Default);

               if ((abs(Prim1.x1 - X) < 100) and (abs(Prim1.y1 - Y) < 100)) then
               begin
                  Arc.XCenter := X + Radius;
                  Arc.YCenter := Y + Radius;

                  Arc.Radius := Radius;
                  Arc.StartAngle := 180;
                  Arc.EndAngle := 270;

                  Prim1.x1 := X + Radius;
               end
               else if((abs(Prim1.x2 - X) < 100) and (abs(Prim1.y2 - Y) < 100)) then
               begin
                  Arc.XCenter := X - Radius;
                  Arc.YCenter := Y + Radius;

                  Arc.Radius := Radius;
                  Arc.StartAngle := 270;
                  Arc.EndAngle := 0;

                  Prim1.x2 := X - Radius;
               end;

               Prim.y1  := Y + Radius;
               Arc.LineWidth := Prim.Width;
               Arc.layer := Prim.Layer;
               Board.AddPCBObject(Arc);
               ModifyList.AddObject('1',Arc);

               Prim1.GraphicallyInvalidate;
               Prim.GraphicallyInvalidate;

               Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim.I_ObjectAddress);
               Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim1.I_ObjectAddress);
               Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Arc.I_ObjectAddress);

               if (Sqrt(sqr(Prim.x2 - Prim.x1) + sqr(Prim.y2 - Prim.y1)) < 100) then
                  ModifyList.AddObject('1',Prim);


               if (Sqrt(sqr(Prim1.x2 - Prim1.x1) + sqr(Prim1.y2 - Prim1.y1)) < 100) then
                  ModifyList.AddObject('1',Prim1);
            end;

            NumOfConnections(Prim,Prim.x2,Prim.y2, NumConn, Prim1);

            if (NumConn = 1) then
            begin
               // Prim is vertical, and Prim1 is horizontal
               if Prim1.x1 > Prim1.x2 then
               begin
                  X := Prim1.x1;
                  Prim1.x1 := Prim1.x2;
                  Prim1.x2 := X;
               end;

               X := Prim.x2;
               Y := Prim.y2;

               Arc := PCBServer.PCBObjectFactory(eArcObject,eNodimension,eCreate_Default);

               if ((abs(Prim1.x1 - X) < 100) and (abs(Prim1.y1 - Y) < 100)) then
               begin
                  Arc.XCenter := X + Radius;
                  Arc.YCenter := Y - Radius;

                  Arc.Radius  := Radius;
                  Arc.StartAngle := 90;
                  Arc.EndAngle := 180;

                  Prim1.x1 := X + Radius;
               end
               else if((abs(Prim1.x2 - X) < 100) and (abs(Prim1.y2 - Y) < 100)) then
               begin
                  Arc.XCenter := X - Radius;
                  Arc.YCenter := Y - Radius;

                  Arc.Radius := Radius;
                  Arc.StartAngle := 0;
                  Arc.EndAngle := 90;

                  Prim1.x2 := X - Radius;
               end;

               Prim.y2  := Y - Radius;
               Arc.LineWidth := Prim.Width;
               Arc.layer := Prim.Layer;
               Board.AddPCBObject(Arc);
               ModifyList.AddObject('1',Arc);

               Prim1.GraphicallyInvalidate;
               Prim.GraphicallyInvalidate;

               Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim.I_ObjectAddress);
               Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Prim1.I_ObjectAddress);
               Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Arc.I_ObjectAddress);

               if (Sqrt(sqr(Prim.x2 - Prim.x1) + sqr(Prim.y2 - Prim.y1)) < 100) then
                  ModifyList.AddObject('1',Prim);

               if (Sqrt(sqr(Prim1.x2 - Prim1.x1) + sqr(Prim1.y2 - Prim1.y1)) < 100) then
                  ModifyList.AddObject('1',Prim1);

            end;
         end;
      end;
   end;


   for i := 0 to ModifyList.Count - 1 do
   begin
      Prim := ModifyList.GetObject(i);
      if Prim.ObjectId = eTrackObject then
         Board.RemovePCBObject(Prim)
      else if Prim.ObjectId = eArcObject then
      Begin
         Prim.Selected := True;
         Prim.GraphicallyInvalidate;
      end;
   end;

   close;
end;
