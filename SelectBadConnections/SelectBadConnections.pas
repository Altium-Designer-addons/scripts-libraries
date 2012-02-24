{..............................................................................}
{ Summary   This script checks weather Tracks and arcs on signal connect totaly on some  }
{           other object. Center-to-center check is done. If not, it is        }
{           selected.                                                          }
{                                                                              }
{           You set tolerance in a form. Zero tolerance is supported.          }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}


Var
   Board     : IPCB_Board;
   Tolerance : TCoord;


function IsStringANum(Tekst : String) : Boolean;
var
   i : Integer;
   dotCount : Integer;
begin
   Result := True;

   // Test weather we have number, dot or comma
   for i := 1 to Length(Tekst) do
      if not(((ord(Tekst[i]) > 47) and (ord(Tekst[i]) < 58)) or (ord(Tekst[i]) = 44) or (ord(Tekst[i]) = 46)) then
         Result := False;

   // Test if we have more than one dot or comma
   dotCount := 0;
   for i := 1 to Length(Tekst) do
      if ((ord(Tekst[i]) = 44) or (ord(Tekst[i]) = 46)) then
         Inc(dotCount);

   if dotCount > 1 then Result := False;
end;

function CheckWithTolerance(X1, Y1, X2, Y2) : Boolean;
begin
   if (Abs(X1 - X2) <= Tolerance) and (Abs(Y1 - Y2) <= Tolerance) then
      Result := True
   else
      Result := False;
end;


procedure TFormSelectBadConnections.ButtonOKClick(Sender: TObject);
var
   BIter      : IPCB_BoardIterator;
   SIter      : IPCB_SpatialIterator;
   Prim1      : IPCB_Primitive;
   Prim2      : IPCB_Primitive;
   i, X, Y    : Integer;
   Found      : Boolean;
   TempString : String;

begin
   TempString := EditTolerance.Text;
   if LastDelimiter(',.', TempString) = Length(TempString) then
      SetLength(TempString, Length(TempString - 1))
   else if LastDelimiter(',.', TempString) <> 0 then
      TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

   if RadioButtonMil.Checked then
      Tolerance := MilsToCoord(StrToFloat(TempString))
   else
      Tolerance := MMsToCoord(StrToFloat(TempString));

   ResetParameters;
   AddStringParameter('Scope','All');
   RunProcess('PCB:DeSelect');

   BIter := Board.BoardIterator_Create;
   BIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject));
   BIter.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);

   Prim1 := BIter.FirstPCBObject;

   While Prim1 <> nil do
   begin
      if Prim1.TearDrop or Prim1.InComponent or not Prim1.InNet then
         Found := True
      else if (Prim1.ObjectId = eArcObject) and (Prim1.StartAngle = 0) and (Prim1.EndAngle = 360) then
         Found := True
      else
      for i := 1 to 2 do
      begin
         if i = 1 then
         begin
            if Prim1.ObjectId = eTrackObject then
            begin
               X := Prim1.x1;
               Y := Prim1.y1;
            end
            else
            begin
               X := Prim1.StartX;
               Y := Prim1.StartY;
            end;
         end
         else
         begin
            if Prim1.ObjectId = eTrackObject then
            begin
               X := Prim1.x2;
               Y := Prim1.y2;
            end
            else
            begin
               X := Prim1.EndX;
               Y := Prim1.EndY;
            end;
         end;

         SIter := Board.SpatialIterator_Create;
         SIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject, ePadObject, eViaObject));
         SIter.AddFilter_LayerSet(MkSet(Prim1.Layer, String2Layer('Multi Layer')));
         SIter.AddFilter_Area(X - Tolerance, Y - Tolerance, X + Tolerance, Y + Tolerance);

         Found := False;

         Prim2 := SIter.FirstPCBObject;
         While (Prim2 <> nil) and not Found do
         begin
            if Prim2.InNet and (Prim2.Net.Name = Prim1.Net.Name) and (Prim1.I_ObjectAddress <> Prim2.I_ObjectAddress) and not Prim2.TearDrop then
            begin
               if (Prim2.ObjectId = eTrackObject) and (Prim2.Layer = Prim1.Layer) then
               begin
                  if CheckWithTolerance(Prim2.x1, Prim2.y1, X, Y) or CheckWithTolerance(Prim2.x2, Prim2.y2, X, Y) then
                  begin
                     Found := True;
                     break;
                  end;
               end
               else if (Prim2.ObjectId = eArcObject) and (Prim2.Layer = Prim1.Layer) then
               begin
                  if CheckWithTolerance(Prim2.StartX, Prim2.StartY, X, Y) or CheckWithTolerance(Prim2.EndX, Prim2.EndY, X, Y) then
                  begin
                     Found := True;
                     break;
                  end;
               end
               else if Prim2.ObjectId = ePadObject then
               begin
                  if ((Prim2.Layer = String2Layer('Multi Layer')) or (Prim2.Layer = Prim1.Layer)) and CheckWithTolerance(Prim2.x, Prim2.y, X, Y) then
                  begin
                     Found := True;
                     break;
                  end;
               end
               else if Prim2.ObjectId = eViaObject then
               begin
                  if Prim2.IntersectLayer(Prim1.Layer) and CheckWithTolerance(Prim2.x, Prim2.y, X, Y) then
                  begin
                     Found := True;
                     break;
                  end;
               end;
            end;

            Prim2 := SIter.NextPCBObject;
         end;
         Board.SpatialIterator_Destroy(SIter);

         if not Found then Prim1.Selected := True;
      end;

      Prim1 := BIter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(BIter);

   Client.PostMessage('PCB:RunQuery','Apply=True|Expr=IsSelected|Mask=True', Length('Apply=True|Expr=IsSelected|Mask=True'), Client.CurrentView);
   
   close;
end;



procedure TFormSelectBadConnections.ButtonCancelClick(Sender: TObject);
begin
   close;
end;



procedure TFormSelectBadConnections.EditToleranceChange(Sender: TObject);
begin
   if not IsStringANum(EditTolerance.Text) then
   begin
      ButtonOK.Enabled := False;
      EditTolerance.Font.Color := clRed;
   end
   else
   begin
      EditTolerance.Font.Color := clWindowText;
      ButtonOK.Enabled := True;
   end;
end;



Procedure Start;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   FormSelectBadConnections.ShowModal;
end;

