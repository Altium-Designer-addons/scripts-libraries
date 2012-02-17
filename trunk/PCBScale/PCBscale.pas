{..............................................................................}
{                                                                              }
{ Summary   This script can be used to scale selected PCB Objects.             }
{                                                                              }
{           Known issues:                                                      }
{           - Does not work for dimensions and coordinates at the moment       }
{           - scales only extruded 3D models (I do not know how to get radius) }
{                                                                              }
{           Changelog:                                                         }
{           - v1.0 - Initial Release                                           }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
var
   X, Y       : TCoord;
   Ratio      : Float;
   Board      : IPCB_Board;

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



procedure TFormPCBscale.ButtonCancelClick(Sender: TObject);
begin
   close;
end;



procedure TFormPCBscale.EditRatioChange(Sender: TObject);
begin
   if IsStringANum(EditRatio.Text) then
   begin
      EditRatio.Font.Color := clWindowText;
      ButtonOK.Enabled := True;
   end
   else
   begin
      ButtonOK.Enabled := False;
      EditRatio.Font.Color := clRed;
   end;
end;


// Done
Procedure ScaleTrack(Prim : IPCB_Track);
begin
   Prim.x1    := X + Int(Ratio * (Prim.x1 - X));
   Prim.x2    := X + Int(Ratio * (Prim.x2 - X));
   Prim.y1    := Y + Int(Ratio * (Prim.y1 - Y));
   Prim.y2    := Y + Int(Ratio * (Prim.y2 - Y));
   Prim.Width := Int(Prim.Width * Ratio);
end;

// Done
Procedure ScaleArc(Prim : IPCB_Arc);
begin
   Prim.XCenter   := X + Int(Ratio * (Prim.XCenter - X));
   Prim.YCenter   := Y + Int(Ratio * (Prim.YCenter - Y));
   Prim.Radius    := Int(Prim.Radius * Ratio);
   Prim.LineWidth := Int(Prim.LineWidth * Ratio);
end;

// Done
Procedure ScalePad(Prim : IPCB_Pad2);
var
   i : Integer;
begin
   Prim.x := X + Int(Ratio * (Prim.x - X));
   Prim.y := Y + Int(Ratio * (Prim.y - Y));
   Prim.HoleSize := Int(Prim.HoleSize * Ratio);
   if Prim.HoleType = eSlotHole then
      Prim.HoleWidth := Int(Prim.HoleWidth * Ratio);

   // I'll do pad later - It's complicated
   if Prim.Mode = ePadMode_Simple then
   begin
      Prim.TopXSize := Int(Prim.TopXSize * Ratio);
      Prim.TopYSize := Int(Prim.TopYSize * Ratio);
   end
   else if Prim.Mode = ePadMode_LocalStack then
   begin
      Prim.TopXSize := Int(Prim.TopXSize * Ratio);
      Prim.TopYSize := Int(Prim.TopYSize * Ratio);
      Prim.MidXSize := Int(Prim.MidXSize * Ratio);
      Prim.MidYSize := Int(Prim.MidYSize * Ratio);
      Prim.BotXSize := Int(Prim.BotXSize * Ratio);
      Prim.BotYSize := Int(Prim.BotYSize * Ratio);
   end
   else
   begin
      Prim.XSizeOnLayer[String2Layer('Top Layer')]         := Int(Prim.XSizeOnLayer[String2Layer('Top Layer')] * Ratio);
      Prim.YSizeOnLayer[String2Layer('Top Layer')]         := Int(Prim.YSizeOnLayer[String2Layer('Top Layer')] * Ratio);
      Prim.XStackSizeOnLayer[String2Layer('Top Layer')]    := Int(Prim.XStackSizeOnLayer[String2Layer('Top Layer')] * Ratio);
      Prim.YStackSizeOnLayer[String2Layer('Top Layer')]    := Int(Prim.YStackSizeOnLayer[String2Layer('Top Layer')] * Ratio);
      Prim.XPadOffset[String2Layer('Top Layer')]           := Int(Prim.XPadOffset[String2Layer('Top Layer')] * Ratio);
      Prim.YPadOffset[String2Layer('Top Layer')]           := Int(Prim.YPadOffset[String2Layer('Top Layer')] * Ratio);
      Prim.XSizeOnLayer[String2Layer('Bottom Layer')]      := Int(Prim.XSizeOnLayer[String2Layer('Bottom Layer')] * Ratio);
      Prim.YSizeOnLayer[String2Layer('Bottom Layer')]      := Int(Prim.YSizeOnLayer[String2Layer('Bottom Layer')] * Ratio);
      Prim.XStackSizeOnLayer[String2Layer('Bottom Layer')] := Int(Prim.XStackSizeOnLayer[String2Layer('Bottom Layer')] * Ratio);
      Prim.YStackSizeOnLayer[String2Layer('Bottom Layer')] := Int(Prim.YStackSizeOnLayer[String2Layer('Bottom Layer')] * Ratio);
      Prim.XPadOffset[String2Layer('Bottom Layer')]        := Int(Prim.XPadOffset[String2Layer('Bottom Layer')] * Ratio);
      Prim.YPadOffset[String2Layer('Bottom Layer')]        := Int(Prim.YPadOffset[String2Layer('Bottom Layer')] * Ratio);

      for i := 1 to 30 do
      begin
          Prim.XSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))]      := Int(Prim.XSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
          Prim.YSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))]      := Int(Prim.YSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
          Prim.XStackSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] := Int(Prim.XStackSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
          Prim.YStackSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] := Int(Prim.YStackSizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
          Prim.XPadOffset[String2Layer('Mid Layer ' + IntToStr(i))]        := Int(Prim.XPadOffset[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
          Prim.YPadOffset[String2Layer('Mid Layer ' + IntToStr(i))]        := Int(Prim.YPadOffset[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
      end;
   end;
end;

// Done
Procedure ScaleVia(Prim : IPCB_Via);
var
   i : Integer;
begin
   Prim.x := X + Int(Ratio * (Prim.x - X));
   Prim.y := Y + Int(Ratio * (Prim.y - Y));
   Prim.HoleSize := Int(Prim.HoleSize * Ratio);
   Prim.Size :=     Int(Prim.Size * Ratio);

   if Prim.Mode <> ePadMode_Simple then
   begin
      Prim.SizeOnLayer[String2Layer('Top Layer')]    := Int(Prim.SizeOnLayer[String2Layer('Top Layer')] * Ratio);
      Prim.SizeOnLayer[String2Layer('Bottom Layer')] := Int(Prim.SizeOnLayer[String2Layer('Bottom Layer')] * Ratio);

      for i := 1 to 30 do
          Prim.SizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] := Int(Prim.SizeOnLayer[String2Layer('Mid Layer ' + IntToStr(i))] * Ratio);
   end;
end;

// Done
Procedure ScaleFill(Prim : IPCB_Fill);
begin
   Prim.X1Location := X + Int(Ratio * (Prim.X1Location - X));
   Prim.X2Location := X + Int(Ratio * (Prim.X2Location - X));
   Prim.Y1Location := Y + Int(Ratio * (Prim.Y1Location - Y));
   Prim.Y2Location := Y + Int(Ratio * (Prim.Y2Location - Y));
end;

// Done
Procedure ScaleRegion(Prim : IPCB_Region);
var
   i, j : Integer;
   Contour : IPCB_Contour;
   Xa, Ya  : Integer;
   Prim2   : IPCB_Region;
begin
   Contour := PCBServer.PCBContourFactory;
   Prim2 := Prim.Replicate;

   for i := 1 to Prim.MainContour.Count do
   begin
      Xa := X + Int(Ratio * (Prim.MainContour.x[i] - X));
      Ya := Y + Int(Ratio * (Prim.MainContour.y[i] - Y));
      Contour.AddPoint(Xa, Ya);
   end;

   Prim.SetOutlineContour(Contour);
   Contour.Clear;

   for j := 0 to Prim2.HoleCount - 1 do
   begin
      for i := 1 to Prim2.Holes[j].Count do
      begin
         Xa := X + Int(Ratio * (Prim2.Holes[j].x[i] - X));
         Ya := Y + Int(Ratio * (Prim2.Holes[j].y[i] - Y));
         Contour.AddPoint(Xa, Ya);
      end;
      Prim.GeometricPolygon.AddContourIsHole(Contour, True);
      Contour.Clear;
   end;

   Prim.GraphicallyInvalidate;
end;

// Done
Procedure ScaleText(Prim : IPCB_Text);
begin
   Prim.XLocation := X + Int(Ratio * (Prim.XLocation - X));
   Prim.YLocation := Y + Int(Ratio * (Prim.YLocation - Y));
   Prim.Size      := Int(Prim.Size  * Ratio);
   Prim.Width     := Int(Prim.Width * Ratio);
   If Prim.Inverted then
      if Prim.UseInvertedRectangle then
      begin
         Prim.InvRectHeight := Int(Prim.InvRectHeight * Ratio);
         Prim.InvRectWidth  := Int(Prim.InvRectWidth  * Ratio);
         Prim.TTFOffsetFromInvertedRect := Int(Prim.TTFOffsetFromInvertedRect  * Ratio);
      end
      else
         Prim.InvertedTTTextBorder := Int(Prim.InvertedTTTextBorder * Ratio);    

   if Prim.TextKind = eText_BarCode then
   begin
      Prim.BarCodeFullHeight := Int(Prim.BarCodeFullHeight * Ratio);
      Prim.BarCodeXMargin  := Int(Prim.BarCodeXMargin  * Ratio);
      Prim.BarCodeYMargin  := Int(Prim.BarCodeYMargin  * Ratio);
      if Prim.BarCodeRenderMode = eRender_ByMinWidth then
         Prim.BarCodeMinWidth := Int(Prim.BarCodeMinWidth * Ratio)
      else
         Prim.BarCodeFullWidth := Int(Prim.BarCodeFullWidth * Ratio);
   end;
   Prim.GraphicallyInvalidate;
end;

// Done
Procedure ScalePoly(Polygon : IPCB_Polygon);
var
   i    : Integer;
   Iter : IPCB_GroupIterator;
   Prim : IPCB_Primitive;
   NewSegment : TPolySegment;
begin
   For I := 0 To Polygon.PointCount Do
   begin
      NewSegment := TPolySegment;
      NewSegment.Kind := Polygon.Segments[I].Kind;
      If Polygon.Segments[I].Kind = ePolySegmentArc then
      begin
         NewSegment.cx := X + Int(Ratio * (Polygon.Segments[I].cx - X));
         NewSegment.cy := Y + Int(Ratio * (Polygon.Segments[I].cy - Y));
         NewSegment.Radius := Int(Polygon.Segments[I].Radius * Ratio);
      end
      else
      begin
         NewSegment.vx := X + Int(Ratio * (Polygon.Segments[I].vx - X));
         NewSegment.vy := Y + Int(Ratio * (Polygon.Segments[I].vy - Y));
      end;
      Polygon.Segments[I] := NewSegment;
   end;

   Iter := Polygon.GroupIterator_Create;

   Prim := Iter.FirstPCBObject;
   While Prim <> nil do
   begin
      Prim.BeginModify;
      case Prim.ObjectId of
         eTrackObject         : ScaleTrack(Prim);
         eArcObject           : ScaleArc(Prim);
         eRegionObject        : ScaleRegion(Prim);
      end;
      Prim.EndModify;
      Prim.GraphicallyInvalidate;
      Prim := Iter.NextPCBObject;
   end;
   Polygon.GroupIterator_Destroy(Iter);
   Polygon.GraphicallyInvalidate;
end;

// Can not read radius - can't scale cylinder - so - Done
Procedure Scale3DModel(Prim : IPCB_ComponentBody);
var
   Xa, Ya  : TCoord;
   Radius  : TCoord;
   Contour : IPCB_Contour;
   i       : Integer;
begin
   Xa := Prim.BoundingRectangle.Right - Prim.BoundingRectangle.Left;
   Ya := Prim.BoundingRectangle.Top   - Prim.BoundingRectangle.Bottom;


   if Prim.Model.ModelType = e3DModelType_Extruded then
   begin
      Contour := PCBServer.PCBContourFactory;
      for i := 1 to Prim.MainContour.Count do
      begin
         Xa := X + Int(Ratio * (Prim.MainContour.x[i] - X));
         Ya := Y + Int(Ratio * (Prim.MainContour.y[i] - Y));
         Contour.AddPoint(Xa, Ya);
      end;

      Prim.SetOutlineContour(Contour);
      Prim.OverallHeight  := Int(Prim.OverallHeight * Ratio);
      Prim.StandoffHeight := Int(Prim.StandoffHeight * Ratio);
   end
   else if Prim.Model.ModelType = e3DModelType_Cylinder then
   begin
      Radius := Int(Xa / 2);
      Prim.MoveByXY(Xa - X, Ya - Y);
      Prim.ModelFactory_UpdateModel(Int(Xa / 2 * Ratio),0,e3DModelType_Cylinder);
   end
   else if Prim.Model.ModelType = e3DModelType_Sphere then
   begin
      Prim.MoveByXY(Xa - X, Ya - Y);
   end
   else if Prim.Model.ModelType = e3DModelType_Generic then
   begin
      Prim.MoveByXY(Xa - X, Ya - Y);
   end;
   Prim.GraphicallyInvalidate;
end;

// Not done
Procedure ScaleDimension(Prim : IPCB_Primitive);
begin

end;

// Not done - bad placement and can not refresh
Procedure ScaleCoordinate(Prim : IPCB_Coordinate);
begin

   Prim.LineWidth  := Int(Prim.LineWidth * Ratio);
   Prim.Size       := Int(Prim.Size * Ratio);
   Prim.MoveByXY(Int((Prim.BoundingRectangle.Left - X + (Prim.Size + Prim.LineWidth) / 2) * Ratio), Int((Prim.BoundingRectangle.Bottom - Y + (Prim.Size + Prim.LineWidth) / 2) * Ratio));

   Prim.TextHeight := Int(Prim.TextHeight * Ratio);
   if not Prim.UseTTFonts then
      Prim.TextWidth := Int(Prim.TextWidth * Ratio);

   Prim.Track1.Width := Prim.LineWidth;
   Prim.Track2.Width := Prim.LineWidth;
   Prim.Text.Size := Prim.Size;
   if not Prim.UseTTFonts then
      Prim.Text.Width := Prim.TextWidth;

   Prim.Track1.GraphicallyInvalidate;
   Prim.Track2.GraphicallyInvalidate;
   Prim.Text.GraphicallyInvalidate;

end;

// Done
Procedure ScaleRoom(Prim : IPCB_ConfinementConstraint);
var
   i    : Integer;
   Iter : IPCB_GroupIterator;
   NewSegment : TPolySegment;
begin
   For I := 0 To Prim.PointCount Do
   begin
      NewSegment := TPolySegment;
      NewSegment.Kind := Prim.Segments[I].Kind;
      If Prim.Segments[I].Kind = ePolySegmentArc then
      begin
         NewSegment.cx := X + Int(Ratio * (Prim.Segments[I].cx - X));
         NewSegment.cy := Y + Int(Ratio * (Prim.Segments[I].cy - Y));
         NewSegment.Radius := Int(Prim.Segments[I].Radius * Ratio);
      end
      else
      begin
         NewSegment.vx := X + Int(Ratio * (Prim.Segments[I].vx - X));
         NewSegment.vy := Y + Int(Ratio * (Prim.Segments[I].vy - Y));
      end;
      Prim.Segments[I] := NewSegment;
   end;

   Prim.GraphicallyInvalidate;
end;

// Done
Procedure ScaleComponent(Component : IPCB_Component);
var
   Iter : IPCB_GroupIterator;
   Prim : IPCB_Primitive;
   TempX, TempY : TCoord;
begin

   Component.x := X + Int(Ratio * (Component.x - X));
   Component.y := Y + Int(Ratio * (Component.y - Y));

   TempX := X;
   TempY := Y;

   X := Component.x;
   Y := Component.y;

   ScaleText(Component.Name);
   ScaleText(Component.Comment);

   Iter := Component.GroupIterator_Create;

   Prim := Iter.FirstPCBObject;
   While Prim <> nil do
   begin
      Prim.BeginModify;
      case Prim.ObjectId of
         eTrackObject         : ScaleTrack(Prim);
         eArcObject           : ScaleArc(Prim);
         ePadObject           : ScalePad(Prim);
         eViaObject           : ScaleVia(Prim);
         eFillobject          : ScaleFill(Prim);
         eRegionObject        : ScaleRegion(Prim);
         eTextObject          : ScaleText(Prim);
         ePolyObject          : ScalePoly(Prim);
         eComponentBodyObject : Scale3DModel(Prim);
      // eDimensionObject     : ScaleDimension(Prim);
      // eCoordinateObject    : ScaleCoordinate(Prim);
      end;
      Prim.EndModify;
      Prim.GraphicallyInvalidate;

      Prim := Iter.NextPCBObject;
   end;
   Component.GroupIterator_Destroy(Iter);

   X := TempX;
   Y := TempY;
end;



procedure TFormPCBscale.ButtonOKClick(Sender: TObject);
var
   Rectang    : TCoordRect;
   TempString : String;
   i          : Integer;
   Prim       : IPCB_Primitive;
begin
   Board := PCBServer.GetCurrentPCBBoard;

   if Board = nil then exit;
   if Board.SelectecObjectCount = 0 then exit;

   TempString := EditRatio.Text;
   if LastDelimiter(',.', TempString) <> 0 then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

   Ratio := StrToFloat(TempString);

   X := Board.SelectecObject[0].BoundingRectangle.Left;
   Y := Board.SelectecObject[0].BoundingRectangle.Bottom;

   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      Prim := Board.SelectecObject[i];
      if X > Prim.BoundingRectangle.Left then
         X := Prim.BoundingRectangle.Left;
      if Y > Prim.BoundingRectangle.Bottom then
         Y := Prim.BoundingRectangle.Bottom;
   end;

   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      Prim := Board.SelectecObject[i];

      if ((Prim.InComponent or Prim.InCoordinate or Prim.InDimension or Prim.InPolygon) = False) then
      begin
         Prim.BeginModify;
         case Prim.ObjectId of
              eTrackObject                : ScaleTrack(Prim);
              eArcObject                  : ScaleArc(Prim);
              ePadObject                  : ScalePad(Prim);
              eViaObject                  : ScaleVia(Prim);
              eFillobject                 : ScaleFill(Prim);
              eRegionObject               : ScaleRegion(Prim);
              eTextObject                 : ScaleText(Prim);
              eComponentObject            : ScaleComponent(Prim);
              ePolyObject                 : ScalePoly(Prim);
              eComponentBodyObject        : Scale3DModel(Prim);
              eDimensionObject            : ScaleDimension(Prim);
              eCoordinateObject           : ScaleCoordinate(Prim);
              eRuleObject                 : ScaleRoom(Prim);
         end;
         Prim.EndModify;
         Prim.GraphicallyInvalidate;
      end;
   end;
   Board.ViewManager_FullUpdate;
   close;
end;



Procedure Start;
begin
   Board := PCBServer.GetCurrentPCBBoard;

   if Board = nil then exit;
   if Board.SelectecObjectCount = 0 then
   begin
      ShowMessage('There are no selected objects');
      exit;
   end;

   FormPCBscale.ShowModal;
end;



