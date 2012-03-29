


Procedure FixOverlaps;
var
   Board      : IPCB_Board;
   BIter      : IPCB_BoardIterator;
   SIter      : IPCB_SpatialIterator;
   Prim1      : IPCB_Primitive;
   Prim2      : IPCB_Primitive;
   k1, k2     : Double;
   c1, c2     : TCoord;
   X11, Y11   : TCoord;
   X12, Y12   : TCoord;
   X21, Y21   : TCoord;
   X22, Y22   : TCoord;
   IsVertical : Boolean;
   IsExtended : Boolean;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   ResetParameters;
   AddStringParameter('Scope','All');
   RunProcess('PCB:DeSelect');

   BIter := Board.BoardIterator_Create;
   BIter.AddFilter_ObjectSet(MkSet(eTrackObject));
   BIter.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);

   Prim1 := BIter.FirstPCBObject;

   While Prim1 <> nil do
   begin
      IsExtended := False;
      if (not Prim1.Selected) and Prim1.InNet and (not Prim1.TearDrop) and (not Prim1.InComponent) then
      begin
         IsVertical := False;
         if (Prim1.x1 = Prim1.x2) then
         begin
            IsVertical := True;
            X11 := Prim1.x1;
            X12 := Prim1.x2;
            if Prim1.y1 < Prim1.y2 then
            begin
               Y11 := Prim1.y1;
               Y12 := Prim1.y2;
            end
            else
            begin
               Y11 := Prim1.y2;
               Y12 := Prim1.y1;
            end
         end
         else if Prim1.x1 < Prim1.x2 then
         begin
            X11 := Prim1.x1;
            Y11 := Prim1.y1;
            X12 := Prim1.x2;
            Y12 := Prim1.y2;
            k1  := (Y12 - Y11)/(X12 - X11);
            c1  := Prim1.y1 - k1 * Prim1.x1;
         end
         else
         begin
            X11 := Prim1.x2;
            Y11 := Prim1.y2;
            X12 := Prim1.x1;
            Y12 := Prim1.y1;
            k1  := (Y12 - Y11)/(X12 - X11);
            c1  := Prim1.y1 - k1 * Prim1.x1;
         end;

         SIter := Board.SpatialIterator_Create;
         SIter.AddFilter_ObjectSet(MkSet(eTrackObject));
         SIter.AddFilter_LayerSet(MkSet(Prim1.Layer));
         SIter.AddFilter_Area(Prim1.BoundingRectangle.Left, Prim1.BoundingRectangle.bottom, Prim1.BoundingRectangle.Right, Prim1.BoundingRectangle.Top);

         Prim2 := SIter.FirstPCBObject;
         While (Prim2 <> nil) do
         begin
            if (not Prim2.Selected) and (Prim1.I_ObjectAddress <> Prim2.I_ObjectAddress) and Prim2.InNet and (Prim2.Net.Name = Prim1.Net.Name) and
               (Prim1.Width = Prim2.Width) and (not Prim2.TearDrop) and (not Prim2.InComponent)then
            begin
               if IsVertical and (Prim2.x1 = Prim2.x2) and (Abs(Prim1.x1 - Prim2.x1) < 100) then
               begin
                  if Prim2.y1 < Prim2.y2 then
                  begin
                     Y21 := Prim2.y1;
                     Y22 := Prim2.y2;
                  end
                  else
                  begin
                     Y21 := Prim2.y2;
                     Y22 := Prim2.y1;
                  end;

                  if (Y11 < Y21) then Prim1.y1 := Y11
                  else                Prim1.y1 := Y21;

                  if (Y12 > Y22) then Prim1.y2 := Y12
                  else                Prim1.y2 := Y22;

                  Prim1.GraphicallyInvalidate;
                  Prim2.Selected := True;
                  IsExtended := True;
               end
               else if (not IsVertical) and (Abs(Prim2.x1 - Prim2.x2) > 100) then
               begin
                  if Prim2.x1 < Prim2.x2 then
                  begin
                     X21 := Prim2.x1;
                     Y21 := Prim2.y1;
                     X22 := Prim2.x2;
                     Y22 := Prim2.y2;
                  end
                  else
                  begin
                     X21 := Prim2.x2;
                     Y21 := Prim2.y2;
                     X22 := Prim2.x1;
                     Y22 := Prim2.y1;
                  end;

                  k2  := (Y22 - Y21)/(X22 - X21);
                  c2  := Prim2.y1 - k2 * Prim2.x1;

                  if (Abs(k1 - k2) < 0.1) and (Abs(c1 - c2) < 100) then
                  begin
                     if (X11 < X21) then
                     begin
                        Prim1.x1 := X11;
                        Prim1.y1 := Y11;
                     end
                     else
                     begin
                        Prim1.x1 := X21;
                        Prim1.y1 := Y21;
                     end;

                     if (X12 > X22) then
                     begin
                        Prim1.x2 := X12;
                        Prim1.y2 := Y12;
                     end
                     else
                     begin
                        Prim1.x2 := X22;
                        Prim1.y2 := Y22;
                     end;

                     Prim1.GraphicallyInvalidate;
                     Prim2.Selected := True;
                     IsExtended := True;
                  end;
               end;
            end;
            Prim2 := SIter.NextPCBObject;
         end;
         Board.SpatialIterator_Destroy(SIter);
      end;

      // If track is extended, repeat all for it, since there might be other prims that touch it
      if not IsExtended then
         Prim1 := BIter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(BIter);

   // Now arcs
   BIter := Board.BoardIterator_Create;
   BIter.AddFilter_ObjectSet(MkSet(eArcObject));
   BIter.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);

   Prim1 := BIter.FirstPCBObject;
   While Prim1 <> nil do
   begin
      IsExtended := False;

      if (not Prim1.Selected) and Prim1.InNet and (not Prim1.TearDrop) and (not Prim1.InComponent) then
      begin

         SIter := Board.SpatialIterator_Create;
         SIter.AddFilter_ObjectSet(MkSet(eArcObject));
         SIter.AddFilter_LayerSet(MkSet(Prim1.Layer));
         SIter.AddFilter_Area(Prim1.BoundingRectangle.Left, Prim1.BoundingRectangle.bottom, Prim1.BoundingRectangle.Right, Prim1.BoundingRectangle.Top);

         Prim2 := SIter.FirstPCBObject;
         While (Prim2 <> nil) do
         begin
            if (not Prim2.Selected) and (not Prim2.TearDrop) and (Prim1.I_ObjectAddress <> Prim2.I_ObjectAddress)  and Prim2.InNet and
               (Prim2.Net.Name = Prim1.Net.Name) and (not Prim2.InComponent) and (Prim1.LineWidth = Prim2.LineWidth) and
               (Abs(Prim1.Radius - Prim2.Radius) < 100) and (Abs(Prim1.XCenter - Prim2.XCenter) < 100) and (Abs(Prim1.YCenter - Prim2.YCenter) < 100) then
            begin
               if (Prim1.StartAngle < Prim1.EndAngle) and (Prim2.StartAngle < Prim2.EndAngle) then
               begin
                  if not ((Prim1.StartAngle >= Prim2.EndAngle) or (Prim2.StartAngle >= Prim1.EndAngle)) then
                  begin
                     if Prim2.StartAngle <= Prim1.StartAngle then Prim1.StartAngle := Prim2.StartAngle;
                     if Prim2.EndAngle   <= Prim1.EndAngle   then Prim1.EndAngle   := Prim2.EndAngle;

                     Prim1.GraphicallyInvalidate;
                     Prim2.Selected := True;
                     IsExtended := True;
                  end;
               end
               else if ((Prim1.StartAngle < Prim1.EndAngle) and (Prim2.StartAngle > Prim2.EndAngle)) or
                       ((Prim1.StartAngle > Prim1.EndAngle) and (Prim2.StartAngle < Prim2.EndAngle)) then
               begin
                  if (Prim1.StartAngle <= Prim2.EndAngle) or (Prim2.StartAngle <= Prim1.EndAngle) then
                  begin
                     if (Prim1.StartAngle <= Prim2.EndAngle) and (Prim2.StartAngle <= Prim1.EndAngle) then
                     begin
                        Prim1.StartAngle := 0;
                        Prim1.EndAngle   := 360;
                     end
                     else
                     begin
                        if Prim1.StartAngle <= Prim2.EndAngle then Prim1.StartAngle := Prim2.StartAngle;
                        if Prim1.EndAngle >= Prim2.StartAngle then Prim1.EndAngle   := Prim2.EndAngle;
                     end;

                     Prim1.GraphicallyInvalidate;
                     Prim2.Selected := True;
                     IsExtended := True;
                  end;
               end
               else if (Prim1.StartAngle > Prim1.EndAngle) and (Prim2.StartAngle > Prim2.EndAngle) then
               begin
                  // They always overlap here
                  if (Prim1.StartAngle <= Prim2.EndAngle) or (Prim2.StartAngle <= Prim1.EndAngle) then
                  begin
                     Prim1.StartAngle := 0;
                     Prim1.EndAngle   := 360;
                  end
                  else
                  begin
                     if Prim1.StartAngle > Prim2.StartAngle then Prim1.StartAngle := Prim2.StartAngle;
                     if Prim1.EndAngle   < Prim2.EndAngle   then Prim1.EndAngle   := Prim2.EndAngle;
                  end;

                  Prim1.GraphicallyInvalidate;
                  Prim2.Selected := True;
                  IsExtended := True;
               end;
            end;
            Prim2 := SIter.NextPCBObject;
         end;
         Board.SpatialIterator_Destroy(SIter);
      end;
      if not IsExtended then
         Prim1 := BIter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(BIter);

   RunProcess('PCB:Clear');// Deletes selected objects

   Board.ViewManager_FullUpdate;
end;
