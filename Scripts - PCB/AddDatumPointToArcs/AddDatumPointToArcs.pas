procedure AddDatumPointToArcs;
var
   Board    : IPCB_Board;
   Arc      : IPCB_Arc;
   i        : Integer;
   Tr1, Tr2 : IPCB_Track;
   Tr3, Tr4 : IPCB_Track;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   if Board.SelectecObjectCount = 0 then exit;

   for i := 0 to Board.SelectecObjectCount - 1 do
      if Board.SelectecObject[i].ObjectId = eArcObject then
      begin
         Arc := Board.SelectecObject[i];
         Tr1 := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
         Tr1.Width := Arc.LineWidth;
         Tr1.Layer := Arc.Layer;

         if (Arc.StartAngle = 0) and (Arc.EndAngle = 360) then
         begin
            Tr1.x1 := Arc.XCenter;
            Tr1.y1 := Arc.YCenter;
            Tr1.x2 := Arc.XCenter + Arc.Radius * 1.3;
            Tr1.y2 := Arc.YCenter;
            Board.AddPCBObject(Tr1);

            Tr2 := Tr1.Replicate;
            Tr2.x2 := Arc.XCenter;
            Tr2.y2 := Arc.YCenter + Arc.Radius * 1.3;
            Board.AddPCBObject(Tr2);

            Tr3 := Tr1.Replicate;
            Tr3.x2 := Arc.XCenter - Arc.Radius * 1.3;
            Tr3.y2 := Arc.YCenter;
            Board.AddPCBObject(Tr3);

            Tr4 := Tr1.Replicate;
            Tr4.x2 := Arc.XCenter;
            Tr4.y2 := Arc.YCenter - Arc.Radius * 1.3;
            Board.AddPCBObject(Tr4);
         end
         else
         begin
            Tr1.x1 := Arc.XCenter;
            Tr1.y1 := Arc.YCenter;
            Tr1.x2 := Arc.XCenter + Arc.Radius * 1.3 * cos((Arc.StartAngle + 0.5 * (Arc.EndAngle - Arc.StartAngle)) * pi / 180);
            Tr1.y2 := Arc.YCenter + Arc.Radius * 1.3 * sin((Arc.StartAngle + 0.5 * (Arc.EndAngle - Arc.StartAngle)) * pi / 180);
            Board.AddPCBObject(Tr1);
         end;
      end;
   Board.ViewManager_FullUpdate;
end;
