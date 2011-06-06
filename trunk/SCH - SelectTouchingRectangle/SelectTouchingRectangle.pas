Function Start;
var
   CurrentSheet    : ISch_Document;
   SpatialIterator : ISch_Iterator;
   Obj             : TObject;
   Rect            : TCoordRect;
   boolRect        : bool;
begin
   If SchServer = Nil Then Exit;
   CurrentSheet := SchServer.GetCurrentSchDocument;
   If CurrentSheet = Nil Then Exit;

   Rect := TCoordRect;

   boolRect := CurrentSheet.ChooseRectangleInteractively(Rect,
                                                     'Please select the first corner',
                                                     'Please select the second corner');
   If Not boolRect Then Exit;

   SchServer.ProcessControl.PreProcess(CurrentSheet, '');

   // Check components for selection
   SpatialIterator := CurrentSheet.SchIterator_Create;
   If SpatialIterator = Nil Then Exit;
   Try                                                         
      SpatialIterator.AddFilter_Area(Rect.left, Rect.bottom, Rect.right, Rect.top);
      //SpatialIterator.addfilt

      Obj := SpatialIterator.FirstSchObject;
      While Obj <> Nil Do
      Begin
         SchServer.RobotManager.SendMessage(Obj.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
             Obj.Selection := True;    // write new designator to the component
         SchServer.RobotManager.SendMessage(Obj.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

         Obj := SpatialIterator.NextSchObject;
      End;
   Finally
      CurrentSheet.SchIterator_Destroy(SpatialIterator);
   End;
end;
