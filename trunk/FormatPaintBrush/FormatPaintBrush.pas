function FormatPaintBrush;
var
   // SCH variables and objects
   SchDoc          : ISCH_Document;
   Location        : TLocation;
   SchSourcePrim   : TObject;
   SchDestinPrim   : TObject;
   SpatialIterator : ISch_Iterator;

   // PCB variables and objects
   PCBBoard      : IPCB_Board;
   SourcePrim    : IPCB_Primitive;
   DestinPrim    : IPCB_Primitive;
   BoardIterator : IPCB_BoardIterator;

   // Common variables
   boolLoc       : bool;

Begin
   If GetWorkspace.DM_FocusedDocument.DM_DocumentKind = 'SCH' then
   Begin
      // Get the document
      if SchServer = nil then exit;
      SchDoc := SchServer.GetCurrentSchDocument;
      if SchDoc = nil then exit;

      ResetParameters;
      AddStringParameter('Action', 'AllOpenDocuments');
      RunProcess('Sch:DeSelect');

      Location := TLocation;

      // Get Source Object
      boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose Source Object');

      If Not boolLoc Then Exit;

      SpatialIterator := SchDoc.SchIterator_Create;
      If SpatialIterator = Nil Then Exit;
      Try
         SpatialIterator.AddFilter_Area(Location.X - 1, Location.Y - 1, Location.X + 1, Location.Y + 1);

         SchSourcePrim := SpatialIterator.FirstSchObject;
      Finally
         SchDoc.SchIterator_Destroy(SpatialIterator);
      End;

      // Get First Destination Object
      boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose Destination Object');

      If Not boolLoc Then Exit;

      SpatialIterator := SchDoc.SchIterator_Create;
      If SpatialIterator = Nil Then Exit;
      Try
         SpatialIterator.AddFilter_ObjectSet(MkSet(SchSourcePrim.ObjectId));
         SpatialIterator.AddFilter_Area(Location.X - 1, Location.Y - 1, Location.X + 1, Location.Y + 1);

         SchDestinPrim := SpatialIterator.FirstSchObject;
      Finally
         SchDoc.SchIterator_Destroy(SpatialIterator);
      End;

      While (SchSourcePrim <> nil) and (SchDestinPrim <> nil) do
      begin
         SchServer.ProcessControl.PreProcess(SchDoc, '');

         SchServer.RobotManager.SendMessage(SchDestinPrim.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);

         // This is where modification goes.
         if SchSourcePrim.ObjectId = eWire then
         begin
            SchDestinPrim.LineStyle := SchSourcePrim.LineStyle;
            SchDestinPrim.LineWidth := SchSourcePrim.LineWidth;
            SchDestinPrim.Color     := SchSourcePrim.Color;     

         //---------------------------------------------------------------------
         //
         //----------------      NEW SCH OBJECTS HERE      ---------------------
         //
         //---------------------------------------------------------------------
         //
         // if you want to add some new SCH objects, uncomment this
     (*
         end
         else if (SchSourcePrim.ObjectId = <ObjectId-here>) then
         begin
            // This is where object properties are being copies
     *)

         end;
         SchServer.RobotManager.SendMessage(SchDestinPrim.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

         SchServer.ProcessControl.PostProcess(SchDoc, '');

         // Get Next Destination Object
         boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose Destination Object');

         If Not boolLoc Then Exit;

         SpatialIterator := SchDoc.SchIterator_Create;
         If SpatialIterator = Nil Then Exit;
         Try
            SpatialIterator.AddFilter_ObjectSet(MkSet(SchSourcePrim.ObjectId));
            SpatialIterator.AddFilter_Area(Location.X - 1, Location.Y - 1, Location.X + 1, Location.Y + 1);

            SchDestinPrim := SpatialIterator.FirstSchObject;
         Finally
            SchDoc.SchIterator_Destroy(SpatialIterator);
         End;
      end;

   End
   Else If GetWorkspace.DM_FocusedDocument.DM_DocumentKind = 'PCB' then
   Begin
      // Get the document
      If PCBServer = Nil Then Exit;
      PCBBoard := PCBServer.GetCurrentPCBBoard;
      If PCBBoard = Nil Then Exit;

      ResetParameters;
      AddStringParameter('Scope', 'All');
      RunProcess('PCB:DeSelect');

      SourcePrim := PCBBoard.GetObjectAtCursor(AllObjects, AllLayers,'Choose Source Primitive');
      DestinPrim := PCBBoard.GetObjectAtCursor(MkSet(SourcePrim.ObjectId), AllLayers,'Choose Destination Primitive');

      While Assigned(DestinPrim) And Assigned(SourcePrim) Do
      Begin
         // copy formatting of PCB dimension
         PCBServer.PreProcess;
         Try
             // Always use IPCB_Primitive.BeginModify instead of PCBServer.SendMessageToRobots because is deprecated
             DestinPrim.BeginModify;
             If (SourcePrim.ObjectId = eDimensionObject) Then
             Begin
                 DestinPrim.ArrowLength        := SourcePrim.ArrowLength;
                 DestinPrim.ArrowLineWidth     := SourcePrim.ArrowLineWidth;
                 DestinPrim.ArrowSize          := SourcePrim.ArrowSize;
                 DestinPrim.ArrowPosition      := SourcePrim.ArrowPosition;
                 DestinPrim.LineWidth          := SourcePrim.LineWidth;

                 DestinPrim.TextHeight         := SourcePrim.TextHeight;
                 DestinPrim.TextWidth          := SourcePrim.TextWidth;
                 DestinPrim.TextFont           := SourcePrim.TextFont;
                 DestinPrim.TextLineWidth      := SourcePrim.TextLineWidth;
                 DestinPrim.TextGap            := SourcePrim.TextGap;
                 DestinPrim.TextFormat         := SourcePrim.TextFormat;
                 DestinPrim.TextDimensionUnit  := SourcePrim.TextDimensionUnit;
                 DestinPrim.TextPrecision      := SourcePrim.TextPrecision;
                 DestinPrim.TextPosition       := SourcePrim.TextPosition;
                 DestinPrim.TextPrefix         := SourcePrim.TextPrefix;
                 DestinPrim.TextSuffix         := SourcePrim.TextSuffix;
                 DestinPrim.TextValue          := SourcePrim.TextValue;
                 DestinPrim.ExtensionOffset    := SourcePrim.ExtensionOffset;
                 DestinPrim.ExtensionLineWidth := SourcePrim.ExtensionLineWidth;
                 DestinPrim.ExtensionPickGap   := SourcePrim.ExtensionPickGap;
                 DestinPrim.Style              := SourcePrim.Style;
                 DestinPrim.UseTTFonts         := SourcePrim.UseTTFonts;
                 DestinPrim.Bold               := SourcePrim.Bold;
                 DestinPrim.Italic             := SourcePrim.Italic;
                 DestinPrim.FontName           := SourcePrim.FontName;
                 DestinPrim.Size               := SourcePrim.Size;

                 // !!! Workaround for now - needed to fake Dimension has changed semantics. This
                 // is necesary because we don't currenly have access to the Dimension method that
                 // force a dimension update. Without this the call to DestinPrim.SetState_XSizeYSize
                 // is not doing anything
                 DestinPrim.TextX              := DestinPrim.TextX + MilsToCoord(0.01);
                 DestinPrim.SetState_XSizeYSize;
                 DestinPrim.GraphicallyInvalidate;
                 DestinPrim.TextX              := DestinPrim.TextX - MilsToCoord(0.01);
                 DestinPrim.SetState_XSizeYSize;
                 DestinPrim.GraphicallyInvalidate;

             End
             else if (SourcePrim.ObjectId = eCoordinateObject) Then
             begin
                // this is where we put coordinate formatting
                 DestinPrim.Size               := SourcePrim.Size;
                 DestinPrim.LineWidth          := SourcePrim.LineWidth;
                 DestinPrim.TextHeight         := SourcePrim.TextHeight;
                 DestinPrim.TextWidth          := SourcePrim.TextWidth;
                 DestinPrim.TextFont           := SourcePrim.TextFont;
                 DestinPrim.Style              := SourcePrim.Style;
                 DestinPrim.UseTTFonts         := SourcePrim.UseTTFonts;
                 DestinPrim.Bold               := SourcePrim.Bold;
                 DestinPrim.Italic             := SourcePrim.Italic;
                 DestinPrim.FontName           := SourcePrim.FontName;

                 // !!! Workaround for now - needed to fake Dimension has changed semantics. This
                 // is necesary because we don't currenly have access to the Dimension method that
                 // force a dimension update. Without this the call to DestinPrim.SetState_XSizeYSize
                 // is not doing anything
                 DestinPrim.X     := DestinPrim.X + MilsToCoord(0.01);
                 DestinPrim.SetState_XSizeYSize;
                 DestinPrim.GraphicallyInvalidate;
                 DestinPrim.X     := DestinPrim.X - MilsToCoord(0.01);
                 DestinPrim.SetState_XSizeYSize;
                 DestinPrim.GraphicallyInvalidate;

             //-----------------------------------------------------------------
             //
             //----------      NEW PCB OBJECTS HERE      -----------------------
             //
             //-----------------------------------------------------------------
             //
             // if you want to add some new PCB objects, uncomment this
          (*
             end
             else if (SourcePrim.ObjectId = <ObjectId-here>) then
             begin
               // This is where object properties are being copies
          *) 


             end;
             // Always use IPCB_Primitive.EndModify / CancelMdify instead of PCBServer.SendMessageToRobots because is deprecated
             DestinPrim.EndModify;
             PCBBoard.ViewManager_FullUpdate;
         Finally
             PCBServer.PostProcess;
         End;
         DestinPrim := PCBBoard.GetObjectAtCursor(MkSet(SourcePrim.ObjectId), AllLayers,'Choose Next Destination Primitive');
      End;
   End;
End;
