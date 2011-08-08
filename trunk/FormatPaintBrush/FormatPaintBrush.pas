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

         // Bus
         if (SchSourcePrim.ObjectId = eBus) then
         begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
            SchDestinPrim.LineWidth := SchSourcePrim.LineWidth;
         end

         // Parameter Set
         else if (SchSourcePrim.ObjectId = eParameterSet) then
         begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
            SchDestinPrim.Style     := SchSourcePrim.Style;
         end

         // Bus Entry
         else if (SchSourcePrim.ObjectId = eBusEntry) then
         begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
            SchDestinPrim.LineWidth := SchSourcePrim.LineWidth;
         end

         // Wire
         else if (SchSourcePrim.ObjectId = eWire) then
         begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
            SchDestinPrim.LineWidth := SchSourcePrim.LineWidth;
         end

         // Net Label
         else if (SchSourcePrim.ObjectId = eNetLabel) then
         begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
            SchDestinPrim.FontId    := SchSourcePrim.FontId;
         end

         (*
         // Probe
         else if (SchSourcePrim.ObjectId = eProbe) then
         begin

         end
         *)

         // NoERC Marker
         else if (SchSourcePrim.ObjectId = eNoERC) then
         begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
         end

         // Port
         else if (SchSourcePrim.ObjectId = ePort) then
         begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
            SchDestinPrim.TextColor := SchSourcePrim.TextColor;
            SchDestinPrim.AreaColor := SchSourcePrim.AreaColor;
            SchDestinPrim.Alignment := SchSourcePrim.Alignment;
            SchDestinPrim.Style     := SchSourcePrim.Style;
         end

         // Off-Sheet Connector
         else if (SchSourcePrim.ObjectId = eCrossSheetConnector) then
         begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
            SchDestinPrim.CrossSheetStyle := SchSourcePrim.CrossSheetStyle;
         end

         // Part
         else if (SchSourcePrim.ObjectId = eSchComponent) then
         begin
            SchDestinPrim.Designator.ShowName := SchSourcePrim.Designator.ShowName;
            SchDestinPrim.Comment.ShowName    := SchSourcePrim.Comment.ShowName;
            SchDestinPrim.OverideColors       := SchSourcePrim.OverideColors;
            if SchDestinPrim.OverideColors = True then
            begin
               SchDestinPrim.Color            := SchSourcePrim.Color;
               SchDestinPrim.AreaColor        := SchSourcePrim.AreaColor;
               SchDestinPrim.PinColor         := SchSourcePrim.PinColor;
            end;
         end

         (*
         // Pin
         else if (SchSourcePrim.ObjectId = ePin) then
         begin

         end
         *)

         // Designator
         else if (SchSourcePrim.ObjectId = eDesignator) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.Justification := SchSourcePrim.Justification;
            SchDestinPrim.Autoposition  := SchSourcePrim.Autoposition;
         end

         // Parameter
         else if (SchSourcePrim.ObjectId = eParameter) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.Justification := SchSourcePrim.Justification;
            SchDestinPrim.Autoposition  := SchSourcePrim.Autoposition;
         end

         // Text Frame
         else if (SchSourcePrim.ObjectId = eTextFrame) then
         begin
            SchDestinPrim.Color      := SchSourcePrim.Color;
            SchDestinPrim.TextColor  := SchSourcePrim.TextColor;
            SchDestinPrim.AreaColor  := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid    := SchSourcePrim.IsSolid;
            SchDestinPrim.FontID     := SchSourcePrim.FontID;
            SchDestinPrim.Alignment  := SchSourcePrim.Alignment;
            SchDestinPrim.LineWidth  := SchSourcePrim.LineWidth;
            SchDestinPrim.ShowBorder := SchSourcePrim.ShowBorder;
            SchDestinPrim.WordWrap   := SchSourcePrim.WordWrap;
            SchDestinPrim.ClipToRect := SchSourcePrim.ClipToRect;
         end

         // Text String (Annotation, Label)
         else if (SchSourcePrim.ObjectId = eLabel) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.Justification := SchSourcePrim.Justification; 
            SchDestinPrim.IsMirrored    := SchSourcePrim.IsMirrored;
         end

         // Ellipse
         else if (SchSourcePrim.ObjectId = eEllipse) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.Transparent   := SchSourcePrim.Transparent;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
         end

         // Elliptical Arc
         else if (SchSourcePrim.ObjectId = eEllipticalArc) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
         end

         // Arc
         else if (SchSourcePrim.ObjectId = eArc) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
         end

         // Power Port
         else if (SchSourcePrim.ObjectId = ePowerObject) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.Style         := SchSourcePrim.Style;
            SchDestinPrim.ShowNetName   := SchSourcePrim.ShowNetName;
         end

         // Polygon
         else if (SchSourcePrim.ObjectId = ePolygon) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.Transparent   := SchSourcePrim.Transparent;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
         end

         // Sheet Symbol
         else if (SchSourcePrim.ObjectId = eSheetSymbol) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
         end

         // Sheet Name
         else if (SchSourcePrim.ObjectId = eSheetName) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.IsHidden      := SchSourcePrim.IsHidden;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.Justification := SchSourcePrim.Justification;
            SchDestinPrim.TextHorzAnchor:= SchSourcePrim.TextHorzAnchor;
            SchDestinPrim.TextVertAnchor:= SchSourcePrim.TextVertAnchor;
            SchDestinPrim.Autoposition  := SchSourcePrim.Autoposition;
         end

         // Sheet File Name
         else if (SchSourcePrim.ObjectId = eSheetFileName) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.IsHidden      := SchSourcePrim.IsHidden;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.Justification := SchSourcePrim.Justification;
            SchDestinPrim.TextHorzAnchor:= SchSourcePrim.TextHorzAnchor;
            SchDestinPrim.TextVertAnchor:= SchSourcePrim.TextVertAnchor;
            SchDestinPrim.Autoposition  := SchSourcePrim.Autoposition;
         end

         (* Have no idea what is interface for this
         // C Code Symbol
         else if (SchSourcePrim.ObjectId = ) then
         begin
         end
         *)

         // Note
         else if (SchSourcePrim.ObjectId = eNote) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.TextColor     := SchSourcePrim.TextColor;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.Alignment     := SchSourcePrim.Alignment;
            SchDestinPrim.WordWrap      := SchSourcePrim.WordWrap;
            SchDestinPrim.ClipToRect    := SchSourcePrim.ClipToRect;
         end

         // Brezier
         else if (SchSourcePrim.ObjectId = eBezier) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
         end

         // Image
         else if (SchSourcePrim.ObjectId = eImage) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
            SchDestinPrim.KeepAspect    := SchSourcePrim.KeepAspect;
            // I do not know what property is for show border
         end

         // Pie Chart
         else if (SchSourcePrim.ObjectId = ePie) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
         end

         // Round Rectangle
         else if (SchSourcePrim.ObjectId = eRoundRectangle) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
         end

         // Line
         else if (SchSourcePrim.ObjectId = eLine) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.LineStyle     := SchSourcePrim.LineStyle;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
         end

         // Polyline
         else if (SchSourcePrim.ObjectId = ePolyline) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.LineStyle     := SchSourcePrim.LineStyle;
            SchDestinPrim.StartLineShape:= SchSourcePrim.StartLineShape;
            SchDestinPrim.EndLineShape  := SchSourcePrim.EndLineShape;
            SchDestinPrim.LineShapeSize := SchSourcePrim.LineShapeSize;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
         end

         // Rectangle
         else if (SchSourcePrim.ObjectId = eRectangle) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
            SchDestinPrim.Transparent   := SchSourcePrim.Transparent;
         end

         // Harness Connector
         else if (SchSourcePrim.ObjectId = eHarnessConnector) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
            SchDestinPrim.HarnessConnectorType.IsHidden  := SchSourcePrim.HarnessConnectorType.IsHidden;
         end

         // Signal Harness
         else if (SchSourcePrim.ObjectId = eSignalHarness) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
         end

         // Harness Entry - This "else" is never executed
         else if (SchSourcePrim.ObjectId = eHarnessEntry) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.TextColor     := SchSourcePrim.TextColor;

         end;


         SchServer.RobotManager.SendMessage(SchDestinPrim.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

         SchServer.ProcessControl.PostProcess(SchDoc, '');

         // Get Next Destination Object
         boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose Next Destination Object');

         If Not boolLoc Then
         begin
            // Get New Source Object
            boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose New Source Object');

            If Not boolLoc Then Exit;

            SpatialIterator := SchDoc.SchIterator_Create;
            If SpatialIterator = Nil Then Exit;
            Try
               SpatialIterator.AddFilter_Area(Location.X - 1, Location.Y - 1, Location.X + 1, Location.Y + 1);

               SchSourcePrim := SpatialIterator.FirstSchObject;
            Finally
               SchDoc.SchIterator_Destroy(SpatialIterator);
            End;

            // Get Destination Object
            boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose Destination Object');
            If Not boolLoc Then Exit;
         end;

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
