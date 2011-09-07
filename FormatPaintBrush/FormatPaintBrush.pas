{..............................................................................}
{ Summary   This scripts can be used to copy some formatting properties from   }
{           one (source) primitive to many other (destination) primitives of   }
{           the same type.                                                     }
{                                                                              }
{           Currently script works with all schematic objects (in SCH document }
{           and library) and only some objects (dimension and coordinate) in   }
{           PCB Document                                                       }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
function FormatPaintBrush;
var
   // SCH variables and objects
   SchDoc          : ISCH_Document;
   Location        : TLocation;
   SchSourcePrim   : TObject;
   SchDestinPrim   : TObject;
   SchTempPrim     : TObject;
   SpatialIterator : ISch_Iterator;

   // PCB variables and objects
   PCBBoard        : IPCB_Board;
   SourcePrim      : IPCB_Primitive;
   DestinPrim      : IPCB_Primitive;
   BoardIterator   : IPCB_BoardIterator;
   Layer           : TLayer;

   // Common variables
   ASetOfObjects   : TObjectSet;
   boolLoc         : Integer;
   DocKind         : String;

Begin
   DocKind := GetWorkspace.DM_FocusedDocument.DM_DocumentKind;
   If (DocKind = 'SCH') or (DocKind = 'SCHLIB') then
   Begin
      // Get the document
      if SchServer = nil then exit;
      SchDoc := SchServer.GetCurrentSchDocument;
      if SchDoc = nil then exit;

      ResetParameters;
      AddStringParameter('Action', 'AllOpenDocuments');
      RunProcess('Sch:DeSelect');

      Location := TLocation;
      SchSourcePrim := nil;
      SchDestinPrim := nil;

      While ((SchSourcePrim = nil) or (SchDestinPrim = nil)) do
      Begin

         while SchSourcePrim = nil do
         begin
            // Get Source Object
            boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose Source Object');
            If Not boolLoc Then Exit;

            SpatialIterator := SchDoc.SchIterator_Create;
            If SpatialIterator = Nil Then Exit;
            Try
               SpatialIterator.AddFilter_Area(Location.X - 1, Location.Y - 1, Location.X + 1, Location.Y + 1);
               if (DocKind = 'SCHLIB') then
               begin
                  SpatialIterator.AddFilter_CurrentPartPrimitives;
                  SpatialIterator.AddFilter_CurrentDisplayModePrimitives;
               end;

               SchTempPrim := SpatialIterator.FirstSchObject;


               // If it got hidden parameter move it away
               while ((SchTempPrim <> nil) and (((SchTempPrim.ObjectId = eDesignator) or (SchTempPrim.ObjectId = eParameter)) and SchTempPrim.IsHidden)) do
                  SchTempPrim   := SpatialIterator.NextSchObject;

               SchSourcePrim := SchTempPrim;       


               // here we need to test weather we clicked on Harness Entry, Sheet Entry or C Code Entry
               if SchTempPrim <> nil then
               begin
                  if (SchTempPrim.ObjectId = eHarnessConnector) or (SchTempPrim.ObjectId = eSheetSymbol) or (SchTempPrim.ObjectId = '56') then
                  while SchTempPrim <> nil do
                  begin
                     if (SchTempPrim.ObjectId = eHarnessEntry) or (SchTempPrim.ObjectId = eSheetEntry) or (SchTempPrim.ObjectId = '57') then
                        SchSourcePrim := SchTempPrim;

                     SchTempPrim   := SpatialIterator.NextSchObject;
                  end;
               end;
            Finally
               SchDoc.SchIterator_Destroy(SpatialIterator);
            End;
         end;

         // Get Destination Object

         While SchDestinPrim = nil do
         begin
            boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose Destination Object');
            If Not boolLoc Then
            begin
               SchSourcePrim := nil;
               SchDestinPrim := nil;
               Break;
            end;
            SpatialIterator := SchDoc.SchIterator_Create;
            If SpatialIterator = Nil Then Exit;
            Try
               if (SchSourcePrim.ObjectId = eLabel) or (SchSourcePrim.ObjectId = eDesignator) or (SchSourcePrim.ObjectId = eParameter) then
                  ASetOfObjects := MkSet(eDesignator, eParameter, eLabel)
               else
                  ASetOfObjects := MkSet(SchSourcePrim.ObjectId);

               SpatialIterator.AddFilter_ObjectSet(ASetOfObjects);
               SpatialIterator.AddFilter_Area(Location.X - 1, Location.Y - 1, Location.X + 1, Location.Y + 1);
               if (DocKind = 'SCHLIB') then
               begin
                  SpatialIterator.AddFilter_CurrentPartPrimitives;
                  SpatialIterator.AddFilter_CurrentDisplayModePrimitives;
               end;

               SchDestinPrim := SpatialIterator.FirstSchObject;
               while ((SchDestinPrim <> nil) and (((SchDestinPrim.ObjectId = eDesignator) or (SchDestinPrim.ObjectId = eParameter)) and SchDestinPrim.IsHidden)) do
                  SchDestinPrim   := SpatialIterator.NextSchObject;

            Finally
               SchDoc.SchIterator_Destroy(SpatialIterator);
            End;
         end;
      end;

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
         // Probe - has nothing to copy
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

         // Pin - Only use in SCHLIB
         else if (SchSourcePrim.ObjectId = ePin) and (DocKind = 'SCHLIB') then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.ShowName      := SchSourcePrim.ShowName;
            SchDestinPrim.ShowDesignator:= SchSourcePrim.ShowDesignator;
         end

         // Designator
         else if (SchSourcePrim.ObjectId = eDesignator) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.Justification := SchSourcePrim.Justification;
            if (SchDestinPrim.ObjectId <> eLabel) then
               SchDestinPrim.Autoposition := SchSourcePrim.Autoposition;
         end

         // Parameter
         else if (SchSourcePrim.ObjectId = eParameter) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.Justification := SchSourcePrim.Justification;
            if (SchDestinPrim.ObjectId <> eLabel) then
               SchDestinPrim.Autoposition := SchSourcePrim.Autoposition;
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
            if (SchDestinPrim.ObjectId = eLabel) then
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

         // Sheet Entry
         else if (SchSourcePrim.ObjectId = eSheetEntry) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.TextColor     := SchSourcePrim.TextColor;
            SchDestinPrim.Style         := SchSourcePrim.Style;
            SchDestinPrim.HarnessColor  := SchSourcePrim.HarnessColor;
         end

         // C Code Symbol
         else if (SchSourcePrim.ObjectId = '56') then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
         end

         // C Code Entry
         else if (SchSourcePrim.ObjectId = '57') then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.TextColor     := SchSourcePrim.TextColor;
            SchDestinPrim.Style         := SchSourcePrim.Style;
            SchDestinPrim.HarnessColor  := SchSourcePrim.HarnessColor;
         end

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
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
         end

         // Image
         else if (SchSourcePrim.ObjectId = eImage) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
            SchDestinPrim.KeepAspect    := SchSourcePrim.KeepAspect;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
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

         // Harness Entry
         else if (SchSourcePrim.ObjectId = eHarnessEntry) then
         begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            // SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.TextColor     := SchSourcePrim.TextColor;
         end;

         SchServer.RobotManager.SendMessage(SchDestinPrim.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

         SchServer.ProcessControl.PostProcess(SchDoc, '');

         // Get Next Destination Object
         SchDestinPrim := nil;

         While ((SchSourcePrim = nil) or (SchDestinPrim = nil)) do
         Begin
            SchDestinPrim := nil;

            While SchDestinPrim = nil do
            begin
               // Get Object
               boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose Destination Object');
               If Not boolLoc Then
               begin
                  SchSourcePrim := nil;
                  SchDestinPrim := nil;
                  Break;
               end;

               SpatialIterator := SchDoc.SchIterator_Create;
               If SpatialIterator = Nil Then Exit;
               Try
                  if (SchSourcePrim.ObjectId = eLabel) or (SchSourcePrim.ObjectId = eDesignator) or (SchSourcePrim.ObjectId = eParameter) then
                     ASetOfObjects := MkSet(eDesignator, eParameter, eLabel)
                  else
                     ASetOfObjects := MkSet(SchSourcePrim.ObjectId);

                  SpatialIterator.AddFilter_ObjectSet(ASetOfObjects);
                  SpatialIterator.AddFilter_Area(Location.X - 1, Location.Y - 1, Location.X + 1, Location.Y + 1);
                  if (DocKind = 'SCHLIB') then
                  begin
                     SpatialIterator.AddFilter_CurrentPartPrimitives;
                     SpatialIterator.AddFilter_CurrentDisplayModePrimitives;
                  end;

                  SchDestinPrim := SpatialIterator.FirstSchObject;

                  while ((SchDestinPrim <> nil) and (((SchDestinPrim.ObjectId = eDesignator) or (SchDestinPrim.ObjectId = eParameter)) and SchDestinPrim.IsHidden)) do
                     SchDestinPrim   := SpatialIterator.NextSchObject;

               Finally
                  SchDoc.SchIterator_Destroy(SpatialIterator);
               End;
            end;

            while SchSourcePrim = nil do
            begin
               boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose Source Object');
               If Not boolLoc Then Exit;

               SpatialIterator := SchDoc.SchIterator_Create;
               If SpatialIterator = Nil Then Exit;

               Try
                  SpatialIterator.AddFilter_Area(Location.X - 1, Location.Y - 1, Location.X + 1, Location.Y + 1);
                  if (DocKind = 'SCHLIB') then
                  begin
                     SpatialIterator.AddFilter_CurrentPartPrimitives;
                     SpatialIterator.AddFilter_CurrentDisplayModePrimitives;
                  end;

                  SchTempPrim := SpatialIterator.FirstSchObject;

                  // If it got hidden parameter move it away
                  while ((SchTempPrim <> nil) and (((SchTempPrim.ObjectId = eDesignator) or (SchTempPrim.ObjectId = eParameter)) and SchTempPrim.IsHidden)) do
                     SchTempPrim   := SpatialIterator.NextSchObject;

                  SchSourcePrim := SchTempPrim;

                  // here we need to test weather we clicked on Harness Entry, Sheet Entry or C Code Entry
                  if SchTempPrim <> nil then
                  begin
                     if (SchTempPrim.ObjectId = eHarnessConnector) or (SchTempPrim.ObjectId = eSheetSymbol) or (SchTempPrim.ObjectId = '56') then
                     while SchTempPrim <> nil do
                     begin
                        if (((SchTempPrim.ObjectId = eDesignator) or (SchTempPrim.ObjectId = eParameter)) and not SchTempPrim.IsHidden) or
                        (SchTempPrim.ObjectId = eHarnessEntry) or (SchTempPrim.ObjectId = eSheetEntry) or (SchTempPrim.ObjectId = '57')  then
                           SchSourcePrim := SchTempPrim;

                        SchTempPrim   := SpatialIterator.NextSchObject;
                     end;
                  end;
               Finally
                  SchDoc.SchIterator_Destroy(SpatialIterator);
               End;
            end;
         end;
      end;

   End
   Else If DocKind = 'PCB' then
   Begin
      // Get the document
      If PCBServer = Nil Then Exit;
      PCBBoard := PCBServer.GetCurrentPCBBoard;
      If PCBBoard = Nil Then Exit;

      // Make it work for Pads, Vias, Strings, Polygons, Dimensions and coordinates
      ASetOfObjects := MkSet((*ePadObject, eViaObject,*) eTextObject, ePolyObject, eDimensionObject, eCoordinateObject);

      SourcePrim := nil;
      DestinPrim := nil;

      While (DestinPrim = nil) or (SourcePrim = nil) Do
      Begin
         SourcePrim := PCBBoard.GetObjectAtCursor(ASetOfObjects, AllLayers,'Choose Source Primitive');
         if SourcePrim = nil then exit
         else
         begin
            if SourcePrim.ObjectId = ePadObject then
               boolLoc := MessageDlg('Do you want to copy layer info (SMD/Thru)', mtConfirmation, mbYesNo, 0);
            if SourcePrim.ObjectId = eViaObject then
               boolLoc := MessageDlg('Do you want to copy StartLayer/StopLayer info', mtConfirmation, mbYesNo, 0);
         end;

         DestinPrim := PCBBoard.GetObjectAtCursor(MkSet(SourcePrim.ObjectId), AllLayers,'Choose Destination Primitive');
         if DestinPrim = nil then SourcePrim := nil;
      end;

      While Assigned(DestinPrim) And Assigned(SourcePrim) Do
      Begin
         // copy formatting of PCB dimension
         PCBServer.PreProcess;
         Try
             // Always use IPCB_Primitive.BeginModify instead of PCBServer.SendMessageToRobots because is deprecated
             DestinPrim.BeginModify;

             // Pads - do this only if they both are through-hole or not
             if (SourcePrim.ObjectId = ePadObject) and (not SourcePrim.InComponent) and (not DestinPrim.InComponent) then
             begin
                if boolLoc = mrYes then
                   DestinPrim.Layer             := SourcePrim.Layer;

                DestinPrim.Mode                 := SourcePrim.Mode;
                DestinPrim.TopYSize             := SourcePrim.TopYSize;
                DestinPrim.TopXSize             := SourcePrim.TopXSize;
                DestinPrim.TopShape             := SourcePrim.TopShape;
                DestinPrim.Cache.SolderMaskExpansionValid  := SourcePrim.Cache.SolderMaskExpansionValid;
                DestinPrim.Cache.SolderMaskExpansion       := SourcePrim.Cache.SolderMaskExpansion;
                DestinPrim.Cache.PasteMaskExpansionValid   := SourcePrim.Cache.PasteMaskExpansionValid;
                DestinPrim.Cache.PasteMaskExpansion        := SourcePrim.Cache.PasteMaskExpansion;
                DestinPrim.IsTenting            := SourcePrim.IsTenting;
                DestinPrim.IsTenting_Top        := SourcePrim.IsTenting_Top;
                DestinPrim.IsTenting_Bottom     := SourcePrim.IsTenting_Bottom;
                DestinPrim.HoleWidth            := SourcePrim.HoleWidth;
                DestinPrim.HoleType             := SourcePrim.HoleType;
                DestinPrim.HoleRotation         := SourcePrim.HoleRotation;
                DestinPrim.HoleSize             := SourcePrim.HoleSize;
                DestinPrim.MidYSize             := SourcePrim.MidYSize;
                DestinPrim.MidXSize             := SourcePrim.MidXSize;
                DestinPrim.MidShape             := SourcePrim.MidShape;
                DestinPrim.BotYSize             := SourcePrim.BotYSize;
                DestinPrim.BotXSize             := SourcePrim.BotXSize;
                DestinPrim.BotShape             := SourcePrim.BotShape;

                for Layer := eTopLayer to eBottomLayer Do
                begin
                   DestinPrim.StackShapeOnLayer[Layer]     := SourcePrim.XStackSizeOnLayer[Layer];
                   DestinPrim.StackCRPctOnLayer[Layer]     := SourcePrim.XStackSizeOnLayer[Layer];

                   DestinPrim.XStackSizeOnLayer[Layer]     := SourcePrim.XStackSizeOnLayer[Layer];
                   DestinPrim.YStackSizeOnLayer[Layer]     := SourcePrim.YStackSizeOnLayer[Layer];

                   DestinPrim.XPadOffset[Layer]            := SourcePrim.XPadOffset[Layer];
                   DestinPrim.YPadOffset[Layer]            := SourcePrim.YPadOffset[Layer];

                end;
                DestinPrim.GraphicallyInvalidate;
             end

             // Vias
             else if (SourcePrim.ObjectId = eViaObject) then
             begin
                DestinPrim.Mode                 := SourcePrim.Mode;
                DestinPrim.HoleSize             := SourcePrim.HoleSize;
                DestinPrim.Cache.SolderMaskExpansionValid  := SourcePrim.Cache.SolderMaskExpansionValid;
                DestinPrim.Cache.SolderMaskExpansion       := SourcePrim.Cache.SolderMaskExpansion;

                DestinPrim.Size                 := SourcePrim.Size;

                if boolLoc = mrYes then
                begin
                   DestinPrim.HighLayer         := SourcePrim.HighLayer;
                   DestinPrim.LowLayer          := SourcePrim.LowLayer;
                end;


                DestinPrim.IsTenting_Top        := SourcePrim.IsTenting_Top;
                DestinPrim.IsTenting_Bottom     := SourcePrim.IsTenting_Bottom;
                DestinPrim.IsTenting            := SourcePrim.IsTenting;


                for Layer := eTopLayer to eBottomLayer Do
                begin
                   DestinPrim.StackSizeOnLayer[Layer]      := SourcePrim.StackSizeOnLayer[Layer];
                   DestinPrim.SizeOnLayer[Layer]           := SourcePrim.SizeOnLayer[Layer];
                end;
                DestinPrim.GraphicallyInvalidate;
             end

             // Strings
             else if (SourcePrim.ObjectId = eTextObject) then
             begin
                DestinPrim.Width                := SourcePrim.Width;
                DestinPrim.UseTTFonts           := SourcePrim.UseTTFonts;
                DestinPrim.UseInvertedRectangle := SourcePrim.UseInvertedRectangle;
                DestinPrim.TTFTextWidth         := SourcePrim.TTFTextWidth;
                DestinPrim.TTFTextHeight        := SourcePrim.TTFTextHeight;
                DestinPrim.TTFOffsetFromInvertedRect       := SourcePrim.TTFOffsetFromInvertedRect;
                DestinPrim.TTFInvertedTextJustify          := SourcePrim.TTFInvertedTextJustify;
                DestinPrim.TextKind             := SourcePrim.TextKind;
                DestinPrim.Size                 := SourcePrim.Size;
                DestinPrim.Italic               := SourcePrim.Italic;
                DestinPrim.InvRectWidth         := SourcePrim.InvRectWidth;
                DestinPrim.InvRectHeight        := SourcePrim.InvRectHeight;
                DestinPrim.InvertedTTTextBorder := SourcePrim.InvertedTTTextBorder;
                DestinPrim.Inverted             := SourcePrim.Inverted;
                DestinPrim.FontName             := SourcePrim.FontName;
                DestinPrim.FontID               := SourcePrim.FontID;
                DestinPrim.Bold                 := SourcePrim.Bold;
                DestinPrim.BarCodeYMargin       := SourcePrim.BarCodeYMargin;
                DestinPrim.BarCodeXMargin       := SourcePrim.BarCodeXMargin;
                DestinPrim.BarCodeShowText      := SourcePrim.BarCodeShowText;
                DestinPrim.BarCodeRenderMode    := SourcePrim.BarCodeRenderMode;
                DestinPrim.BarCodeMinWidth      := SourcePrim.BarCodeMinWidth;
                DestinPrim.BarCodeKind          := SourcePrim.BarCodeKind;
                DestinPrim.BarCodeInverted      := SourcePrim.BarCodeInverted;
                DestinPrim.BarCodeFullWidth     := SourcePrim.BarCodeFullWidth;
                DestinPrim.BarCodeFullHeight    := SourcePrim.BarCodeFullHeight;
                DestinPrim.BarCodeFontName      := SourcePrim.BarCodeFontName;
                DestinPrim.GraphicallyInvalidate;
             end

             // Polygons
             else if (SourcePrim.ObjectId = ePolyObject) then
             begin
                DestinPrim.PolyHatchStyle       := SourcePrim.PolyHatchStyle;
                DestinPrim.PolygonType          := SourcePrim.PolygonType;
                DestinPrim.IgnoreViolations     := SourcePrim.IgnoreViolations;
                DestinPrim.PrimitiveLock        := SourcePrim.PrimitiveLock;
                DestinPrim.MinTrack             := SourcePrim.MinTrack;
                DestinPrim.PourOver             := SourcePrim.PourOver;
                DestinPrim.UseOctagons          := SourcePrim.UseOctagons;
                DestinPrim.RemoveNarrowNecks    := SourcePrim.RemoveNarrowNecks;
                DestinPrim.RemoveIslandsByArea  := SourcePrim.RemoveIslandsByArea;
                DestinPrim.RemoveDead           := SourcePrim.RemoveDead;
                DestinPrim.NeckWidthThreshold   := SourcePrim.NeckWidthThreshold;
                DestinPrim.IslandAreaThreshold  := SourcePrim.IslandAreaThreshold;
                DestinPrim.Grid                 := SourcePrim.Grid;
                DestinPrim.TrackSize            := SourcePrim.TrackSize;
                DestinPrim.ArcApproximation     := SourcePrim.ArcApproximation;
                // DestinPrim.Rebuild;
                // DestinPrim.GraphicallyInvalidate;
             end

             // Dimensions
             else if (SourcePrim.ObjectId = eDimensionObject) Then
             Begin
                 DestinPrim.ArrowLength         := SourcePrim.ArrowLength;
                 DestinPrim.ArrowLineWidth      := SourcePrim.ArrowLineWidth;
                 DestinPrim.ArrowSize           := SourcePrim.ArrowSize;
                 DestinPrim.ArrowPosition       := SourcePrim.ArrowPosition;
                 DestinPrim.LineWidth           := SourcePrim.LineWidth;

                 DestinPrim.TextHeight          := SourcePrim.TextHeight;
                 DestinPrim.TextWidth           := SourcePrim.TextWidth;
                 DestinPrim.TextFont            := SourcePrim.TextFont;
                 DestinPrim.TextLineWidth       := SourcePrim.TextLineWidth;
                 DestinPrim.TextGap             := SourcePrim.TextGap;
                 DestinPrim.TextFormat          := SourcePrim.TextFormat;
                 DestinPrim.TextDimensionUnit   := SourcePrim.TextDimensionUnit;
                 DestinPrim.TextPrecision       := SourcePrim.TextPrecision;
                 DestinPrim.TextPosition        := SourcePrim.TextPosition;
                 DestinPrim.TextPrefix          := SourcePrim.TextPrefix;
                 DestinPrim.TextSuffix          := SourcePrim.TextSuffix;
                 DestinPrim.TextValue           := SourcePrim.TextValue;
                 DestinPrim.ExtensionOffset     := SourcePrim.ExtensionOffset;
                 DestinPrim.ExtensionLineWidth  := SourcePrim.ExtensionLineWidth;
                 DestinPrim.ExtensionPickGap    := SourcePrim.ExtensionPickGap;
                 DestinPrim.Style               := SourcePrim.Style;
                 DestinPrim.UseTTFonts          := SourcePrim.UseTTFonts;
                 DestinPrim.Bold                := SourcePrim.Bold;
                 DestinPrim.Italic              := SourcePrim.Italic;
                 DestinPrim.FontName            := SourcePrim.FontName;
                 DestinPrim.Size                := SourcePrim.Size;

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

             // Coordinates
             else if (SourcePrim.ObjectId = eCoordinateObject) Then
             begin
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
             end;

             // Always use IPCB_Primitive.EndModify / CancelModify instead of PCBServer.SendMessageToRobots because is deprecated
             DestinPrim.EndModify;
             PCBBoard.ViewManager_FullUpdate;
         Finally
             PCBServer.PostProcess;
         End;

         DestinPrim := nil;

         // Get next PCB Object
         While (DestinPrim = nil) or (SourcePrim = nil) Do
         Begin
            DestinPrim := PCBBoard.GetObjectAtCursor(MkSet(SourcePrim.ObjectId), AllLayers,'Choose Destination Primitive');
            if DestinPrim = nil then SourcePrim := nil;

            if SourcePrim = nil then
               SourcePrim := PCBBoard.GetObjectAtCursor(ASetOfObjects, AllLayers,'Choose Source Primitive');
            if SourcePrim = nil then exit;
         end;
      End;
   End;
End;
