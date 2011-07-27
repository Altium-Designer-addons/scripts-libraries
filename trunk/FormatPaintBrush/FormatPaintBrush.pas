function FormatPaintBrush;
var
   // SCH variables and objects
   SchDoc : ISCH_Document;

   // PCB variables and objects
   PCBBoard      : IPCB_Board;
   SourcePrim    : IPCB_Primitive;
   DestinPrim    : IPCB_Primitive;
   BoardIterator : IPCB_BoardIterator;
   // Common variables
Begin
   If GetWorkspace.DM_FocusedDocument.DM_DocumentKind = 'SCH' then
   Begin
      // Get the document
      if SchServer = nil then exit;
      SchDoc := SchServer.GetCurrentSchDocument;
      if SchDoc = nil then exit;
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
                 DestinPrim.Layer              := SourcePrim.Layer;
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
                 (*
             End
             else if (SourcePrim.ObjectId = eCoordinateObject) Then
             begin
                // I need to put something here
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
