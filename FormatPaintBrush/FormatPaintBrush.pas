function FormatPaintBrush;
var
   // SCH variables and objects
   SchDoc : ISCH_Document;

   // PCB variables and objects
   PCBBoard   : IPCB_Board;
   SourcePrim : IPCB_Primitive;
   DestinPrim : IPCB_Primitive;         


   // Common variables
begin
   if GetWorkspace.DM_FocusedDocument.DM_DocumentKind = 'SCH' then
   begin
      // Get the document
      if SchServer = nil then exit;
      SchDoc := SchServer.GetCurrentSchDocument;
      if SchDoc = nil then exit;




   end
   else if GetWorkspace.DM_FocusedDocument.DM_DocumentKind = 'PCB' then
   begin
      // Get the document
      if PCBServer = nil then exit;
      PCBBoard := PCBServer.GetCurrentPCBBoard;
      if PCBBoard = nil then exit;

      ResetParameters;
      AddStringParameter('Scope','All');
      RunProcess('PCB:DeSelect');


      SourcePrim := PCBBoard.GetObjectAtCursor(AllObjects, AllLayers,'Choose Source Primitive');
      DestinPrim := PCBBoard.GetObjectAtCursor(MkSet(SourcePrim.ObjectId), AllLayers,'Choose Destination Primitive');

      PCBServer.PreProcess;

      While Assigned(DestinPrim) and Assigned(SourcePrim) do
      begin
         // copy formatting of PCB dimension

         PCBServer.SendMessageToRobots(DestinPrim.I_ObjectAddress ,c_Broadcast, PCBM_BeginModify , c_NoEventData);
         if (SourcePrim.DimensionKind = eLinearDimension) then
         begin
            DestinPrim.ArrowLength        := SourcePrim.ArrowLength;
            DestinPrim.ArrowLineWidth     := SourcePrim.ArrowLineWidth;
            DestinPrim.ArrowSize          := SourcePrim.ArrowSize;
            DestinPrim.LineWidth          := SourcePrim.LineWidth;
            DestinPrim.TextHeight         := SourcePrim.TextHeight;
            DestinPrim.TextWidth          := SourcePrim.TextWidth;
            DestinPrim.TextFont           := SourcePrim.TextFont;
            DestinPrim.TextLineWidth      := SourcePrim.TextLineWidth;
            DestinPrim.TextGap            := SourcePrim.TextGap;
            DestinPrim.TextFormat         := SourcePrim.TextFormat;
            DestinPrim.TextDimensionUnit  := SourcePrim.TextDimensionUnit;
            DestinPrim.TextPrecision      := SourcePrim.TextPrecision;
            DestinPrim.ExtensionOffset    := SourcePrim.ExtensionOffset;
            DestinPrim.ExtensionLineWidth := SourcePrim.ExtensionLineWidth;
            DestinPrim.ExtensionPickGap   := SourcePrim.ExtensionPickGap;
            DestinPrim.Style              := SourcePrim.Style;
            DestinPrim.UseTTFonts         := SourcePrim.UseTTFonts;
            DestinPrim.Bold               := SourcePrim.Bold;
            DestinPrim.Italic             := SourcePrim.Italic;
            DestinPrim.FontName           := SourcePrim.FontName;
            DestinPrim.Selected           := True;
         end;


         PCBServer.SendMessageToRobots(DestinPrim.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
         PCBServer.SendMessageToRobots(PCBBoard.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, DestinPrim.I_ObjectAddress);

         PCBBoard.ViewManager_FullUpdate;

         PCBServer.PostProcess;



         ResetParameters;
         AddStringParameter('Action','Redraw');
         RunProcess('PCB:Zoom');

         DestinPrim := PCBBoard.GetObjectAtCursor(MkSet(SourcePrim.ObjectId), AllLayers,'Choose Next Destination Primitive');
      end;
   end;
end;
