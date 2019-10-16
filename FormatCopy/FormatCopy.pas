{
 FormatCopy.pas
 Summary   Used to copy some formatting properties from one (source) primitive
           to other (destination) primitive(s) of the same or similar type.

           Works with all schematic objects (in SCH document
           and library) and some objects in PCB Document.
           Cross object format copying is possible in SchDoc simple text

 From FormatPaintbrush.pas
 Created by : Petar Perisin
 Modified   : B. Miller
08/08/2019 v0.1 fn() nGetObjectAtCursor from ModifyFootprintTracks.pas, handles layers beyond eMech16
09/08/2019 v0.2 Refactored "repeat" loops
10/08/2019 v0.3 Via & Pad format copying implemented/fixed.
           v0.4 Recoded Sch obj selector with "sub" object priority/bias
15/08/2019 v0.5 Recoded PCB obj pick with current layer & obj bias.
19/08/2019 v0.6 De-select all Sch objects, *fix* PCB layer bias

tbd :  *fix* Current layer bias is a partial soln. Only works up to eMech16

}
{..............................................................................}

{ AllLayer = [MinLayer..eConnectLayer] , Set of TLayer
  MinLayer = eTopLayer;
  MaxLayer = eViaHoleLayer;              // 82 Mechanical 26 Via Holes
  eConnectLayer = eMechanical16 + 3;     // 75 Mechanical 19 Connections
  MaxMechanicalLayer = eMechanical16;

  eMech17 = 67108881   returned by AD17 Obj.Layer; but crashes GetObjectAtCursor()
  eMech32 = 67108896   so can't use to make new layer objset
}
const
    cNeverAsk = false;    // set true for no layer prompt (default copy layer yes)
    cESC      = -1;

    eMech17 = 73;         // potentially correct emun values
    eMech32 = 88;         // but still useless as can't get layer num returned

var
    // Common variables
    ASetOfObjects   : TObjectSet;
    TextObjSet      : TObjectSet;
    boolLoc         : Integer;
    DocKind         : String;
    Prompt          : WideString;

    NewAllLayers    : TObjectSet;     // does NOT solve problem with GetObjectAtCursor()

{..............................................................................}

function nGetObjectAtCursor(Board : TPCB_Board, const ObjectSet: TObjectSet, const LayerSet : TObjectSet, msg : TString) : IPCB_Primitive;
var
    x, y     : TCoord;
    Iterator : IPCB_SpatialIterator;
    Prim     : IPCB_Primitive;
    Prev     : IPCB_Primitive;
    Area     : double;
    CLayer   : TLayer;
    MechLayer   : IPCB_MechanicalLayer;

begin
    Result := eNoObject;

    if Board.ChooseLocation(x,y,msg) then  // false = ESC Key is pressed
    begin
// layer can be changed during ChooseLocation fn UI!
        CLayer := Board.CurrentLayer;
//        CLayer := BoardCurrentLayer(Board);

        Iterator := Board.SpatialIterator_Create;
        Iterator.SetState_FilterAll;
        Iterator.AddFilter_AllLayers;
        Iterator.AddFilter_ObjectSet(ObjectSet);
        Iterator.AddFilter_Area (x - 10, y + 10, x + 10, y - 10);   //TCoord
        Prim := Iterator.FirstPCBObject;
        while (Prim <> Nil) do
        begin
            if Board.LayerIsDisplayed(Prim.Layer) then   // filter on visible layers
         //   if InSet(Prim.Layer, LayerSet) then        // can not use as actual layer numbers eM17-eM32 crash set assignment!
            begin
                // need to exclude board region
                if not ((Prim.Layer = eMultiLayer) and (Prim.ObjectID = eRegionObject)) then  // eBoardObject
                begin
                    if Result <> eNoObject then
                    begin
                      // prioritise small area & sub objects & on current layer.
                      //  if PrimArea(Result) > PrimArea(Prim) then Result := Prim;
                        if (Prev.ObjectID = ePolyObject) and (Prim.ObjectID <> ePolyObject)     then Result := Prim;
                        if (Prev.ObjectID = eRegionObject) and (Prim.ObjectID <> eRegionObject) then Result := Prim;
                        if ((Prev.ObjectID = eRegionObject) or (Prev.ObjectID = ePolyObject)) and
                            (Prev.Layer <> CLayer)                         then Result := Prim;
                        if (Prev.ObjectID = eTextObject) and Prev.IsHidden then Result := Prim;
                        if Prim.Layer = CLayer                             then Result := Prim;
                    end
                    else Result := Prim;
                    Prev := Result;
                end;
            end;
            Prim := Iterator.NextPCBObject;
        end;
        Board.SpatialIterator_Destroy(Iterator);
    end
    else
        Result := cESC;
end;

Function ObjectIDToString ( I : Integer) : WideString;
begin
// Sch builtin names don't match with ObjectID String fn cContextHelpStringsByObjectId(I);
    case I of
        eNoObject            : Result := 'No Object';
        eSchComponent        : Result := 'Component';
        eDesignator          : Result := 'Designator';
        ePin                 : Result := 'Pin';
        eParameter           : Result := 'Parameter';
        eLine                : Result := 'Line';
        eWire                : Result := 'Wire';
        eJunction            : Result := 'Junction';
        eLabel               : Result := 'Label';
        eNetLabel            : Result := 'Net Label';
        ePort                : Result := 'Port';
        eSheetSymbol         : Result := 'Sheet Symbol';
        eSheetEntry          : Result := 'Sheet Entry';
        eHarnessConnector    : Result := 'Harness';
        eHarnessEntry        : Result := 'Harness Entry';
        eBusEntry            : Result := 'Bus Entry';     // Sch_Line
        ePowerObject         : Result := 'Power Object';
        eCrossSheetConnector : Result := 'Cross-Sheet Connector';
    else
        Result := 'unk';
    end;
end;

procedure ProcessSCHPrim(SchSourcePrim : ISch_Object, SchDestinPrim : ISch_Object, DocKind : WideString);
begin
    case SchSourcePrim.ObjectId of
    eBus :
        begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
            SchDestinPrim.LineWidth := SchSourcePrim.LineWidth;
        end;

    eBusEntry :
        begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
            SchDestinPrim.LineWidth := SchSourcePrim.LineWidth;
        end;

    eWire :
        begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
            SchDestinPrim.LineWidth := SchSourcePrim.LineWidth;
        end;
    
    eNetLabel :
        begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
            SchDestinPrim.FontId    := SchSourcePrim.FontId;
        end;

         (*
    // Probe - has nothing to copy
    eProbe :
         begin

         end;
         *)
    
    // NoERC Marker
    eNoERC :
        begin
            SchDestinPrim.Color        := SchSourcePrim.Color;
            SchDestinPrim.Orientation  := SchSourcePrim.Orientation;
            SchDestinPrim.Symbol       := SchSourcePrim.Symbol;
            SchDestinPrim.IsActive     := SchSourcePrim.IsActive;
            SchDestinPrim.SuppressAll  := SchSourcePrim.SuppressAll;
            //SchDestinPrim.CONNECTIONPAIRSTOSUPPRESS     := SchSourcePrim.CONNECTIONPAIRSTOSUPPRESS;

        end;
    
    ePort :
        begin
            SchDestinPrim.Color       := SchSourcePrim.Color;
            SchDestinPrim.TextColor   := SchSourcePrim.TextColor;
            SchDestinPrim.AreaColor   := SchSourcePrim.AreaColor;
            SchDestinPrim.Alignment   := SchSourcePrim.Alignment;
            SchDestinPrim.Style       := SchSourcePrim.Style;
            SchDestinPrim.FontId      := SchSourcePrim.FontId;
            SchDestinPrim.IOType      := SchSourcePrim.IOType;
            SchDestinPrim.Height      := SchSourcePrim.Height;
            SchDestinPrim.Width       := SchSourcePrim.Width;
            SchDestinPrim.BorderWidth := SchSourcePrim.BorderWidth;
        end;

    // Off-Sheet Connector
    eCrossSheetConnector :
        begin
            SchDestinPrim.FontID          := SchSourcePrim.FontID;
            SchDestinPrim.Color           := SchSourcePrim.Color;
            SchDestinPrim.CrossSheetStyle := SchSourcePrim.CrossSheetStyle;
        end;

    // Part
    eSchComponent :
        begin
            SchDestinPrim.SetState_DisplayMode        (SchSourcePrim.DisplayMode);
            SchDestinPrim.SetState_IsMirrored         (SchSourcePrim.IsMirrored);
            SchDestinPrim.SetState_ComponentKind      (SchSourcePrim.ComponentKind);
            SchDestinPrim.SetState_ShowHiddenFields   (SchSourcePrim.ShowHiddenFields);
            SchDestinPrim.SetState_ShowHiddenPins     (SchSourcePrim.ShowHiddenPins);
            SchDestinPrim.Designator.SetState_ShowName(SchSourcePrim.Designator.ShowName);
            SchDestinPrim.Comment.SetState_ShowName   (SchSourcePrim.Comment.ShowName);
            SchDestinPrim.SetState_Description        (SchSourcePrim.SetState_Description);
            SchDestinPrim.SetState_OverideColors      (SchSourcePrim.OverideColors);
            if SchDestinPrim.OverideColors = True then
            begin
               SchDestinPrim.Color            := SchSourcePrim.Color;
               SchDestinPrim.AreaColor        := SchSourcePrim.AreaColor;
               SchDestinPrim.PinColor         := SchSourcePrim.PinColor;
            end;
        end;

    // Pin - Only use in SCHLIB
    ePin :
        if (DocKind = cDocKind_SchLib) then
        begin
            SchDestinPrim.Color          := SchSourcePrim.Color;
            SchDestinPrim.ShowName       := SchSourcePrim.ShowName;
            SchDestinPrim.ShowDesignator := SchSourcePrim.ShowDesignator;
        end;

    eDesignator :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.Justification := SchSourcePrim.Justification;
            if (SchDestinPrim.ObjectId <> eLabel) then
               SchDestinPrim.Autoposition := SchSourcePrim.Autoposition;
        end;

    eParameter :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.Justification := SchSourcePrim.Justification;
            if (SchDestinPrim.ObjectId <> eLabel) then
               SchDestinPrim.Autoposition := SchSourcePrim.Autoposition;
        end;

    eParameterSet :
        begin
            SchDestinPrim.Color     := SchSourcePrim.Color;
            SchDestinPrim.Style     := SchSourcePrim.Style;
        end;
    
    eTextFrame :
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
        end;

    // Text String (Annotation, Label)
    eLabel :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.Justification := SchSourcePrim.Justification;
            if (SchDestinPrim.ObjectId = eLabel) then
               SchDestinPrim.IsMirrored    := SchSourcePrim.IsMirrored;
        end;

    eEllipse :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.Transparent   := SchSourcePrim.Transparent;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
        end;
    
    eEllipticalArc :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
        end;

    eArc :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
        end;

    // Power Port
    ePowerObject :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.Style         := SchSourcePrim.Style;
            SchDestinPrim.ShowNetName   := SchSourcePrim.ShowNetName;
        end;

    ePolygon :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.Transparent   := SchSourcePrim.Transparent;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
        end;

    eSheetSymbol :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
        end;

    eSheetName :
        begin
            SchDestinPrim.Color          := SchSourcePrim.Color;
            SchDestinPrim.IsHidden       := SchSourcePrim.IsHidden;
            SchDestinPrim.FontID         := SchSourcePrim.FontID;
            SchDestinPrim.Justification  := SchSourcePrim.Justification;
            SchDestinPrim.TextHorzAnchor := SchSourcePrim.TextHorzAnchor;
            SchDestinPrim.TextVertAnchor := SchSourcePrim.TextVertAnchor;
            SchDestinPrim.Autoposition   := SchSourcePrim.Autoposition;
        end;

    eSheetFileName :
        begin
            SchDestinPrim.Color          := SchSourcePrim.Color;
            SchDestinPrim.IsHidden       := SchSourcePrim.IsHidden;
            SchDestinPrim.FontID         := SchSourcePrim.FontID;
            SchDestinPrim.Justification  := SchSourcePrim.Justification;
            SchDestinPrim.TextHorzAnchor := SchSourcePrim.TextHorzAnchor;
            SchDestinPrim.TextVertAnchor := SchSourcePrim.TextVertAnchor;
            SchDestinPrim.Autoposition   := SchSourcePrim.Autoposition;
        end;

    eSheetEntry :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.Style         := SchSourcePrim.Style;
            SchDestinPrim.ArrowKind     := SchSourcePrim.ArrowKind;
            SchDestinPrim.HarnessColor  := SchSourcePrim.HarnessColor;
            SchDestinPrim.TextFontID    := SchSourcePrim.TextFontID;
            SchDestinPrim.TextColor     := SchSourcePrim.TextColor;
            SchDestinPrim.TextStyle     := SchSourcePrim.TextStyle;
        end;

    // C Code Symbol
    56 :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
        end;

    // C Code Entry
    57 :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.TextColor     := SchSourcePrim.TextColor;
            SchDestinPrim.Style         := SchSourcePrim.Style;
            SchDestinPrim.HarnessColor  := SchSourcePrim.HarnessColor;
        end;

    eNote :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.TextColor     := SchSourcePrim.TextColor;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.FontID        := SchSourcePrim.FontID;
            SchDestinPrim.Alignment     := SchSourcePrim.Alignment;
            SchDestinPrim.WordWrap      := SchSourcePrim.WordWrap;
            SchDestinPrim.ClipToRect    := SchSourcePrim.ClipToRect;
            SchDestinPrim.Collapsed     := SchSourcePrim.Collapsed;
            SchDestinPrim.Author        := SchSourcePrim.Author;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
        end;
    
    eCompileMask :
        begin
            SchDestinPrim.Color          := SchSourcePrim.Color;
            SchDestinPrim.AreaColor      := SchSourcePrim.AreaColor;
            SchDestinPrim.Collapsed      := SchSourcePrim.Collapsed;
            SchDestinPrim.LineWidth      := SchSourcePrim.LineWidth;
        end;

   // Blanket 61
    eBlanket :
        begin
            SchDestinPrim.Color          := SchSourcePrim.Color;
            SchDestinPrim.AreaColor      := SchSourcePrim.AreaColor;
            SchDestinPrim.LineStyle      := SchSourcePrim.LineStyle;
            SchDestinPrim.LineWidth      := SchSourcePrim.LineWidth;        //NOT LineStyleExt as in sch ASCII file
            SchDestinPrim.Collapsed      := SchSourcePrim.Collapsed;
        end;

   // Diff Pair 28  ??? 18
    eDifferentialPairObject :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            //SchDestinPrim.FontID        := SchSourcePrim.FontID;
            //SchDestinPrim.IsHidden      := SchSourcePrim.IsHidden;
            //SchDestinPrim.Text          := SchSourcePrim.Text;
        end;

    eBezier :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
        end;

    eImage :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
            SchDestinPrim.KeepAspect    := SchSourcePrim.KeepAspect;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
        end;

    // Pie Chart
    ePie :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
        end;

    eRoundRectangle :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.AreaColor     := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
        end;

    eLine :
        begin
            SchDestinPrim.Color         := SchSourcePrim.Color;
            SchDestinPrim.LineStyle     := SchSourcePrim.LineStyle;
            SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
        end;

    ePolyline :
        begin
            SchDestinPrim.Color          := SchSourcePrim.Color;
            SchDestinPrim.LineStyle      := SchSourcePrim.LineStyle;
            SchDestinPrim.StartLineShape := SchSourcePrim.StartLineShape;
            SchDestinPrim.EndLineShape   := SchSourcePrim.EndLineShape;
            SchDestinPrim.LineShapeSize  := SchSourcePrim.LineShapeSize;
            SchDestinPrim.LineWidth      := SchSourcePrim.LineWidth;
        end;

    eRectangle :
        begin
            SchDestinPrim.Color          := SchSourcePrim.Color;
            SchDestinPrim.AreaColor      := SchSourcePrim.AreaColor;
            SchDestinPrim.IsSolid        := SchSourcePrim.IsSolid;
            SchDestinPrim.LineWidth      := SchSourcePrim.LineWidth;
            SchDestinPrim.Transparent    := SchSourcePrim.Transparent;
        end;
    
    eHarnessConnector :
        begin
            SchDestinPrim.Color          := SchSourcePrim.Color;
            SchDestinPrim.AreaColor      := SchSourcePrim.AreaColor;
            SchDestinPrim.LineWidth      := SchSourcePrim.LineWidth;
            SchDestinPrim.HarnessConnectorType.IsHidden  := SchSourcePrim.HarnessConnectorType.IsHidden;
        end;

    eSignalHarness :
        begin
            SchDestinPrim.Color          := SchSourcePrim.Color;
            SchDestinPrim.LineWidth      := SchSourcePrim.LineWidth;
        end;
    
    eHarnessEntry :
        begin
            SchDestinPrim.Color          := SchSourcePrim.Color;
            SchDestinPrim.TextFontID     := SchSourcePrim.TextFontID;
            SchDestinPrim.TextColor      := SchSourcePrim.TextColor;
        end;
    end; //case
end;

procedure FormatCopySch(Doc : IDocument);
var
   SchDoc          : ISCH_Document;
   Location        : TLocation;
   SchSourcePrim   : ISch_Object;
   SchDestinPrim   : ISch_Object;
   SchTempPrim     : TObject;
   SpatialIterator : ISch_Iterator;
   found           : boolean;

begin
    // Get the focused (loaded & open)document; Server must already be running
    SchDoc := SchServer.GetCurrentSchDocument;
    if SchDoc = nil then exit;

//    ResetParameters;
//    AddStringParameter('Action', 'AllOpenDocuments');
//    RunProcess('Sch:DeSelect');
    Client.SendMessage('SCH:DeSelect', 'Action=AllOpenDocuments', 255, Client.CurrentView);

    Location := TLocation;
    SchSourcePrim := nil;
    SchDestinPrim := nil;

    repeat

        if (SchSourcePrim <> nil) and (SchDestinPrim <> nil) then
        begin
            SchServer.ProcessControl.PreProcess(SchDoc, '');
            SchServer.RobotManager.SendMessage(SchDestinPrim.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);

            ProcessSCHPrim(SchSourcePrim, SchDestinPrim, DocKind);

            SchServer.RobotManager.SendMessage(SchDestinPrim.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);
            SchServer.ProcessControl.PostProcess(SchDoc, '');
            // Get Next Destination Object
            SchDestinPrim := nil;
        end;

        // Get Destination Object
        if SchSourcePrim <> Nil then
        begin
            Prompt := 'Choose Destination Object';
            ASetOfObjects := MkSet(eDesignator, eParameter, eLabel);    // allow formatting across objects ?
            if not InSet(SchSourcePrim.ObjectId, ASetOfObjects) then
            begin
                ASetOfObjects := MkSet(SchSourcePrim.ObjectId);
                Prompt := Prompt + ' : ' + ObjectIDToString(SchSourcePrim.ObjectId);
            end
            else Prompt := Prompt + ' : ' + ObjectIDToString(eDesignator) + ',' + ObjectIDToString(eParameter) + ',' + ObjectIDToString(eLabel);

            // Get Object
            boolLoc := SchDoc.ChooseLocationInteractively(Location, Prompt);
            If Not boolLoc Then
            begin
                  SchSourcePrim := nil;
                  SchDestinPrim := nil;
            end;

            if SchSourcePrim <> Nil then
            begin
                SpatialIterator := SchDoc.SchIterator_Create;
                Try


                    SpatialIterator.AddFilter_ObjectSet(ASetOfObjects);
                    SpatialIterator.AddFilter_Area(Location.X - 1, Location.Y - 1, Location.X + 1, Location.Y + 1);
                    if (DocKind = cDocKind_SchLib) then
                    begin
                        SpatialIterator.AddFilter_CurrentPartPrimitives;
                        SpatialIterator.AddFilter_CurrentDisplayModePrimitives;
                    end;

                    SchTempPrim := SpatialIterator.FirstSchObject;

                    while (SchTempPrim <> nil) do
                    begin
                        SchDestinPrim := SchTempPrim;
                        if (SchTempPrim.ObjectId = eDesignator) or (SchTempPrim.ObjectId = eParameter) then
                            if not SchTempPrim.IsHidden then break;
                        SchTempPrim   := SpatialIterator.NextSchObject;
                    end;
                Finally
                    SchDoc.SchIterator_Destroy(SpatialIterator);
                End;
            end;
        end;

        if SchSourcePrim = nil then
        begin
            // Get Source Object
            boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose Source Object');
            If Not boolLoc Then continue;

            SpatialIterator := SchDoc.SchIterator_Create;
            Try
                SpatialIterator.AddFilter_Area(Location.X - 2, Location.Y - 2, Location.X + 2, Location.Y + 2);
                if (DocKind = cDocKind_SchLib) then
                begin
                    SpatialIterator.AddFilter_CurrentPartPrimitives;
                    SpatialIterator.AddFilter_CurrentDisplayModePrimitives;
                end;

                found := false;
                SchTempPrim := SpatialIterator.FirstSchObject;

                while (SchTempPrim <> nil) do
                begin
                    if SchSourcePrim <> nil then
                    begin
                        // prioritize unhidden & Harness, Sheet & C Code Entries over parent obj.
                        if (SchSourcePrim.ObjectId = eDesignator) or (SchSourcePrim.ObjectId = eParameter) then
                            if SchSourcePrim.IsHidden                 then SchSourcePrim := SchTempPrim;
                        if SchSourcePrim.ObjectId = eSchComponent     then SchSourcePrim := SchTempPrim;
                        if SchSourcePrim.ObjectId = eHarnessConnector then SchSourcePrim := SchTempPrim;
                        if SchSourcePrim.ObjectId = eSheetSymbol      then SchSourcePrim := SchTempPrim;
                        if SchSourcePrim.ObjectId = 56                then SchSourcePrim := SchTempPrim;
                    end
                    else SchSourcePrim := SchTempPrim;
                    //SchSourcePrim.GetState_DescriptionString;

                    SchTempPrim  := SpatialIterator.NextSchObject;
                end;
            Finally
               SchDoc.SchIterator_Destroy(SpatialIterator);
            End;
        end;

    until ((SchSourcePrim = nil) and (SchDestinPrim = nil));
end;


procedure ProcessPCBPrim(SourcePrim : IPCB_Primitive, DestinPrim : IPCB_Primitive, boolLoc : boolean);
var
    Layer    : TLayer;
    PadCache : TPadCache;
    Pad      : IPCB_Pad;
//    Flag     : Integer;

begin
    // Always use IPCB_Primitive.BeginModify instead of PCBServer.SendMessageToRobots because is deprecated (?? no citation!)
    DestinPrim.BeginModify;

    case SourcePrim.ObjectId of
    // Pads
    ePadObject :
        if (not SourcePrim.InComponent) and (not DestinPrim.InComponent) then
        begin
            Pad := SourcePrim;
            //if boolLoc = mrYes then
            DestinPrim.Mode             := Pad.Mode;   //simple local or full stack
            // all single layer pads must have holesize set (zero) before changing from multilayer
            if DestinPrim.Layer = eMultiLayer then
            begin
                DestinPrim.HoleSize     := Pad.HoleSize;
                DestinPrim.Layer        := Pad.Layer;
            end else
            begin
                DestinPrim.Layer        := Pad.Layer;
                DestinPrim.HoleSize     := Pad.HoleSize;
            end;
            DestinPrim.HoleWidth        := Pad.HoleWidth;
            DestinPrim.HoleType         := Pad.HoleType;
            DestinPrim.HoleRotation     := Pad.HoleRotation;
            DestinPrim.DrillType        := Pad.DrillType;
            DestinPrim.Plated           := Pad.Plated;

            DestinPrim.TopYSize         := Pad.TopYSize;
            DestinPrim.TopXSize         := Pad.TopXSize;
            DestinPrim.TopShape         := Pad.TopShape;
            if Pad.Mode = ePadMode_LocalStack then
            begin
                DestinPrim.MidYSize     := Pad.MidYSize;
                DestinPrim.MidXSize     := Pad.MidXSize;
                DestinPrim.MidShape     := Pad.MidShape;
                DestinPrim.BotYSize     := Pad.BotYSize;
                DestinPrim.BotXSize     := Pad.BotXSize;
                DestinPrim.BotShape     := Pad.BotShape;
            end;
            
            if Pad.Mode <> ePadMode_Simple then
            begin
                for Layer := eTopLayer to eBottomLayer Do
                begin
                    DestinPrim.StackShapeOnLayer(Layer)     := Pad.StackShapeOnLayer(Layer);
                    if Pad.ShapeOnLayer(Layer) = eRectangular then                            // read only property TShape
                        DestinPrim.CornerRadiusOnLayer      := Pad.CornerRadiusOnLayer;

                    if Pad.ShapeOnLayer(Layer) = eRoundedRectangular then  //TShape
                    begin
                        DestinPrim.CRPercentageOnLayer      := Pad.CRPercentageOnLayer;
                        DestinPrim.StackCRPctOnLayer(Layer) := Pad.StackCRPctOnLayer(Layer);  //IPCB_Pad2 interface
                    end;
                    if  Pad.Mode = ePadMode_ExternalStack then
                    begin
                        DestinPrim.XStackSizeOnLayer(Layer) := Pad.XStackSizeOnLayer(Layer);
                        DestinPrim.YStackSizeOnLayer(Layer) := Pad.YStackSizeOnLayer(Layer);
                    end;
                end;
          //      DestinPrim.XPadOffset(Layer)            := Pad.XPadOffset(Layer);  This property is not implemented.
          //      DestinPrim.YPadOffset(Layer)            := Pad.YPadOffset(Layer);  This property is not implemented.
            end;
            DestinPrim.InvalidateSizeShape;
            PadCache                        := Pad.GetState_Cache;
            //DestinPrim.Cache.SolderMaskExpansionValid  := SourcePrim.Cache.SolderMaskExpansionValid;
            //DestinPrim.Cache.SolderMaskExpansion       := SourcePrim.Cache.SolderMaskExpansion;
            //DestinPrim.Cache.PasteMaskExpansionValid   := SourcePrim.Cache.PasteMaskExpansionValid;
            //DestinPrim.Cache.PasteMaskExpansion        := SourcePrim.Cache.PasteMaskExpansion;
            DestinPrim.SetState_Cache       := PadCache;
            DestinPrim.PadCacheRobotFlag;
            DestinPrim.IsTenting            := Pad.IsTenting;
            DestinPrim.IsTenting_Top        := Pad.IsTenting_Top;
            DestinPrim.IsTenting_Bottom     := Pad.IsTenting_Bottom;

            DestinPrim.GraphicallyInvalidate;
        end;

        // Vias
    eViaObject :
        begin
            DestinPrim.Mode                 := SourcePrim.Mode;
            DestinPrim.HoleSize             := SourcePrim.HoleSize;
            DestinPrim.Size                 := SourcePrim.Size;

            if boolLoc = mrYes then
            begin
                if DestinPrim.HighLayer > SourcePrim.HighLayer then
                begin
                    DestinPrim.HighLayer        := SourcePrim.HighLayer;
                    DestinPrim.LowLayer         := SourcePrim.LowLayer;
                end else
                begin
                    DestinPrim.LowLayer         := SourcePrim.LowLayer;
                    DestinPrim.HighLayer        := SourcePrim.HighLayer;
                end;
            end;
            for Layer := SourcePrim.LowLayer to SourcePrim.HighLayer Do
            begin
                DestinPrim.StackSizeOnLayer(Layer)      := SourcePrim.StackSizeOnLayer(Layer);
                DestinPrim.SizeOnLayer(Layer)           := SourcePrim.SizeOnLayer(Layer);
            end;
            PadCache                        := SourcePrim.GetState_Cache;
            //Padcache.ReliefAirGap
            //Padcache.PowerPlaneReliefExpansion
            //Padcache.PowerPlaneClearance
            //Padcache.ReliefConductorWidth
            //Padcache.SolderMaskExpansion
            //Padcache.SolderMaskExpansionValid
            //  DestinPrim.Cache.SolderMaskExpansionValid  := SourcePrim.Cache.SolderMaskExpansionValid;
            //  DestinPrim.Cache.SolderMaskExpansion       := SourcePrim.Cache.SolderMaskExpansion;
            //Padcache.PasteMaskExpansion
            //Padcache.PasteMaskExpansionValid
            DestinPrim.SetState_Cache       := Padcache;
            DestinPrim.PadCacheRobotFlag;
            DestinPrim.IsTenting_Top        := SourcePrim.IsTenting_Top;
            DestinPrim.IsTenting_Bottom     := SourcePrim.IsTenting_Bottom;
            DestinPrim.IsTenting            := SourcePrim.IsTenting;
            DestinPrim.GraphicallyInvalidate;
        end;

    // Strings
    eTextObject :
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


            if boolLoc = mrYes then
                DestinPrim.Layer            := SourcePrim.Layer;
            
            if (SourcePrim.IsDesignator and DestinPrim.IsDesignator) then
            begin
                DestinPrim.Component.Name.BeginModify;
                DestinPrim.Component.ChangeNameAutoposition(SourcePrim.Component.GetState_NameAutoPos);
                DestinPrim.Component.Name.EndModify;
            end;

            if (SourcePrim.IsComment and DestinPrim.IsComment) then
            begin
                DestinPrim.Component.Comment.BeginModify;
                DestinPrim.Component.ChangeCommentAutoposition(SourcePrim.Component.CommentAutoPosition);
                DestinPrim.Component.Comment.EndModify;
            end;
            DestinPrim.GraphicallyInvalidate;
        end;

    // Polygons
    ePolyObject :
        begin
            DestinPrim.PolyHatchStyle       := SourcePrim.PolyHatchStyle;
            DestinPrim.PolygonType          := SourcePrim.PolygonType;
            DestinPrim.IgnoreViolations     := SourcePrim.IgnoreViolations;
            DestinPrim.PrimitiveLock        := SourcePrim.PrimitiveLock;
            DestinPrim.MinTrack             := SourcePrim.MinTrack;
            DestinPrim.PourOver             := SourcePrim.PourOver;
            DestinPrim.AvoidObsticles       := SourcePrim.AvoidObsticles;
            DestinPrim.UseOctagons          := SourcePrim.UseOctagons;
            DestinPrim.RemoveNarrowNecks    := SourcePrim.RemoveNarrowNecks;
            DestinPrim.RemoveIslandsByArea  := SourcePrim.RemoveIslandsByArea;
            DestinPrim.RemoveDead           := SourcePrim.RemoveDead;
            DestinPrim.NeckWidthThreshold   := SourcePrim.NeckWidthThreshold;
            DestinPrim.IslandAreaThreshold  := SourcePrim.IslandAreaThreshold;
            DestinPrim.Grid                 := SourcePrim.Grid;
            DestinPrim.TrackSize            := SourcePrim.TrackSize;
            DestinPrim.ArcApproximation     := SourcePrim.ArcApproximation;
            //DestinPrim.Net                  := SourcePrim.Net;

            if boolLoc = mrYes then
                DestinPrim.Layer            := SourcePrim.Layer;
            DestinPrim.SetState_CopperPourInvalid;
            DestinPrim.Rebuild;
            DestinPrim.SetState_CopperPourValid;
            DestinPrim.GraphicallyInvalidate;
        end;

    eRegionObject :
        if not (SourcePrim.InComponent or SourcePrim.InPolygon) then
        begin
            if boolLoc = mrYes then
                DestinPrim.Layer            := SourcePrim.Layer;
            DestinPrim.SetState_Kind (SourcePrim.Kind);
        //    DestinPrim.IsSimpleRegion       := SourcePrim.IsSimpleRegion;
            DestinPrim.IsKeepout            := SourcePrim.IsKeepout;
            DestinPrim.InNet                := SourcePrim.InNet;    
            //DestinPrim.Net                  := SourcePrim.Net;
            DestinPrim.GraphicallyInvalidate;
        end;

    // Dimensions
    eDimensionObject :
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

            if boolLoc = mrYes then
                DestinPrim.Layer           := SourcePrim.Layer;

            // !!! Workaround for now - needed to fake Dimension has changed semantics. This
            // is necesary because we don't currenly have access to the Dimension method that
            // force a dimension update. Without this the call to DestinPrim.SetState_XSizeYSize
            // is not doing anything
            DestinPrim.TextX              := DestinPrim.TextX + MilsToCoord(0.01);
            DestinPrim.SetState_XSizeYSize;
            DestinPrim.EndModify;
            DestinPrim.BeginModify;
            //DestinPrim.GraphicallyInvalidate;
            DestinPrim.TextX              := DestinPrim.TextX - MilsToCoord(0.01);
            DestinPrim.SetState_XSizeYSize;
            DestinPrim.GraphicallyInvalidate;
        End;

    // Coordinates
    eCoordinateObject :
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

            if boolLoc = mrYes then
                DestinPrim.Layer          := SourcePrim.Layer;
            
            // !!! Workaround for now - needed to fake Dimension has changed semantics. This
            // is necessary because we don't currently have access to the Dimension method that
            // force a dimension update. Without this the call to DestinPrim.SetState_XSizeYSize
            // is not doing anything
            
            DestinPrim.X     := DestinPrim.X + MilsToCoord(0.01);
            DestinPrim.SetState_XSizeYSize;
            DestinPrim.EndModify;               // this could be enough
          //  DestinPrim.GraphicallyInvalidate;
            DestinPrim.BeginModify;
            DestinPrim.X     := DestinPrim.X - MilsToCoord(0.01);
            DestinPrim.SetState_XSizeYSize;
            DestinPrim.GraphicallyInvalidate;
        end;
    end; //case

    DestinPrim.EndModify;
end;

procedure FormatCopyPCB(Doc : IDocument);
var
    Board         : IPCB_Board;
    SourcePrim    : IPCB_Primitive;
    DestinPrim    : IPCB_Primitive;
    BoardIterator : IPCB_BoardIterator;
    bFirstTime    : boolean;
    bNeverAsk     : boolean;
    bTempAsk      : boolean;

begin
    // Get the document
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;
    
    // Make it work for Pads, Vias, Strings, Polygons, Dimensions and coordinates
    ASetOfObjects  := MkSet(ePadObject, eViaObject, eTextObject, ePolyObject, eRegionObject, eDimensionObject, eCoordinateObject);
  //  TextObjSet := MkSet(eTextObject);
    NewAllLayers := SetUnion(AllLayers, MkSetRange(eMech17, eMech32));

    SourcePrim := Nil;
    DestinPrim := Nil;
    boolLoc    := mrYes;      // default copy layer info
    bFirstTime := true;
    bNeverAsk  := cNeverAsk;

    repeat
        // process if source & destination are selected
        if Assigned(DestinPrim) And Assigned(SourcePrim) then
        begin
            // copy formatting of PCB dimension
            PCBServer.PreProcess;
            ProcessPCBPrim(SourcePrim, DestinPrim, boolLoc);

            DestinPrim := Nil;

            PCBServer.PostProcess;
            Board.ViewManager_FullUpdate;
        end;

        // Get PCB Object
        if Assigned(SourcePrim) then
        begin
            Prompt := 'Choose Destination Primitive' + ' : ' + SourcePrim.ObjectIdString + ' on Layer ' + Board.LayerName(SourcePrim.Layer) + '  ';
//            DestinPrim := PCBBoard.GetObjectAtCursor(MkSet(SourcePrim.ObjectId), NewAllLayers, Prompt);
            DestinPrim := nGetObjectAtCursor(Board, MkSet(SourcePrim.ObjectId), NewAllLayers, Prompt);
        end;

        if (not Assigned(DestinPrim)) or (DestinPrim = cESC) then SourcePrim := Nil;        //pick a new source obj

        if not Assigned(SourcePrim) then
        begin
            DestinPrim := Nil;
//            SourcePrim := PCBBoard.GetObjectAtCursor(ASetOfObjects, NewAllLayers, 'Choose Source Primitive');
            repeat
               SourcePrim := nGetObjectAtCursor(Board, ASetOfObjects, NewAllLayers, 'Choose Source Primitive');
            until Assigned(SourcePrim) or (SourcePrim = cEsc);

            if Assigned(SourcePrim) and (SourcePrim <> cESC)then
            begin
                bTempAsk := bNeverAsk;
                if SourcePrim.ObjectId = ePolyObject then     // polygons more likely to be on diff layers so ask user..
                begin
                   bTempAsk  := false;
                   bFirstTime := true;
                end;
                if (not bTempAsk) and bFirstTime then
                begin
                    // supporting pad format copy without full layer handling is problematic.
                    // if SourcePrim.ObjectId = ePadObject then
                    //    Prompt := 'Pad : Copy layer info (SMD/Thru)';
                    if SourcePrim.ObjectId = ePolyObject then
                        Prompt := 'Polygon on layer : ' + Board.LayerName(SourcePrim.Layer) + '. Copy layer info ?'
                    else if SourcePrim.ObjectId = eViaObject then
                        Prompt := 'Via between : ' + Board.LayerName(SourcePrim.LowLayer) + '-' + Board.LayerName(SourcePrim.HighLayer)
                                    + '. Copy Start/Stop Layer info ?'
                    else
                        Prompt := SourcePrim.ObjectIdString + '. Copy layer ' + Board.LayerName(SourcePrim.Layer) + ' info ?';

                    boolLoc := MessageDlg(Prompt, mtConfirmation, mbYesNoCancel, 0);
                    if boolLoc = mrCancel then SourcePrim := Nil;
                    bFirstTime := false;
                end;
            end;
        end;
    until (SourcePrim = cESC);  // And (DestinPrim = cESC);
end;

// main call entry point
procedure FormatCopier;
var
   WS              : IWorkSpace;
   Doc             : IDocument;

begin
   WS := GetWorkSpace;
   Doc := WS.DM_FocusedDocument;
   DocKind := Doc.DM_DocumentKind;

   if (DocKind = cDocKind_Sch) or (DocKind = cDocKind_SchLib) then
       FormatCopySch(Doc);

   if DocKind = cDocKind_Pcb then
       FormatCopyPCB(Doc);
end;

