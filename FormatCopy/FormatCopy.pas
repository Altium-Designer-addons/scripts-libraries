{ FormatCopy.pas
Summary   Used to copy some formatting properties from one (source) primitive
          to other (destination) primitive(s) of the same or similar type.

          Works with almost all schematic objects (in SchDoc and SchLib)
          and some objects in PCB Document.
          SchDoc:
            Cross object format copying is possible.
            <cntl>  key modifier prevent Text colour pasting   (C==Colour)
            <alt>   key modifier prevents Area colour pasting  (A==Area)
            <shift> key could prevent text Size change         (S==Size)
Usage Notes:
    While script is running:
    - <cntl>+<Z> is active/usable.
    - Current Layer (PCB) can be changed with <+> & <-> keys to influence layer pick bias.

 From FormatPaintbrush.pas
 Created by : Petar Perisin
 Modified   : B. Miller
08/08/2019 v0.1  fn() nGetObjectAtCursor from ModifyFootprintTracks.pas, handles layers beyond eMech16
09/08/2019 v0.2  Refactored "repeat" loops
10/08/2019 v0.3  Via & Pad format copying implemented/fixed.
           v0.4  Recoded Sch obj selector with "sub" object priority/bias
15/08/2019 v0.5  Recoded PCB obj pick with current layer & obj bias.
19/08/2019 v0.6  De-select all Sch objects, *fix* PCB layer bias
28/08/2019 v0.7  Group Sch Obj formatting operations by ancestor obj.
31/08/2019 v0.71 Allow basic cross object type copy for text
01/09/2019 v0.72 Sch pick object set based biasing.
03/09/2019 v0.73 Sch: Move more operations to ancestor objects.
14/09/2019 v0.74 Sch: Add modifier <cntl><alt> support
14/09/2019 v0.75 PCB: remove layerset, use Source object & current layer as bias.

tbd: <shift> modifier key was to prevent font size change but FontManager is borked in AD19.

}

{ AllLayer = [MinLayer..eConnectLayer] , Set of TLayer
  MinLayer = eTopLayer;
  MaxLayer = eViaHoleLayer;              // 82 Mechanical 26 Via Holes
  eConnectLayer = eMechanical16 + 3;     // 75 Mechanical 19 Connections
  MaxMechanicalLayer = eMechanical16;

 i=17, eMech17 : LayerID = 67108881   returned by AD17 Obj.Layer; but crashes GetObjectAtCursor(TLayer_V6)
 i=32, eMech32 : LayerID = 67108896   LayerUtils.MechanicalLayer(i) returns correct values
 Delphi Sets can only have max 256 elements.
 The values 67108### above crash MkSet()
{..............................................................................}

const
    cNeverAsk = false;    // set true for no layer prompt (default copy layer yes)
    cESC      =-1;
    cAltKey   = 1;
    cShiftKey = 2;
    cCntlKey  = 3;

var
    // Common variables
    ASetOfObjects   : TObjectSet;
    TextObjSet      : TObjectSet;
    KeySet          : TObjectSet;    // keyboard key modifiers <alt> <shift> <cntl>
    boolLoc         : Integer;
    DocKind         : String;
    VerMajor        : Widestring;
    Prompt          : WideString;
{..............................................................................}

function Version(const dummy : boolean) : TStringList;
begin
    Result := TStringList.Create;
    Result.Delimiter := '.';
    Result.Duplicates := dupAccept;
    Result.DelimitedText := Client.GetProductVersion;
end;

function nGetObjectAtCursor(Board : TPCB_Board, const ObjectSet: TObjectSet, const SourcPrimLayer : TLayer, msg : TString) : IPCB_Primitive;
var
    x, y      : TCoord;
    Iterator  : IPCB_SpatialIterator;
    Prim      : IPCB_Primitive;
    Prev      : IPCB_Primitive;
    Area      : double;
    CLayer    : TLayer;
    MechLayer : IPCB_MechanicalLayer;

begin
    Result := eNoObject;

    if Board.ChooseLocation(x,y,msg) then  // false = ESC Key is pressed
    begin
// layer can be changed during ChooseLocation fn UI!
        CLayer := Board.CurrentLayer;               // returns 0 for any unsupported layers.
  
        Iterator := Board.SpatialIterator_Create;
        Iterator.SetState_FilterAll;
        Iterator.AddFilter_AllLayers;
        Iterator.AddFilter_ObjectSet(ObjectSet);
        Iterator.AddFilter_Area (x - 10, y + 10, x + 10, y - 10);   //TCoord
        Prim := Iterator.FirstPCBObject;
        while (Prim <> Nil) do
        begin
            if Board.LayerIsDisplayed(Prim.Layer) then   // filter on visible layers
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
                        if (Prim.Layer = SourcPrimLayer)                   then Result := Prim;         // bias to same layer as SourcPrim
                        if (Prim.Layer = CLayer) or (CLayer = 0)           then Result := Prim;         // highest bias to current layer
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
        eParameterSet        : Result := 'ParameterSet';
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
        eBlanket             : Result := 'Blanket';
        eCompileMask         : Result := 'CompileMask';
        eRectangle           : Result := 'Rectangle';
        eRoundRectangle      : Result := 'RndRectangle';
        eArc                 : Result := 'Arc';
    else
        Result := '';
    end;
end;

procedure ProcessSCHPrim(SchSourcePrim : ISch_Object, SchDestinPrim : ISch_Object, DocKind : WideString, KeySet : TObjectSet);
var
    SubSetObj   : TObjectSet;
begin
// Objects do NOT have to be the same ObjectId, just same ancestor ObjectId /type.
// ISch_Port text & colour is a messy mixture of label & entry properties; ignore.

// ISch_GraphicalObject
    if not InSet(cAltKey, KeySet) then
        SchDestinPrim.AreaColor := SchSourcePrim.AreaColor;
    if not InSet(cCntlKey, KeySet) then
        SchDestinPrim.Color     := SchSourcePrim.Color;

// ISch_GraphicalObject/ISch_Polygon & ./ISch_Line & ./ISch_Rectangle & ./ISch_RectangularGroup
    SubSetObj := MkSet(ePolygon, ePolyLine, eBus, eWire, eBezier, eSignalHarness, eLine, eBusEntry, eRectangle, eRoundRectangle, eImage, eBlanket, eCompileMask, eSheetSymbol, eHighLevelCodeSymbol);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
    end;

// ISch_GraphicalObject/ISch_Label
    SubSetObj := MkSet(eLabel, eCrossSheetConnector, eDesignator, eParameter, eNetlabel, ePowerObject, eSheetName, eSheetFileName);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.FontID        := SchSourcePrim.FontID;
        SchDestinPrim.Justification := SchSourcePrim.Justification;
        SchDestinPrim.IsMirrored    := SchSourcePrim.IsMirrored;
        SchDestinPrim.Orientation   := SchSourcePrim.Orientation;
    end;

// ISch_GraphicalObject/ISch_Label/ISch_ComplexText
    SubSetObj := MkSet(eDesignator, eParameter, eSheetName, eSheetFileName, eHarnessConnectorType);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.Autoposition   := SchSourcePrim.Autoposition;
        SchDestinPrim.IsHidden       := SchSourcePrim.IsHidden;
        SchDestinPrim.TextHorzAnchor := SchSourcePrim.TextHorzAnchor;
        SchDestinPrim.TextVertAnchor := SchSourcePrim.TextVertAnchor;
    end;

// ISch_GraphicalObject/ISch_Label/ISch_ComplexText/ISch_Parameter
    SubSetObj := MkSet(eDesignator, eParameter);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.ShowName      := SchSourcePrim.ShowName;
    end;

//  ISch_GraphicalObject/ISch_Label/ISch_PowerObject
    SubSetObj := MkSet(ePowerObject, eCrossSheetConnector);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.Style         := SchSourcePrim.Style;
        SchDestinPrim.ShowNetName   := SchSourcePrim.ShowNetName;
    end;

// ISch_GraphicalObject/ISch_Polygon
    SubSetObj := MkSet(ePolygon, ePolyLine, eBus, eWire, eBezier, eSignalHarness);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
        SchDestinPrim.Transparent   := SchSourcePrim.Transparent;
    end;

// ISch_GraphicalObject/ISch_Polygon/ISch_BasicPolyline
    SubSetObj := MkSet(ePolyLine, eBus, eWire, eBezier, eSignalHarness);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.LineStyle      := SchSourcePrim.LineStyle;
    end;

// ISch_GraphicalObject/ISch_Polygon/ISch_BasicPolyline/ISch_Wire
    SubSetObj := MkSet(eBus, eWire, eSignalHarness);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.UnderLineColor := SchSourcePrim.UnderLineColor;
    end;

// ISch_GraphicalObject/ISch_Line
    SubSetObj := MkSet(eLine, eBusEntry);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.LineStyle   := SchSourcePrim.LineStyle;
    end;

// ISch_GraphicalObject/ISch_Rectangle
    SubSetObj := MkSet(eRectangle, eRoundRectangle, eImage, eBlanket, eCompileMask);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.IsSolid     := SchSourcePrim.IsSolid;
        SchDestinPrim.Transparent := SchSourcePrim.Transparent;
    end;

// ISch_GraphicalObject/ISch_Rectangle/ISch_TextFrame
    SubSetObj := MkSet(eTextFrame, eNote);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.Alignment   := SchSourcePrim.Alignment;
        SchDestinPrim.ClipToRect  := SchSourcePrim.ClipToRect;
        SchDestinPrim.FontID      := SchSourcePrim.FontID;
        SchDestinPrim.ShowBorder  := SchSourcePrim.ShowBorder;
        if not InSet(cCntlKey, KeySet) then
            SchDestinPrim.TextColor   := SchSourcePrim.TextColor;
        SchDestinPrim.WordWrap    := SchSourcePrim.WordWrap;
    end;

// ISch_GraphicalObject/ISch_ParametrizedGroup/ISch_RectangularGroup     excl eHarnessConnector
    SubSetObj := MkSet(eSheetSymbol, eHighLevelCodeSymbol);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.IsSolid          := SchSourcePrim.IsSolid;
    end;

// ISch_GraphicalObject/ISch_SheetEntry & ./ISch_HarnessEntry  & ./ISch_HighLevelCodeEntry
    SubSetObj := MkSet(eSheetEntry, eHighLevelCodeEntry, eHarnessEntry);
    if InSet(SchSourcePrim.ObjectId, SubsetObj) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
      //  SchDestinPrim.IsVertical     := SchSourcePrim.IsVertical;
        if not InSet(cCntlKey, KeySet) then
            SchDestinPrim.TextColor      := SchSourcePrim.TextColor;
        SchDestinPrim.TextFontID     := SchSourcePrim.TextFontID;
        SchDestinPrim.TextStyle      := SchSourcePrim.TextStyle;
    end;


// Special format copy for non matching objects
// ISch_Label to ISch_SheetEntry or ISch_HarnessEntry
    SubSetObj := MkSet(eLabel, eCrossSheetConnector, eDesignator, eParameter, eNetlabel, ePowerObject, eSheetName, eSheetFileName);
    if InSet(SchSourcePrim.ObjectId, SubSetObj) and InSet(SchDestinPrim.ObjectID, MkSet(eSheetEntry, eHarnessEntry)) then
    begin
        SchDestinPrim.TextFontID  := SchSourcePrim.FontID;
        if not InSet(cCntlKey, KeySet) then
            SchDestinPrim.TextColor   := SchSourcePrim.Color;
//        SchDestinPrim.TextStyle   := SchSourcePrim.TextStyle;
    end;
    if InSet(SchSourcePrim.ObjectId, MkSet(eSheetEntry, eHarnessEntry)) and InSet(SchDestinPrim.ObjectID, SubSetObj) then
    begin
        SchDestinPrim.FontID      := SchSourcePrim.TextFontID;
        if not InSet(cCntlKey, KeySet) then
            SchDestinPrim.Color       := SchSourcePrim.TextColor;
//        SchDestinPrim.TextStyle   := SchSourcePrim.TextStyle;
    end;


// Objects now must be the same ObjectId to continue.
    if SchSourcePrim.ObjectId <> SchDestinPrim.ObjectId then exit;

    case SchSourcePrim.ObjectId of
    eJunction :      // ISch_GraphicalObject/ISch_Junction
        begin
            SchDestinPrim.Locked    := SchSourcePrim.Locked;
            SchDestinPrim.Size      := SchSourcePrim.Size;
        end;

    eProbe :         // ISch_GraphicalObject/ISch_ParametrizedGroup/ISch_ParameterSet/ISch_Probe
        begin
            SchDestinPrim.Orientation := SchSourcePrim.Orientation;
            SchDestinPrim.Style       := SchSourcePrim.Style;
        end;

    // NoERC Marker
    eNoERC :         // ISch_GraphicalObject/ISch_NoERC
        begin
            SchDestinPrim.Orientation  := SchSourcePrim.Orientation;
            SchDestinPrim.Symbol       := SchSourcePrim.Symbol;
            SchDestinPrim.IsActive     := SchSourcePrim.IsActive;
            SchDestinPrim.SuppressAll  := SchSourcePrim.SuppressAll;
            //SchDestinPrim.CONNECTIONPAIRSTOSUPPRESS     := SchSourcePrim.CONNECTIONPAIRSTOSUPPRESS;
        end;

    ePort :          // ISch_GraphicalObject/ISch_ParametrizedGroup/ISch_Port
        begin
            if not InSet(cCntlKey, KeySet) then
                SchDestinPrim.TextColor   := SchSourcePrim.TextColor;
            SchDestinPrim.Alignment   := SchSourcePrim.Alignment;
            SchDestinPrim.Style       := SchSourcePrim.Style;
            SchDestinPrim.FontId      := SchSourcePrim.FontId;
            SchDestinPrim.IOType      := SchSourcePrim.IOType;
            SchDestinPrim.Height      := SchSourcePrim.Height;
            SchDestinPrim.Width       := SchSourcePrim.Width;
            SchDestinPrim.BorderWidth := SchSourcePrim.BorderWidth;
        end;

    // Off-Sheet Connector
    eCrossSheetConnector : //             ../ISch_Label/ISch_PowerObject/ISch_CrossSheetConnector
        begin
            SchDestinPrim.CrossSheetStyle := SchSourcePrim.CrossSheetStyle;
        end;

    // Part
    eSchComponent :  // ISch_GraphicalObject/ISch_ParametrizedGroup/ISch_Component
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
            SchDestinPrim.PinColor                    := SchSourcePrim.PinColor;
        end;

    // Pin - Only use in SCHLIB
    ePin :            // ISch_GraphicalObject/ISch_ParameterizedGroup/ISch_Pin
        if (DocKind = cDocKind_SchLib) then
        begin
            SchDestinPrim.ShowName       := SchSourcePrim.ShowName;
            SchDestinPrim.ShowDesignator := SchSourcePrim.ShowDesignator;
        end;

    eParameterSet :   // ISch_GraphicalObject/ISch_ParametrizedGroup/ISch_ParameterSet
        begin
            SchDestinPrim.Style     := SchSourcePrim.Style;
        end;

//  C Code Symbol
//    eHighLevelCodeSymbol {58}:  //       ../ISch_ParametrizedGroup/ISch_RectangularGroup/ISch_HighLevelCodeSymbol

    eSheetSymbol :    // ISch_GraphicalObject/ISch_ParametrizedGroup/ISch_RectangularGroup/ISch_SheetSymbol
        SchDestinPrim.ShowHiddenFields := SchSourcePrim.ShowHiddenFields;

    eHarnessConnector : //                 ../ISch_ParametrizedGroup/ISch_RectangularGroup/ISch_HarnessConnector
        begin
            SchDestinPrim.LineWidth                      := SchSourcePrim.LineWidth;
            SchDestinPrim.MasterEntryLocation            := SchSourcePrim.MasterEntryLocation;
            SchDestinPrim.HarnessConnectorType.IsHidden  := SchSourcePrim.HarnessConnectorType.IsHidden;
        end;

    eEllipse :        // ISch_GraphicalObject/ISch_Circle/ISch_Ellipse
        begin
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
            SchDestinPrim.Transparent   := SchSourcePrim.Transparent;
        end;

    eArc,             // ISch_GraphicalObject/ISch_Arc/ISch_EllipticalArc
    eEllipticalArc :  // ISch_GraphicalObject/ISch_Arc/ISch_EllipticalArc
        begin
            SchDestinPrim.EndAngle      := SchSourcePrim.EndAngle;
            SchDestinPrim.StartAngle    := SchSourcePrim.StartAngle;
        end;

    ePie :            // ISch_GraphicalObject/ISch_Arc/ISch_Pie
        begin
            SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
        end;

//    eSignalHarness :  // ISch_GraphicalObject/ISch_Polygon/ISch_Wire/ ??

    ePolyline :       // ISch_GraphicalObject/ISch_Polygon/ISch_BasicPolyline/ISch_Polyline
        begin
            SchDestinPrim.StartLineShape := SchSourcePrim.StartLineShape;
            SchDestinPrim.EndLineShape   := SchSourcePrim.EndLineShape;
            SchDestinPrim.LineShapeSize  := SchSourcePrim.LineShapeSize;
        end;

    // SheetSym & C Code Entry
    eSheetEntry,      // ISch_GraphicalObject/ISch_SheetEntry
    eHighLevelCodeEntry : // ISch_Graphical../ISch_HighLevelCodeEntry
        begin
            SchDestinPrim.Style          := SchSourcePrim.Style;
            SchDestinPrim.ArrowKind      := SchSourcePrim.ArrowKind;
            if not InSet(cAltKey, KeySet) then
                SchDestinPrim.HarnessColor   := SchSourcePrim.HarnessColor;
        end;

//    eHarnessEntry :   // ISch_GraphicalObject/ISch_HarnessEntry

    eNote :          // ISch_GraphicalObject/ISch_Rectangle/ISch_TextFrame/ISch_Note
        begin
            SchDestinPrim.Collapsed     := SchSourcePrim.Collapsed;
            SchDestinPrim.Author        := SchSourcePrim.Author;
        end;

    eImage :          // ISch_GraphicalObject/ISch_Rectangle/ISch_Image
        begin
            SchDestinPrim.KeepAspect    := SchSourcePrim.KeepAspect;
        end;

    eRoundRectangle : // ISch_GraphicalObject/ISch_Rectangle/ISch_RoundRectangle
        begin
            SchDestinPrim.CornerXRadius := SchSourcePrim.CornerXRadius;
            SchDestinPrim.CornerYRadius := SchSourcePrim.CornerYRadius;
        end;

    eCompileMask :    // ISch_GraphicalObject/ISch_Rectangle/ISch_CompileMask
        begin
            SchDestinPrim.Collapsed      := SchSourcePrim.Collapsed;
        end;

    eBlanket :        // ISch_GraphicalObject/ISch_Rectangle/ISch_Blanket
        begin
            SchDestinPrim.LineStyle      := SchSourcePrim.LineStyle;
            SchDestinPrim.Collapsed      := SchSourcePrim.Collapsed;
        end;
    end; //case
end;

procedure FormatCopySch(Doc : IDocument);
var
   SchDoc          : ISCH_Document;
   Location        : TLocation;
   SchSourcePrim   : ISch_Object;
   SchDestinPrim   : ISch_Object;
   SchTempPrim     : ISch_Object;
   SpatialIterator : ISch_Iterator;
   bRepeat         : boolean;

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

            ProcessSCHPrim(SchSourcePrim, SchDestinPrim, DocKind, KeySet);

            SchServer.RobotManager.SendMessage(SchDestinPrim.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);
            SchServer.ProcessControl.PostProcess(SchDoc, '');

            SchDestinPrim := nil;         // force Get Next Destination Object
        end;

//   Get Destination Object
        if SchSourcePrim <> Nil then
        begin     // allow formatting across objects ?
            ASetOfObjects := MkSet(eDesignator, eParameter, eLabel, eNetlabel, ePort, eCrossSheetConnector, eHarnessEntry, eSheetEntry);
            if not InSet(SchSourcePrim.ObjectId, ASetOfObjects) then
            begin
                ASetOfObjects := MkSet(eRectangle, eRoundRectangle, eImage, eBlanket, eCompileMask);    // allow formatting across objects ?
                if not InSet(SchSourcePrim.ObjectId, ASetOfObjects) then
                    ASetOfObjects := MkSetRange(eFirstObjectID, eLastObjectID);
            end;

            Prompt := ObjectIDToString(SchSourcePrim.ObjectId);
            if Prompt = '' then Prompt :=  GetStateString_ObjectId(SchSourcePrim.ObjectId);  // SchSourcePrim.GetState_DescriptionString;
            Prompt := 'Choose Destination Object : ' + Prompt; 


//        Get the right Object
            boolLoc := SchDoc.ChooseLocationInteractively(Location, Prompt);

//        store key modifiers
            KeySet := MkSet();
            if ShiftKeyDown   then KeySet := MkSet(cShiftKey);
            if AltKeyDown     then KeySet := SetUnion(KeySet, MkSet(cAltKey));
            if ControlKeyDown then KeySet := SetUnion(KeySet, MkSet(cCntlKey));

            If Not boolLoc Then
            begin
                  SchSourcePrim := nil;
                  SchDestinPrim := nil;
            end;

            if SchSourcePrim <> Nil then
            begin
                SpatialIterator := SchDoc.SchIterator_Create;
                SpatialIterator.SetState_FilterAll;

                Try
                    if (DocKind = cDocKind_SchLib) then
                    begin
                        SpatialIterator.AddFilter_CurrentPartPrimitives;
                        SpatialIterator.AddFilter_CurrentDisplayModePrimitives;
                    end;
                    SpatialIterator.AddFilter_Area(Location.X - 1, Location.Y - 1, Location.X + 1, Location.Y + 1);

                    bRepeat := false;
                    Repeat
                        SchTempPrim := SpatialIterator.FirstSchObject;
                        while (SchTempPrim <> nil) do
                        begin
                            if Inset(SchTempPrim.ObjectId, ASetOfObjects) then
                            begin
                                if SchTempPrim.ObjectId = SchSourcePrim.ObjectID then SchDestinPrim := SchTempPrim
                                else
                                if InSet(SchTempPrim.ObjectId, MkSet(eDesignator, eParameter)) then
                                begin
                                    if not SchTempPrim.IsHidden then SchDestinPrim := SchTempPrim;
                                end
                                else
                                if InSet(SchTempPrim.ObjectId, MkSet(eSheetEntry, eHarnessEntry)) then SchDestinPrim := SchTempPrim
                                else
                                if InSet(SchTempPrim.ObjectId, MkSet(eNetLabel, ePort, eCrossSheetConnector)) then SchDestinPrim := SchTempPrim;

                                if bRepeat and (SchDestinPrim = nil) then SchDestinPrim := SchTempPrim;
                            end;
                            SchTempPrim   := SpatialIterator.NextSchObject;
                        end;

                        if (SchDestinPrim = nil) and not bRepeat then        // go around with all/any objects
                        begin
                            ASetOfObjects := MkSetRange(eFirstObjectID, eLastObjectID);
                            bRepeat := true;
                        end
                        else bRepeat := false;                               // force failed exit
                    until (SchDestinPrim <> nil) or (bRepeat = false);

                Finally
                    SchDoc.SchIterator_Destroy(SpatialIterator);
                End;
            end;
        end;

//   Get Source Object
        if SchSourcePrim = nil then
        begin
            boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose Source Object');
            If Not boolLoc Then continue;

            SpatialIterator := SchDoc.SchIterator_Create;
            Try
                SpatialIterator.SetState_FilterAll;
                SpatialIterator.AddFilter_Area(Location.X - 2, Location.Y - 2, Location.X + 2, Location.Y + 2);

                if (DocKind = cDocKind_SchLib) then
                begin
                    SpatialIterator.AddFilter_CurrentPartPrimitives;
                    SpatialIterator.AddFilter_CurrentDisplayModePrimitives;
                end;

                SchTempPrim := SpatialIterator.FirstSchObject;

                while (SchTempPrim <> nil) do
                begin
                    if SchSourcePrim <> nil then
                    begin
                        // prioritize unhidden & Harness & Sheet Entries over parent obj.
                        if InSet(SchSourcePrim.ObjectId, MkSet(eDesignator, eParameter)) then
                            if SchSourcePrim.IsHidden then SchSourcePrim := SchTempPrim;
                        if InSet(SchSourcePrim.ObjectId, MkSet(eSchComponent, eHarnessConnector, eSheetSymbol)) then
                            SchSourcePrim := SchTempPrim;
//                        if SchSourcePrim.ObjectId = eSignalHarness {56?} then SchSourcePrim := SchTempPrim;
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
                // DestinPrim.XPadOffset(Layer)           := Pad.XPadOffset(Layer);  This property is not implemented.
                // DestinPrim.YPadOffset(Layer)           := Pad.YPadOffset(Layer);  This property is not implemented.
            end;
            DestinPrim.InvalidateSizeShape;
            PadCache                        := Pad.GetState_Cache;
            // DestinPrim.Cache.SolderMaskExpansionValid  := SourcePrim.Cache.SolderMaskExpansionValid;
            // DestinPrim.Cache.SolderMaskExpansion       := SourcePrim.Cache.SolderMaskExpansion;
            // DestinPrim.Cache.PasteMaskExpansionValid   := SourcePrim.Cache.PasteMaskExpansionValid;
            // DestinPrim.Cache.PasteMaskExpansion        := SourcePrim.Cache.PasteMaskExpansion;
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
            // Padcache.ReliefAirGap
            // Padcache.PowerPlaneReliefExpansion
            // Padcache.PowerPlaneClearance
            // Padcache.ReliefConductorWidth
            // Padcache.SolderMaskExpansion
            // Padcache.SolderMaskExpansionValid
            // DestinPrim.Cache.SolderMaskExpansionValid  := SourcePrim.Cache.SolderMaskExpansionValid;
            // DestinPrim.Cache.SolderMaskExpansion       := SourcePrim.Cache.SolderMaskExpansion;
            // Padcache.PasteMaskExpansion
            // Padcache.PasteMaskExpansionValid
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
//            PCBServer.SendMessageToRobots(DestinPrim.I_ObjectAddress ,c_Broadcast, PCBM_BeginModify, c_NoEventData);

            ProcessPCBPrim(SourcePrim, DestinPrim, boolLoc);
            DestinPrim := Nil;

//            PCBServer.SendMessageToRobots(DestinPrim.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
            PCBServer.PostProcess;
            Board.ViewManager_FullUpdate;
        end;

        // Get PCB Object
        if Assigned(SourcePrim) then
        begin
            Prompt := 'Choose Destination Primitive' + ' : ' + SourcePrim.ObjectIdString + ' on Layer ' + Board.LayerName(SourcePrim.Layer) + '  ';
            DestinPrim := nGetObjectAtCursor(Board, MkSet(SourcePrim.ObjectId), SourcePrim.Layer, Prompt);
        end;

        if (not Assigned(DestinPrim)) or (DestinPrim = cESC) then SourcePrim := Nil;        //pick a new source obj

        if not Assigned(SourcePrim) then
        begin
            DestinPrim := Nil;
            repeat
               SourcePrim := nGetObjectAtCursor(Board, ASetOfObjects, 0, 'Choose Source Primitive');
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
   WS   : IWorkSpace;
   Doc  : IDocument;

begin
   WS       := GetWorkSpace;
   Doc      := WS.DM_FocusedDocument;
   DocKind  := Doc.DM_DocumentKind;
   VerMajor := Version(true).Strings(0);

   if (DocKind = cDocKind_Sch) or (DocKind = cDocKind_SchLib) then
       FormatCopySch(Doc);

   if DocKind = cDocKind_Pcb then
       FormatCopyPCB(Doc);
end;

