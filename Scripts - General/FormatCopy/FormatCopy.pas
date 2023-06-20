{ FormatCopy.pas
Summary   Used to copy some formatting properties from one (source) primitive
          to other (destination) primitive(s) of the same or similar type.

Works with almost all schematic objects (in SchDoc and SchLib) and some objects in PCB Document.
  SchDoc:
    Cross object format copying is possible.
    <cntl>  key modifier prevent Text colour pasting   (C==Colour)
    <alt>   key modifier prevents Area colour pasting  (A==Area)
    <shift> key could prevent text Size change         (S==Size)
  PcbDoc:
    No cross object
    <cntl>  key modifier Current Layer only pick
    <shift> key modifier same Layer pick as Source Prim Layer
    <+>, <->  changes current layer.

Usage Notes:
    While script is running:
    - <cntl>+<Z> is active/usable.
    - Current Layer (PCB) can be changed with <+> & <-> keys to influence layer pick bias.
    - Ambiguious UI Dialog ignores modifier keys if Layer > MaxLayers

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
16/09/2019 v0.76 PCB: Spatial iterator does NOT work with groups: dimensions & components.
                      Pick UI does not cancel out from missed picks; more like the SchDoc picking
17/09/2019 v0.80 PCB: Use Ambiguious object UI Dialog when possible, else uses prev. method
21/09/2019 v0.81 SCH: eTextFrame (& eNote?) was not copying IsSolid, Transparent or LineWidth
22/09/2019 v0.82 SCH: Use CreateHitTest in place of SpatialIterator
27/09/2019 v0.83 SCH: Refactor out nested InSet & MkSet to avoid weirdness.
17/10/2019 v0.84 SCH: Refactor out more nested Inset MkSet around line 600 in just in case.
18/02/2020 v0.85 PCB: Improve Pad & Via expansion rule & value copying
07/05/2020 v0.86 SCH: Graphically.Invalidate after each copy; trigger bounding box resize for components; fix bug in Comp desc.
07/05/2020 v0.87 SCH: simplified pick ranking/weighting.
26/12/2020 v0.88 PCB: remove 2 useless lines in Dimensions. Change MessageDlg to mtCustom. Does it beep?
24/06/2022 v0.89 PCB: add pad & via template link copying.
24/06/2022 v0.90 PCB: copy over primitive keepout restrictions.
17/06/2023 v0.91 PCB: missed the simple padstack radius corners, refactor pad & via to layer iterators.

tbd: <shift> modifier key was intended to prevent font size change but FontManager is borked in AD19.
     special SchLib filters disabled.
     implement ranking/weighting for PCB obj pick.
}
{ Current API enumerations:  (in sad need of work)
  AllLayer = [MinLayer..eConnectLayer] , Set of TLayer
  MinLayer = eTopLayer;
  MaxLayer = eViaHoleLayer;              // 82 Mechanical 26 Via Holes
  eConnectLayer = eMechanical16 + 3;     // 75 Mechanical 19 Connections
  MaxMechanicalLayer = eMechanical16;

 i=17, eMech17 : LayerID = 67108881   returned by AD17 Obj.Layer; but crashes GetObjectAtCursor(TLayer_V6)
 i=32, eMech32 : LayerID = 67108896   LayerUtils.MechanicalLayer(i) returns correct values TV7_Layer
 Delphi Sets can only have max 256 elements.
 The values 67108### above crash MkSet()
{..............................................................................}

const
    cNeverAsk = false;    // set true for no layer prompt (default copy layer yes)
    cAlwaysAsk = false;   // set true to always present layer prompt after source selection
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
    Result := CreateObject(TStringList);
    Result.Delimiter := '.';
    Result.Duplicates := dupAccept;
    Result.DelimitedText := Client.GetProductVersion;
end;
function ShrinkBoundingRectangle( BR : CoordRect) : TCoordRect;
begin                         // Rect(L, T, R, B)
    Result := RectToCoordRect(
    Rect(BR.X1 + RectWidth(BR) / 4, BR.Y2 - RectHeight(BR) / 4, BR.X2 - RectWidth(BR) / 4, BR.Y1 + RectHeight(BR) / 4) );
end;

function nGetObjectAtCursor(Board : TPCB_Board, const ObjectSet: TObjectSet, const SourcePrim : IPCB_Primitive, msg : TString) : IPCB_Primitive;
var
    x, y          : TCoord;
    Iterator      : IPCB_BoardIterator;
    Prim          : IPCB_Primitive;
    Prev          : IPCB_Primitive;
//    Area      : double;
    LayerSet      : TLayerSet;
    TV6_LayerSet  : TLayerSet;
    PCB_LayerSet  : IPCB_LayerSet;
    RealAllLayers : boolean;
    CLayer        : TLayer;
    SPLayer       : TLayer;
    SPId          : WideString;
    BRect         : TCoordRect;

begin
    KeySet := MkSet();
    TV6_LayerSet := AllLayers;
    LayerSet := AllLayers;       // can not make larger or add layers above MaxLayer
    RealAllLayers := true;
    Result := eNoObject;

    if Board.ChooseLocation(x, y, msg) then  // false = ESC Key is pressed
    begin
//   read modifier keys just as/after the "pick" mouse click
        if ShiftKeyDown   then KeySet := MkSet(cShiftKey);

// ALT not possible with ChooseLocation
//        if AltKeyDown     then KeySet := SetUnion(KeySet, MkSet(cAltKey));
        if ControlKeyDown then KeySet := SetUnion(KeySet, MkSet(cCntlKey));
//   layer can be changed during ChooseLocation fn UI!
        CLayer := Board.CurrentLayer;               // returns 0 for any unsupported layers.
        SPLayer := 0; SPId := '';
        if SourcePrim <> Nil then
        begin
            SPLayer := SourcePrim.Layer;
            SPId  := SourcePrim.Identifier;
            if InSet(cShiftKey, KeySet) then
            begin
                LayerSet      := MkSet(SPLayer);
                if (SPLayer <= MaxLayer) then TV6_LayerSet := LayerSet;
                RealAllLayers := false;
            end;
        end;
        if InSet(cCntlKey, KeySet) then
        begin
            LayerSet      := MkSet(CLayer);
            if (CLayer <= MaxLayer) then TV6_LayerSet := LayerSet;
            RealAllLayers := false;
        end;
//                                                                        vvv TV6_Layerset
        Result := Board.GetObjectAtXYAskUserIfAmbiguous(x, y, ObjectSet, TV6_LayerSet, eEditAction_Focus);         // eEditAction_DontCare
        if (Result = Nil) then Result := eNoObject;

// try a bit harder, above call does not handle above emech16
        if Result = eNoObject then
        begin
            PCB_LayerSet := LayerSetUtils.EmptySet;
            PCB_LayerSet.IncludeAllLayers;
            Iterator := Board.BoardIterator_Create;
            Iterator.SetState_FilterAll;
            Iterator.AddFilter_IPCB_LayerSet(PCB_LayerSet);
            Iterator.AddFilter_ObjectSet(ObjectSet);
//            Iterator.AddFilter_Area (x - 100, y + 100, x + 100, y - 100);   // 1 Coord == 10Kmils   published method does not work board iterator

            Prim := Iterator.FirstPCBObject;
            while (Prim <> Nil) do
            begin
                BRect := Prim.BoundingRectangle;                          //  ShrinkBoundingRectangle(Prim.BoundingRectangle);
//        apply layer filter  real AllLayers
                if RealAllLayers or InSet(Prim.Layer, LayerSet) then
//        avoid picking Source nested objects with big bounding boxes
                if not (Prim.Identifier = SPId) then
                if (BRect.X1 < x) and (BRect.X2 > x) and  (BRect.Y1 < y) and (BRect.Y2 > y) then
//        filter on visible layers
                if Board.LayerIsDisplayed(Prim.Layer) then
//        need to exclude board region
                if not (Prim.ViewableObjectID = eViewableObject_BoardRegion) then   // Prim.Layer = eMultiLayer) and (Prim.ObjectID = eRegionObject))
//        exclude primitives in components
                if not Prim.InComponent then
                if Result <> eNoObject then
                begin
//        prioritise small & sub objects & on current layer.
                  //  if PrimArea(Result) > PrimArea(Prim) then Result := Prim;
                    if (Prev.ObjectID = ePolyObject) and (Prim.ObjectID <> ePolyObject)     then Result := Prim;
                    if (Prev.ObjectID = eRegionObject) and (Prim.ObjectID <> eRegionObject) then Result := Prim;
                    if ((Prev.ObjectID = eRegionObject) or (Prev.ObjectID = ePolyObject)) and
                        (Prev.Layer <> CLayer)                                              then Result := Prim;
                    if (Prev.ObjectID = eTextObject) and Prev.IsHidden                      then Result := Prim;
//              pick parent dim over the child text
                    if (Prev.InDimension) and (Prim.ObjectID = eDimensionObject)            then Result := Prim;
//              bias to same layer as SourcPrim
                    if (Prim.Layer = SPLayer)                                               then Result := Prim;
//              highest bias to current layer
                    if (Prim.Layer = CLayer)                                                then Result := Prim;
                end
                else Result := Prim;

                Prev := Result;
                Prim := Iterator.NextPCBObject;
            end;
            Board.BoardIterator_Destroy(Iterator);
        end;
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
    SubSetObj2  : TObjectSet;
    SourceObjId : TObjectId;
    DestinObjId : TObjectId;

begin
// Objects do NOT have to be the same ObjectId, just same ancestor ObjectId /type.
SourceObjId := SchSourcePrim.ObjectId;     // deref might help with weird InSet() issues
DestinObjId := SchDestinPrim.ObjectId;

// ISch_Port text & colour is a messy mixture of label & entry properties; ignore.
// ISch_GraphicalObject
    if not InSet(cAltKey, KeySet) then
        SchDestinPrim.AreaColor := SchSourcePrim.AreaColor;
    if not InSet(cCntlKey, KeySet) then
        SchDestinPrim.Color     := SchSourcePrim.Color;

// ISch_GraphicalObject/ISch_Polygon & ./ISch_Line & ./ISch_Rectangle
// ./ISch_ParametrizedGroup/ISch_RectangularGroup  & ./ISch_HarnessConnector
    SubSetObj := MkSet(ePolygon, ePolyLine, eBus, eWire, eBezier, eSignalHarness, eHarnessConnector, eLine, eBusEntry, eRectangle, eRoundRectangle,
                       eArc, eEllipticalArc, eImage, eTextFrame, eNote, eBlanket, eCompileMask, eSheetSymbol, eHighLevelCodeSymbol);
    if InSet(SourceObjId, SubSetObj) and InSet(DestinObjId, SubSetObj) then
    begin
        SchDestinPrim.LineWidth     := SchSourcePrim.LineWidth;
    end;

// ISch_GraphicalObject/ISch_Polygon & ./ISch_Rectangle
// ./ISch_ParametrizedGroup/ISch_RectangularGroup         excl eHarnessConnector
// ISch_GraphicalObject/ISch_Circle/ISch_Ellipse          excl ePie
    SubSetObj := MkSet(ePolygon, ePolyLine, eBus, eWire, eBezier, eSignalHarness, eRectangle, eRoundRectangle,
                       eEllipse, eImage, eTextFrame, eNote, eBlanket, eCompileMask);
    if InSet(SourceObjId, SubSetObj) and InSet(DestinObjId, SubSetObj) then
    begin
        SchDestinPrim.IsSolid       := SchSourcePrim.IsSolid;
        SchDestinPrim.Transparent   := SchSourcePrim.Transparent;
    end;

// ISch_GraphicalObject/ISch_Polygon/ISch_BasicPolyline; ISch_GraphicalObject/ISch_Line
    SubSetObj := MkSet(eLine, eBusEntry, ePolyLine, eBus, eWire, eBezier, eSignalHarness, eBlanket);
    if InSet(SourceObjId, SubSetObj) and InSet(DestinObjId, SubSetObj) then
    begin
        SchDestinPrim.LineStyle      := SchSourcePrim.LineStyle;
    end;

// ISch_GraphicalObject/ISch_Label
    SubSetObj := MkSet(eLabel, eCrossSheetConnector, eDesignator, eParameter, eNetlabel, ePowerObject, eSheetName, eSheetFileName);
    if InSet(SourceObjId, SubSetObj) and InSet(DestinObjId, SubSetObj) then
    begin
        SchDestinPrim.FontID        := SchSourcePrim.FontID;
        SchDestinPrim.Justification := SchSourcePrim.Justification;
        SchDestinPrim.IsMirrored    := SchSourcePrim.IsMirrored;
        SchDestinPrim.Orientation   := SchSourcePrim.Orientation;
    end;

// ISch_GraphicalObject/ISch_Label/ISch_ComplexText
    SubSetObj := MkSet(eDesignator, eParameter, eSheetName, eSheetFileName, eHarnessConnectorType);
    if InSet(SourceObjId, SubSetObj) and InSet(DestinObjId, SubSetObj) then
    begin
        SchDestinPrim.Autoposition   := SchSourcePrim.Autoposition;
        SchDestinPrim.IsHidden       := SchSourcePrim.IsHidden;
        SchDestinPrim.TextHorzAnchor := SchSourcePrim.TextHorzAnchor;
        SchDestinPrim.TextVertAnchor := SchSourcePrim.TextVertAnchor;
    end;

// ISch_GraphicalObject/ISch_Label/ISch_ComplexText/ISch_Parameter
    SubSetObj := MkSet(eDesignator, eParameter);
    if InSet(SourceObjId, SubSetObj) and InSet(DestinObjId, SubSetObj) then
    begin
        SchDestinPrim.ShowName      := SchSourcePrim.ShowName;
    end;

//  ISch_GraphicalObject/ISch_Label/ISch_PowerObject
    SubSetObj := MkSet(ePowerObject, eCrossSheetConnector);
    if InSet(SourceObjId, SubSetObj) and InSet(DestinObjId, SubSetObj) then
    begin
        SchDestinPrim.Style         := SchSourcePrim.Style;
        SchDestinPrim.ShowNetName   := SchSourcePrim.ShowNetName;
    end;

// ISch_GraphicalObject/ISch_Polygon/ISch_BasicPolyline/ISch_Wire
    SubSetObj := MkSet(eBus, eWire, eSignalHarness);
    if InSet(SourceObjId, SubSetObj) and InSet(DestinObjId, SubSetObj) then
    begin
        SchDestinPrim.UnderLineColor := SchSourcePrim.UnderLineColor;
    end;

// ISch_GraphicalObject/ISch_Rectangle/ISch_TextFrame
    SubSetObj := MkSet(eTextFrame, eNote);
    if InSet(SourceObjId, SubSetObj) and InSet(DestinObjId, SubSetObj) then
    begin
        SchDestinPrim.Alignment   := SchSourcePrim.Alignment;
        SchDestinPrim.ClipToRect  := SchSourcePrim.ClipToRect;
        SchDestinPrim.FontID      := SchSourcePrim.FontID;
        SchDestinPrim.ShowBorder  := SchSourcePrim.ShowBorder;
        if not InSet(cCntlKey, KeySet) then
            SchDestinPrim.TextColor   := SchSourcePrim.TextColor;
        SchDestinPrim.WordWrap    := SchSourcePrim.WordWrap;
    end;

// ISch_GraphicalObject/ISch_SheetEntry & ISch_HarnessEntry  & ISch_HighLevelCodeEntry
// ISch_GraphicalObject/ISch_Polygon/ISch_Wire/ ??
    SubSetObj := MkSet(eSheetEntry, eHighLevelCodeEntry, eHarnessEntry);
    if InSet(SourceObjId, SubSetObj) and InSet(DestinObjId, SubSetObj) then
    begin
      //  SchDestinPrim.IsVertical     := SchSourcePrim.IsVertical;
        if not InSet(cCntlKey, KeySet) then
            SchDestinPrim.TextColor      := SchSourcePrim.TextColor;
        SchDestinPrim.TextFontID     := SchSourcePrim.TextFontID;
        SchDestinPrim.TextStyle      := SchSourcePrim.TextStyle;
    end;


// Special format copy for non matching objects
// ISch_Label to ISch_SheetEntry or ISch_HarnessEntry
    SubSetObj  := MkSet(eLabel, eCrossSheetConnector, eDesignator, eParameter, eNetlabel, ePowerObject, eSheetName, eSheetFileName);
    SubSetObj2 := MkSet(eSheetEntry, eHarnessEntry);
    if InSet(SourceObjId, SubSetObj) and InSet(DestinObjId, SubSetObj2) then
    begin
        SchDestinPrim.TextFontID  := SchSourcePrim.FontID;
        if not InSet(cCntlKey, KeySet) then
            SchDestinPrim.TextColor   := SchSourcePrim.Color;
//        SchDestinPrim.TextStyle   := SchSourcePrim.TextStyle;
    end;

    SubSetObj := MkSet(eLabel, eCrossSheetConnector, eDesignator, eParameter, eNetlabel, ePowerObject, eSheetName, eSheetFileName);
    if InSet(DestinObjId, SubSetObj) and InSet(SourceObjId, SubSetObj2) then
    begin
        SchDestinPrim.FontID      := SchSourcePrim.TextFontID;
        if not InSet(cCntlKey, KeySet) then
            SchDestinPrim.Color       := SchSourcePrim.TextColor;
//        SchDestinPrim.TextStyle   := SchSourcePrim.TextStyle;
    end;


// Objects now must be the same ObjectId to continue.
    if SourceObjId <> DestinObjId then exit;

// ISch_GraphicalObject/ISch_ParametrizedGroup/ISch_Port & ISch_ParameterSet
// ISch_GraphicalObject/ISch_SheetEntry & ISch_HighLevelCodeEntry
    SubSetObj := MkSet(eProbe, ePort, eParameterSet, eSheetEntry, eHighLevelCodeEntry);
    if InSet(SourceObjId, SubsetObj) then
    begin
        SchDestinPrim.Style       := SchSourcePrim.Style;
    end;

    case SourceObjId of
    eJunction :      // ISch_GraphicalObject/ISch_Junction
        begin
            SchDestinPrim.Locked    := SchSourcePrim.Locked;
            SchDestinPrim.Size      := SchSourcePrim.Size;
        end;

    eProbe :         // ISch_GraphicalObject/ISch_ParametrizedGroup/ISch_ParameterSet/ISch_Probe
        begin
            SchDestinPrim.Orientation := SchSourcePrim.Orientation;
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
            SchDestinPrim.SetState_DisplayMode          (SchSourcePrim.DisplayMode);
            SchDestinPrim.SetState_IsMirrored           (SchSourcePrim.IsMirrored);
            SchDestinPrim.SetState_ComponentKind        (SchSourcePrim.ComponentKind);
            SchDestinPrim.SetState_ShowHiddenFields     (SchSourcePrim.ShowHiddenFields);
            SchDestinPrim.SetState_ShowHiddenPins       (SchSourcePrim.ShowHiddenPins);
            SchDestinPrim.Designator.SetState_ShowName  (SchSourcePrim.Designator.ShowName);
            SchDestinPrim.Comment.SetState_ShowName     (SchSourcePrim.Comment.ShowName);
            SchDestinPrim.SetState_ComponentDescription (SchSourcePrim.GetState_ComponentDescription);
            SchDestinPrim.SetState_OverideColors        (SchSourcePrim.OverideColors);
            SchDestinPrim.PinColor                    := SchSourcePrim.PinColor;
            SchDestinPrim.SetState_xSizeySize;
        end;

    // Pin - Only use in SCHLIB
    ePin :            // ISch_GraphicalObject/ISch_ParameterizedGroup/ISch_Pin
        if (DocKind = cDocKind_SchLib) then
        begin
            SchDestinPrim.ShowName       := SchSourcePrim.ShowName;
            SchDestinPrim.ShowDesignator := SchSourcePrim.ShowDesignator;
        end;

//  C Code Symbol
//    eHighLevelCodeSymbol {58}:  //       ../ISch_ParametrizedGroup/ISch_RectangularGroup/ISch_HighLevelCodeSymbol

    eSheetSymbol :    // ISch_GraphicalObject/ISch_ParametrizedGroup/ISch_RectangularGroup/ISch_SheetSymbol
        begin
            SchDestinPrim.IsSolid          := SchSourcePrim.IsSolid;           // no Transparent!
            SchDestinPrim.ShowHiddenFields := SchSourcePrim.ShowHiddenFields;
        end;
    eHarnessConnector : //                 ../ISch_ParametrizedGroup/ISch_RectangularGroup/ISch_HarnessConnector
        begin
            SchDestinPrim.MasterEntryLocation            := SchSourcePrim.MasterEntryLocation;
            SchDestinPrim.HarnessConnectorType.IsHidden  := SchSourcePrim.HarnessConnectorType.IsHidden;
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

    ePolyline :       // ISch_GraphicalObject/ISch_Polygon/ISch_BasicPolyline/ISch_Polyline
        begin
            SchDestinPrim.StartLineShape := SchSourcePrim.StartLineShape;
            SchDestinPrim.EndLineShape   := SchSourcePrim.EndLineShape;
            SchDestinPrim.LineShapeSize  := SchSourcePrim.LineShapeSize;
        end;

    // SheetSym & C Code Entry
    eSheetEntry,          // ISch_GraphicalObject/ISch_SheetEntry
    eHighLevelCodeEntry : // ISch_GraphicalObject/ISch_HighLevelCodeEntry
        begin
            SchDestinPrim.ArrowKind      := SchSourcePrim.ArrowKind;
            if not InSet(cAltKey, KeySet) then
                SchDestinPrim.HarnessColor   := SchSourcePrim.HarnessColor;
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

    eCompileMask,     // ISch_GraphicalObject/ISch_Rectangle/ISch_CompileMask
    eBlanket,         // ISch_GraphicalObject/ISch_Rectangle/ISch_Blanket
    eNote :           // ISch_GraphicalObject/ISch_Rectangle/ISch_TextFrame/ISch_Note
        begin
            SchDestinPrim.Collapsed      := SchSourcePrim.Collapsed;
            if (SourceObjId = eNote) then
                SchDestinPrim.Author        := SchSourcePrim.Author;
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
   PrimID          : TObjectId;
   HitTest         : ISch_HitTest;
   HitTestMode     : THitTestMode;
   TempSet         : TObjectset;
   I               : integer;
   Cursor          : TCursor;
//   SpatialIterator : ISch_Iterator;
   bRepeat         : boolean;
   iWeight         : integer;
   iBestWeight     : integer;
   bCycleSPrim     : boolean;

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
            SchDestinPrim.GraphicallyInvalidate;

            SchDestinPrim := nil;         // force Get Next Destination Object
        end;

//   Get Destination Object
        if SchSourcePrim <> Nil then
        begin     // allow formatting across objects ?
            ASetOfObjects := MkSet(eDesignator, eParameter, eLabel, eNetlabel, ePort, eCrossSheetConnector, eHarnessEntry, eSheetEntry);
            if not InSet(SchSourcePrim.ObjectId, ASetOfObjects) then
            begin
                ASetOfObjects := MkSet(eRectangle, eRoundRectangle, eNote, eTextFrame, eImage, eBlanket, eCompileMask);
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

//   Get Destination Object
            if (SchSourcePrim <> Nil) then
            begin
                HitTestMode := eHitTest_AllObjects;                   // eHitTest_OnlyAccessible
                HitTest := SchDoc.CreateHitTest(HitTestMode,Location);
                if (HitTest <> Nil) then
                begin
                    for I := 0 to (HitTest.HitTestCount - 1) do
                    begin
//                    if (DocKind = cDocKind_SchLib) then
//                    begin
//                        SpatialIterator.AddFilter_CurrentPartPrimitives;
//                        SpatialIterator.AddFilter_CurrentDisplayModePrimitives;
//                    end;
                        iBestWeight := 0;
                        bRepeat := false;
                        SchTempPrim := HitTest.HitObject(I);

                        Case SchTempPrim.ObjectId of
                          eDesignator, eParameter :
                          begin
                              iWeight := 5;
                              if SchTempPrim.IsHidden then  iWeight := iWeight - 3;
                          end;
                          eHarnessEntry, eSheetEntry                     : iWeight := 4;
                          eLabel, eNetlabel, ePort, eCrossSheetConnector : iWeight := 2;
                          eSchComponent, eHarnessConnector, eSheetSymbol : iWeight := 3;
                        else iWeight := 1;
                        end;

                        if SchSourcePrim.ObjectID = SchTempPrim.ObjectId then iWeight := 6;
                        if iWeight >= iBestWeight then SchDestinPrim := SchTempPrim;
                        iBestWeight := Max(iBestWeight,iWeight);
                    end;  // for i
                end;      // not nil
            end;
        end;

//   Get Source Object
        if SchSourcePrim = nil then
        begin

            boolLoc := SchDoc.ChooseLocationInteractively(Location, 'Choose Source Object');
            If Not boolLoc Then continue;

            HitTestMode := eHitTest_AllObjects;                     // eHitTest_OnlyAccessible
            HitTest := SchDoc.CreateHitTest(HitTestMode,Location);
{            if ShiftKeyDown then
            begin
//             cursor := HitTestResultToCursor(eHitTest_NoAction);   // eHitTest_CopyPaste : THitTestResult
                    SchDoc.PopupMenuHitTest := HitTest;          // last UI obj selected ??
            end;
}
            if HitTest <> Nil then
            begin
                iBestWeight := 0;

                for I := 0 to (HitTest.HitTestCount - 1) do
                begin
                    SchTempPrim := HitTest.HitObject(I);

                    Case SchTempPrim.ObjectId of
                      eDesignator, eParameter :
                      begin
                          iWeight := 5;
                          if SchTempPrim.IsHidden then  iWeight := iWeight - 3;
                      end;
                      eHarnessEntry, eSheetEntry                     : iWeight := 5;
                      eLabel, eNetlabel, ePort, eCrossSheetConnector : iWeight := 3;
                      eSchComponent, eHarnessConnector, eSheetSymbol : iWeight := 4;
                    else iWeight := 1;
                    end;

                    if iWeight >= iBestWeight then SchSourcePrim := SchTempPrim;
                    iBestWeight := Max(iBestWeight,iWeight);

                end;  // for i
            end;
        end;
    until ((SchSourcePrim = nil) and (SchDestinPrim = nil));
end;


procedure ProcessPCBPrim(SourcePrim : IPCB_Primitive, DestinPrim : IPCB_Primitive, boolLoc : boolean, KeySet : TObjectSet);
var
    Layer     : TLayer;
    SPadCache : TPadCache;
    DPadCache : TPadCache;
    PVTLink   : IPCB_PadViaTemplateLink;
    ViaTPlate : IPCB_ViaTemplate;
    PadTPlate : IPCB_PadTemplate;
    Pad       : IPCB_Pad;
    Via       : IPCB_Via;
    KORS      : TKeepoutRestrictionsSet;
    Iterator  : IPCB_LayerIterator;
//    Flag     : Integer;

begin
// No support for cross objects format copying.
// Objects now must be the same ObjectId to continue.
    if SourcePrim.ObjectId <> DestinPrim.ObjectId then exit;

    // Always use IPCB_Primitive.BeginModify instead of PCBServer.SendMessageToRobots because is deprecated (?? no citation!)
    DestinPrim.BeginModify;

    case SourcePrim.ObjectId of
    // Pads
    ePadObject :
        if (not SourcePrim.InComponent) and (not DestinPrim.InComponent) then
        begin
            Pad := SourcePrim;
            PadTPlate := DestinPrim.TemplateLink;
            Pad.TemplateLink.CopyTo(PadTPlate);

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

            Iterator := Pad.DefinitionLayerIterator;
            Iterator.AddFilter_ElectricalLayers;
            Iterator.SetBeforeFirst;
            while (Iterator.Next) do
            begin
                Layer := Iterator.Layer;
//                if DestinPrim.StackShapeOnLayer(Layer) = eNoShape then continue;

                DestinPrim.SetState_StackShapeOnLayer(Layer, Pad.StackShapeOnLayer(Layer) );
                if  Pad.Mode = ePadMode_ExternalStack then
                begin
                    DestinPrim.XStackSizeOnLayer(Layer) := Pad.XStackSizeOnLayer(Layer);
                    DestinPrim.YStackSizeOnLayer(Layer) := Pad.YStackSizeOnLayer(Layer);
                end;

                if Pad.StackShapeOnLayer(Layer) = eRoundedRectangular then  //TShape
                begin
//                    DestinPrim.CRPercentageOnLayer      := Pad.CRPercentageOnLayer;
                    DestinPrim.Setstate_StackCRPctOnLayer(Layer, Pad.StackCRPctOnLayer(Layer) );
                end;

                // DestinPrim.XPadOffset(Layer)           := Pad.XPadOffset(Layer);  This property is not implemented.
                // DestinPrim.YPadOffset(Layer)           := Pad.YPadOffset(Layer);  This property is not implemented.
            end;
            DestinPrim.InvalidateSizeShape;

            SPadCache := Pad.GetState_Cache;
            DPadCache := DestinPrim.GetState_Cache;

            DPadCache.SolderMaskExpansionValid  := eCacheManual;
            DPadCache.SolderMaskExpansion       := SPadCache.SolderMaskExpansion;
            DPadCache.UseSeparateExpansions     := SPadCache.UseSeparateExpansions;
            DPadCache.SolderMaskBottomExpansion := SPadCache.SolderMaskBottomExpansion;
            if SPadCache.SolderMaskExpansionValid = eCacheValid then DPadCache.SolderMaskExpansionValid := eCacheValid;

            DPadCache.PasteMaskExpansionValid   := eCacheManual;
            DPadCache.PasteMaskExpansion        := SPadCache.PasteMaskExpansion;
            if SPadCache.PasteMaskExpansionValid = eCacheValid then DPadCache.PasteMaskExpansionValid := eCacheValid;

            DestinPrim.SetState_Cache           := DPadCache;
            DestinPrim.PadCacheRobotFlag;
            DestinPrim.IsTenting            := Pad.IsTenting;
            DestinPrim.IsTenting_Top        := Pad.IsTenting_Top;
            DestinPrim.IsTenting_Bottom     := Pad.IsTenting_Bottom;
            Destinprim.UpdateCache;
            DestinPrim.ReValidateSizeShape;
            DestinPrim.GraphicallyInvalidate;
        end;

    // Vias
    eViaObject :
        begin
            Via := SourcePrim;
            ViaTPlate : = DestinPrim.TemplateLink;
            Via.TemplateLink.CopyTo(ViaTPlate);

            DestinPrim.Mode              := Via.Mode;
            DestinPrim.HoleSize          := Via.HoleSize;
            DestinPrim.Size              := Via.Size;

            if boolLoc = mrYes then
            begin
                if DestinPrim.HighLayer > Via.HighLayer then
                begin
                    DestinPrim.HighLayer := Via.HighLayer;
                    DestinPrim.LowLayer  := Via.LowLayer;
                end else
                begin
                    DestinPrim.LowLayer  := Via.LowLayer;
                    DestinPrim.HighLayer := Via.HighLayer;
                end;
            end;

            Iterator := Via.DefinitionLayerIterator;
            Iterator.AddFilter_ElectricalLayers;
            Iterator.SetBeforeFirst;
            while (Iterator.Next) do
            begin
                Layer := Iterator.Layer;
                if DestinPrim.IntersectLayer(Layer) then
                begin
                    DestinPrim.SetState_StackShapeOnLayer(Layer, Via.StackShapeOnLayer(Layer) );
                    DestinPrim.SizeOnLayer(Layer) := Via.SizeOnLayer(Layer);
                    DestinPrim.Setstate_StackSizeOnLayer(Layer, Via.StackSizeOnLayer(Layer) );
                end;
            end;
            SPadCache := Via.GetState_Cache;
            DPadCache := DestinPrim.GetState_Cache;
            // SPadcache.ReliefAirGap
            // SPadcache.PowerPlaneReliefExpansion
            // SPadcache.PowerPlaneClearance
            // SPadcache.ReliefConductorWidth
            DPadCache.SolderMaskExpansionValid  := eCacheManual;
            DPadCache.SolderMaskExpansion       := SPadCache.SolderMaskExpansion;
            DPadCache.UseSeparateExpansions     := SPadCache.UseSeparateExpansions;
            DPadCache.SolderMaskBottomExpansion := SPadCache.SolderMaskBottomExpansion;
            if SPadCache.SolderMaskExpansionValid = eCacheValid then DPadCache.SolderMaskExpansionValid := eCacheValid;

            // SPadcache.PasteMaskExpansion
            // SPadCache.PasteMaskExpansionValid
//            DestinPrim.PadCacheRobotFlag;
            DestinPrim.SetState_Cache       := DPadCache;
            DestinPrim.IsTenting_Top        := Via.IsTenting_Top;
            DestinPrim.IsTenting_Bottom     := Via.IsTenting_Bottom;
            DestinPrim.IsTenting            := Via.IsTenting;
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
            if DestinPrim.InDimension then
                DestinPrim.SetState_XSizeYSize;

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

     // !!! Workaround - need to fake Dimension has changed.
     // Necesary because we don't currenly have access to the Dimension method that trigger
     // a dimension update. Without that DestinPrim.SetState_XSizeYSize does nothing.
            DestinPrim.TextX              := DestinPrim.TextX + MilsToCoord(0.01);
            DestinPrim.SetState_XSizeYSize;
            DestinPrim.GraphicallyInvalidate;
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

      // !!! Workaround - need to fake Coordinate has changed.
      // see Dimension comment above.
            DestinPrim.X     := DestinPrim.X + MilsToCoord(0.01);
            DestinPrim.SetState_XSizeYSize;
            DestinPrim.GraphicallyInvalidate;
            DestinPrim.X     := DestinPrim.X - MilsToCoord(0.01);
            DestinPrim.SetState_XSizeYSize;
            DestinPrim.GraphicallyInvalidate;
        end;
    end; //case

// handle primitives with KeepOut property.
    if SourcePrim.IsKeepout then
    begin
        KORS := SourcePrim.GetState_KeepoutRestrictions;
        DestinPrim.SetState_KeepoutRestrictions(KORS);
    end;

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
    bAlwaysAsk    : boolean;
    bTempAsk      : boolean;

begin
    // Get the document
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;
{
    if Board.SelectecObjectCount > 0 then
    begin
        SourcePrim := Board.SelectecObject(0);
        SourcePrim.ObjectID;
    end;
}
    // Make it work for Pads, Vias, Strings, Polygons, Dimensions and coordinates
    ASetOfObjects  := MkSet(ePadObject, eViaObject, eTextObject, ePolyObject, eRegionObject, eDimensionObject, eCoordinateObject);

    SourcePrim := Nil;
    DestinPrim := Nil;
    boolLoc    := mrYes;      // default copy layer info
    KeySet     := MkSet();
    bFirstTime := true;
    bNeverAsk  := cNeverAsk;
    bAlwaysAsk := cAlwaysAsk;

    repeat
        // process if source & destination are selected
        if Assigned(DestinPrim) And Assigned(SourcePrim) then
        begin
            // copy formatting of PCB dimension
            PCBServer.PreProcess;
//            PCBServer.SendMessageToRobots(DestinPrim.I_ObjectAddress ,c_Broadcast, PCBM_BeginModify, c_NoEventData);

            ProcessPCBPrim(SourcePrim, DestinPrim, boolLoc, KeySet);
            DestinPrim := Nil;

//            PCBServer.SendMessageToRobots(DestinPrim.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
            PCBServer.PostProcess;
            Board.ViewManager_FullUpdate;
        end;

//    Get PCB Object
        if Assigned(SourcePrim) then
        begin
            Prompt := 'Choose Destination Primitive' + ' : ' + SourcePrim.ObjectIdString;
            if SourcePrim.ObjectID = eViaObject then
                Prompt := Prompt + ' on Layer ' + Board.LayerName(SourcePrim.LowLayer) + ' - ' + Board.LayerName(SourcePrim.HighLayer) + '  '
            else
                Prompt := Prompt + ' on Layer ' + Board.LayerName(SourcePrim.Layer) + '  ';

            DestinPrim := nGetObjectAtCursor(Board, MkSet(SourcePrim.ObjectId), SourcePrim, Prompt);
        end;

        if (DestinPrim = cESC) then SourcePrim := Nil;        //pick a new source obj

        if not Assigned(SourcePrim) then
        begin
            DestinPrim := Nil;
            repeat
               SourcePrim := nGetObjectAtCursor(Board, ASetOfObjects, Nil, 'Choose Source Primitive not in component ');
            until Assigned(SourcePrim) or (SourcePrim = cEsc);

            if Assigned(SourcePrim) and (SourcePrim <> cESC)then
            begin
                bTempAsk := bNeverAsk;
                if SourcePrim.ObjectId = ePolyObject then     // polygons more likely to be on diff layers so ask user..
                begin
                   bTempAsk  := false;
                   bFirstTime := true;
                end;
                if bAlwaysAsk or ((not bTempAsk) and bFirstTime) then
                begin
                    // supporting pad format copy without full layer handling is problematic.
                    // if SourcePrim.ObjectId = ePadObject then
                    //    Prompt := 'Pad : Copy layer info (SMD/Thru)';
                    if SourcePrim.ObjectId = ePolyObject then
                        Prompt := 'Polygon on layer : ' + Board.LayerName(SourcePrim.Layer) + '. Copy layer info ?  '
                    else if SourcePrim.ObjectId = eViaObject then
                        Prompt := 'Via between : ' + Board.LayerName(SourcePrim.LowLayer) + '-' + Board.LayerName(SourcePrim.HighLayer)
                                    + '. Copy Start/Stop Layer info ? '
                    else
                        Prompt := SourcePrim.ObjectIdString + '. Copy layer ' + Board.LayerName(SourcePrim.Layer) + ' info ?  ';

                    boolLoc := ConfirmNoYesCancel(Prompt);
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
