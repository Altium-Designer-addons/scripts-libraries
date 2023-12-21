const
    cScriptTitle    = 'PCBObjectInspector';
    cScriptVersion  = '0.31';
    cDEBUGLEVEL     = 1;
    sLineBreak2     = sLineBreak + sLineBreak;

var
    Board           : IPCB_Board;
    IsAtLeastAD19   : Boolean;
    iDebugLevel     : Integer;
    LayerStack      : IPCB_MasterLayerStack;
    sPadModeStrings : array[0..2];
    sObjectIdStrings : array[0..26];
    sComponentKindStrings : array[0..6];
    sShapeStrings : array[0..9];
    sTextAutoposition : array[0..9];
    sPolyHatchStyleStrings : array[0..5];
    sPolygonPourOverStrings : array[0..2];
    sPolygonTypeStrings : array[0..2];
    sExtendedDrillTypeStrings : array[0..3];
    sExtendedHoleTypeStrings : array[0..2];
    sPlaneConnectStyleStrings : array[0..2];
    sRegionKindStrings : array[0..4];
    sBoardSideStrings : array[0..1];
    sUnitStyleStrings : array[0..2];
    sDimensionKindStrings : array[0..10];
    sDimensionTextPositionStrings : array[0..9];
    sDimensionUnitStrings : array[0..6];
    sDimensionArrowPositionStrings : array[0..1];


procedure Inspect_IPCB_Polygon(const Obj : IPCB_Polygon; const MyLabel : string = ''); forward;
procedure Inspect_IPCB_Region(const Obj : IPCB_Region; const MyLabel : string = ''); forward;

procedure ClientNavigate(URL : WideString);
begin
    Client.SendMessage('Client:Navigate', 'Mode=Go | Address=' + URL, 255, Client.CurrentView);
end;

procedure About;
var
    MsgText : string;
begin
    MsgText := '"' + cScriptTitle + '" script version ' + cScriptVersion + sLineBreak +
        sLineBreak +
        'Use "_Inspect" to inspect the underlying properties of selected PcbDoc items.' + sLineBreak +
        sLineBreak +
        'Updated versions and documentation may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries' + sLineBreak +
        '/tree/master/Scripts - PCB/PCBObjectInspector' + sLineBreak2 +
        'Do you want to open the Script''s Github page?';

    if ConfirmNoYesWithCaption('About Script', MsgText) then
    begin
        ClientNavigate('https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts - PCB/PCBObjectInspector');
    end;
end;

function CoordToDisplayStr(coords : TCoord) : String;
const
    MAXINT = 2147483647;
    MININT = -2147483647;
begin
    if coords < MININT then coords := MININT
    else if coords > MAXINT then coords := MAXINT;

    //result := CoordUnitToString(coords, Board.DisplayUnit xor 1); // only uses 2 digits of precision for metric

    if (Board.DisplayUnit xor 1) = eImperial then Result := FloatToStr(CoordToMils(coords)) + 'mil'
    else Result := FloatToStr(CoordToMMs(coords)) + 'mm';
end;

function CoordToDisplayX(coords : TCoord) : String;
begin
    result := CoordUnitToString(coords - Board.XOrigin, Board.DisplayUnit xor 1);
end;

function CoordToDisplayY(coords : TCoord) : String;
begin
    result := CoordUnitToString(coords - Board.YOrigin, Board.DisplayUnit xor 1);
end;

function CoordToMils2(coords : TCoord) : Double;
begin
    //Result := CoordToMils(coords) + (coords mod 10) / 10000;
    Result := coords / 10000;
end;

function DebugLevelStr(dummy : String = '') : String;
begin
    Result := '-------------------------  Debug Level: ' + IntToStr(iDebugLevel) + '  -------------------------' + sLineBreak;
end;

procedure DebugMessage(const ShowLevel : Integer; const msg : WideString; const Caption : String = 'Confirm or Cancel Debug');
begin
    // iDebugLevel must be an Integer global variable initialized before first call to this procedure
    if iDebugLevel >= ShowLevel then
    begin
        msg := DebugLevelStr + msg;
        // if user clicks on Cancel, downgrade the debug level by 1 until it reaches 0
        if ConfirmOKCancelWithCaption(Caption, msg) = False then
            iDebugLevel := Max(iDebugLevel - 1, 0);
    end;
end;

function DebugContourInfo(contour : IPCB_Contour) : TStringList;
var
    PointList: TStringList;
    iPoint: Integer;
begin
    PointList := CreateObject(TStringList);
    if contour.Count > 0 then
    begin
        PointList.Add('contour points');
        for iPoint := 0 to contour.Count - 1 do
        begin
            PointList.Add(Format('%d:%s,%s', [iPoint, CoordToDisplayX(contour.x(iPoint)), CoordToDisplayY(contour.y(iPoint))]));
        end;
    end
    else PointList.Add('Empty or invalid contour');

    Result := PointList;
end;

function DebugGeometricPolygonInfo(poly : IPCB_GeometricPolygon) : TStringList;
var
    PointList: TStringList;
    contour: IPCB_Contour;
    iPoly, iPoint: Integer;
begin
    PointList := CreateObject(TStringList);
    PointList.Clear;
    if (poly <> nil) and (poly.Count > 0) then
    begin
        for iPoly := 0 to poly.Count - 1 do
        begin
            contour := poly.Contour(iPoly);
            PointList.AddStrings(DebugContourInfo(contour));
        end;
    end
    else
    begin
        PointList.Add('Polygon has no contours');
    end;

    Result := PointList;
end;

function DocumentIsPCB(dummy : Integer = 0) : Boolean;
begin
    // set AD build flag
    if not Assigned(IsAtLeastAD19) then if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;
    if not Assigned(iDebugLevel) then iDebugLevel := cDEBUGLEVEL;

    sPadModeStrings := ['ePadMode_Simple', 'ePadMode_LocalStack', 'ePadMode_ExternalStack'];
    sObjectIdStrings := ['eNoObject', 'eArcObject', 'ePadObject', 'eViaObject', 'eTrackObject', 'eTextObject', 'eFillObject', 'eConnectionObject', 'eNetObject', 'eComponentObject', 'ePolyObject', 'eRegionObject', 'eComponentBodyObject', 'eDimensionObject', 'eCoordinateObject', 'eClassObject', 'eRuleObject', 'eFromToObject', 'eDifferentialPairObject', 'eViolationObject', 'eEmbeddedObject', 'eEmbeddedBoardObject', 'eSplitPlaneObject', 'eTraceObject', 'eSpareViaObject', 'eBoardObject', 'eBoardOutlineObject'];
    sComponentKindStrings := ['eComponentKind_Standard', 'eComponentKind_Mechanical', 'eComponentKind_Graphical', 'eComponentKind_NetTie_BOM', 'eComponentKind_NetTie_NoBOM', 'eComponentKind_Standard_NoBOM', 'eComponentKind_Jumper'];
    sShapeStrings :=  ['eNoShape', 'eRounded', 'eRectangular', 'eOctagonal', 'eCircleShape', 'eArcShape', 'eTerminator', 'eRoundRectShape', 'eRotatedRectShape', 'eRoundedRectangular'];
    sTextAutoposition := ['eAutoPos_Manual', 'eAutoPos_TopLeft', 'eAutoPos_CenterLeft', 'eAutoPos_BottomLeft', 'eAutoPos_TopCenter', 'eAutoPos_CenterCenter', 'eAutoPos_BottomCenter', 'eAutoPos_TopRight', 'eAutoPos_CenterRight', 'eAutoPos_BottomRight'];
    sPolyHatchStyleStrings := ['ePolyHatch90', 'ePolyHatch45', 'ePolyVHatch', 'ePolyHHatch', 'ePolyNoHatch', 'ePolySolid'];
    sPolygonPourOverStrings := ['ePolygonPourOver_None', 'ePolygonPourOver_SameNet', 'ePolygonPourOver_SameNetPolygons'];
    sPolygonTypeStrings := ['eSignalLayerPolygon', 'eSplitPlanePolygon', 'eCoverlayOutlinePolygon'];
    sExtendedDrillTypeStrings := ['eDrilledHole', 'ePunchedHole', 'eLaserDrilledHole', 'ePlasmaDrilledHole'];
    sExtendedHoleTypeStrings := ['eRoundHole', 'eSquareHole', 'eSlotHole'];
    sPlaneConnectStyleStrings := ['eReliefConnectToPlane', 'eDirectConnectToPlane', 'eNoConnect'];
    sRegionKindStrings := ['eRegionKind_Copper', 'eRegionKind_Cutout', 'eRegionKind_NamedRegion', 'eRegionKind_BoardCutout', 'eRegionKind_Cavity'];
    sBoardSideStrings := ['eBoardSide_Top', 'eBoardSide_Bottom'];
    sUnitStyleStrings := ['eNoUnits', 'eYesUnits', 'eParenthUnits'];
    sDimensionKindStrings := ['eNoDimension', 'eLinearDimension', 'eAngularDimension', 'eRadialDimension', 'eLeaderDimension', 'eDatumDimension', 'eBaselineDimension', 'eCenterDimension', 'eOriginalDimension', 'eLinearDiameterDimension', 'eRadialDiameterDimension'];
    sDimensionTextPositionStrings := ['eTextAuto', 'eTextCenter', 'eTextTop', 'eTextBottom', 'eTextRight', 'eTextLeft', 'eTextInsideRight', 'eTextInsideLeft', 'eTextUniDirectional', 'eTextManual'];
    sDimensionUnitStrings := ['eMils', 'eInches', 'eMillimeters', 'eCentimeters', 'eDegrees', 'eRadians', 'eAutomaticUnit'];
    sDimensionArrowPositionStrings := ['eInside', 'eOutside'];

    // Checks if current document is a PCB kind if not, show error and return false.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then
    begin
        ShowError('This script must be run from a PCB document.');
        Result := False;
    end
    else Result := True;

    LayerStack := Board.MasterLayerStack;
end;

function IsConnectedToLayer(PVPrim : IPCB_Primitive; Layer : TV7_Layer) : Boolean;
var
    bExistsOnLayer, bConnected : Boolean;
    PIter               : IPCB_BoardIterator;
    GIter               : IPCB_GroupIterator;
    SIter               : IPCB_SpatialIterator;
    SpatialPrim         : IPCB_Primitive;
    BRect               : TCoordRect;
    LayerSet            : IPCB_LayerSet;
    SplitPlane          : IPCB_SplitPlane;
    SplitPlaneRegion    : IPCB_SplitPlaneRegion;
    GeoPolysTouch       : Boolean;
    PVPrimGeoPoly       : IPCB_GeometricPolygon;
    PVPrimContour       : IPCB_Contour;
begin
    Result := False;
    if PVPrim = nil then exit;
    if (PVPrim.ObjectId <> eViaObject) and (PVPrim.ObjectId <> ePadObject) then
    begin
        ShowError('IsConnectedToLayer() called on object that is not via or pad');
        exit;
    end;

    bExistsOnLayer := False;
    bConnected := False;

    if PVPrim.ObjectId = eViaObject then
    begin
        if PVPrim.IntersectLayer(Layer) and ((PVPrim.SizeOnLayer[Layer] > PVPrim.HoleSize) or LayerUtils.IsInternalPlaneLayer(Layer)) then bExistsOnLayer := true;
        //DebugMessage(1, Format('Layer %s: via intersects layer = %s; SizeOnLayer = %s', [IntToStr(Layer), BoolToStr(PVPrim.IntersectLayer(Layer), True), IntToStr(PVPrim.SizeOnLayer[Layer])]));
    end
    else if PVPrim.ObjectId = ePadObject then
    begin
        if (PVPrim.Layer = eMultiLayer) and (PVPrim.StackShapeOnLayer(Layer) <> eNoShape) and (not PVPrim.IsPadRemoved(Layer)) then bExistsOnLayer := True
        else if PVPrim.Layer = Layer then bExistsOnLayer := True;
    end;

    if not bExistsOnLayer then exit;

    // check for actual connections created by other objects on signal layer
    if LayerUtils.IsSignalLayer(Layer) then
    begin
        BRect := PVPrim.BoundingRectangle;

        //LayerSet := LayerSetUtils.Factory(Layer); // should I include multilayer as well?
        LayerSet := LayerSetUtils.Union(LayerSetUtils.Factory(Layer), LayerSetUtils.Factory(eMultiLayer)); // layer of interest or multilayer

        SIter := Board.SpatialIterator_Create;
        SIter.AddFilter_ObjectSet(MkSet(ePadObject, eArcObject, eTrackObject, eFillObject, eRegionObject, ePolyObject));
        SIter.AddFilter_Area(BRect.Left - 100, BRect.Bottom - 100, BRect.Right + 100, BRect.Top + 100);
        SIter.AddFilter_IPCB_LayerSet(LayerSet);

        SpatialPrim := SIter.FirstPCBObject;
        while (SpatialPrim <> nil) do
        begin
            if PVPrim.I_ObjectAddress = SpatialPrim.I_ObjectAddress then
            begin
                SpatialPrim := SIter.NextPCBObject;
                continue;
            end;

            if (SpatialPrim.ObjectId <> ePadObject) and (SpatialPrim.InNet) and (Board.PrimPrimDistance(SpatialPrim, PVPrim) = 0) then
            begin
                bConnected := True;
                break;
            end
            else if (SpatialPrim.ObjectId = ePadObject) and (SpatialPrim.InNet) and (SpatialPrim.ShapeOnLayer(Layer) <> eNoShape) and (Board.PrimPrimDistance(SpatialPrim, PVPrim) = 0) then
            begin
                bConnected := True;
                break;
            end
            else if (SpatialPrim.ObjectId = eRegionObject) and (SpatialPrim.Kind = eRegionKind_Copper) and (SpatialPrim.InPolygon) and (PVPrim.Net = SpatialPrim.Polygon.Net) then // polygons and their poured regions are not "InNet" for whatever reason
            begin
                //Inspect_IPCB_Region(SpatialPrim, IntToStr(Board.PrimPrimDistance(SpatialPrim, PVPrim)));

                if (Board.PrimPrimDistance(SpatialPrim, PVPrim) = 0) then
                begin
                    //Inspect_IPCB_Polygon(SpatialPrim.Polygon, IntToStr(Board.PrimPrimDistance(SpatialPrim.Polygon, PVPrim)));
                    bConnected := True;
                    break;
                end;
            end;

            SpatialPrim := SIter.NextPCBObject;
        end;
        Board.SpatialIterator_Destroy(SIter);

        if bConnected then
        begin
            Result := True;
            exit;
        end;
    end;

    // check for connections on plane layers
    if LayerUtils.IsInternalPlaneLayer(Layer) then
    begin
        //DebugMessage(1, 'if LayerUtils.IsInternalPlaneLayer(Layer) then');
        PIter := Board.BoardIterator_Create;
        PIter.AddFilter_ObjectSet(MkSet(eSplitPlaneObject));
        PIter.AddFilter_IPCB_LayerSet(LayerSetUtils.Factory(Layer));

        SplitPlane := PIter.FirstPCBObject;
        while (SplitPlane <> nil) do
        begin
            if not ((SplitPlane.Net <> nil) and (SplitPlane.Net = PVPrim.Net)) then
            begin
                SplitPlane := PIter.NextPCBObject;
                continue;
            end;

            GIter := SplitPlane.GroupIterator_Create;
            GIter.AddFilter_IPCB_LayerSet(LayerSetUtils.Factory(Layer));
            GIter.AddFilter_ObjectSet(MkSet(eRegionObject));
            SplitPlaneRegion := GIter.FirstPCBObject;
            while SplitPlaneRegion <> nil do
            begin
                //Inspect_IPCB_Region(SplitPlaneRegion, Format('HitPrimitive: %s; PrimitiveInsidePoly: %s; StrictHitTest: %s', [BoolToStr(SplitPlane.GetState_HitPrimitive(PVPrim), True), BoolToStr(SplitPlane.PrimitiveInsidePoly(PVPrim), True), BoolToStr(SplitPlane.GetState_StrictHitTest(PVPrim.x, PVPrim.y), True)]));

                //if Board.PrimPrimDistance(SplitPlaneRegion, PVPrim) = 0 then // doesn't work correctly with splitplaneregion to detect if primitive actually touches plane
                //begin
                    //bConnected := True;
                    //break;
                //end;

                if SplitPlane.GetState_HitPrimitive(PVPrim) and (PVPrim.PowerPlaneConnectStyle <> eNoConnect) then // unfortunately GetState_HitPrimitive and similar functions only tell when plane overlaps the primitive
                begin
                    // use PCBContourUtilities.GeometricPolygonsTouch to determine if there is actually a connection on layer. Likely not very performant but PrimPrimDistance does not seem to work for SplitPlaneRegions.
                    // just eliminate as many factors as possible before checking at this level
                    if PVPrim.PowerPlaneConnectStyle = eDirectConnectToPlane then
                    begin
                        //PVPrimGeoPoly := PCBServer.PCBContourMaker.MakeContour(PVPrim, 100, Layer) // using with eDirectConnectToPlane will return empty GeometricPolygon

                        // create a new polygon from scratch. A simple diamond inside the hole should do.
                        PVPrimGeoPoly := PCBServer.PCBGeometricPolygonFactory;
                        PVPrimGeoPoly.AddEmptyContour;
                        PVPrimContour := PVPrimGeoPoly.Contour(0);
                        PVPrimContour.AddPoint(PVPrim.x - (PVPrim.HoleSize div 2)   , PVPrim.y);
                        PVPrimContour.AddPoint(PVPrim.x                             , PVPrim.y + (PVPrim.HoleSize div 2));
                        PVPrimContour.AddPoint(PVPrim.x + (PVPrim.HoleSize div 2)   , PVPrim.y);
                        PVPrimContour.AddPoint(PVPrim.x                             , PVPrim.y - (PVPrim.HoleSize div 2));
                    end
                    else PVPrimGeoPoly := PCBServer.PCBContourMaker.MakeContour(PVPrim, -PVPrim.PowerPlaneClearance, Layer);

                    //DebugMessage(1, DebugGeometricPolygonInfo(PVPrimGeoPoly).Text);
                    //DebugMessage(1, DebugGeometricPolygonInfo(SplitPlaneRegion.GetGeometricPolygon).Text); // AHHHHHHH!!!!!!

                    GeoPolysTouch := PCBServer.PCBContourUtilities.GeometricPolygonsTouch(SplitPlaneRegion.GetGeometricPolygon, PVPrimGeoPoly);

                    //DebugMessage(1, 'PrimPrimDistance = ' + IntToStr(Board.PrimPrimDistance(SplitPlaneRegion, PVPrim)) + sLineBreak2 + 'GeometricPolygonsTouch = ' + BoolToStr(GeoPolysTouch, True));
                    if GeoPolysTouch then
                    begin
                        bConnected := True;
                        break;
                    end;
                end;

                SplitPlaneRegion := GIter.NextPCBObject;
            end;
            SplitPlane.GroupIterator_Destroy(GIter);
            SplitPlane := PIter.NextPCBObject;

            if bConnected then break; // stop iterating planes after connection found
        end;
        Board.BoardIterator_Destroy(PIter);

        if bConnected then
        begin
            Result := True;
            exit;
        end;
    end;
end;

function GetConnectedLayersList(PVPrim : IPCB_Primitive) : TInterfaceList;
var
    idx         : Integer;
    LayerObject : IPCB_LayerObject;
begin
    Result := CreateObject(TInterfaceList);

    LayerObject := LayerStack.First(eLayerClass_Electrical);
    while LayerObject <> nil do
    begin
        if IsConnectedToLayer(PVPrim, LayerObject.V7_LayerID.ID) then Result.Add(LayerObject);

        LayerObject := LayerStack.Next(eLayerClass_Electrical, LayerObject);
    end;

    //LayerObject := LayerStack.LayerObject(eMultiLayer);
    //if IsConnectedToLayer(PVPrim, LayerObject.V7_LayerID.ID) then Result.Add(LayerObject);
end;

function GetConnectedLayersInfo(PVPrim : IPCB_Primitive) : String;
var
    idx                 : Integer;
    LayerObject         : IPCB_LayerObject;
    LayerList           : TInterfaceList;
begin
    Result := '';

    LayerList := GetConnectedLayersList(PVPrim);

    if LayerList = nil then exit;
    if LayerList.Count = 0 then exit;

    LayerObject := LayerList[0];
    Result := LayerObject.Name;

    for idx := 1 to LayerList.Count - 1 do
    begin
        LayerObject := LayerList[idx];
        if LayerObject <> nil then Result := Result + ', ' + LayerObject.Name
    end;
end;

function GetLayerInfo(LayerObject : IPCB_LayerObject) : String;
begin
    if LayerObject = nil then
    begin
        Result := 'LayerObject is nil';
        exit;
    end;

    Result := 'Layer: ' + LayerObject.Name + ' (' + Layer2String(LayerObject.LayerID) + ')' + ', ' +
            FloatToStr(LayerObject.CopperThickness / 10000) + 'mil Cu' + sLineBreak;

    if LayerObject.Dielectric.DielectricType <> eNoDielectric then
    begin
        Result := Result + 'Dielectric below: ' + FloatToStr(LayerObject.Dielectric.DielectricHeight / 10000) + 'mil ' +
                ConvertDielectricTypeTOString(LayerObject.Dielectric.DielectricType) + sLineBreak +
                LayerObject.Dielectric.DielectricMaterial +  ', Dk=' +
                FloatToStr(LayerObject.Dielectric.DielectricConstant);
    end;
end;

function GetLayerDistance(FromLayerObj, ToLayerObj : IPCB_LayerObject) : TCoord;
begin
end;

function VarTypeString(arg) : String;
begin
    case VarType(arg) of
        0 : Result := 'varEmpty';
        1 : Result := 'varNull';
        2 : Result := 'varSmallInt';
        3 : Result := 'varInteger (int32)';
        4 : Result := 'varSingle';
        5 : Result := 'varDouble';
        11 : Result := 'varBoolean';
        20 : Result := 'varInt64';
        else Result := 'missing case (incomplete list)';
    end;
end;

procedure Inspect_IPCB_Arc(const Obj : IPCB_Track; const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    if Obj.Net <> nil then AD19DebugStr := Format('%s : %s', ['Net.Name', Obj.Net.Name]) + sLineBreak else AD19DebugStr := '';

    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------------------------------------' + sLineBreak +
                AD19DebugStr +
                Format('%s : %s', ['XCenter', IntToStr(Obj.XCenter)]) + sLineBreak +
                Format('%s : %s', ['YCenter', IntToStr(Obj.YCenter)]) + sLineBreak +
                Format('%s : %s', ['Radius', IntToStr(Obj.Radius)]) + sLineBreak +
                Format('%s : %s', ['LineWidth', IntToStr(Obj.LineWidth)]) + sLineBreak +
                Format('%s : %s', ['StartAngle', FloatToStr(Obj.StartAngle)]) + sLineBreak +
                Format('%s : %s', ['EndAngle', FloatToStr(Obj.EndAngle)]) + sLineBreak +
                Format('%s : %s', ['StartX', IntToStr(Obj.StartX)]) + sLineBreak +
                Format('%s : %s', ['StartY', IntToStr(Obj.StartY)]) + sLineBreak +
                Format('%s : %s', ['EndX', IntToStr(Obj.EndX)]) + sLineBreak +
                Format('%s : %s', ['EndY', IntToStr(Obj.EndY)]) + sLineBreak +
                Format('%s : %s', ['ObjectId', sObjectIdStrings[Obj.ObjectId]]) + sLineBreak +
                Format('%s : %s (%s)', ['Layer', Layer2String(Obj.Layer)]) + sLineBreak +
                Format('%s : %s', ['DRCError', BoolToStr(Obj.DRCError, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag1', BoolToStr(Obj.MiscFlag1, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag2', BoolToStr(Obj.MiscFlag2, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag3', BoolToStr(Obj.MiscFlag3, True)]) + sLineBreak +
                Format('%s : %s', ['IsKeepout', BoolToStr(Obj.IsKeepout, True)]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString', Obj.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['Identifier', Obj.Identifier]) + sLineBreak +
                Format('%s : %s', ['Descriptor', Obj.Descriptor]) + sLineBreak +
                Format('%s : %s', ['Detail', Obj.Detail]) + sLineBreak
                , 'Confirm IPCB_Arc Info (partial)')
end;

procedure Inspect_IPCB_Component(const Obj : IPCB_Component; const MyLabel : string = '');
begin
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------------------------------------' + sLineBreak +
                Format('%s : %s', ['AxisCount', IntToStr(Obj.AxisCount)]) + sLineBreak +
                Format('%s : %s', ['ChannelOffset', IntToStr(Obj.ChannelOffset)]) + sLineBreak +
                Format('%s : %s', ['ComponentKind:TComponentKind', sComponentKindStrings[Obj.ComponentKind]]) + sLineBreak +
                Format('%s : %s', ['Pattern', Obj.Pattern]) + sLineBreak +
                Format('%s : %s', ['NameOn', BoolToStr(Obj.NameOn, True)]) + sLineBreak +
                Format('%s : %s', ['CommentOn', BoolToStr(Obj.CommentOn, True)]) + sLineBreak +
                Format('%s : %s', ['LockStrings', BoolToStr(Obj.LockStrings, True)]) + sLineBreak +
                Format('%s : %s', ['GroupNum', IntToStr(Obj.GroupNum)]) + sLineBreak +
                Format('%s : %s', ['Rotation', FloatToStr(Obj.Rotation)]) + sLineBreak +
                Format('%s : %s', ['Height', IntToStr(Obj.Height)]) + sLineBreak +
                Format('%s : %s', ['NameAutoPosition:TTextAutoposition', sTextAutoposition[Obj.NameAutoPosition]]) + sLineBreak +
                Format('%s : %s', ['CommentAutoPosition:TTextAutoposition', sTextAutoposition[Obj.CommentAutoPosition]]) + sLineBreak +
                Format('%s : %s', ['SourceDesignator', Obj.SourceDesignator]) + sLineBreak +
                Format('%s : %s', ['SourceUniqueId', Obj.SourceUniqueId]) + sLineBreak +
                Format('%s : %s', ['SourceHierarchicalPath', Obj.SourceHierarchicalPath]) + sLineBreak +
                Format('%s : %s', ['SourceFootprintLibrary', Obj.SourceFootprintLibrary]) + sLineBreak +
                Format('%s : %s', ['SourceComponentLibrary', Obj.SourceComponentLibrary]) + sLineBreak +
                Format('%s : %s', ['SourceLibReference', Obj.SourceLibReference]) + sLineBreak +
                Format('%s : %s', ['SourceCompDesignItemID', Obj.SourceCompDesignItemID]) + sLineBreak +
                Format('%s : %s', ['SourceDescription', Obj.SourceDescription]) + sLineBreak +
                Format('%s : %s', ['FootprintDescription', Obj.FootprintDescription]) + sLineBreak +
                Format('%s : %s', ['DefaultPCB3DModel', Obj.DefaultPCB3DModel]) + sLineBreak +
                Format('%s : %s', ['IsBGA', BoolToStr(Obj.IsBGA, True)]) + sLineBreak +
                Format('%s : %s', ['EnablePinSwapping', BoolToStr(Obj.EnablePinSwapping, True)]) + sLineBreak +
                Format('%s : %s', ['EnablePartSwapping', BoolToStr(Obj.EnablePartSwapping, True)]) + sLineBreak +
                Format('%s : %s', ['FootprintConfigurableParameters_Encoded', Obj.FootprintConfigurableParameters_Encoded]) + sLineBreak +
                Format('%s : %s', ['FootprintConfiguratorName', Obj.FootprintConfiguratorName]) + sLineBreak +
                Format('%s : %s', ['VaultGUID', Obj.VaultGUID]) + sLineBreak +
                Format('%s : %s', ['ItemGUID', Obj.ItemGUID]) + sLineBreak +
                Format('%s : %s', ['ItemRevisionGUID', Obj.ItemRevisionGUID]) + sLineBreak +
                Format('%s : %s', ['FlippedOnLayer', BoolToStr(Obj.FlippedOnLayer, True)]) + sLineBreak +
                Format('%s : %s', ['JumpersVisible', BoolToStr(Obj.JumpersVisible, True)]) + sLineBreak +
                Format('%s : %s', ['x', IntToStr(Obj.x)]) + sLineBreak +
                Format('%s : %s', ['y', IntToStr(Obj.y)]) + sLineBreak +
                Format('%s : %s', ['PrimitiveLock', BoolToStr(Obj.PrimitiveLock, True)]) + sLineBreak +
                Format('%s : %s', ['ObjectId', sObjectIdStrings[Obj.ObjectId]]) + sLineBreak +
                Format('%s : %s', ['Layer', Layer2String(Obj.Layer)]) + sLineBreak +
                Format('%s : %s', ['DRCError', BoolToStr(Obj.DRCError, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag1', BoolToStr(Obj.MiscFlag1, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag2', BoolToStr(Obj.MiscFlag2, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag3', BoolToStr(Obj.MiscFlag3, True)]) + sLineBreak +
                Format('%s : %s', ['Moveable', BoolToStr(Obj.Moveable, True)]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString', Obj.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['Identifier', Obj.Identifier]) + sLineBreak +
                Format('%s : %s', ['Descriptor', Obj.Descriptor]) + sLineBreak +
                Format('%s : %s', ['Detail', Obj.Detail]) + sLineBreak +
                Format('%s : %s', ['UniqueId', Obj.UniqueId])
                , 'Confirm IPCB_Component Info (partial)')
end;

procedure Inspect_IPCB_ComponentBody(const Obj : IPCB_ComponentBody; const MyLabel : string = '');
begin
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------------------------------------' + sLineBreak +
            Format('%s : %s', ['ModelHasChanged', BoolToStr(Obj.ModelHasChanged, True)]) + sLineBreak +
            Format('%s : %s', ['StandoffHeight', IntToStr(Obj.StandoffHeight)]) + sLineBreak +
            Format('%s : %s', ['OverallHeight', IntToStr(Obj.OverallHeight)]) + sLineBreak +
            Format('%s : %s', ['BodyProjection:TBoardSide', sBoardSideStrings[Obj.BodyProjection]]) + sLineBreak +
            Format('%s : %s', ['BodyColor3D', IntToStr(Obj.BodyColor3D)]) + sLineBreak +
            Format('%s : %s', ['BodyOpacity3D', IntToStr(Obj.BodyOpacity3D)]) + sLineBreak +
            Format('%s : %s', ['Texture', Obj.Texture]) + sLineBreak +
            Format('%s : %s', ['TextureCenter', IntToStr(Obj.TextureCenter)]) + sLineBreak +
            Format('%s : %s', ['TextureSize', IntToStr(Obj.TextureSize)]) + sLineBreak +
            Format('%s : %s', ['TextureRotation', FloatToStr(Obj.TextureRotation)]) + sLineBreak +
            Format('%s : %s', ['OverrideColor', BoolToStr(Obj.OverrideColor, True)]) + sLineBreak +
            Format('%s : %s', ['Kind:TRegionKind', sRegionKindStrings[Obj.Kind]]) + sLineBreak +
            Format('%s : %s', ['Name', Obj.Name]) + sLineBreak +
            Format('%s : %s', ['HoleCount', IntToStr(Obj.HoleCount)]) + sLineBreak +
            Format('%s : %s', ['Area', IntToStr(Obj.Area)]) + sLineBreak +
            Format('%s : %s', ['CavityHeight', IntToStr(Obj.CavityHeight)]) + sLineBreak +
            Format('%s : %s', ['ObjectId', sObjectIdStrings[Obj.ObjectId]]) + sLineBreak +
            Format('%s : %s', ['Layer', Layer2String(Obj.Layer)]) + sLineBreak +
            Format('%s : %s', ['DRCError', BoolToStr(Obj.DRCError, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag1', BoolToStr(Obj.MiscFlag1, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag2', BoolToStr(Obj.MiscFlag2, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag3', BoolToStr(Obj.MiscFlag3, True)]) + sLineBreak +
            Format('%s : %s', ['EnableDraw', BoolToStr(Obj.EnableDraw, True)]) + sLineBreak +
            Format('%s : %s', ['Moveable', BoolToStr(Obj.Moveable, True)]) + sLineBreak +
            Format('%s : %s', ['InBoard', BoolToStr(Obj.InBoard, True)]) + sLineBreak +
            Format('%s : %s', ['InComponent', BoolToStr(Obj.InComponent, True)]) + sLineBreak +
            Format('%s : %s', ['ObjectIDString', Obj.ObjectIDString]) + sLineBreak +
            Format('%s : %s', ['Identifier', Obj.Identifier]) + sLineBreak +
            Format('%s : %s', ['Descriptor', Obj.Descriptor]) + sLineBreak +
            Format('%s : %s', ['Detail', Obj.Detail]) + sLineBreak +
            Format('%s : %s', ['UniqueId', Obj.UniqueId])
            , 'Confirm IPCB_ComponentBody Info (partial)')
end;

procedure Inspect_IPCB_Dimension(const Obj : IPCB_Dimension; const MyLabel : string = '');
begin
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------------------------------------' + sLineBreak +
            Format('%s : %s', ['DimensionKind:TDimensionKind', sDimensionKindStrings[Obj.DimensionKind]]) + sLineBreak +
            Format('%s : %s', ['TextX', IntToStr(Obj.TextX)]) + sLineBreak +
            Format('%s : %s', ['TextY', IntToStr(Obj.TextY)]) + sLineBreak +
            Format('%s : %s', ['X1Location', IntToStr(Obj.X1Location)]) + sLineBreak +
            Format('%s : %s', ['Y1Location', IntToStr(Obj.Y1Location)]) + sLineBreak +
            Format('%s : %s', ['Size', IntToStr(Obj.Size)]) + sLineBreak +
            Format('%s : %s', ['LineWidth', IntToStr(Obj.LineWidth)]) + sLineBreak +
            Format('%s : %s', ['TextHeight', IntToStr(Obj.TextHeight)]) + sLineBreak +
            Format('%s : %s', ['TextWidth', IntToStr(Obj.TextWidth)]) + sLineBreak +
            Format('%s : %s', ['TextLineWidth', IntToStr(Obj.TextLineWidth)]) + sLineBreak +
            Format('%s : %s', ['TextPosition:TDimensionTextPosition', sDimensionTextPositionStrings[Obj.TextPosition]]) + sLineBreak +
            Format('%s : %s', ['TextGap', IntToStr(Obj.TextGap)]) + sLineBreak +
            Format('%s : %s', ['TextFormat', Obj.TextFormat]) + sLineBreak +
            Format('%s : %s', ['TextDimensionUnit:TDimensionUnit', sDimensionUnitStrings[Obj.TextDimensionUnit]]) + sLineBreak +
            Format('%s : %s', ['TextPrecision', IntToStr(Obj.TextPrecision)]) + sLineBreak +
            Format('%s : %s', ['TextPrefix', Obj.TextPrefix]) + sLineBreak +
            Format('%s : %s', ['TextSuffix', Obj.TextSuffix]) + sLineBreak +
            Format('%s : %s', ['TextValue', FloatToStr(Obj.TextValue)]) + sLineBreak +
            Format('%s : %s', ['ArrowSize', IntToStr(Obj.ArrowSize)]) + sLineBreak +
            Format('%s : %s', ['ArrowLineWidth', IntToStr(Obj.ArrowLineWidth)]) + sLineBreak +
            Format('%s : %s', ['ArrowLength', IntToStr(Obj.ArrowLength)]) + sLineBreak +
            Format('%s : %s', ['ArrowPosition:TDimensionArrowPosition', sDimensionArrowPositionStrings[Obj.ArrowPosition]]) + sLineBreak +
            Format('%s : %s', ['ExtensionOffset', IntToStr(Obj.ExtensionOffset)]) + sLineBreak +
            Format('%s : %s', ['ExtensionLineWidth', IntToStr(Obj.ExtensionLineWidth)]) + sLineBreak +
            Format('%s : %s', ['ExtensionPickGap', IntToStr(Obj.ExtensionPickGap)]) + sLineBreak +
            Format('%s : %s', ['Style:TUnitStyle', sUnitStyleStrings[Obj.Style]]) + sLineBreak +
            Format('%s : %s', ['References_Count', IntToStr(Obj.References_Count)]) + sLineBreak +
            Format('%s : %s', ['UseTTFonts', BoolToStr(Obj.UseTTFonts, True)]) + sLineBreak +
            Format('%s : %s', ['Bold', BoolToStr(Obj.Bold, True)]) + sLineBreak +
            Format('%s : %s', ['Italic', BoolToStr(Obj.Italic, True)]) + sLineBreak +
            Format('%s : %s', ['FontName', Obj.FontName]) + sLineBreak +
            Format('%s : %s', ['x', IntToStr(Obj.x)]) + sLineBreak +
            Format('%s : %s', ['y', IntToStr(Obj.y)]) + sLineBreak +
            Format('%s : %s', ['PrimitiveLock', BoolToStr(Obj.PrimitiveLock, True)]) + sLineBreak +
            Format('%s : %s', ['ObjectId', sObjectIdStrings[Obj.ObjectId]]) + sLineBreak +
            Format('%s : %s', ['Layer', Layer2String(Obj.Layer)]) + sLineBreak +
            Format('%s : %s', ['DRCError', BoolToStr(Obj.DRCError, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag1', BoolToStr(Obj.MiscFlag1, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag2', BoolToStr(Obj.MiscFlag2, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag3', BoolToStr(Obj.MiscFlag3, True)]) + sLineBreak +
            Format('%s : %s', ['Moveable', BoolToStr(Obj.Moveable, True)]) + sLineBreak +
            Format('%s : %s', ['InComponent', BoolToStr(Obj.InComponent, True)]) + sLineBreak +
            Format('%s : %s', ['ObjectIDString', Obj.ObjectIDString]) + sLineBreak +
            Format('%s : %s', ['Identifier', Obj.Identifier]) + sLineBreak +
            Format('%s : %s', ['Descriptor', Obj.Descriptor]) + sLineBreak +
            Format('%s : %s', ['Detail', Obj.Detail]) + sLineBreak +
            Format('%s : %s', ['UniqueId', Obj.UniqueId])
            , 'Confirm IPCB_Dimension Info (partial)')
end;

procedure Inspect_IPCB_Fill(const Obj : IPCB_Fill; const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    if Obj.Net <> nil then AD19DebugStr := Format('%s : %s', ['Net.Name', Obj.Net.Name]) + sLineBreak else AD19DebugStr := '';

    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------------------------------------' + sLineBreak +
                AD19DebugStr +
                Format('%s : %s', ['XLocation', IntToStr(Obj.XLocation)]) + sLineBreak +
                Format('%s : %s', ['YLocation', IntToStr(Obj.YLocation)]) + sLineBreak +
                Format('%s : %s', ['X1Location', IntToStr(Obj.X1Location)]) + sLineBreak +
                Format('%s : %s', ['Y1Location', IntToStr(Obj.Y1Location)]) + sLineBreak +
                Format('%s : %s', ['X2Location', IntToStr(Obj.X2Location)]) + sLineBreak +
                Format('%s : %s', ['Y2Location', IntToStr(Obj.Y2Location)]) + sLineBreak +
                Format('%s : %s', ['Rotation', FloatToStr(Obj.Rotation)]) + sLineBreak +
                Format('%s : %s', ['ObjectId', sObjectIdStrings[Obj.ObjectId]]) + sLineBreak +
                Format('%s : %s', ['Layer', Layer2String(Obj.Layer)]) + sLineBreak +
                Format('%s : %s', ['DRCError', BoolToStr(Obj.DRCError, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag1', BoolToStr(Obj.MiscFlag1, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag2', BoolToStr(Obj.MiscFlag2, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag3', BoolToStr(Obj.MiscFlag3, True)]) + sLineBreak +
                Format('%s : %s', ['Moveable', BoolToStr(Obj.Moveable, True)]) + sLineBreak +
                Format('%s : %s', ['IsKeepout', BoolToStr(Obj.IsKeepout, True)]) + sLineBreak +
                Format('%s : %s', ['InComponent', BoolToStr(Obj.InComponent, True)]) + sLineBreak +
                Format('%s : %s', ['InNet', BoolToStr(Obj.InNet, True)]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString', Obj.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['Identifier', Obj.Identifier]) + sLineBreak +
                Format('%s : %s', ['Descriptor', Obj.Descriptor]) + sLineBreak +
                Format('%s : %s', ['Detail', Obj.Detail]) + sLineBreak +
                Format('%s : %s', ['PasteMaskExpansion', IntToStr(Obj.PasteMaskExpansion)]) + sLineBreak +
                Format('%s : %s', ['SolderMaskExpansion', IntToStr(Obj.SolderMaskExpansion)]) + sLineBreak +
                Format('%s : %s', ['PowerPlaneClearance', IntToStr(Obj.PowerPlaneClearance)]) + sLineBreak +
                Format('%s : %s', ['PowerPlaneReliefExpansion', IntToStr(Obj.PowerPlaneReliefExpansion)])
                , 'Confirm IPCB_Fill Info (partial)')
end;

{ IPCB_LayerObject is not a primitive type so doesn't have an ObjectId, but I'm leaving this here for possible future use}
procedure Inspect_IPCB_LayerObject(const LayerObject : IPCB_LayerObject, const MyLabel : string = '');
var
    LayerInfo : String;
begin
    if LayerObject = nil then exit;
    LayerInfo := GetLayerInfo(LayerObject);
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------------------------------------' + sLineBreak +
            Format('%s : %s', ['ObjectIDString',  LayerObject.ObjectIDString]) + sLineBreak +
            LayerInfo + sLineBreak
            , 'Confirm IPCB_LayerObject Info (partial)')
end;

procedure Inspect_IPCB_Pad(const Obj : IPCB_Pad4; const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    AD19DebugStr := Format('%s : %s', ['Pad is connected on layers', GetConnectedLayersInfo(Obj)]) + sLineBreak2 +
            Format('%s : %s', ['MaxXSignalLayers', IntToStr(Obj.MaxXSignalLayers)]) + sLineBreak +
            Format('%s : %s', ['MaxYSignalLayers', IntToStr(Obj.MaxYSignalLayers)]) + sLineBreak +
            Format('%s : %s', ['PinPackageLength', IntToStr(Obj.PinPackageLength)]) + sLineBreak +
            Format('%s : %s', ['XPadOffsetAll', IntToStr(Obj.XPadOffsetAll)]) + sLineBreak +
            Format('%s : %s', ['YPadOffsetAll', IntToStr(Obj.YPadOffsetAll)]) + sLineBreak;

    if Obj.Net <> nil then AD19DebugStr := AD19DebugStr + Format('%s : %s', ['Net.Name', Obj.Net.Name]) + sLineBreak;

    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------------------------------------' + sLineBreak +
            AD19DebugStr +
            Format('%s : %s', ['Net.Name', Obj.Net.Name]) + sLineBreak +
            Format('%s : %s', ['x', IntToStr(Obj.x)]) + sLineBreak +
            Format('%s : %s', ['y', IntToStr(Obj.y)]) + sLineBreak +
            Format('%s : %s', ['PinDescriptor', Obj.PinDescriptor]) + sLineBreak +
            Format('%s : %s', ['Mode:TPadMode', sPadModeStrings[Obj.Mode]]) + sLineBreak +
            Format('%s : %s', ['TopXSize', IntToStr(Obj.TopXSize)]) + sLineBreak +
            Format('%s : %s', ['TopYSize', IntToStr(Obj.TopYSize)]) + sLineBreak +
            Format('%s : %s', ['MidXSize', IntToStr(Obj.MidXSize)]) + sLineBreak +
            Format('%s : %s', ['MidYSize', IntToStr(Obj.MidYSize)]) + sLineBreak +
            Format('%s : %s', ['BotXSize', IntToStr(Obj.BotXSize)]) + sLineBreak +
            Format('%s : %s', ['BotYSize', IntToStr(Obj.BotYSize)]) + sLineBreak +
            Format('%s : %s', ['TopShape:TShape', sShapeStrings[Obj.TopShape]]) + sLineBreak +
            Format('%s : %s', ['MidShape:TShape', sShapeStrings[Obj.MidShape]]) + sLineBreak +
            Format('%s : %s', ['BotShape:TShape', sShapeStrings[Obj.BotShape]]) + sLineBreak +
            Format('%s : %s', ['HoleSize', IntToStr(Obj.HoleSize)]) + sLineBreak +
            Format('%s : %s', ['Rotation', FloatToStr(Obj.Rotation)]) + sLineBreak +
            Format('%s : %s', ['Name', Obj.Name]) + sLineBreak +
            Format('%s : %s', ['SwapID_Pad', Obj.SwapID_Pad]) + sLineBreak +
            Format('%s : %s', ['SwapID_Part', Obj.SwapID_Part]) + sLineBreak +
            Format('%s : %s', ['OwnerPart_ID', IntToStr(Obj.OwnerPart_ID)]) + sLineBreak +
            Format('%s : %s', ['SwappedPadName', Obj.SwappedPadName]) + sLineBreak +
            Format('%s : %s', ['Plated', BoolToStr(Obj.Plated, True)]) + sLineBreak +
            Format('%s : %s', ['DrillType:TExtendedDrillType', sExtendedDrillTypeStrings[Obj.DrillType]]) + sLineBreak +
            Format('%s : %s', ['HoleType:TExtendedHoleType', sExtendedHoleTypeStrings[Obj.HoleType]]) + sLineBreak +
            Format('%s : %s', ['HoleWidth', IntToStr(Obj.HoleWidth)]) + sLineBreak +
            Format('%s : %s', ['HoleRotation', FloatToStr(Obj.HoleRotation)]) + sLineBreak +
            Format('%s : %s', ['JumperID', IntToStr(Obj.JumperID)]) + sLineBreak +
            Format('%s : %s', ['SolderMaskExpansionFromHoleEdge', BoolToStr(Obj.SolderMaskExpansionFromHoleEdge, True)]) + sLineBreak +
            Format('%s : %s', ['HolePositiveTolerance', IntToStr(Obj.HolePositiveTolerance)]) + sLineBreak +
            Format('%s : %s', ['HoleNegativeTolerance', IntToStr(Obj.HoleNegativeTolerance)]) + sLineBreak +
            Format('%s : %s', ['ObjectId', sObjectIdStrings[Obj.ObjectId]]) + sLineBreak +
            Format('%s : %s', ['Layer', Layer2String(Obj.Layer)]) + sLineBreak +
            Format('%s : %s', ['DRCError', BoolToStr(Obj.DRCError, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag1', BoolToStr(Obj.MiscFlag1, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag2', BoolToStr(Obj.MiscFlag2, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag3', BoolToStr(Obj.MiscFlag3, True)]) + sLineBreak +
            Format('%s : %s', ['IsTenting', BoolToStr(Obj.IsTenting, True)]) + sLineBreak +
            Format('%s : %s', ['IsTenting_Top', BoolToStr(Obj.IsTenting_Top, True)]) + sLineBreak +
            Format('%s : %s', ['IsTenting_Bottom', BoolToStr(Obj.IsTenting_Bottom, True)]) + sLineBreak +
            Format('%s : %s', ['IsTestpoint_Top', BoolToStr(Obj.IsTestpoint_Top, True)]) + sLineBreak +
            Format('%s : %s', ['IsTestpoint_Bottom', BoolToStr(Obj.IsTestpoint_Bottom, True)]) + sLineBreak +
            Format('%s : %s', ['IsAssyTestpoint_Top', BoolToStr(Obj.IsAssyTestpoint_Top, True)]) + sLineBreak +
            Format('%s : %s', ['IsAssyTestpoint_Bottom', BoolToStr(Obj.IsAssyTestpoint_Bottom, True)]) + sLineBreak +
            Format('%s : %s', ['IsKeepout', BoolToStr(Obj.IsKeepout, True)]) + sLineBreak +
            Format('%s : %s', ['InComponent', BoolToStr(Obj.InComponent, True)]) + sLineBreak +
            Format('%s : %s', ['InNet', BoolToStr(Obj.InNet, True)]) + sLineBreak +
            Format('%s : %s', ['ObjectIDString', Obj.ObjectIDString]) + sLineBreak +
            Format('%s : %s', ['Identifier', Obj.Identifier]) + sLineBreak +
            Format('%s : %s', ['Descriptor', Obj.Descriptor]) + sLineBreak +
            Format('%s : %s', ['Detail', Obj.Detail]) + sLineBreak +
            Format('%s : %s', ['PowerPlaneConnectStyle:TPlaneConnectStyle', sPlaneConnectStyleStrings[Obj.PowerPlaneConnectStyle]]) + sLineBreak +
            Format('%s : %s', ['ReliefConductorWidth', IntToStr(Obj.ReliefConductorWidth)]) + sLineBreak +
            Format('%s : %s', ['ReliefEntries', IntToStr(Obj.ReliefEntries)]) + sLineBreak +
            Format('%s : %s', ['ReliefAirGap', IntToStr(Obj.ReliefAirGap)]) + sLineBreak +
            Format('%s : %s', ['PasteMaskExpansion', IntToStr(Obj.PasteMaskExpansion)]) + sLineBreak +
            Format('%s : %s', ['SolderMaskExpansion', IntToStr(Obj.SolderMaskExpansion)]) + sLineBreak +
            Format('%s : %s', ['PowerPlaneClearance', IntToStr(Obj.PowerPlaneClearance)]) + sLineBreak +
            Format('%s : %s', ['PowerPlaneReliefExpansion', IntToStr(Obj.PowerPlaneReliefExpansion)]) + sLineBreak +
            Format('%s : %s', ['UniqueId', Obj.UniqueId])
            , 'Confirm IPCB_Pad Info (partial)')
end;

procedure Inspect_IPCB_Polygon(const Obj : IPCB_Polygon; const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    if Obj.Net <> nil then AD19DebugStr := Format('%s : %s', ['Net.Name', Obj.Net.Name]) + sLineBreak else AD19DebugStr := '';

    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------------------------------------' + sLineBreak +
            AD19DebugStr +
            Format('%s : %s', ['AreaSize', FloatToStr(Obj.AreaSize)]) + sLineBreak +
            Format('%s : %s', ['PolygonType:TPolygonType', sPolygonTypeStrings[Obj.PolygonType]]) + sLineBreak +
            Format('%s : %s', ['RemoveDead', BoolToStr(Obj.RemoveDead, True)]) + sLineBreak +
            Format('%s : %s', ['UseOctagons', BoolToStr(Obj.UseOctagons, True)]) + sLineBreak +
            Format('%s : %s', ['AvoidObsticles', BoolToStr(Obj.AvoidObsticles, True)]) + sLineBreak +
            Format('%s : %s', ['PourOver:TPolygonPourOver', sPolygonPourOverStrings[Obj.PourOver]]) + sLineBreak +
            Format('%s : %s', ['Grid', IntToStr(Obj.Grid)]) + sLineBreak +
            Format('%s : %s', ['TrackSize', IntToStr(Obj.TrackSize)]) + sLineBreak +
            Format('%s : %s', ['MinTrack', IntToStr(Obj.MinTrack)]) + sLineBreak +
            Format('%s : %s', ['PointCount', IntToStr(Obj.PointCount)]) + sLineBreak +
            Format('%s : %s', ['PolyHatchStyle:TPolyHatchStyle', sPolyHatchStyleStrings[Obj.PolyHatchStyle]]) + sLineBreak +
            Format('%s : %s', ['BorderWidth', IntToStr(Obj.BorderWidth)]) + sLineBreak +
            Format('%s : %s', ['ExpandOutline', BoolToStr(Obj.ExpandOutline, True)]) + sLineBreak +
            Format('%s : %s', ['RemoveIslandsByArea', BoolToStr(Obj.RemoveIslandsByArea, True)]) + sLineBreak +
            Format('%s : %s', ['IslandAreaThreshold', FloatToStr(Obj.IslandAreaThreshold)]) + sLineBreak +
            Format('%s : %s', ['RemoveNarrowNecks', BoolToStr(Obj.RemoveNarrowNecks, True)]) + sLineBreak +
            Format('%s : %s', ['NeckWidthThreshold', IntToStr(Obj.NeckWidthThreshold)]) + sLineBreak +
            Format('%s : %s', ['ClipAcuteCorners', BoolToStr(Obj.ClipAcuteCorners, True)]) + sLineBreak +
            Format('%s : %s', ['MitreCorners', BoolToStr(Obj.MitreCorners, True)]) + sLineBreak +
            Format('%s : %s', ['DrawRemovedNecks', BoolToStr(Obj.DrawRemovedNecks, True)]) + sLineBreak +
            Format('%s : %s', ['DrawRemovedIslands', BoolToStr(Obj.DrawRemovedIslands, True)]) + sLineBreak +
            Format('%s : %s', ['DrawDeadCopper', BoolToStr(Obj.DrawDeadCopper, True)]) + sLineBreak +
            Format('%s : %s', ['ArcApproximation', IntToStr(Obj.ArcApproximation)]) + sLineBreak +
            Format('%s : %s', ['Name', Obj.Name]) + sLineBreak +
            Format('%s : %s', ['IgnoreViolations', BoolToStr(Obj.IgnoreViolations, True)]) + sLineBreak +
            Format('%s : %s', ['PourIndex', IntToStr(Obj.PourIndex)]) + sLineBreak +
            Format('%s : %s', ['Poured', BoolToStr(Obj.Poured, True)]) + sLineBreak +
            Format('%s : %s', ['AutoGenerateName', BoolToStr(Obj.AutoGenerateName, True)]) + sLineBreak +
            Format('%s : %s', ['ArcPourMode', BoolToStr(Obj.ArcPourMode, True)]) + sLineBreak +
            Format('%s : %s', ['x', IntToStr(Obj.x)]) + sLineBreak +
            Format('%s : %s', ['y', IntToStr(Obj.y)]) + sLineBreak +
            Format('%s : %s', ['PrimitiveLock', BoolToStr(Obj.PrimitiveLock, True)]) + sLineBreak +
            Format('%s : %s', ['ObjectId', sObjectIdStrings[Obj.ObjectId]]) + sLineBreak +
            Format('%s : %s', ['Layer', Layer2String(Obj.Layer)]) + sLineBreak +
            Format('%s : %s', ['DRCError', BoolToStr(Obj.DRCError, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag1', BoolToStr(Obj.MiscFlag1, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag2', BoolToStr(Obj.MiscFlag2, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag3', BoolToStr(Obj.MiscFlag3, True)]) + sLineBreak +
            Format('%s : %s', ['Moveable', BoolToStr(Obj.Moveable, True)]) + sLineBreak +
            Format('%s : %s', ['PolygonOutline', BoolToStr(Obj.PolygonOutline, True)]) + sLineBreak +
            Format('%s : %s', ['InNet', BoolToStr(Obj.InNet, True)]) + sLineBreak +
            Format('%s : %s', ['ObjectIDString', Obj.ObjectIDString]) + sLineBreak +
            Format('%s : %s', ['Identifier', Obj.Identifier]) + sLineBreak +
            Format('%s : %s', ['Descriptor', Obj.Descriptor]) + sLineBreak +
            Format('%s : %s', ['Detail', Obj.Detail]) + sLineBreak +
            Format('%s : %s', ['UniqueId', Obj.UniqueId])
            , 'Confirm IPCB_Polygon Info (partial)')
end;

procedure Inspect_IPCB_Region(const Obj : IPCB_Region; const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    if Obj.Net <> nil then AD19DebugStr := Format('%s : %s', ['Net.Name', Obj.Net.Name]) + sLineBreak else AD19DebugStr := '';

    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------------------------------------' + sLineBreak +
            AD19DebugStr +
            Format('%s : %s', ['Kind:TRegionKind', sRegionKindStrings[Obj.Kind]]) + sLineBreak +
            Format('%s : %s', ['Name', Obj.Name]) + sLineBreak +
            Format('%s : %s', ['HoleCount', IntToStr(Obj.HoleCount)]) + sLineBreak +
            Format('%s : %s', ['Area', IntToStr(Obj.Area)]) + sLineBreak +
            Format('%s : %s', ['CavityHeight', IntToStr(Obj.CavityHeight)]) + sLineBreak +
            Format('%s : %s', ['ObjectId', sObjectIdStrings[Obj.ObjectId]]) + sLineBreak +
            Format('%s : %s', ['Layer', Layer2String(Obj.Layer)]) + sLineBreak +
            Format('%s : %s', ['DRCError', BoolToStr(Obj.DRCError, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag1', BoolToStr(Obj.MiscFlag1, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag2', BoolToStr(Obj.MiscFlag2, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag3', BoolToStr(Obj.MiscFlag3, True)]) + sLineBreak +
            Format('%s : %s', ['Moveable', BoolToStr(Obj.Moveable, True)]) + sLineBreak +
            Format('%s : %s', ['TearDrop', BoolToStr(Obj.TearDrop, True)]) + sLineBreak +
            Format('%s : %s', ['IsKeepout', BoolToStr(Obj.IsKeepout, True)]) + sLineBreak +
            Format('%s : %s', ['PolygonOutline', BoolToStr(Obj.PolygonOutline, True)]) + sLineBreak +
            Format('%s : %s', ['InPolygon', BoolToStr(Obj.InPolygon, True)]) + sLineBreak +
            Format('%s : %s', ['InComponent', BoolToStr(Obj.InComponent, True)]) + sLineBreak +
            Format('%s : %s', ['InNet', BoolToStr(Obj.InNet, True)]) + sLineBreak +
            Format('%s : %s', ['IsElectricalPrim', BoolToStr(Obj.IsElectricalPrim, True)]) + sLineBreak +
            Format('%s : %s', ['ObjectIDString', Obj.ObjectIDString]) + sLineBreak +
            Format('%s : %s', ['Identifier', Obj.Identifier]) + sLineBreak +
            Format('%s : %s', ['Descriptor', Obj.Descriptor]) + sLineBreak +
            Format('%s : %s', ['Detail', Obj.Detail]) + sLineBreak +
            Format('%s : %s', ['PasteMaskExpansion', IntToStr(Obj.PasteMaskExpansion)]) + sLineBreak +
            Format('%s : %s', ['SolderMaskExpansion', IntToStr(Obj.SolderMaskExpansion)]) + sLineBreak +
            Format('%s : %s', ['UnionIndex', IntToStr(Obj.UnionIndex)]) + sLineBreak +
            Format('%s : %s', ['UniqueId', Obj.UniqueId])
            , 'Confirm IPCB_Region Info (partial)')
end;

procedure Inspect_IPCB_Text(const Obj : IPCB_Text3; const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    AD19DebugStr := Format('%s : %s', ['AdvanceSnapping',  BoolToStr(Obj.AdvanceSnapping, True)]) + sLineBreak +
            Format('%s : %s', ['SnapPointX',  CoordToDisplayX(Obj.SnapPointX)]) + sLineBreak +
            Format('%s : %s', ['SnapPointY',  CoordToDisplayY(Obj.SnapPointY)]);

    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------------------------------------' + sLineBreak +
            AD19DebugStr + sLineBreak +
            Format('%s : %s', ['Moveable',  BoolToStr(Obj.Moveable, True)]) + sLineBreak +
            Format('%s : %s', ['AllowGlobalEdit',  BoolToStr(Obj.AllowGlobalEdit, True)]) + sLineBreak +
            Format('%s : %s', ['Descriptor',  Obj.Descriptor]) + sLineBreak +
            Format('%s : %s', ['Detail',  Obj.Detail]) + sLineBreak +
            Format('%s : %s', ['EnableDraw',  BoolToStr(Obj.EnableDraw, True)]) + sLineBreak +
            Format('%s : %s', ['FontID',  IntToStr(Obj.FontID)]) + sLineBreak +
            Format('%s : %s', ['Handle',  Obj.Handle]) + sLineBreak +
            Format('%s : %s', ['Identifier',  Obj.Identifier]) + sLineBreak +
            Format('%s : %s', ['IsSaveable',  BoolToStr(Obj.IsSaveable(eAdvPCBFormat_Binary_V6), True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag1',  BoolToStr(Obj.MiscFlag1, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag2',  BoolToStr(Obj.MiscFlag2, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag3',  BoolToStr(Obj.MiscFlag3, True)]) + sLineBreak +
            Format('%s : %s', ['MirrorFlag',  BoolToStr(Obj.MirrorFlag, True)]) + sLineBreak +
            Format('%s : %s', ['MultiLine',  BoolToStr(Obj.Multiline, True)]) + sLineBreak +
            Format('%s : %s', ['MultilineTextAutoPosition:TTextAutoposition', sTextAutoposition[Obj.MultilineTextAutoPosition]]) + sLineBreak +
            Format('%s : %s', ['MultilineTextHeight',  CoordToDisplayStr(Obj.MultilineTextHeight)]) + sLineBreak +
            Format('%s : %s', ['MultilineTextResizeEnabled',  BoolToStr(Obj.MultilineTextResizeEnabled, True)]) + sLineBreak +
            Format('%s : %s', ['MultilineTextWidth',  CoordToDisplayStr(Obj.MultilineTextWidth)]) + sLineBreak +
            Format('%s : %s', ['ObjectId',  sObjectIdStrings[Obj.ObjectId]]) + sLineBreak +
            Format('%s : %s', ['Layer',  Layer2String(Obj.Layer)]) + sLineBreak +
            Format('%s : %s', ['ObjectIDString',  Obj.ObjectIDString]) + sLineBreak +
            Format('%s : %s', ['PadCacheRobotFlag',  BoolToStr(Obj.PadCacheRobotFlag, True)]) + sLineBreak +
            Format('%s : %s', ['Rotation',  FloatToStr(Obj.Rotation)]) + sLineBreak +
            Format('%s : %s', ['Size',  CoordToDisplayStr(Obj.Size)]) + sLineBreak +
            Format('%s : %s', ['Text',  Obj.Text]) + sLineBreak +
            Format('%s : %s', ['TextKind',  IntToStr(Obj.TextKind)]) + sLineBreak +
            Format('%s : %s', ['TTFInvertedTextJustify:TTextAutoposition', sTextAutoposition[Obj.TTFInvertedTextJustify]]) + sLineBreak +
            Format('%s : %s', ['TTFTextWidth',  CoordToDisplayStr(Obj.TTFTextWidth)]) + sLineBreak +
            Format('%s : %s', ['TTFTextHeight',  CoordToDisplayStr(Obj.TTFTextHeight)]) + sLineBreak +
            Format('%s : %s', ['UseTTFonts',  BoolToStr(Obj.UseTTFonts, True)]) + sLineBreak +
            Format('%s : %s', ['Inverted',  BoolToStr(Obj.Inverted, True)]) + sLineBreak +
            Format('%s : %s', ['UseInvertedRectangle',  BoolToStr(Obj.UseInvertedRectangle, True)]) + sLineBreak +
            Format('%s : %s', ['WordWrap',  BoolToStr(Obj.WordWrap, True)]) + sLineBreak +
            Format('%s : %s', ['BorderSpaceType',  IntToStr(Obj.BorderSpaceType)]) + sLineBreak +
            Format('%s : %s', ['Width',  CoordToDisplayStr(Obj.Width)]) + sLineBreak +
            Format('%s : %s', ['WordWrap',  BoolToStr(Obj.WordWrap, True)])
            , 'Confirm IPCB_Text Info (partial)')
end;

procedure Inspect_IPCB_Track(const Obj : IPCB_Track; const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    if Obj.Net <> nil then AD19DebugStr := Format('%s : %s', ['Net.Name', Obj.Net.Name]) + sLineBreak else AD19DebugStr := '';

    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------------------------------------' + sLineBreak +
            AD19DebugStr +
            Format('%s : %s', ['x1', IntToStr(Obj.x1)]) + sLineBreak +
            Format('%s : %s', ['y1', IntToStr(Obj.y1)]) + sLineBreak +
            Format('%s : %s', ['x2', IntToStr(Obj.x2)]) + sLineBreak +
            Format('%s : %s', ['y2', IntToStr(Obj.y2)]) + sLineBreak +
            Format('%s : %s', ['Width', IntToStr(Obj.Width)]) + sLineBreak +
            Format('%s : %s', ['GetState_Length', IntToStr(Obj.GetState_Length )]) + sLineBreak +
            Format('%s : %s', ['ObjectId', sObjectIdStrings[Obj.ObjectId]]) + sLineBreak +
            Format('%s : %s', ['Layer', Layer2String(Obj.Layer)]) + sLineBreak +
            Format('%s : %s', ['DRCError', BoolToStr(Obj.DRCError, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag1', BoolToStr(Obj.MiscFlag1, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag2', BoolToStr(Obj.MiscFlag2, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag3', BoolToStr(Obj.MiscFlag3, True)]) + sLineBreak +
            Format('%s : %s', ['Moveable', BoolToStr(Obj.Moveable, True)]) + sLineBreak +
            Format('%s : %s', ['IsKeepout', BoolToStr(Obj.IsKeepout, True)]) + sLineBreak +
            Format('%s : %s', ['InComponent', BoolToStr(Obj.InComponent, True)]) + sLineBreak +
            Format('%s : %s', ['InNet', BoolToStr(Obj.InNet, True)]) + sLineBreak +
            Format('%s : %s', ['ObjectIDString', Obj.ObjectIDString]) + sLineBreak +
            Format('%s : %s', ['Identifier', Obj.Identifier]) + sLineBreak +
            Format('%s : %s', ['Descriptor', Obj.Descriptor]) + sLineBreak +
            Format('%s : %s', ['Detail', Obj.Detail]) + sLineBreak
            , 'Confirm IPCB_Track Info (partial)')
end;

procedure Inspect_IPCB_Via(const Obj : IPCB_Via, const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    if Obj.Net <> nil then AD19DebugStr := Format('%s : %s', ['Net.Name', Obj.Net.Name]) + sLineBreak else AD19DebugStr := '';

    if Obj = nil then exit;
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------------------------------------' + sLineBreak +
            Format('%s : %s', ['Via is connected on layers', GetConnectedLayersInfo(Obj)]) + sLineBreak2 +
            AD19DebugStr +
            Format('%s : %s', ['Mode:TPadMode', sPadModeStrings[Obj.Mode]]) + sLineBreak +
            Format('%s : %s', ['x', IntToStr(Obj.x)]) + sLineBreak +
            Format('%s : %s', ['y', IntToStr(Obj.y)]) + sLineBreak +
            Format('%s : %s', ['LowLayer', IntToStr(Obj.LowLayer)]) + sLineBreak +
            Format('%s : %s', ['HighLayer', IntToStr(Obj.HighLayer)]) + sLineBreak +
            Format('%s : %s', ['HoleSize', IntToStr(Obj.HoleSize)]) + sLineBreak +
            Format('%s : %s', ['Size', IntToStr(Obj.Size)]) + sLineBreak +
            Format('%s : %s', ['Height', IntToStr(Obj.Height)]) + sLineBreak +
            Format('%s : %s', ['SolderMaskExpansionFromHoleEdge', BoolToStr(Obj.SolderMaskExpansionFromHoleEdge, True)]) + sLineBreak +
            Format('%s : %s', ['HolePositiveTolerance', IntToStr(Obj.HolePositiveTolerance)]) + sLineBreak +
            Format('%s : %s', ['HoleNegativeTolerance', IntToStr(Obj.HoleNegativeTolerance)]) + sLineBreak +
            Format('%s : %s', ['ObjectId', sObjectIdStrings[Obj.ObjectId]]) + sLineBreak +
            Format('%s : %s', ['Layer', Layer2String(Obj.Layer)]) + sLineBreak +
            Format('%s : %s', ['DRCError', BoolToStr(Obj.DRCError, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag1', BoolToStr(Obj.MiscFlag1, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag2', BoolToStr(Obj.MiscFlag2, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag3', BoolToStr(Obj.MiscFlag3, True)]) + sLineBreak +
            Format('%s : %s', ['Moveable', BoolToStr(Obj.Moveable, True)]) + sLineBreak +
            Format('%s : %s', ['IsTenting', BoolToStr(Obj.IsTenting, True)]) + sLineBreak +
            Format('%s : %s', ['IsTenting_Top', BoolToStr(Obj.IsTenting_Top, True)]) + sLineBreak +
            Format('%s : %s', ['IsTenting_Bottom', BoolToStr(Obj.IsTenting_Bottom, True)]) + sLineBreak +
            Format('%s : %s', ['IsTestpoint_Top', BoolToStr(Obj.IsTestpoint_Top, True)]) + sLineBreak +
            Format('%s : %s', ['IsTestpoint_Bottom', BoolToStr(Obj.IsTestpoint_Bottom, True)]) + sLineBreak +
            Format('%s : %s', ['IsAssyTestpoint_Top', BoolToStr(Obj.IsAssyTestpoint_Top, True)]) + sLineBreak +
            Format('%s : %s', ['IsAssyTestpoint_Bottom', BoolToStr(Obj.IsAssyTestpoint_Bottom, True)]) + sLineBreak +
            Format('%s : %s', ['InComponent', BoolToStr(Obj.InComponent, True)]) + sLineBreak +
            Format('%s : %s', ['InNet', BoolToStr(Obj.InNet, True)]) + sLineBreak +
            Format('%s : %s', ['IsElectricalPrim', BoolToStr(Obj.IsElectricalPrim, True)]) + sLineBreak +
            Format('%s : %s', ['ObjectIDString', Obj.ObjectIDString]) + sLineBreak +
            Format('%s : %s', ['Identifier', Obj.Identifier]) + sLineBreak +
            Format('%s : %s', ['Descriptor', Obj.Descriptor]) + sLineBreak +
            Format('%s : %s', ['Detail', Obj.Detail]) + sLineBreak +
            Format('%s : %s', ['PowerPlaneConnectStyle:TPlaneConnectStyle', sPlaneConnectStyleStrings[Obj.PowerPlaneConnectStyle]]) + sLineBreak +
            Format('%s : %s', ['ReliefConductorWidth', IntToStr(Obj.ReliefConductorWidth)]) + sLineBreak +
            Format('%s : %s', ['ReliefEntries', IntToStr(Obj.ReliefEntries)]) + sLineBreak +
            Format('%s : %s', ['ReliefAirGap', IntToStr(Obj.ReliefAirGap)]) + sLineBreak +
            Format('%s : %s', ['PasteMaskExpansion', IntToStr(Obj.PasteMaskExpansion)]) + sLineBreak +
            Format('%s : %s', ['SolderMaskExpansion', IntToStr(Obj.SolderMaskExpansion)]) + sLineBreak +
            Format('%s : %s', ['PowerPlaneClearance', IntToStr(Obj.PowerPlaneClearance)]) + sLineBreak +
            Format('%s : %s', ['PowerPlaneReliefExpansion', IntToStr(Obj.PowerPlaneReliefExpansion)])
            , 'Confirm IPCB_Via Info (partial)')
end;

procedure _Inspect;
var
    idx         : Integer;
    Obj         : IPCB_ObjectClass;
begin
    if not DocumentIsPCB then exit;

    for idx := 0 to Board.SelectecObjectCount - 1 do
    begin
        Obj := Board.SelectecObject[idx];
        if Obj = nil then continue;

        case Obj.ObjectId of
            eArcObject          : Inspect_IPCB_Arc(Obj);
            ePadObject          : Inspect_IPCB_Pad(Obj);
            eViaObject          : Inspect_IPCB_Via(Obj);
            eTrackObject        : Inspect_IPCB_Track(Obj);
            eTextObject         : Inspect_IPCB_Text(Obj);
            eFillObject         : Inspect_IPCB_Fill(Obj);
            eComponentObject    : Inspect_IPCB_Component(Obj);
            ePolyObject         : Inspect_IPCB_Polygon(Obj);
            eRegionObject       : Inspect_IPCB_Region(Obj);
            eComponentBodyObject: Inspect_IPCB_ComponentBody(Obj);
            eDimensionObject    : Inspect_IPCB_Dimension(Obj);
            else DebugMessage(1, 'Script has not implemented inspector for ' + Obj.ObjectIDString);
        end;

        if iDebugLevel = 0 then break;
    end;
end;

procedure TurnOffAdvanceSnapping;
var
    idx         : Integer;
    Obj         : IPCB_ObjectClass;
    bAskAbort   : Boolean;
begin
    if not DocumentIsPCB then exit;
    bAskAbort := True;

    PCBServer.PreProcess;
    try
        for idx := 0 to Board.SelectecObjectCount - 1 do
        begin
            Obj := Board.SelectecObject[idx];
            if Obj = nil then continue;

            if Obj.ObjectId = eTextObject then
            begin
                Inspect_IPCB_Text(Obj, Obj.Descriptor + ' Before Change');
                Obj.BeginModify;
                Obj.AdvanceSnapping := False;
                Obj.GraphicallyInvalidate;
                Obj.EndModify;
                Inspect_IPCB_Text(Obj, Obj.Descriptor + ' After Change');
            end;

            if (iDebugLevel = 0) and bAskAbort then
            begin
                If ConfirmNoYes('Inspection messages dismissed - do you want to silently continue processing remaining objects?') then break else bAskAbort := False;
            end;
        end;
    finally
        PCBServer.PostProcess;
    end;
end;

procedure TurnOnAdvanceSnapping;
var
    idx         : Integer;
    Obj         : IPCB_ObjectClass;
    bAskAbort   : Boolean;
begin
    if not DocumentIsPCB then exit;
    bAskAbort := True;

    PCBServer.PreProcess;
    try
        for idx := 0 to Board.SelectecObjectCount - 1 do
        begin
            Obj := Board.SelectecObject[idx];
            if Obj = nil then continue;

            if Obj.ObjectId = eTextObject then
            begin
                Inspect_IPCB_Text(Obj, Obj.Descriptor + ' Before Change');
                Obj.BeginModify;
                Obj.AdvanceSnapping := True;
                Obj.GraphicallyInvalidate;
                Obj.EndModify;
                Inspect_IPCB_Text(Obj, Obj.Descriptor + ' After Change');
            end;

            if (iDebugLevel = 0) and bAskAbort then
            begin
                If ConfirmNoYes('Inspection messages dismissed - do you want to silently continue processing remaining objects?') then break else bAskAbort := False;
            end;
        end;
    finally
        PCBServer.PostProcess;
    end;
end;

