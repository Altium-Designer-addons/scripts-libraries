const
    cScriptTitle    = 'PCBObjectInspector';
    cScriptVersion  = '0.20';
    cDEBUGLEVEL     = 1;
    sLineBreak2     = sLineBreak + sLineBreak;

var
    Board           : IPCB_Board;
    IsAtLeastAD19   : Boolean;
    iDebugLevel     : Integer;
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
    sPolyHatchStyleStrings = ['ePolyHatch90', 'ePolyHatch45', 'ePolyVHatch', 'ePolyHHatch', 'ePolyNoHatch', 'ePolySolid'];
    sPolygonPourOverStrings = ['ePolygonPourOver_None', 'ePolygonPourOver_SameNet', 'ePolygonPourOver_SameNetPolygons'];
    sPolygonTypeStrings = ['eSignalLayerPolygon', 'eSplitPlanePolygon', 'eCoverlayOutlinePolygon'];
    sExtendedDrillTypeStrings = ['eDrilledHole', 'ePunchedHole', 'eLaserDrilledHole', 'ePlasmaDrilledHole'];
    sExtendedHoleTypeStrings = ['eRoundHole', 'eSquareHole', 'eSlotHole'];
    sPlaneConnectStyleStrings = ['eReliefConnectToPlane', 'eDirectConnectToPlane', 'eNoConnect'];
    sRegionKindStrings = ['eRegionKind_Copper', 'eRegionKind_Cutout', 'eRegionKind_NamedRegion', 'eRegionKind_BoardCutout', 'eRegionKind_Cavity'];
    sBoardSideStrings = ['eBoardSide_Top', 'eBoardSide_Bottom'];
    sUnitStyleStrings = ['eNoUnits', 'eYesUnits', 'eParenthUnits'];
    sDimensionKindStrings = ['eNoDimension', 'eLinearDimension', 'eAngularDimension', 'eRadialDimension', 'eLeaderDimension', 'eDatumDimension', 'eBaselineDimension', 'eCenterDimension', 'eOriginalDimension', 'eLinearDiameterDimension', 'eRadialDiameterDimension'];
    sDimensionTextPositionStrings = ['eTextAuto', 'eTextCenter', 'eTextTop', 'eTextBottom', 'eTextRight', 'eTextLeft', 'eTextInsideRight', 'eTextInsideLeft', 'eTextUniDirectional', 'eTextManual');
    sDimensionUnitStrings = ['eMils', 'eInches', 'eMillimeters', 'eCentimeters', 'eDegrees', 'eRadians', 'eAutomaticUnit'];
    sDimensionArrowPositionStrings = ['eInside', 'eOutside'];

    // Checks if current document is a PCB kind if not, show error and return false.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then
    begin
        ShowError('This script must be run from a PCB document.');
        Result := False;
    end
    else Result := True;
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

procedure Inspect_IPCB_Arc(const AnArc : IPCB_Track; const MyLabel : string = '');
begin
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------------------------------------' + sLineBreak +
                Format('%s : %s', ['XCenter', IntToStr(AnArc.XCenter)]) + sLineBreak +
                Format('%s : %s', ['YCenter', IntToStr(AnArc.YCenter)]) + sLineBreak +
                Format('%s : %s', ['Radius', IntToStr(AnArc.Radius)]) + sLineBreak +
                Format('%s : %s', ['LineWidth', IntToStr(AnArc.LineWidth)]) + sLineBreak +
                Format('%s : %s', ['StartAngle', FloatToStr(AnArc.StartAngle)]) + sLineBreak +
                Format('%s : %s', ['EndAngle', FloatToStr(AnArc.EndAngle)]) + sLineBreak +
                Format('%s : %s', ['StartX', IntToStr(AnArc.StartX)]) + sLineBreak +
                Format('%s : %s', ['StartY', IntToStr(AnArc.StartY)]) + sLineBreak +
                Format('%s : %s', ['EndX', IntToStr(AnArc.EndX)]) + sLineBreak +
                Format('%s : %s', ['EndY', IntToStr(AnArc.EndY)]) + sLineBreak +
                Format('%s : %s', ['ObjectId', sObjectIdStrings[AnArc.ObjectId]]) + sLineBreak +
                Format('%s : %s', ['Layer', IntToStr(AnArc.Layer)]) + sLineBreak +
                Format('%s : %s', ['DRCError', BoolToStr(AnArc.DRCError, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag1', BoolToStr(AnArc.MiscFlag1, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag2', BoolToStr(AnArc.MiscFlag2, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag3', BoolToStr(AnArc.MiscFlag3, True)]) + sLineBreak +
                Format('%s : %s', ['IsKeepout', BoolToStr(AnArc.IsKeepout, True)]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString', AnArc.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['Identifier', AnArc.Identifier]) + sLineBreak +
                Format('%s : %s', ['Descriptor', AnArc.Descriptor]) + sLineBreak +
                Format('%s : %s', ['Detail', AnArc.Detail]) + sLineBreak
                , 'Confirm IPCB_Arc Info (partial)')
end;

procedure Inspect_IPCB_Component(const Comp : IPCB_Component; const MyLabel : string = '');
begin
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------------------------------------' + sLineBreak +
                Format('%s : %s', ['AxisCount', IntToStr(Comp.AxisCount)]) + sLineBreak +
                Format('%s : %s', ['ChannelOffset', IntToStr(Comp.ChannelOffset)]) + sLineBreak +
                Format('%s : %s', ['ComponentKind:TComponentKind', sComponentKindStrings[Comp.ComponentKind]]) + sLineBreak +
                Format('%s : %s', ['Pattern', Comp.Pattern]) + sLineBreak +
                Format('%s : %s', ['NameOn', BoolToStr(Comp.NameOn, True)]) + sLineBreak +
                Format('%s : %s', ['CommentOn', BoolToStr(Comp.CommentOn, True)]) + sLineBreak +
                Format('%s : %s', ['LockStrings', BoolToStr(Comp.LockStrings, True)]) + sLineBreak +
                Format('%s : %s', ['GroupNum', IntToStr(Comp.GroupNum)]) + sLineBreak +
                Format('%s : %s', ['Rotation', FloatToStr(Comp.Rotation)]) + sLineBreak +
                Format('%s : %s', ['Height', IntToStr(Comp.Height)]) + sLineBreak +
                Format('%s : %s', ['NameAutoPosition:TTextAutoposition', sTextAutoposition[Comp.NameAutoPosition]]) + sLineBreak +
                Format('%s : %s', ['CommentAutoPosition:TTextAutoposition', sTextAutoposition[Comp.CommentAutoPosition]]) + sLineBreak +
                Format('%s : %s', ['SourceDesignator', Comp.SourceDesignator]) + sLineBreak +
                Format('%s : %s', ['SourceUniqueId', Comp.SourceUniqueId]) + sLineBreak +
                Format('%s : %s', ['SourceHierarchicalPath', Comp.SourceHierarchicalPath]) + sLineBreak +
                Format('%s : %s', ['SourceFootprintLibrary', Comp.SourceFootprintLibrary]) + sLineBreak +
                Format('%s : %s', ['SourceComponentLibrary', Comp.SourceComponentLibrary]) + sLineBreak +
                Format('%s : %s', ['SourceLibReference', Comp.SourceLibReference]) + sLineBreak +
                Format('%s : %s', ['SourceCompDesignItemID', Comp.SourceCompDesignItemID]) + sLineBreak +
                Format('%s : %s', ['SourceDescription', Comp.SourceDescription]) + sLineBreak +
                Format('%s : %s', ['FootprintDescription', Comp.FootprintDescription]) + sLineBreak +
                Format('%s : %s', ['DefaultPCB3DModel', Comp.DefaultPCB3DModel]) + sLineBreak +
                Format('%s : %s', ['IsBGA', BoolToStr(Comp.IsBGA, True)]) + sLineBreak +
                Format('%s : %s', ['EnablePinSwapping', BoolToStr(Comp.EnablePinSwapping, True)]) + sLineBreak +
                Format('%s : %s', ['EnablePartSwapping', BoolToStr(Comp.EnablePartSwapping, True)]) + sLineBreak +
                Format('%s : %s', ['FootprintConfigurableParameters_Encoded', Comp.FootprintConfigurableParameters_Encoded]) + sLineBreak +
                Format('%s : %s', ['FootprintConfiguratorName', Comp.FootprintConfiguratorName]) + sLineBreak +
                Format('%s : %s', ['VaultGUID', Comp.VaultGUID]) + sLineBreak +
                Format('%s : %s', ['ItemGUID', Comp.ItemGUID]) + sLineBreak +
                Format('%s : %s', ['ItemRevisionGUID', Comp.ItemRevisionGUID]) + sLineBreak +
                Format('%s : %s', ['FlippedOnLayer', BoolToStr(Comp.FlippedOnLayer, True)]) + sLineBreak +
                Format('%s : %s', ['JumpersVisible', BoolToStr(Comp.JumpersVisible, True)]) + sLineBreak +
                Format('%s : %s', ['x', IntToStr(Comp.x)]) + sLineBreak +
                Format('%s : %s', ['y', IntToStr(Comp.y)]) + sLineBreak +
                Format('%s : %s', ['PrimitiveLock', BoolToStr(Comp.PrimitiveLock, True)]) + sLineBreak +
                Format('%s : %s', ['ObjectId', sObjectIdStrings[Comp.ObjectId]]) + sLineBreak +
                Format('%s : %s', ['Layer', IntToStr(Comp.Layer)]) + sLineBreak +
                Format('%s : %s', ['DRCError', BoolToStr(Comp.DRCError, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag1', BoolToStr(Comp.MiscFlag1, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag2', BoolToStr(Comp.MiscFlag2, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag3', BoolToStr(Comp.MiscFlag3, True)]) + sLineBreak +
                Format('%s : %s', ['Moveable', BoolToStr(Comp.Moveable, True)]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString', Comp.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['Identifier', Comp.Identifier]) + sLineBreak +
                Format('%s : %s', ['Descriptor', Comp.Descriptor]) + sLineBreak +
                Format('%s : %s', ['Detail', Comp.Detail]) + sLineBreak +
                Format('%s : %s', ['UniqueId', Comp.UniqueId])
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
            Format('%s : %s', ['Layer', IntToStr(Obj.Layer)]) + sLineBreak +
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
            Format('%s : %s', ['Layer', IntToStr(Obj.Layer)]) + sLineBreak +
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

procedure Inspect_IPCB_Fill(const AFill : IPCB_Fill; const MyLabel : string = '');
begin
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------------------------------------' + sLineBreak +
                Format('%s : %s', ['XLocation', IntToStr(AFill.XLocation)]) + sLineBreak +
                Format('%s : %s', ['YLocation', IntToStr(AFill.YLocation)]) + sLineBreak +
                Format('%s : %s', ['X1Location', IntToStr(AFill.X1Location)]) + sLineBreak +
                Format('%s : %s', ['Y1Location', IntToStr(AFill.Y1Location)]) + sLineBreak +
                Format('%s : %s', ['X2Location', IntToStr(AFill.X2Location)]) + sLineBreak +
                Format('%s : %s', ['Y2Location', IntToStr(AFill.Y2Location)]) + sLineBreak +
                Format('%s : %s', ['Rotation', FloatToStr(AFill.Rotation)]) + sLineBreak +
                Format('%s : %s', ['ObjectId', sObjectIdStrings[AFill.ObjectId]]) + sLineBreak +
                Format('%s : %s', ['Layer', IntToStr(AFill.Layer)]) + sLineBreak +
                Format('%s : %s', ['DRCError', BoolToStr(AFill.DRCError, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag1', BoolToStr(AFill.MiscFlag1, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag2', BoolToStr(AFill.MiscFlag2, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag3', BoolToStr(AFill.MiscFlag3, True)]) + sLineBreak +
                Format('%s : %s', ['Moveable', BoolToStr(AFill.Moveable, True)]) + sLineBreak +
                Format('%s : %s', ['IsKeepout', BoolToStr(AFill.IsKeepout, True)]) + sLineBreak +
                Format('%s : %s', ['InComponent', BoolToStr(AFill.InComponent, True)]) + sLineBreak +
                Format('%s : %s', ['InNet', BoolToStr(AFill.InNet, True)]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString', AFill.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['Identifier', AFill.Identifier]) + sLineBreak +
                Format('%s : %s', ['Descriptor', AFill.Descriptor]) + sLineBreak +
                Format('%s : %s', ['Detail', AFill.Detail]) + sLineBreak +
                Format('%s : %s', ['PasteMaskExpansion', IntToStr(AFill.PasteMaskExpansion)]) + sLineBreak +
                Format('%s : %s', ['SolderMaskExpansion', IntToStr(AFill.SolderMaskExpansion)]) + sLineBreak +
                Format('%s : %s', ['PowerPlaneClearance', IntToStr(AFill.PowerPlaneClearance)]) + sLineBreak +
                Format('%s : %s', ['PowerPlaneReliefExpansion', IntToStr(AFill.PowerPlaneReliefExpansion)])
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

procedure Inspect_IPCB_Pad(const APad : IPCB_Pad4; const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    AD19DebugStr := Format('%s : %s', ['MaxXSignalLayers', IntToStr(APad.MaxXSignalLayers)]) + sLineBreak +
            Format('%s : %s', ['MaxYSignalLayers', IntToStr(APad.MaxYSignalLayers)]) + sLineBreak +
            Format('%s : %s', ['PinPackageLength', IntToStr(APad.PinPackageLength)]) + sLineBreak +
            Format('%s : %s', ['XPadOffsetAll', IntToStr(APad.XPadOffsetAll)]) + sLineBreak +
            Format('%s : %s', ['YPadOffsetAll', IntToStr(APad.YPadOffsetAll)]);

    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------------------------------------' + sLineBreak +
                AD19DebugStr + sLineBreak +
                Format('%s : %s', ['x', IntToStr(APad.x)]) + sLineBreak +
                Format('%s : %s', ['y', IntToStr(APad.y)]) + sLineBreak +
                Format('%s : %s', ['PinDescriptor', APad.PinDescriptor]) + sLineBreak +
                Format('%s : %s', ['Mode:TPadMode', sPadModeStrings[APad.Mode]]) + sLineBreak +
                Format('%s : %s', ['TopXSize', IntToStr(APad.TopXSize)]) + sLineBreak +
                Format('%s : %s', ['TopYSize', IntToStr(APad.TopYSize)]) + sLineBreak +
                Format('%s : %s', ['MidXSize', IntToStr(APad.MidXSize)]) + sLineBreak +
                Format('%s : %s', ['MidYSize', IntToStr(APad.MidYSize)]) + sLineBreak +
                Format('%s : %s', ['BotXSize', IntToStr(APad.BotXSize)]) + sLineBreak +
                Format('%s : %s', ['BotYSize', IntToStr(APad.BotYSize)]) + sLineBreak +
                Format('%s : %s', ['TopShape:TShape', sShapeStrings[APad.TopShape]]) + sLineBreak +
                Format('%s : %s', ['MidShape:TShape', sShapeStrings[APad.MidShape]]) + sLineBreak +
                Format('%s : %s', ['BotShape:TShape', sShapeStrings[APad.BotShape]]) + sLineBreak +
                Format('%s : %s', ['HoleSize', IntToStr(APad.HoleSize)]) + sLineBreak +
                Format('%s : %s', ['Rotation', FloatToStr(APad.Rotation)]) + sLineBreak +
                Format('%s : %s', ['Name', APad.Name]) + sLineBreak +
                Format('%s : %s', ['SwapID_Pad', APad.SwapID_Pad]) + sLineBreak +
                Format('%s : %s', ['SwapID_Part', APad.SwapID_Part]) + sLineBreak +
                Format('%s : %s', ['OwnerPart_ID', IntToStr(APad.OwnerPart_ID)]) + sLineBreak +
                Format('%s : %s', ['SwappedPadName', APad.SwappedPadName]) + sLineBreak +
                Format('%s : %s', ['Plated', BoolToStr(APad.Plated, True)]) + sLineBreak +
                Format('%s : %s', ['DrillType:TExtendedDrillType', sExtendedDrillTypeStrings[APad.DrillType]]) + sLineBreak +
                Format('%s : %s', ['HoleType:TExtendedHoleType', sExtendedHoleTypeStrings[APad.HoleType]]) + sLineBreak +
                Format('%s : %s', ['HoleWidth', IntToStr(APad.HoleWidth)]) + sLineBreak +
                Format('%s : %s', ['HoleRotation', FloatToStr(APad.HoleRotation)]) + sLineBreak +
                Format('%s : %s', ['JumperID', IntToStr(APad.JumperID)]) + sLineBreak +
                Format('%s : %s', ['SolderMaskExpansionFromHoleEdge', BoolToStr(APad.SolderMaskExpansionFromHoleEdge, True)]) + sLineBreak +
                Format('%s : %s', ['HolePositiveTolerance', IntToStr(APad.HolePositiveTolerance)]) + sLineBreak +
                Format('%s : %s', ['HoleNegativeTolerance', IntToStr(APad.HoleNegativeTolerance)]) + sLineBreak +
                Format('%s : %s', ['ObjectId', sObjectIdStrings[APad.ObjectId]]) + sLineBreak +
                Format('%s : %s', ['Layer', IntToStr(APad.Layer)]) + sLineBreak +
                Format('%s : %s', ['DRCError', BoolToStr(APad.DRCError, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag1', BoolToStr(APad.MiscFlag1, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag2', BoolToStr(APad.MiscFlag2, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag3', BoolToStr(APad.MiscFlag3, True)]) + sLineBreak +
                Format('%s : %s', ['IsTenting', BoolToStr(APad.IsTenting, True)]) + sLineBreak +
                Format('%s : %s', ['IsTenting_Top', BoolToStr(APad.IsTenting_Top, True)]) + sLineBreak +
                Format('%s : %s', ['IsTenting_Bottom', BoolToStr(APad.IsTenting_Bottom, True)]) + sLineBreak +
                Format('%s : %s', ['IsTestpoint_Top', BoolToStr(APad.IsTestpoint_Top, True)]) + sLineBreak +
                Format('%s : %s', ['IsTestpoint_Bottom', BoolToStr(APad.IsTestpoint_Bottom, True)]) + sLineBreak +
                Format('%s : %s', ['IsAssyTestpoint_Top', BoolToStr(APad.IsAssyTestpoint_Top, True)]) + sLineBreak +
                Format('%s : %s', ['IsAssyTestpoint_Bottom', BoolToStr(APad.IsAssyTestpoint_Bottom, True)]) + sLineBreak +
                Format('%s : %s', ['IsKeepout', BoolToStr(APad.IsKeepout, True)]) + sLineBreak +
                Format('%s : %s', ['InComponent', BoolToStr(APad.InComponent, True)]) + sLineBreak +
                Format('%s : %s', ['InNet', BoolToStr(APad.InNet, True)]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString', APad.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['Identifier', APad.Identifier]) + sLineBreak +
                Format('%s : %s', ['Descriptor', APad.Descriptor]) + sLineBreak +
                Format('%s : %s', ['Detail', APad.Detail]) + sLineBreak +
                Format('%s : %s', ['PowerPlaneConnectStyle', IntToStr(APad.PowerPlaneConnectStyle)]) + sLineBreak +
                Format('%s : %s', ['ReliefConductorWidth', IntToStr(APad.ReliefConductorWidth)]) + sLineBreak +
                Format('%s : %s', ['ReliefEntries', IntToStr(APad.ReliefEntries)]) + sLineBreak +
                Format('%s : %s', ['ReliefAirGap', IntToStr(APad.ReliefAirGap)]) + sLineBreak +
                Format('%s : %s', ['PasteMaskExpansion', IntToStr(APad.PasteMaskExpansion)]) + sLineBreak +
                Format('%s : %s', ['SolderMaskExpansion', IntToStr(APad.SolderMaskExpansion)]) + sLineBreak +
                Format('%s : %s', ['PowerPlaneClearance', IntToStr(APad.PowerPlaneClearance)]) + sLineBreak +
                Format('%s : %s', ['PowerPlaneReliefExpansion', IntToStr(APad.PowerPlaneReliefExpansion)]) + sLineBreak +
                Format('%s : %s', ['UniqueId', APad.UniqueId])
                , 'Confirm IPCB_Pad Info (partial)')
end;

procedure Inspect_IPCB_Polygon(const Obj : IPCB_Polygon; const MyLabel : string = '');
begin
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------------------------------------' + sLineBreak +
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
            Format('%s : %s', ['Layer', IntToStr(Obj.Layer)]) + sLineBreak +
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
begin
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------------------------------------' + sLineBreak +
            Format('%s : %s', ['Kind:TRegionKind', sRegionKindStrings[Obj.Kind]]) + sLineBreak +
            Format('%s : %s', ['Name', Obj.Name]) + sLineBreak +
            Format('%s : %s', ['HoleCount', IntToStr(Obj.HoleCount)]) + sLineBreak +
            Format('%s : %s', ['Area', IntToStr(Obj.Area)]) + sLineBreak +
            Format('%s : %s', ['CavityHeight', IntToStr(Obj.CavityHeight)]) + sLineBreak +
            Format('%s : %s', ['ObjectId', sObjectIdStrings[Obj.ObjectId]]) + sLineBreak +
            Format('%s : %s', ['Layer', IntToStr(Obj.Layer)]) + sLineBreak +
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

procedure Inspect_IPCB_Text(const Text : IPCB_Text3; const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    AD19DebugStr := Format('%s : %s', ['AdvanceSnapping',  BoolToStr(Text.AdvanceSnapping, True)]) + sLineBreak +
            Format('%s : %s', ['SnapPointX',  CoordToDisplayX(Text.SnapPointX)]) + sLineBreak +
            Format('%s : %s', ['SnapPointY',  CoordToDisplayY(Text.SnapPointY)]);

    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------------------------------------' + sLineBreak +
                AD19DebugStr + sLineBreak +
                Format('%s : %s', ['Moveable',  BoolToStr(Text.Moveable, True)]) + sLineBreak +
                Format('%s : %s', ['AllowGlobalEdit',  BoolToStr(Text.AllowGlobalEdit, True)]) + sLineBreak +
                Format('%s : %s', ['Descriptor',  Text.Descriptor]) + sLineBreak +
                Format('%s : %s', ['Detail',  Text.Detail]) + sLineBreak +
                Format('%s : %s', ['EnableDraw',  BoolToStr(Text.EnableDraw, True)]) + sLineBreak +
                Format('%s : %s', ['FontID',  IntToStr(Text.FontID)]) + sLineBreak +
                Format('%s : %s', ['Handle',  Text.Handle]) + sLineBreak +
                Format('%s : %s', ['Identifier',  Text.Identifier]) + sLineBreak +
                Format('%s : %s', ['IsSaveable',  BoolToStr(Text.IsSaveable(eAdvPCBFormat_Binary_V6), True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag1',  BoolToStr(Text.MiscFlag1, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag2',  BoolToStr(Text.MiscFlag2, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag3',  BoolToStr(Text.MiscFlag3, True)]) + sLineBreak +
                Format('%s : %s', ['MirrorFlag',  BoolToStr(Text.MirrorFlag, True)]) + sLineBreak +
                Format('%s : %s', ['MultiLine',  BoolToStr(Text.Multiline, True)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextAutoPosition:TTextAutoposition', sTextAutoposition[Text.MultilineTextAutoPosition]]) + sLineBreak +
                Format('%s : %s', ['MultilineTextHeight',  CoordToDisplayStr(Text.MultilineTextHeight)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextResizeEnabled',  BoolToStr(Text.MultilineTextResizeEnabled, True)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextWidth',  CoordToDisplayStr(Text.MultilineTextWidth)]) + sLineBreak +
                Format('%s : %s', ['ObjectId',  sObjectIdStrings[Text.ObjectId]]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString',  Text.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['PadCacheRobotFlag',  BoolToStr(Text.PadCacheRobotFlag, True)]) + sLineBreak +
                Format('%s : %s', ['Rotation',  FloatToStr(Text.Rotation)]) + sLineBreak +
                Format('%s : %s', ['Size',  CoordToDisplayStr(Text.Size)]) + sLineBreak +
                Format('%s : %s', ['Text',  Text.Text]) + sLineBreak +
                Format('%s : %s', ['TextKind',  IntToStr(Text.TextKind)]) + sLineBreak +
                Format('%s : %s', ['TTFInvertedTextJustify:TTextAutoposition', sTextAutoposition[Text.TTFInvertedTextJustify]]) + sLineBreak +
                Format('%s : %s', ['TTFTextWidth',  CoordToDisplayStr(Text.TTFTextWidth)]) + sLineBreak +
                Format('%s : %s', ['TTFTextHeight',  CoordToDisplayStr(Text.TTFTextHeight)]) + sLineBreak +
                Format('%s : %s', ['UseTTFonts',  BoolToStr(Text.UseTTFonts, True)]) + sLineBreak +
                Format('%s : %s', ['Inverted',  BoolToStr(Text.Inverted, True)]) + sLineBreak +
                Format('%s : %s', ['UseInvertedRectangle',  BoolToStr(Text.UseInvertedRectangle, True)]) + sLineBreak +
                Format('%s : %s', ['WordWrap',  BoolToStr(Text.WordWrap, True)]) + sLineBreak +
                Format('%s : %s', ['Used',  BoolToStr(Text.Used, True)]) + sLineBreak +
                Format('%s : %s', ['UserRouted',  BoolToStr(Text.UserRouted, True)]) + sLineBreak +
                Format('%s : %s', ['ViewableObjectID',  IntToStr(Text.ViewableObjectID)]) + sLineBreak +
                Format('%s : %s', ['BorderSpaceType',  IntToStr(Text.BorderSpaceType)]) + sLineBreak +
                Format('%s : %s', ['Width',  CoordToDisplayStr(Text.Width)]) + sLineBreak +
                Format('%s : %s', ['WordWrap',  BoolToStr(Text.WordWrap, True)])
                , 'Confirm IPCB_Text Info (partial)')
end;

procedure Inspect_IPCB_Track(const Track : IPCB_Track; const MyLabel : string = '');
begin
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------------------------------------' + sLineBreak +
                Format('%s : %s', ['x1', IntToStr(Track.x1)]) + sLineBreak +
                Format('%s : %s', ['y1', IntToStr(Track.y1)]) + sLineBreak +
                Format('%s : %s', ['x2', IntToStr(Track.x2)]) + sLineBreak +
                Format('%s : %s', ['y2', IntToStr(Track.y2)]) + sLineBreak +
                Format('%s : %s', ['Width', IntToStr(Track.Width)]) + sLineBreak +
                Format('%s : %s', ['GetState_Length', IntToStr(Track.GetState_Length )]) + sLineBreak +
                Format('%s : %s', ['ObjectId', sObjectIdStrings[Track.ObjectId]]) + sLineBreak +
                Format('%s : %s', ['Layer', IntToStr(Track.Layer)]) + sLineBreak +
                Format('%s : %s', ['DRCError', BoolToStr(Track.DRCError, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag1', BoolToStr(Track.MiscFlag1, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag2', BoolToStr(Track.MiscFlag2, True)]) + sLineBreak +
                Format('%s : %s', ['MiscFlag3', BoolToStr(Track.MiscFlag3, True)]) + sLineBreak +
                Format('%s : %s', ['Moveable', BoolToStr(Track.Moveable, True)]) + sLineBreak +
                Format('%s : %s', ['IsKeepout', BoolToStr(Track.IsKeepout, True)]) + sLineBreak +
                Format('%s : %s', ['InComponent', BoolToStr(Track.InComponent, True)]) + sLineBreak +
                Format('%s : %s', ['InNet', BoolToStr(Track.InNet, True)]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString', Track.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['Identifier', Track.Identifier]) + sLineBreak +
                Format('%s : %s', ['Descriptor', Track.Descriptor]) + sLineBreak +
                Format('%s : %s', ['Detail', Track.Detail]) + sLineBreak
                , 'Confirm IPCB_Track Info (partial)')
end;

procedure Inspect_IPCB_Via(const ViaObj : IPCB_Via, const MyLabel : string = '');
begin
    if ViaObj = nil then exit;
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------------------------------------' + sLineBreak +
            Format('%s : %s', ['Mode:TPadMode', sPadModeStrings[ViaObj.Mode]]) + sLineBreak +
            Format('%s : %s', ['x', IntToStr(ViaObj.x)]) + sLineBreak +
            Format('%s : %s', ['y', IntToStr(ViaObj.y)]) + sLineBreak +
            Format('%s : %s', ['LowLayer', IntToStr(ViaObj.LowLayer)]) + sLineBreak +
            Format('%s : %s', ['HighLayer', IntToStr(ViaObj.HighLayer)]) + sLineBreak +
            Format('%s : %s', ['HoleSize', IntToStr(ViaObj.HoleSize)]) + sLineBreak +
            Format('%s : %s', ['Size', IntToStr(ViaObj.Size)]) + sLineBreak +
            Format('%s : %s', ['Height', IntToStr(ViaObj.Height)]) + sLineBreak +
            Format('%s : %s', ['SolderMaskExpansionFromHoleEdge', BoolToStr(ViaObj.SolderMaskExpansionFromHoleEdge, True)]) + sLineBreak +
            Format('%s : %s', ['HolePositiveTolerance', IntToStr(ViaObj.HolePositiveTolerance)]) + sLineBreak +
            Format('%s : %s', ['HoleNegativeTolerance', IntToStr(ViaObj.HoleNegativeTolerance)]) + sLineBreak +
            Format('%s : %s', ['ObjectId', sObjectIdStrings[ViaObj.ObjectId]]) + sLineBreak +
            Format('%s : %s', ['Layer', IntToStr(ViaObj.Layer)]) + sLineBreak +
            Format('%s : %s', ['DRCError', BoolToStr(ViaObj.DRCError, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag1', BoolToStr(ViaObj.MiscFlag1, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag2', BoolToStr(ViaObj.MiscFlag2, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag3', BoolToStr(ViaObj.MiscFlag3, True)]) + sLineBreak +
            Format('%s : %s', ['Moveable', BoolToStr(ViaObj.Moveable, True)]) + sLineBreak +
            Format('%s : %s', ['IsTenting', BoolToStr(ViaObj.IsTenting, True)]) + sLineBreak +
            Format('%s : %s', ['IsTenting_Top', BoolToStr(ViaObj.IsTenting_Top, True)]) + sLineBreak +
            Format('%s : %s', ['IsTenting_Bottom', BoolToStr(ViaObj.IsTenting_Bottom, True)]) + sLineBreak +
            Format('%s : %s', ['IsTestpoint_Top', BoolToStr(ViaObj.IsTestpoint_Top, True)]) + sLineBreak +
            Format('%s : %s', ['IsTestpoint_Bottom', BoolToStr(ViaObj.IsTestpoint_Bottom, True)]) + sLineBreak +
            Format('%s : %s', ['IsAssyTestpoint_Top', BoolToStr(ViaObj.IsAssyTestpoint_Top, True)]) + sLineBreak +
            Format('%s : %s', ['IsAssyTestpoint_Bottom', BoolToStr(ViaObj.IsAssyTestpoint_Bottom, True)]) + sLineBreak +
            Format('%s : %s', ['InComponent', BoolToStr(ViaObj.InComponent, True)]) + sLineBreak +
            Format('%s : %s', ['InNet', BoolToStr(ViaObj.InNet, True)]) + sLineBreak +
            Format('%s : %s', ['IsElectricalPrim', BoolToStr(ViaObj.IsElectricalPrim, True)]) + sLineBreak +
            Format('%s : %s', ['ObjectIDString', ViaObj.ObjectIDString]) + sLineBreak +
            Format('%s : %s', ['Identifier', ViaObj.Identifier]) + sLineBreak +
            Format('%s : %s', ['Descriptor', ViaObj.Descriptor]) + sLineBreak +
            Format('%s : %s', ['Detail', ViaObj.Detail]) + sLineBreak +
            Format('%s : %s', ['PowerPlaneConnectStyle', IntToStr(ViaObj.PowerPlaneConnectStyle)]) + sLineBreak +
            Format('%s : %s', ['ReliefConductorWidth', IntToStr(ViaObj.ReliefConductorWidth)]) + sLineBreak +
            Format('%s : %s', ['ReliefEntries', IntToStr(ViaObj.ReliefEntries)]) + sLineBreak +
            Format('%s : %s', ['ReliefAirGap', IntToStr(ViaObj.ReliefAirGap)]) + sLineBreak +
            Format('%s : %s', ['PasteMaskExpansion', IntToStr(ViaObj.PasteMaskExpansion)]) + sLineBreak +
            Format('%s : %s', ['SolderMaskExpansion', IntToStr(ViaObj.SolderMaskExpansion)]) + sLineBreak +
            Format('%s : %s', ['PowerPlaneClearance', IntToStr(ViaObj.PowerPlaneClearance)]) + sLineBreak +
            Format('%s : %s', ['PowerPlaneReliefExpansion', IntToStr(ViaObj.PowerPlaneReliefExpansion)])
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

