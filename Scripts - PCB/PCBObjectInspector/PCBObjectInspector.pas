const
    cDEBUGLEVEL     = 1;
    sLineBreak2     = sLineBreak + sLineBreak;

var
    Board           : IPCB_Board;
    IsAtLeastAD19   : Boolean;
    iDebugLevel     : Integer;


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

{ IPCB_LayerObject is not a primitive type so doesn't have an ObjectId, but I'm leaving this here for possible future use}
procedure Inspect_IPCB_LayerObject(const LayerObject : IPCB_LayerObject, const MyLabel : string = '');
var
    LayerInfo : String;
begin
    if LayerObject = nil then exit;
    LayerInfo := GetLayerInfo(LayerObject);
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------' + sLineBreak +
            Format('%s : %s', ['ObjectIDString',  LayerObject.ObjectIDString]) + sLineBreak +
            LayerInfo + sLineBreak
            , 'Confirm IPCB_LayerObject Info (partial)')
end;

procedure Inspect_IPCB_Text(const Text : IPCB_Text3; const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    AD19DebugStr := Format('%s : %s', ['AdvanceSnapping',  BoolToStr(Text.AdvanceSnapping, True)]) + sLineBreak +
            Format('%s : %s', ['SnapPointX',  CoordToDisplayX(Text.SnapPointX)]) + sLineBreak +
            Format('%s : %s', ['SnapPointY',  CoordToDisplayY(Text.SnapPointY)]);

    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------' + sLineBreak +
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
                Format('%s : %s', ['MultilineTextAutoPosition',  IntToStr(Text.MultilineTextAutoPosition)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextHeight',  CoordToDisplayStr(Text.MultilineTextHeight)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextResizeEnabled',  BoolToStr(Text.MultilineTextResizeEnabled, True)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextWidth',  CoordToDisplayStr(Text.MultilineTextWidth)]) + sLineBreak +
                Format('%s : %s', ['ObjectId',  IntToStr(Text.ObjectId)]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString',  Text.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['PadCacheRobotFlag',  BoolToStr(Text.PadCacheRobotFlag, True)]) + sLineBreak +
                Format('%s : %s', ['Rotation',  FloatToStr(Text.Rotation)]) + sLineBreak +
                Format('%s : %s', ['Size',  CoordToDisplayStr(Text.Size)]) + sLineBreak +
                Format('%s : %s', ['Text',  Text.Text]) + sLineBreak +
                Format('%s : %s', ['TextKind',  IntToStr(Text.TextKind)]) + sLineBreak +
                Format('%s : %s', ['TTFInvertedTextJustify',  IntToStr(Text.TTFInvertedTextJustify)]) + sLineBreak +
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
                Format('%s : %s', ['WordWrap',  BoolToStr(Text.WordWrap, True)]) + sLineBreak
                , 'Confirm IPCB_Text Info (partial)')
end;

procedure Inspect_IPCB_Track(const Track : IPCB_Track; const MyLabel : string = '');
begin
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------' + sLineBreak +
                Format('%s : %s', ['x1                         ' , IntToStr(Track.x1              )             ]) + sLineBreak +
                Format('%s : %s', ['y1                         ' , IntToStr(Track.y1              )             ]) + sLineBreak +
                Format('%s : %s', ['x2                         ' , IntToStr(Track.x2              )             ]) + sLineBreak +
                Format('%s : %s', ['y2                         ' , IntToStr(Track.y2              )             ]) + sLineBreak +
                Format('%s : %s', ['Width                      ' , IntToStr(Track.Width           )             ]) + sLineBreak +
                Format('%s : %s', ['GetState_Length            ' , IntToStr(Track.GetState_Length )             ]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString             ' ,          Track.ObjectIDString                ]) + sLineBreak +
                Format('%s : %s', ['Identifier                 ' ,          Track.Identifier                    ]) + sLineBreak +
                Format('%s : %s', ['Descriptor                 ' ,          Track.Descriptor                    ]) + sLineBreak +
                Format('%s : %s', ['Detail                     ' ,          Track.Detail                        ]) + sLineBreak
                , 'Confirm IPCB_Track Info (partial)')
end;

procedure Inspect_IPCB_Via(const ViaObj : IPCB_Via, const MyLabel : string = '');
var
    sPadModeStrings : array[0..2];
    sObjectIdStrings : array[0..26];
begin
    sPadModeStrings := ['ePadMode_Simple', 'ePadMode_LocalStack', 'ePadMode_ExternalStack'];
    sObjectIdStrings := ['eNoObject', 'eArcObject', 'ePadObject', 'eViaObject', 'eTrackObject', 'eTextObject', 'eFillObject', 'eConnectionObject', 'eNetObject', 'eComponentObject', 'ePolyObject', 'eRegionObject', 'eComponentBodyObject', 'eDimensionObject', 'eCoordinateObject', 'eClassObject', 'eRuleObject', 'eFromToObject', 'eDifferentialPairObject', 'eViolationObject', 'eEmbeddedObject', 'eEmbeddedBoardObject', 'eSplitPlaneObject', 'eTraceObject', 'eSpareViaObject', 'eBoardObject', eBoardOutlineObject];
    if ViaObj = nil then exit;
    DebugMessage(1, 'DEBUGGING: ' + MyLabel + sLineBreak +
            '------------------------------' + sLineBreak +
            Format('%s : %s', ['Descriptor',  ViaObj.Descriptor]) + sLineBreak +
            Format('%s : %s', ['x',  CoordUnitToString(ViaObj.x, eImperial)]) + sLineBreak +
            Format('%s : %s', ['y',  CoordUnitToString(ViaObj.y, eImperial)]) + sLineBreak +
            Format('%s : %s', ['Mode:TPadMode',  sPadModeStrings[ViaObj.Mode]]) + sLineBreak +
            Format('%s : %d', ['LowLayer.Number',  ViaObj.LowLayer]) + sLineBreak +
            Format('%s : %d', ['HighLayer.Number',  ViaObj.HighLayer]) + sLineBreak +
            Format('%s : %s', ['StartLayer.Name',  ViaObj.StartLayer.Name]) + sLineBreak +
            Format('%s : %s', ['StopLayer.Name',  ViaObj.StopLayer.Name]) + sLineBreak +
            Format('%s : %s', ['HoleSize',  CoordUnitToString(ViaObj.HoleSize, eImperial)]) + sLineBreak +
            Format('%s : %s', ['Size',  CoordUnitToString(ViaObj.Size, eImperial)]) + sLineBreak +
            Format('%s : %s', ['Height',  CoordUnitToString(ViaObj.Height, eImperial)]) + sLineBreak +
            Format('%s : %s', ['SolderMaskExpansionFromHoleEdge',  BoolToStr(ViaObj.SolderMaskExpansionFromHoleEdge, True)]) + sLineBreak +
            Format('%s : %s', ['HolePositiveTolerance',  CoordUnitToString(ViaObj.HolePositiveTolerance, eImperial)]) + sLineBreak +
            Format('%s : %s', ['HoleNegativeTolerance',  CoordUnitToString(ViaObj.HoleNegativeTolerance, eImperial)]) + sLineBreak +
            Format('%s : %s', ['ObjectId',  sObjectIdStrings[ViaObj.ObjectId]]) + sLineBreak +
            Format('%s : %s', ['Index',  IntToStr(ViaObj.Index)]) + sLineBreak +
            Format('%s : %s', ['Selected',  BoolToStr(ViaObj.Selected, True)]) + sLineBreak +
            Format('%s : %s', ['Enabled',  BoolToStr(ViaObj.Enabled, True)]) + sLineBreak +
            Format('%s : %s', ['Used',  BoolToStr(ViaObj.Used, True)]) + sLineBreak +
            Format('%s : %s', ['DRCError',  BoolToStr(ViaObj.DRCError, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag1',  BoolToStr(ViaObj.MiscFlag1, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag2',  BoolToStr(ViaObj.MiscFlag2, True)]) + sLineBreak +
            Format('%s : %s', ['MiscFlag3',  BoolToStr(ViaObj.MiscFlag3, True)]) + sLineBreak +
            Format('%s : %s', ['InComponent',  BoolToStr(ViaObj.InComponent, True)]) + sLineBreak +
            Format('%s : %s', ['InNet',  BoolToStr(ViaObj.InNet, True)]) + sLineBreak +
            Format('%s : %s', ['ObjectIDString',  ViaObj.ObjectIDString]) + sLineBreak +
            Format('%s : %s', ['Identifier',  ViaObj.Identifier]) + sLineBreak +
            Format('%s : %s', ['Detail',  ViaObj.Detail]) + sLineBreak +
            Format('%s : %s', ['SolderMaskExpansion',  CoordUnitToString(ViaObj.SolderMaskExpansion, eImperial)]) + sLineBreak
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
            eViaObject          : Inspect_IPCB_Via(Obj);
            eTrackObject        : Inspect_IPCB_Track(Obj);
            eTextObject         : Inspect_IPCB_Text(Obj);
            else DebugMessage(1, 'Script has not implemented inspector for ' + Obj.ObjectIDString);
        end;

        if iDebugLevel = 0 then break;
    end;
end;

procedure TurnOffAdvanceSnapping;
var
    idx         : Integer;
    Obj         : IPCB_ObjectClass;
begin
    if not DocumentIsPCB then exit;

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

            if iDebugLevel = 0 then break;
        end;
    finally
        PCBServer.PostProcess;
    end;
end;

procedure TurnOnAdvanceSnapping;
var
    idx         : Integer;
    Obj         : IPCB_ObjectClass;
begin
    if not DocumentIsPCB then exit;

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

            if iDebugLevel = 0 then break;
        end;
    finally
        PCBServer.PostProcess;
    end;
end;

