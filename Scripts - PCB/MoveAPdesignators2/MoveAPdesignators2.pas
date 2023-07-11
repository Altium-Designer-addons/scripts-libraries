{ ****************************************************************************** }
{ * See README.md for release info
{ ****************************************************************************** }

var
    Board                           : IPCB_Board;
    IsAtLeastAD19                   : Boolean;
    iDebugLevel                     : Integer;
    KeySet                          : TObjectSet;    // keyboard key modifiers <alt> <shift> <cntl>
    bAbortScript                    : Boolean;
    bAutoMode                       : Boolean;
    bLazyAutoMove                   : Boolean;
    bUnHideDesignators              : Boolean;
    PresetFilePath                  : String;
    PresetList                      : TStringList;


const
    cESC                = -1;
    cAltKey             = 1; // don't use when selecting component. Okay when clicking location.
    cShiftKey           = 2; // don't use it for selecting component or location. Makes funny selection stuff happen
    cCtrlKey            = 3; // available for use during component and location selection
    cDEBUGLEVEL         = 0;
    cPersistentMode     = False;
    cEnableAnyAngle     = True;
    cUseConfigFile      = True;
    cNumPresets         = 12; // not just for presets, also used to save previous state
    cConfigFileName     = 'MoveAPdesignators2Settings.ini'
    cScriptTitle        = 'MoveAPdesignators2';
    cScriptVersion      = '2.10';


procedure About; forward;
function AutoMove(var NameOrComment : IPCB_Text; SearchDist : TCoord = 1200000; ParentOnly : Boolean = True; StartDist : TCoord = 400000; MinDist : TCoord = 25000; ForceAutoPos : TTextAutoposition = eAutoPos_Manual) : TCoord; forward;
procedure AutoPosDeltaAdjust(var NameOrComment : IPCB_Text; MoveDistance : TCoord; autoPos : TTextAutoposition; bAnyAngleFlag : Boolean = False); forward;
function AutopositionJustify(var NameOrComment : IPCB_Text; const tc_AutoPos : TTextAutoposition): TTextAutoposition; forward;
procedure BothInitialCheck(var status : Integer); forward;
function CoordToStr(Coords : TCoord) : String; forward;
function CoordToX(Coords : TCoord) : String; forward;
function CoordToY(Coords : TCoord) : String; forward;
function DebugContourInfo(contour : IPCB_Contour) : TStringList; forward;
function DebugGeometricPolygonInfo(poly : IPCB_GeometricPolygon) : TStringList; forward;
function DebugLevelStr(dummy : String = '') : String; forward;
procedure DebugMessage(const ShowLevel : Integer; const msg : WideString; const Caption : String = 'Confirm or Cancel Debug'); forward;
function GetComponentAreaMils(Comp : IPCB_Component) : Int64; forward;
function GetComponentAtCursor(const sPrompt : TString) : IPCB_Primitive; forward;
function GetComponentBodyLargest(Comp : IPCB_Component) : IPCB_ComponentBody; forward;
function GetComponentBodyLayerSet(Comp : IPCB_Component) : TV6_LayerSet; forward;
function GetObjPoly(Obj: IPCB_ObjectClass, Expansion: TCoord = 0): IPCB_GeometricPolygon; forward;
function GetLayerSet(SlkLayer: Integer; ObjID: Integer): TV6_LayerSet; forward;
function GetMinDesignatorClearance(var Comp : IPCB_Component) : TCoord; forward;
function GetRelativeAutoposition(var Comp : IPCB_Component; const loc_x, loc_y : TCoord) : TTextAutoposition; forward;
procedure InteractivelyAutoposition; forward;
function IsOverlapping(Text: IPCB_ObjectClass; Obj2: IPCB_ObjectClass) : Boolean; forward;
function IsSameSide(Obj1: IPCB_ObjectClass; Obj2: IPCB_ObjectClass): Boolean; forward;
function IsStringANum(Text : string) : Boolean; forward;
function IsTextOverObj(Text: IPCB_Text; ObjID: Integer; Filter_Size: Integer; ParentOnly: Boolean = False): Boolean; forward;
function IsValidPlacement(Silkscreen : IPCB_Text; ParentOnly : Boolean = False) : Boolean; forward;
procedure LoadPresetListFromFile(const dummy : Integer); forward;
function NormalizeText(var Text : IPCB_Text) : Boolean; forward;
procedure PresetButtonClicked(Sender : TObject); forward;
function RotateTextToAngle(var Text : IPCB_Text; const Angle : Double; const Normalize : Boolean = False; const Ortho : Boolean = False) : Double; forward;
function SelectCompAndDesignators(dummy : Boolean = False) : Boolean; forward;
procedure SetAutopositionLocation(var Comp : IPCB_Component; const tc_Autopos : TTextAutoposition; const KeySet : TObjectSet; bDesignator : Boolean = True); forward;
function StrFromAutoPos(eAutoPos: TTextAutoposition): String; forward;
function StrFromObjectId(ObjectId: TObjectId): String; forward;
procedure Start; forward;
procedure TweakDesignators; forward;
procedure UserKeyPress(Sender : TObject; var Key : Char); forward;
procedure ValidateOnChange(Sender : TObject); forward;
procedure TTweakDesForm.ButtonAutoClick(Sender : TObject); forward;
procedure TTweakDesForm.ButtonCancelClick(Sender : TObject); forward;
procedure TTweakDesForm.ButtonInteractiveStartClick(Sender: TObject); forward;
procedure TTweakDesForm.ButtonOKClick(Sender : TObject); forward;
procedure TTweakDesForm.MMmilButtonClick(Sender : TObject); forward;
procedure TTweakDesForm.EditDistanceChange(Sender : TObject); forward;
procedure TTweakDesForm.EditMaxDistanceChange(Sender : TObject); forward;
procedure TTweakDesForm.TweakDesFormShow(Sender : TObject); forward;
procedure TTweakDesForm.LabelVersionClick(Sender : TObject); forward;
function ConfigFile_GetPath(dummy : String = ''): String; forward;
procedure ConfigFile_Write(AFileName : String); forward;
procedure ConfigFile_Read(AFileName : String); forward;


procedure About;
var
    MsgText : string;
begin
    MsgText := '"' + cScriptTitle + '" script version ' + cScriptVersion + sLineBreak +
        sLineBreak +
        'Use "TweakDesignators" to adjust all or selected autopositioned designators' + sLineBreak +
        sLineBreak +
        'Use "InteractivelyAutoposition" to interactively place Designator or Comment for individual components.' + sLineBreak +
        sLineBreak +
        'Updated versions may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries' + sLineBreak +
        sLineBreak +
        'Settings save location:' + sLineBreak +
        ConfigFile_GetPath;

    ShowInfo(MsgText, 'About');
end; { About }


function AutoMove(var NameOrComment : IPCB_Text; SearchDist : TCoord = 1200000; ParentOnly : Boolean = True; StartDist : TCoord = 400000; MinDist : TCoord = 25000; ForceAutoPos : TTextAutoposition = eAutoPos_Manual) : TCoord;
const
    cSEARCHDIST         = 1200000; // don't make this too big or it leaves room to jump into valid placement on other side of part
    cMINSEARCHDIST      = 50000;
var
    MoveDist, TotalDist : TCoord;
    NameOrCommentAP     : TTextAutoposition;
    CurSearchDist       : TCoord;
    bFirstPass          : Boolean;
    bRunoff             : Boolean;
    bDefaultValid       : Boolean;
    bAnyAngleFlag       : Boolean;
    TextTypeStr         : String;
begin
    SearchDist := MIN(SearchDist, cSEARCHDIST);
    StartDist := MIN(StartDist, cMINSEARCHDIST * 8);
    CurSearchDist := SearchDist;
    bFirstPass := False;
    bRunoff := False;
    bDefaultValid := True;
    bAnyAngleFlag := (ForceAutoPos <> eAutoPos_Manual) and cEnableAnyAngle;

    if not (NameOrComment <> nil and NameOrComment.InComponent) then
    begin
        Result := 0;
        Exit;
    end;
    TotalDist := 0;

    if bAnyAngleFlag then NameOrCommentAP := ForceAutoPos
    else if NameOrComment.IsDesignator then NameOrCommentAP := NameOrComment.Component.NameAutoPosition
    else NameOrCommentAP := NameOrComment.Component.CommentAutoPosition;

    if NameOrCommentAP = eAutoPos_Manual then
    begin
        if NameOrComment.IsDesignator then TextTypeStr := 'Designator' else TextTypeStr := 'Comment';
        DebugMessage(1, TextTypeStr + ' for component ' + NameOrComment.Component.Name.Text + ' is not Autopositioned. Automove canceled.');
        Result := 0;
        Exit;
    end;

    if not IsValidPlacement(NameOrComment, ParentOnly) then
    begin
        bDefaultValid := False;
        StartDist := cMINSEARCHDIST * 4;
        DebugMessage(1, 'Initial placement interferes. Starting move distance at ' + CoordToStr(StartDist));
    end;

    DebugMessage(2, 'Starting automove with Ignore other components = ' + BoolToStr(ParentOnly, True));

    // initial move attempt
    MoveDist := StartDist;
    repeat
        if (TotalDist >= CurSearchDist) and (SIGN(MoveDist) > 0) then
        begin
            if not bRunoff then
            begin
                // first time the designator runs off to the search distance limit
                // (such as if the initial search puts it on the opposite side of the part and there is valid placement room)
                bRunoff := True;

                AutoPosDeltaAdjust(NameOrComment, -TotalDist, NameOrCommentAP, bAnyAngleFlag);
                TotalDist := 0;
                if iDebugLevel >= 2 then NameOrComment.GraphicallyInvalidate;
                if iDebugLevel >= 2 then Application.ProcessMessages;
                CurSearchDist := SearchDist div 2;
                MoveDist := cMINSEARCHDIST * 2;
                DebugMessage(2, 'Runaway detected. Resetting to start position.' + sLineBreak +
                        'New MoveDist=' + CoordToStr(MoveDist) + sLineBreak +
                        'bFirstPass=' + BoolToStr(bFirstPass, True) + sLineBreak +
                        'bRunuff=' + BoolToStr(bRunoff, True) + sLineBreak +
                        'Valid Placement=' + BoolToStr(bDefaultValid, True));
            end
            else
            begin
                bFirstPass := False;
                break;
            end;
        end;

        // coerce MoveDist to not exceed search distance, but don't change the value of the variable
        AutoPosDeltaAdjust(NameOrComment, MIN(MoveDist, CurSearchDist - TotalDist), NameOrCommentAP, bAnyAngleFlag);
        if iDebugLevel >= 2 then NameOrComment.GraphicallyInvalidate;
        if iDebugLevel >= 2 then Application.ProcessMessages;
        TotalDist := TotalDist + MIN(MoveDist, CurSearchDist - TotalDist);

        if IsValidPlacement(NameOrComment, ParentOnly) then
        begin
            if (TotalDist = StartDist) or bRunoff or ((TotalDist >= 0) and (SIGN(MoveDist) < 0)) then
            begin
                // if the first step is good, or if we previously identified runoff, or if we get a pass on the negative swing of the oscillator
                bFirstPass := True;
            end;
            DebugMessage(2, 'Total move distance=' + CoordToStr(TotalDist) + sLineBreak +
                    'Moved by MoveDist=' + CoordToStr(MoveDist) + sLineBreak +
                    'bFirstPass=' + BoolToStr(bFirstPass, True) + sLineBreak +
                    'bRunuff=' + BoolToStr(bRunoff, True) + sLineBreak +
                    'Valid Placement=True');
            MoveDist := ABS(MoveDist);  // always move positive after pass
        end
        else
        begin
            DebugMessage(2, 'Total move distance=' + CoordToStr(TotalDist) + sLineBreak +
                    'Failed move by MoveDist=' + CoordToStr(MoveDist) + sLineBreak +
                    'bFirstPass=' + BoolToStr(bFirstPass, True) + sLineBreak +
                    'bRunuff=' + BoolToStr(bRunoff, True) + sLineBreak +
                    'Valid Placement=False');

            if not bFirstPass then
            begin
                // oscillate between 0 and CurSearchDist of movement, halving each time the limit is reached
                if (TotalDist >= CurSearchDist) and (SIGN(MoveDist) > 0) then
                begin
                    bRunoff := True; // treat migration backward as equivalent to runoff
                    MoveDist := -MoveDist div 2; // Start moving in the negative direction and halve distance
                end
                else if (TotalDist <= 0) then
                begin
                    MoveDist := ABS(MoveDist) div 2; // Start moving in the positive direction and halve distance
                end;

                if ABS(MoveDist) < cMINSEARCHDIST then break; // stop once moves are too small to be likely productive
            end
            else
            begin
                // once there has been at least one valid placement, start approaching in half-size steps
                AutoPosDeltaAdjust(NameOrComment, -MoveDist, NameOrCommentAP, bAnyAngleFlag);
                if iDebugLevel >= 2 then NameOrComment.GraphicallyInvalidate;
                if iDebugLevel >= 2 then Application.ProcessMessages;
                TotalDist := TotalDist - MoveDist;
                MoveDist := MoveDist div 2;
            end;
        end;

    until (ABS(MoveDist) < MinDist);

    if not bFirstPass then
    begin
        // undo all the moving
        AutoPosDeltaAdjust(NameOrComment, -TotalDist, NameOrCommentAP, bAnyAngleFlag);
        DebugMessage(2, 'Failed to find passing placement within ' + CoordToStr(TotalDist) + '. Move attempt cancelled.');
        TotalDist := 0;
    end;

    Result := TotalDist;
end;



procedure AutoPosDeltaAdjust(var NameOrComment : IPCB_Text; MoveDistance : TCoord; autoPos : TTextAutoposition; bAnyAngleFlag : Boolean = False);
var
    dx, dy, d: Integer;
    flipx: Integer;
    rect : TCoordRect;
    taller : Boolean;
    TempRotation : Double;
    ParentComp : IPCB_Component;
begin
    if bAnyAngleFlag then
    begin
        ParentComp := NameOrComment.Component;
        TempRotation := ParentComp.Rotation;

        ParentComp.BeginModify;
        ParentComp.Rotation := 0;
        ParentComp.EndModify;
    end;

    d := MoveDistance;
    dx := 0;
    dy := 0;
    flipx := 1; // x Direction flips on the bottom layer
    if NameOrComment.Layer = eBottomOverlay then
        flipx := -1;

    // Top|Bottom Left|Right autopos behaves differently for strings that are height>width
    rect := NameOrComment.BoundingRectangle;
    taller := (rect.Top - rect.Bottom) > (rect.Right - rect.Left);

    Case autoPos of
        eAutoPos_CenterRight:
            dx := -d * flipx;
        eAutoPos_TopCenter:
            dy := -d;
        eAutoPos_CenterLeft:
            dx := d * flipx;
        eAutoPos_BottomCenter:
            dy := d;
        eAutoPos_TopLeft:
            dy := -d;
        eAutoPos_TopRight:
            dy := -d;
        eAutoPos_BottomLeft:
            dy := d;
        eAutoPos_BottomRight:
            dy := d;
    end;

    if taller then
    begin
        if (autoPos = eAutoPos_TopLeft) or (autoPos = eAutoPos_BottomLeft) then
        begin
            dy := 0;
            dx := d * flipx;
        end
        else if (autoPos = eAutoPos_TopRight) or (autoPos = eAutoPos_BottomRight) then
        begin
            dy := 0;
            dx := -d * flipx;
        end;
    end;

    if bAnyAngleFlag then
    begin
        NameOrComment.BeginModify;
        NameOrComment.MoveByXY(dx, dy);
        NameOrComment.EndModify;

        ParentComp.BeginModify;
        ParentComp.Rotation := TempRotation;
        ParentComp.EndModify;
    end
    else
    begin
        NameOrComment.BeginModify;
        NameOrComment.MoveByXY(dx, dy);
        NameOrComment.EndModify;
    end;
end;

{ function to transform text justification based on IPCB_Text autoposition and rotation }
function AutopositionJustify(var NameOrComment : IPCB_Text; const tc_AutoPos : TTextAutoposition): TTextAutoposition;
var
    rotation : Integer;
    mirror   : Boolean;
    taller   : Boolean;
    rect     : TCoordRect;
    APSet    : TSet;

begin
    // AD19+ supports text justification for single-line IPCB_Text with AdvanceSnapping property
    if not IsAtLeastAD19 then
    begin
        Result := tc_AutoPos;
        Exit;
    end;

    NameOrComment.AdvanceSnapping := True;

    rotation := Round(((NameOrComment.Rotation mod 360 + 360) mod 360) / 90) * 90; // coerce any possible value of rotation to a multiple of 90
    mirror := false; // x Direction flips on the bottom layer
    if NameOrComment.Layer = eBottomOverlay then
        mirror := true;

    // Top|Bottom Left|Right autopos behaves differently for strings that are height>width
    rect := NameOrComment.BoundingRectangle;
    taller := (rect.Top - rect.Bottom) > (rect.Right - rect.Left);

    APSet := MkSet(eAutoPos_TopLeft, eAutoPos_BottomLeft, eAutoPos_TopRight, eAutoPos_BottomRight);

    if taller and InSet(tc_AutoPos, APSet) then rotation := (rotation + 180) mod 360; // easier to trick rotation than add all the cases below

    // technically AutoPosition being enabled should coerce to 0,90,180, or 270
    case rotation of
        0 :
            begin
                case tc_AutoPos of
                    eAutoPos_TopLeft        : Result := eAutoPos_BottomLeft;
                    eAutoPos_CenterLeft     : Result := eAutoPos_CenterRight;
                    eAutoPos_BottomLeft     : Result := eAutoPos_TopLeft;
                    eAutoPos_TopCenter      : Result := eAutoPos_BottomCenter;
                    eAutoPos_BottomCenter   : Result := eAutoPos_TopCenter;
                    eAutoPos_TopRight       : Result := eAutoPos_BottomRight;
                    eAutoPos_CenterRight    : Result := eAutoPos_CenterLeft;
                    eAutoPos_BottomRight    : Result := eAutoPos_TopRight;
                    else                      Result := tc_AutoPos;
                end; { case tc_AutoPos }
            end;
        90 :
            begin
                case tc_AutoPos of
                    eAutoPos_TopLeft        : if mirror then Result := eAutoPos_BottomRight else Result := eAutoPos_TopLeft;
                    eAutoPos_CenterLeft     : if mirror then Result := eAutoPos_TopCenter else Result := eAutoPos_BottomCenter;
                    eAutoPos_BottomLeft     : if mirror then Result := eAutoPos_BottomLeft else Result := eAutoPos_TopRight;
                    eAutoPos_TopCenter      : if mirror then Result := eAutoPos_CenterRight else Result := eAutoPos_CenterLeft;
                    eAutoPos_BottomCenter   : if mirror then Result := eAutoPos_CenterLeft else Result := eAutoPos_CenterRight;
                    eAutoPos_TopRight       : if mirror then Result := eAutoPos_TopRight else Result := eAutoPos_BottomLeft;
                    eAutoPos_CenterRight    : if mirror then Result := eAutoPos_BottomCenter else Result := eAutoPos_TopCenter;
                    eAutoPos_BottomRight    : if mirror then Result := eAutoPos_TopLeft else Result := eAutoPos_BottomRight;
                    else                      Result := tc_AutoPos;
                end; { case tc_AutoPos }
            end;
        180 :
            begin
                case tc_AutoPos of
                    eAutoPos_TopLeft        : Result := eAutoPos_TopRight;
                    eAutoPos_CenterLeft     : Result := eAutoPos_CenterLeft;
                    eAutoPos_BottomLeft     : Result := eAutoPos_BottomRight;
                    eAutoPos_TopCenter      : Result := eAutoPos_TopCenter;
                    eAutoPos_BottomCenter   : Result := eAutoPos_BottomCenter;
                    eAutoPos_TopRight       : Result := eAutoPos_TopLeft;
                    eAutoPos_CenterRight    : Result := eAutoPos_CenterRight;
                    eAutoPos_BottomRight    : Result := eAutoPos_BottomLeft;
                    else                      Result := tc_AutoPos;
                end; { case tc_AutoPos }
            end;
        270 :
            begin
                case tc_AutoPos of
                    eAutoPos_TopLeft        : if mirror then Result := eAutoPos_TopLeft else Result := eAutoPos_BottomRight;
                    eAutoPos_CenterLeft     : if mirror then Result := eAutoPos_BottomCenter else Result := eAutoPos_TopCenter;
                    eAutoPos_BottomLeft     : if mirror then Result := eAutoPos_TopRight else Result := eAutoPos_BottomLeft;
                    eAutoPos_TopCenter      : if mirror then Result := eAutoPos_CenterLeft else Result := eAutoPos_CenterRight;
                    eAutoPos_BottomCenter   : if mirror then Result := eAutoPos_CenterRight else Result := eAutoPos_CenterLeft;
                    eAutoPos_TopRight       : if mirror then Result := eAutoPos_BottomLeft else Result := eAutoPos_TopRight;
                    eAutoPos_CenterRight    : if mirror then Result := eAutoPos_TopCenter else Result := eAutoPos_BottomCenter;
                    eAutoPos_BottomRight    : if mirror then Result := eAutoPos_BottomRight else Result := eAutoPos_TopLeft;
                    else                      Result := tc_AutoPos;
                end; { case tc_AutoPos }
            end;
        else Result := tc_AutoPos;
    end; { case rotation }

    // Set text justification according to Autoposition setting
    NameOrComment.TTFInvertedTextJustify := Result;

end; { AutopositionJustify }


{ Initial checks when a mix of components and Designators are ostensibly selected }
procedure BothInitialCheck(var status : Integer);
var
    i                   : Integer;
    Prim1               : IPCB_Primitive;
    ComponentCount      : Integer;
    DesignatorCount     : Integer;
begin
    status := 0; // clear result status

    // Checks if current document is a PCB kind if not, exit.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

    ComponentCount := 0;
    DesignatorCount := 0;

    // Count components and Designators without deselecting them
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if (Prim1.ObjectId = eComponentObject) then ComponentCount := ComponentCount + 1;
        if ((Prim1.ObjectId = eTextObject) and (Prim1.IsDesignator)) then DesignatorCount := DesignatorCount + 1;
    end;

    if ((ComponentCount + DesignatorCount) <> Board.SelectecObjectCount) then
    begin
        // deselect anything that isn't a component or Designator string
        i := 0;
        while i < Board.SelectecObjectCount do
        begin
            Prim1 := Board.SelectecObject[i];
            if not ((Prim1.ObjectId = eComponentObject) or ((Prim1.ObjectId = eTextObject) and (Prim1.IsDesignator))) then Prim1.SetState_Selected(False)
            else i := i + 1; // advance iterator if current object remains selected
        end;
    end;

end;


function CoordToStr(Coords : TCoord) : String;
const
    MAXINT = 2147483647;
    MININT = -2147483647;
begin
    if Coords < MININT then Coords := MININT
    else if Coords > MAXINT then Coords := MAXINT;

    result := CoordUnitToString(Coords, Board.DisplayUnit xor 1);
end;


function CoordToX(Coords : TCoord) : String;
begin
    result := CoordUnitToString(Coords - Board.XOrigin, Board.DisplayUnit xor 1);
end;


function CoordToY(Coords : TCoord) : String;
begin
    result := CoordUnitToString(Coords - Board.YOrigin, Board.DisplayUnit xor 1);
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
            PointList.Add(Format('%d:%s,%s', [iPoint, CoordToX(contour.x(iPoint)), CoordToY(contour.y(iPoint))]));
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


function GetComponentAreaMils(Comp : IPCB_Component) : Int64;
var
    BRect : TCoordRect;
begin
    Result := -1;
    if Comp <> nil then
    begin
        BRect := Comp.BoundingRectangleNoNameComment;
        Result := ((BRect.right - BRect.left) div 10000) * ((BRect.top - BRect.bottom) div 10000);
    end;
end;


function GetComponentAtCursor(const sPrompt : TString) : IPCB_Primitive;
var
    x, y            : TCoord;
    Comp, PrevComp  : IPCB_Component;
    Area, PrevArea  : Int64;
    Iter            : IPCB_BoardIterator;
    BRect           : TCoordRect;

begin
    KeySet := MkSet();
    Result := eNoObject;

    if Board.ChooseLocation(x, y, sPrompt) then  // false = ESC Key is pressed or right-clicked to cancel
    begin
        //   read modifier keys just as/after the "pick" mouse click
        if ShiftKeyDown   then KeySet := MkSet(cShiftKey);
        if ControlKeyDown then KeySet := SetUnion(KeySet, MkSet(cCtrlKey));

        Result := Board.GetObjectAtXYAskUserIfAmbiguous(x, y, MkSet(eComponentObject), MkSet(eTopLayer, eBottomLayer), eEditAction_Focus);         // eEditAction_DontCare
        if (Result = Nil) then Result := eNoObject;

        // look again, component might be locked (has to iterate all components on board)
        if Result = eNoObject then
        begin
            Iter := Board.BoardIterator_Create;
            Iter.AddFilter_ObjectSet(MkSet(eComponentObject));
            Iter.AddFilter_LayerSet(MkSet(eTopLayer, eBottomLayer));
            Iter.AddFilter_Method(eProcessAll);
            //SIter.AddFilter_Area(x - 100, y - 100, x + 100, y + 100); // only applies to spatial iterator, which apparently doesn't see locked objects

            Comp := Iter.FirstPCBObject;
            while (Comp <> Nil) do
            begin
                BRect := Comp.BoundingRectangleNoNameComment;
                Area := GetComponentAreaMils(Comp);
                // click should be within bounding rectangle
                if (BRect.left < x) and (x < BRect.right) and (BRect.bottom < y) and (y < BRect.top) then
                begin
                    // layer should be visible
                    if Board.LayerIsDisplayed(Comp.Layer) then
                    begin
                        if Result <> eNoObject then
                        begin
                            // prioritize component on current layer if previous component is not
                            if (Comp.Layer = Board.CurrentLayer) and (PrevComp.Layer <> Board.CurrentLayer) then Result := Comp
                            // both components are on the same layer, prioritize smaller component
                            else if (Comp.Layer = PrevComp.Layer) and (Area < PrevArea) then Result := Comp;
                        end
                        else
                        begin
                            Result := Comp;
                            Area := GetComponentAreaMils(Comp);
                        end;

                        PrevComp := Result;
                        PrevArea := GetComponentAreaMils(Result);
                    end;
                end;
                Comp := Iter.NextPCBObject;
            end;
            Board.BoardIterator_Destroy(Iter);
        end;
    end
    else Result := cESC;
end;


function GetComponentBodyLargest(Comp : IPCB_Component) : IPCB_ComponentBody;
var
    GIter : IPCB_GroupIterator;
    Prim : IPCB_Primitive;
    Area : Int64;
begin
    Area := 0;
    Result := nil;
    // Create group iterator
    GIter := Comp.GroupIterator_Create;
    GIter.AddFilter_ObjectSet(MkSet(eComponentBodyObject));

    // Try to cast the first element to a primitive
    Prim := GIter.FirstPCBObject;

    while (Prim <> nil) do
    begin
        // only return layer of body with largest area, which means we have to check them all
        if Prim.Area > Area then
        begin
            Area := Prim.Area;
            Result := Prim;
        end;

        // Move to the next Primitive in the group
        Prim := GIter.NextPCBObject;
    end;

    if Result <> nil then DebugMessage(3, StrFromObjectId(Result.ObjectId) + ' with largest area detected on layer ' + Layer2String(Result.Layer) + sLineBreak + Result.Identifier);

    // Clean up the Iterator
    Comp.GroupIterator_Destroy(GIter);
end;


function GetComponentBodyLayerSet(Comp : IPCB_Component) : TV6_LayerSet;
var
    Prim : IPCB_Primitive;
begin
    Result := MkSet(Comp.Layer);

    Prim := GetComponentBodyLargest(Comp);

    if Prim <> nil then Result := MkSet(Prim.Layer);
end;


// Get GeometricPolygon using PCBServer.PCBContourMaker
function GetObjPoly(Obj: IPCB_ObjectClass, Expansion: TCoord = 0): IPCB_GeometricPolygon;
var
    Poly: IPCB_GeometricPolygon;
    OldRect: TCoordRect;
    NewContour: IPCB_Contour;
begin
    if Obj.ObjectId = eBoardObject then
    begin
        //Rect := Obj.BoardOutline.BoundingRectangle;
        Poly := Obj.BoardOutline.BoardOutline_GeometricPolygon;
    end
    else if Obj.ObjectId = eComponentObject then
    begin
        //Rect := Obj.BoundingRectangleNoNameCommentForSignals;
        //Poly := GetComponentBodyLargest(Obj).GeometricPolygon; // doesn't have expansion argument
        Poly := PCBServer.PCBContourMaker.MakeContour(GetComponentBodyLargest(Obj), Expansion, Obj.Layer);
    end
    else if Obj.ObjectId = eArcObject then
    begin
        // Function  MakeContour(APrim : IPCB_Primitive; AExpansion : Integer; ALayer : TV6_Layer) : IPCB_GeometricPolygon;
        Poly := PCBServer.PCBContourMaker.MakeContour(Obj, Expansion, Obj.Layer);
    end
    else if Obj.ObjectId = eTrackObject then
    begin
        // Function  MakeContour(APrim : IPCB_Primitive; AExpansion : Integer; ALayer : TV6_Layer) : IPCB_GeometricPolygon;
        Poly := PCBServer.PCBContourMaker.MakeContour(Obj, Expansion, Obj.Layer);
    end
    else if Obj.ObjectId = eRegionObject then
    begin
        // Function  MakeContour(APrim : IPCB_Primitive; AExpansion : Integer; ALayer : TV6_Layer) : IPCB_GeometricPolygon;
        Poly := PCBServer.PCBContourMaker.MakeContour(Obj, Expansion, Obj.Layer);
    end
    else if Obj.ObjectId = eTextObject then
    begin
        // Function  MakeContour(APrim : IPCB_Primitive; AExpansion : Integer; ALayer : TV6_Layer) : IPCB_GeometricPolygon;
        if Obj.UseTTFonts then Poly := PCBServer.PCBContourMaker.MakeContour(Obj, Expansion, Obj.Layer)
        else Poly := PCBServer.PCBContourMaker.MakeContour(Obj, Expansion - (Obj.Width / 2), Obj.Layer);
    end
    else if Obj.ObjectId = ePadObject then
    begin
        // Function  MakeContour(APrim : IPCB_Primitive; AExpansion : Integer; ALayer : TV6_Layer) : IPCB_GeometricPolygon;
        Poly := PCBServer.PCBContourMaker.MakeContour(Obj, Expansion, Obj.Layer);
    end
    else
    begin
        //Rect := Obj.BoundingRectangle;
        // For uknown types, fall back to a dumb bounding rectangle, but wrapped as IPCB_GeometricPolygon
        OldRect := Obj.BoundingRectangle;
        NewContour := PCBServer.PCBContourFactory;
        // Procedure AddPoint(x : Integer; y : Integer);
        NewContour.AddPoint(OldRect.Left, OldRect.Bottom);
        NewContour.AddPoint(OldRect.Right, OldRect.Bottom);
        NewContour.AddPoint(OldRect.Right, OldRect.Top);
        NewContour.AddPoint(OldRect.Left, OldRect.Top);
        // Function  AddContour(AContour : IPCB_Contour) : IPCB_Contour;
        Poly := PCBServer.PCBGeometricPolygonFactory;
        Poly.AddContour(NewContour);
    end;

    result := Poly;
end;


// Returns correct layer set given the object being used
function GetLayerSet(SlkLayer: Integer; ObjID: Integer): TV6_LayerSet;
var
    TopBot: Integer;
begin
    TopBot := eTopLayer;
    if (Layer2String(SlkLayer) = 'Bottom Overlay') then
        TopBot := eBottomLayer;

    result := MkSet(SlkLayer); // Default layer set
    if (ObjID = eComponentObject) or (ObjID = ePadObject) or (ObjID = eViaObject) then
    begin
        result := MkSet(TopBot, eMultiLayer);
    end
end;


function GetMinDesignatorClearance(var Comp : IPCB_Component) : TCoord;
const
    MAXINT = 2147483647;
var
    Designator      : IPCB_Text;
    Iterator        : IPCB_GroupIterator;
    Primitive       : IPCB_Primitive;
    MinDistance     : TCoord;
    CurrentDistance : TCoord;
begin
    Result := Nil;

    DebugMessage(4, 'function GetMinDesignatorClearance(var Comp : IPCB_Component) called');

    // TODO: Use Comp.GroupIterator_Create to iterate through all component primitives on the component's mounting layer, its silkscreen (except the designator), and its 3D bodies
    // TODO: Use `Function  PrimPrimDistance(APrimitive1 : IPCB_Primitive; APrimitive2 : IPCB_Primitive) : Integer;` to get minimum distance between those primitives and Designator
    // TODO: Return minimum value found

    Designator := Comp.Name;

    // NOTES: inspired by AutoPlaceSilkscreen script's CaclulateHor function, build a dictionary of custom bounding boxes for each footprint, then use those
    // bounding boxes plus a margin number to set distance for silkscreen (or to create a silkscreen region to use PrimPrimDistance against)

    // Initialize minimum distance to a large number (MaxInt doesn't exist in DelphiScript)
    MinDistance := MAXINT;

    // Create group iterator
    Iterator := Comp.GroupIterator_Create;

    // Try to cast the first element to a primitive
    Primitive := Iterator.FirstPCBObject;

    while (Primitive <> nil) do
    begin
        // Ignore the Designator itself
        if Primitive <> Designator then
        begin
            // Calculate distance from Designator to current Primitive
            CurrentDistance := Board.PrimPrimDistance(Designator, Primitive);

            // Update minimum distance if the current distance is smaller
            if CurrentDistance < MinDistance then
                MinDistance := CurrentDistance;
        end;

        // Move to the next Primitive in the group
        Primitive := Iterator.NextPCBObject;
    end;

    // Clean up the Iterator
    Comp.GroupIterator_Destroy(Iterator);

    DebugMessage(1, 'MinDistance: ' + CoordToStr(MinDistance));

    // Return the minimum distance found
    Result := MinDistance;
end;


function GetRelativeAutoposition(var Comp : IPCB_Component; const loc_x, loc_y : TCoord) : TTextAutoposition;
var
    Angle : Integer;
    flipX : Integer;
begin
    { TTextAutoposition = ( eAutoPos_Manual,
                            eAutoPos_TopLeft,
                            eAutoPos_CenterLeft,
                            eAutoPos_BottomLeft,
                            eAutoPos_TopCenter,
                            eAutoPos_CenterCenter,
                            eAutoPos_BottomCenter,
                            eAutoPos_TopRight,
                            eAutoPos_CenterRight,
                            eAutoPos_BottomRight);}

    Result := eAutoPos_Manual;

    if Comp.Layer = eBottomLayer then flipX := -1 else flipX := 1;

    Angle := Round(ArcTan2((loc_y - Comp.y), (loc_x - Comp.x) * flipX) / Pi * 180);

    // relative to component orientation
    if cEnableAnyAngle then
        if Comp.Layer = eBottomLayer then Angle := Round(Angle + Comp.Rotation)
        else Angle := Round(Angle - Comp.Rotation);

    while Angle >= 360 do Angle := Angle - 360;
    while Angle < 0 do Angle := Angle + 360;

    // Map the angle to the relative position (rotate 22 degrees and index into 45 degree segments)
    case ((Angle + 22) mod 360) div 45 of
        0: Result := eAutoPos_CenterRight;
        1: Result := eAutoPos_TopRight;
        2: Result := eAutoPos_TopCenter;
        3: Result := eAutoPos_TopLeft;
        4: Result := eAutoPos_CenterLeft;
        5: Result := eAutoPos_BottomLeft;
        6: Result := eAutoPos_BottomCenter;
        7: Result := eAutoPos_BottomRight;
    end;

    DebugMessage(4, Format('Angle is %d, Result is %s', [Angle, StrFromAutoPos(Result)]));
end;


procedure InteractivelyAutoposition;
var
    x, y                : TCoord;
    ModList             : TStringList;
    Comp                : IPCB_Component;
    NameOrComment       : IPCB_Text;
    bDesignator         : Boolean;
    bLocationFlag       : Boolean;
    bPersistentMode     : Boolean;
    sPrompt             : String;
    tc_Autopos          : TTextAutoposition;
    ParentOnly          : Boolean;
    MoveDist            : TCoord;
begin
    iDebugLevel := cDEBUGLEVEL;
    // set AD build flag
    if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;

    // Get the document
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Make it work for Pads, Vias, Strings, Polygons, Dimensions and coordinates
    //ASetOfObjects  := MkSet(ePadObject, eViaObject, eTextObject, ePolyObject, eRegionObject, eDimensionObject, eCoordinateObject);

    if cDEBUGLEVEL > 0 then
    begin
        ModList := CreateObject(TStringList);
        ModList.Delimiter := '|';
    end;

    Comp := Nil;
    KeySet     := MkSet();
    bPersistentMode := cPersistentMode;

    repeat
        // process if source component & destination location are selected
        if bLocationFlag And Assigned(Comp) then
        begin
            if cDEBUGLEVEL >= 2 then iDebugLevel := cDEBUGLEVEL; // if cDEBUGLEVEL is at least 2, reset debug level for each component in case user canceled previously
            // copy formatting of PCB dimension
            PCBServer.PreProcess;

            if cDEBUGLEVEL > 0 then
            begin
                ModList.Clear;
                if InSet(cAltKey, KeySet) then ModList.Add('ALT');
                if InSet(cShiftKey, KeySet) then ModList.Add('SHIFT');
                if InSet(cCtrlKey, KeySet) then ModList.Add('CTRL');
                DebugMessage(1, Format('Picked %s @ [%s, %s]; location [%s, %s]%sModifiers: %s', [ Comp.Name.Text, CoordToX(Comp.x), CoordToY(Comp.y), CoordToX(x), CoordToY(y), sLineBreak, ModList.DelimitedText ]));
            end;

            if bDesignator then NameOrComment := Comp.Name else NameOrComment := Comp.Comment;

            // get autoposition relative to component origin
            tc_Autopos := GetRelativeAutoposition(Comp, x, y);
            DebugMessage(2, 'Autoposition Chosen: ' + StrFromAutoPos(tc_Autopos) + sLineBreak + 'cEnableAnyAngle=' + BoolToStr(cEnableAnyAngle, True));

            // Set autoposition based on modifiers
            SetAutopositionLocation(Comp, tc_Autopos, KeySet, bDesignator);

            //GetMinDesignatorClearance(Comp);

            if InSet(cCtrlKey, KeySet) then ParentOnly := False else ParentOnly := True; // hold CTRL to also avoid things that are outside parent component
            DebugMessage(3, 'Begin: IsValidPlacement=' + BoolToStr(IsValidPlacement(NameOrComment, ParentOnly), True));

            MoveDist := AutoMove(NameOrComment, 1200000, ParentOnly, 400000, 10000, tc_Autopos);

            NormalizeText(NameOrComment);

            NameOrComment.GraphicallyInvalidate;

            if MoveDist > 0 then
            begin
                Comp.BeginModify;
                if bDesignator then Comp.ChangeNameAutoPosition(eAutoPos_Manual) else Comp.ChangeCommentAutoPosition(eAutoPos_Manual);
                Comp.EndModify;
                Comp.GraphicallyInvalidate;
                DebugMessage(1, 'AutoMove moved by ' + CoordToStr(MoveDist));
            end
            else
            begin
                ShowInfo('No valid placement found' + sLineBreak + 'Ignored other components = ' + BoolToStr(ParentOnly, True));
            end;

            DebugMessage(3, 'End: IsValidPlacement=' + BoolToStr(IsValidPlacement(NameOrComment, ParentOnly), True));

            if not bPersistentMode then Comp := Nil;  // don't clear Source if in persistent mode (i.e. wait for user to escape)

            bLocationFlag := False; // clear location flag to allow picking a new location

            PCBServer.PostProcess;
            Board.ViewManager_FullUpdate;
        end;

        // Get PCB location
        if Assigned(Comp) then
        begin
            if InSet(cCtrlKey, KeySet) then bDesignator := False else bDesignator := True; // hold CTRL to autoposition Comment instead

            if bDesignator then sPrompt := 'Choose location for Designator around ' else sPrompt := 'Choose location for Comment around ';
            sPrompt := sPrompt + Comp.Name.Text + ' (Hold CTRL to avoid things outside parent component, Hold ALT for orthogonal placement)';

            if Board.ChooseLocation(x, y, sPrompt) then  // false = ESC Key is pressed or right-clicked to cancel
            begin
                KeySet := MkSet(); // clear modifier keyset
                // read modifier keys just as/after the "pick" mouse click
                if AltKeyDown   then KeySet := MkSet(cAltKey);  // Plan: hold ALT to rotate orthogonally
                //if ShiftKeyDown then KeySet := SetUnion(KeySet, MkSet(cShiftKey));
                if ControlKeyDown then KeySet := SetUnion(KeySet, MkSet(cCtrlKey)); // Plan: hold CTRL to stop ignoring outside objects

                bLocationFlag := True;

            end
            else Comp := Nil;    // user canceled picking location, time to get a new source component
        end;

        if not Assigned(Comp) then
        begin
            bLocationFlag := False;
            repeat
               Comp := GetComponentAtCursor('Choose Source Component (Hold CTRL to position Comment instead of Designator)');
               if ControlKeyDown then KeySet := MkSet(cCtrlKey); // CTRL key held down during component pick
            until Assigned(Comp) or (Comp = cEsc);

        end;
    until (Comp = cESC);
end;


// Checks if text object overlaps other object
function IsOverlapping(Text: IPCB_ObjectClass; Obj2: IPCB_ObjectClass) : Boolean;
const
    BODYEXPANSION       = 8; // [mils] Expansion for component bodies
    TEXTEXPANSION       = 5; // [mils] Expansion for other text objects
    PADEXPANSION        = 8; // [mils] Expansion for pads
    CUTOUTEXPANSION     = 0; // [mils] Expansion for cutout regions
    DEFAULTEXPANSION    = 6; // [mils] Expansion for everything else
var
    TextPoly, ObjPoly: IPCB_GeometricPolygon;
    Expansion: TCoord;
begin
    // If silkscreen object equals itself, return False
    if Text.I_ObjectAddress = Obj2.I_ObjectAddress then
    begin
        result := False;
        Exit; // Continue
    end;

    // Continue if Hidden
    if Text.IsHidden or Obj2.IsHidden then
    begin
        result := False;
        Exit; // Continue
    end;

    // Continue if Layers Dont Match
    if not IsSameSide(Text, Obj2) then
    begin
        result := False;
        Exit; // Continue
    end;

    Expansion := MilsToCoord(DEFAULTEXPANSION);
    case Obj2.ObjectId of
        eComponentObject:   Expansion := MilsToCoord(BODYEXPANSION);
        eTextObject:        Expansion := MilsToCoord(TEXTEXPANSION);
        ePadObject:         Expansion := MilsToCoord(PADEXPANSION);
        eRegionObject:      if Obj2.Kind = eRegionKind_Cutout then Expansion := MilsToCoord(CUTOUTEXPANSION);
    end;

    // Get geometric polygons for both objects
    TextPoly := GetObjPoly(Text);
    ObjPoly := GetObjPoly(Obj2, Expansion);

    DebugMessage(3, 'TextPoly Contours' + sLineBreak + DebugGeometricPolygonInfo(TextPoly).Text);
    DebugMessage(3, Obj2.Identifier + sLineBreak + 'ObjPoly Contours' + sLineBreak + DebugGeometricPolygonInfo(ObjPoly).Text);

    // IPCB_ContourUtilities Function  GeometricPolygonsTouch(AGeometricPolygon : IPCB_GeometricPolygon; BGeometricPolygon : IPCB_GeometricPolygon) : Boolean;
    result := PCBServer.PCBContourUtilities.GeometricPolygonsTouch(TextPoly, ObjPoly);
end;


// Check if two layers are the on the same side of the board. Handle different layer names.
function IsSameSide(Obj1: IPCB_ObjectClass; Obj2: IPCB_ObjectClass): Boolean;
var
    Layer1, Layer2: Integer;
begin
    Layer1 := Obj1.Layer;
    Layer2 := Obj2.Layer;
    if Obj1.ObjectId = eComponentBodyObject then
        Layer1 := Obj1.Component.Layer;
    if Obj2.ObjectId = eComponentBodyObject then
        Layer2 := Obj2.Component.Layer;

    // Top Layer
    if (Layer1 = eTopLayer) or (Layer1 = eTopOverlay) then
    begin
        if (Layer2 <> eBottomLayer) and (Layer2 <> eBottomOverlay) then
        begin
            result := True;
            Exit; // return True
        end;
    end
    // Bottom Layer
    else if (Layer1 = eBottomLayer) or (Layer1 = eBottomOverlay) then
    begin
        if (Layer2 <> eTopLayer) and (Layer2 <> eTopOverlay) then
        begin
            result := True;
            Exit; // return True
        end;
    end
    // Multi Layer
    else if (Layer1 = eMultiLayer) or (Layer2 = eMultiLayer) then
    begin
        result := True;
        Exit;
    end;

    result := False;
end;

function IsStringANum(Text : string) : Boolean;
var
    i        : Integer;
    dotCount : Integer;
    ChSet    : TSet;
begin
    Result := True;

    // Test for number, hyphen, dot, or comma
    ChSet := SetUnion(MkSet(Ord('-'), Ord('.'), Ord(',')), MkSetRange(Ord('0'), Ord('9')));
    for i := 1 to Length(Text) do
        if not InSet(Ord(Text[i]), ChSet) then Result := False;

    // test if there is a hyphen that isn't leading
    ChSet := MkSet(Ord('-'));
    for i := 2 to Length(Text) do
        if InSet(Ord(Text[i]), ChSet) then Result := False;

    // Test for more than one hyphen, dot, or comma
    dotCount := 0;
    ChSet    := MkSet(Ord('-'), Ord('.'), Ord(','));
    for i    := 1 to Length(Text) do
        if InSet(Ord(Text[i]), ChSet) then inc(dotCount);

    if dotCount > 1 then Result := False;
end; { IsStringANum }


// search area for objects of given ObjectId and check to see if Text is too close
function IsTextOverObj(Text: IPCB_Text; ObjID: Integer; Filter_Size: Integer; ParentOnly: Boolean = False): Boolean;
var
    Iterator: IPCB_SpatialIterator;
    Obj: IPCB_ObjectClass;
    Rect: TCoordRect;
    RectL, RectR, RectB, RectT: TCoord;
    RegIter: Boolean; // Regular Iterator
    Name: TPCBString;
    DebugString: WideString;
begin
    // Spatial Iterators only work with Primitive Objects and not group objects like eComponentObject and dimensions
    if (ObjID = eComponentObject) then
    begin
        DebugMessage(4, StrFromObjectId(ObjID) + ' Board Iterator created');

        Iterator := Board.BoardIterator_Create;
        Iterator.AddFilter_ObjectSet(MkSet(ObjID));
        Iterator.AddFilter_LayerSet(GetLayerSet(Text.Layer, ObjID));
        Iterator.AddFilter_Method(eProcessAll);
        RegIter := True;
    end
    else
    begin
        Rect := Text.BoundingRectangle;
        RectL := Rect.Left - Filter_Size;
        RectR := Rect.Right + Filter_Size;
        RectT := Rect.Top + Filter_Size;
        RectB := Rect.Bottom - Filter_Size;

        if iDebugLevel >= 4 then DebugString := Format('Left: %s, Right: %s, Top: %s, Bot: %s', [CoordToX(RectL), CoordToX(RectR), CoordToY(RectT), CoordToY(RectB)]) else DebugString := '';
        DebugMessage(4, StrFromObjectId(ObjID) + ' Spatial Iterator Area' + sLineBreak + DebugString);

        Iterator := Board.SpatialIterator_Create;
        Iterator.AddFilter_ObjectSet(MkSet(ObjID));
        if ObjID = eComponentBodyObject then Iterator.AddFilter_LayerSet(GetComponentBodyLayerSet(Text.Component))
        else Iterator.AddFilter_LayerSet(GetLayerSet(Text.Layer, ObjID));

        Iterator.AddFilter_Area(RectL, RectB, RectR, RectT);
        RegIter := False;
    end;

    // Iterate through components or pads or silkscreen etc. Depends on which object is passed in.
    Obj := Iterator.FirstPCBObject;
    while Obj <> nil do
    begin
        // ignore itself
        if Text.I_ObjectAddress = Obj.I_ObjectAddress then
        begin
            Obj := Iterator.NextPCBObject;
            Continue;
        end;

        // Ignore Hidden Objects
        if Obj.IsHidden then
        begin
            Obj := Iterator.NextPCBObject;
            Continue;
        end;

        if (not RegIter) and (Obj.ObjectId = eComponentObject) then
        begin
            Obj := Iterator.NextPCBObject;
            Continue;
        end;

        // skip objects that don't belong to the parent component
        if ParentOnly and (Obj.ObjectId <> eComponentObject) then
        begin
            if (Obj.Component = nil) or (Obj.Component.I_ObjectAddress <> Text.Component.I_ObjectAddress) then
            begin
                Obj := Iterator.NextPCBObject;
                Continue;
            end
            else
            begin
                Sleep(0);
            end;
        end;

        // Convert ComponentBody objects to Component objects
        if Obj.ObjectId = eComponentBodyObject then
        begin
            Obj := Obj.Component;
            //GetComponentBodyLargest(Obj); // for dev debugging

            // skip non-components or components on other side
            if (Obj = nil) or (Obj.Name.Layer <> Text.Layer) then
            begin
                Obj := Iterator.NextPCBObject;
                Continue;
            end;
        end;

        try
            // Check if Silkscreen is overlapping with other object (component/pad/silk)
            if IsOverlapping(Text, Obj) then
            begin
                result := True;
                Exit;
            end;
        except
            Name := Text.Text;
        end;

        Obj := Iterator.NextPCBObject;
    end;

    // Destroy Iterator
    if RegIter then
    begin
        Board.BoardIterator_Destroy(Iterator);
    end
    else
    begin
        Board.SpatialIterator_Destroy(Iterator);
    end;

    result := False;    // nothing detected in area
end;


{ function to check if silkscreen placement is valid }
function IsValidPlacement(Silkscreen : IPCB_Text; ParentOnly : Boolean = False) : Boolean;
const
    FILTERSIZE = 200000;
begin
    Result := False;

    // Silkscreen RefDes Overlap Detection
    if IsTextOverObj(Silkscreen, eTextObject, FILTERSIZE, ParentOnly) then
    begin
        DebugMessage(3, 'eTextObject check failed');
        Exit;
    end
    // Silkscreen Arcs Overlap Detection
    else if IsTextOverObj(Silkscreen, eArcObject, FILTERSIZE, ParentOnly) then
    begin
        DebugMessage(3, 'eArcObject check failed');
        Exit;
    end
    // Silkscreen Tracks Overlap Detection
    else if IsTextOverObj(Silkscreen, eTrackObject, FILTERSIZE, ParentOnly) then
    begin
        DebugMessage(3, 'eTrackObject check failed');
        Exit;
    end
    // Silkscreen Regions Overlap Detection
    else if IsTextOverObj(Silkscreen, eRegionObject, FILTERSIZE, ParentOnly) then
    begin
        DebugMessage(3, 'eRegionObject check failed');
        Exit;
    end
    // Silkscreen Fills Overlap Detection
    else if IsTextOverObj(Silkscreen, eFillObject, FILTERSIZE, ParentOnly) then
    begin
        DebugMessage(3, 'eFillObject check failed');
        Exit;
    end
    // same-side pad overlap detection
    else if IsTextOverObj(Silkscreen, ePadObject, FILTERSIZE, ParentOnly) then
    begin
        DebugMessage(3, 'ePadObject check failed');
        Exit;
    end
    // Component Overlap Detection
    else if IsTextOverObj(Silkscreen, eComponentBodyObject, FILTERSIZE, ParentOnly) then
    begin
        DebugMessage(3, 'eComponentBodyObject check failed');
        Exit;
    end
    // none of the previous checks failed
    else
    begin
        DebugMessage(3, 'IsValidPlacement checks PASSED');
        Result := True;
    end;

end;


{ ** DEPRECATED - replaced with ConfigFile_Read ** function to load preset list from file }
procedure LoadPresetListFromFile(const dummy : Integer);
begin
    PresetFilePath := ClientAPI_SpecialFolder_AltiumApplicationData + '\MoveAPdesignators2Settings.txt';
    PresetList     := CreateObject(TStringList);
    if FileExists(PresetFilePath) then
    begin
        // ShowMessage('Loading presets from ' + PresetFilePath);
        PresetList.LoadFromFile(PresetFilePath); // load presets from file if it exists

        // set text boxes to match preset list (redundant if list was regenerated above)
        tPreset1.Text                     := PresetList[1];
        tPreset2.Text                     := PresetList[2];
        tPreset3.Text                     := PresetList[3];
        tPreset4.Text                     := PresetList[4];
        tPreset5.Text                     := PresetList[5];
        tPreset6.Text                     := PresetList[6];
        tPreset7.Text                     := PresetList[7];
        tPreset8.Text                     := PresetList[8];
        MMmilButton.Caption               := PresetList[9];
        SelectedCheckBox.Checked          := PresetList[10];
        UnHideDesignatorsCheckBox.Checked := PresetList[11];
        EditDistance.Text                 := PresetList[0]; // Main input field needs to be set last because setting each preset updates it
    end
    else
    begin
        // if preset file didn't exist at all, just exit (older file format deprecated)
        exit;
    end;
end; { LoadPresetListFromFile }


{ normalizes IPCB_Text object to be right-reading }
function NormalizeText(var Text : IPCB_Text) : Boolean;
var
    OldAngle, NewAngle      : Double;
    OldJustify, NewJustify  : TTextAutoposition;
begin
    // AD19+ functions used for normalization
    if not IsAtLeastAD19 then
    begin
        Result := False;
        Exit;
    end;

    Result := False;

    // coerce angle to 0 ~ 360
    OldAngle := (Text.Rotation mod 360 + 360) mod 360; // technically an integer operation. TODO: make proper floating point mod throughout
    NewAngle := OldAngle; // use coerced value for later comparison

    // rotate text to match Angle, based on how mirrored text reads from the flipside of the board
    if Text.MirrorFlag then
    begin
        // mirrored text should be rotated to match layer flip behavior of text vs components
        if (OldAngle >= 90) and (OldAngle < 270) then NewAngle := (OldAngle + 180) mod 360
    end
    else
    begin
        // slightly different behavior at 90 and 270 for top side
        if (OldAngle > 90) and (OldAngle <= 270) then NewAngle := (OldAngle + 180) mod 360
    end;

    if Text.Rotation <> NewAngle then
    begin
        OldJustify := Text.TTFInvertedTextJustify;

        // only change justification if the rotation *really* changed
        if NewAngle <> OldAngle then
        begin
            // need to transform justification setting
            case OldJustify of
                eAutoPos_TopLeft        : NewJustify := eAutoPos_BottomRight;
                eAutoPos_CenterLeft     : NewJustify := eAutoPos_CenterRight;
                eAutoPos_BottomLeft     : NewJustify := eAutoPos_TopRight;
                eAutoPos_TopCenter      : NewJustify := eAutoPos_BottomCenter;
                eAutoPos_BottomCenter   : NewJustify := eAutoPos_TopCenter;
                eAutoPos_TopRight       : NewJustify := eAutoPos_BottomLeft;
                eAutoPos_CenterRight    : NewJustify := eAutoPos_CenterLeft;
                eAutoPos_BottomRight    : NewJustify := eAutoPos_TopLeft;
                else                      NewJustify := OldJustify;
            end;
        end
        else NewJustify := OldJustify;

        Text.BeginModify;
        Text.TTFInvertedTextJustify := eAutoPos_CenterCenter; // uses center justification to rotate in place
        Text.Rotation := NewAngle;
        // need to EndModify and BeginModify here to refresh internal Text.Rotation cache, else changing justification will move text
        Text.EndModify;
        Text.BeginModify;
        Text.TTFInvertedTextJustify := NewJustify;
        Text.EndModify;
        Result := True;
    end
    else Result := False;

end;


procedure PresetButtonClicked(Sender : TObject);
begin
    // ShowMessage('PresetButtonClicked');
    if Sender = ButtonPreset1 then EditDistance.Text      := tPreset1.Text
    else if Sender = ButtonPreset2 then EditDistance.Text := tPreset2.Text
    else if Sender = ButtonPreset3 then EditDistance.Text := tPreset3.Text
    else if Sender = ButtonPreset4 then EditDistance.Text := tPreset4.Text
    else if Sender = ButtonPreset5 then EditDistance.Text := tPreset5.Text
    else if Sender = ButtonPreset6 then EditDistance.Text := tPreset6.Text
    else if Sender = ButtonPreset7 then EditDistance.Text := tPreset7.Text
    else if Sender = ButtonPreset8 then EditDistance.Text := tPreset8.Text;
    TweakDesForm.Close;
end; { PresetButtonClicked }


{ rotates IPCB_Text object to specific angle, optionally normalizing it to be right-reading, optionally rotating 90° CCW }
function RotateTextToAngle(var Text : IPCB_Text; const Angle : Double; const Normalize : Boolean = False; const Ortho : Boolean = False) : Double;
var
    mirror : Boolean;
begin
    if Ortho then Angle := Angle + 90;

    // coerce angle to 0 ~ 360
    Angle := (Angle mod 360 + 360) mod 360; // technically an integer operation. TODO: make proper floating point mod throughout

    mirror := false; // x Direction flips on the bottom layer
    if Text.Layer = eBottomOverlay then
        mirror := true;

    // rotate text to match Angle, based on how mirrored text reads from the flipside of the board
    case mirror of
        True :
            begin
                // mirrored text should be rotated to match layer flip behavior of text vs components
                if not Ortho then Angle := (Angle + 180) mod 360;
                if Normalize and (Angle >= 90) and (Angle < 270) then
                    Text.Rotation := (Angle + 180) mod 360
                else
                    Text.Rotation := Angle;
            end;
        False :
            begin
                if Normalize and (Angle > 90) and (Angle <= 270) then
                    Text.Rotation := (Angle + 180) mod 360
                else
                    Text.Rotation := Angle;
            end;
    end;

    Result := Text.Rotation;
end;


{ function to select both components and designators for selected objects }
function SelectCompAndDesignators(dummy : Boolean = False) : Boolean;
var
    i           : Integer;
    CompCount   : Integer;
    TextCount   : Integer;
    errText     : String;
    status      : Integer;
    Comp        : IPCB_Component;
    Text        : IPCB_Text;
    Prim1       : IPCB_Primitive;
begin
    Result := True;

    BothInitialCheck(status);
    if status <> 0 then exit;

    CompCount := 0;
    TextCount := 0;

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if (Prim1.ObjectId = eTextObject) and Prim1.IsDesignator then
        begin
            TextCount := TextCount + 1;
            Comp := Prim1.Component;
            if Comp <> nil then
            begin
                Comp.SetState_Selected(True);
                if Comp.GetState_Selected = True then CompCount := CompCount + 1;
            end;
        end
        else if Prim1.ObjectId = eComponentObject then
        begin
            CompCount := CompCount + 1;
            Text := Prim1.Name;
            if Text <> nil then
            begin
                Text.SetState_Selected(True);
                if Text.GetState_Selected = True then TextCount := TextCount + 1;
            end;
        end;
    end;

    if not (CompCount > 0) then Result := False;

    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

end;


procedure SetAutopositionLocation(var Comp : IPCB_Component; const tc_Autopos : TTextAutoposition; const KeySet : TObjectSet; bDesignator : Boolean = True);
var
    NameOrComment       : IPCB_Text;
    Ortho               : Boolean;
    TempRotation        : Double;
    AfterRotation       : Double;
begin
    TempRotation := 0;

    if bDesignator then NameOrComment := Comp.Name else NameOrComment := Comp.Comment;

    // if ALT key was used, set Ortho flag to rotate 90 deg CCW
    if InSet(cAltKey, KeySet) then Ortho := True else Ortho := False;

    // in any-angle mode, temporarily set to zero rotation
    if (Comp.Rotation <> 0) and cEnableAnyAngle then
    begin
        TempRotation := Comp.Rotation;
        Comp.BeginModify;

        Comp.Rotation := 0;

        Comp.EndModify;
    end;

    NameOrComment.BeginModify;

    // rotate NameOrComment
    AfterRotation := RotateTextToAngle(NameOrComment, 0, True, Ortho);

    // Set text justification according to Autoposition setting
    AutopositionJustify(NameOrComment, tc_Autopos);

    NameOrComment.EndModify;
    NameOrComment.GraphicallyInvalidate;

    Comp.BeginModify;

    if bDesignator then
    begin
        if not Comp.NameOn then Comp.NameOn := True;
        Comp.ChangeNameAutoposition(tc_Autopos);
        // after using autoposition, set to manual if it was temp rotated
        if (TempRotation <> 0) then Comp.ChangeNameAutoposition(eAutoPos_Manual);
    end
    else
    begin
        if not Comp.CommentOn then Comp.CommentOn := True;
        Comp.ChangeCommentAutoposition(tc_Autopos);
        // after using autoposition, set to manual if it was temp rotated
        if (TempRotation <> 0) then Comp.ChangeCommentAutoposition(eAutoPos_Manual);
    end;

    // restore original rotation
    if (TempRotation <> 0) then Comp.Rotation := TempRotation;

    Comp.EndModify;
    Comp.GraphicallyInvalidate;
end;


function StrFromAutoPos(eAutoPos: TTextAutoposition): String;
begin
    { TTextAutoposition = ( eAutoPos_Manual,
                        eAutoPos_TopLeft,
                        eAutoPos_CenterLeft,
                        eAutoPos_BottomLeft,
                        eAutoPos_TopCenter,
                        eAutoPos_CenterCenter,
                        eAutoPos_BottomCenter,
                        eAutoPos_TopRight,
                        eAutoPos_CenterRight,
                        eAutoPos_BottomRight);}
    case eAutoPos of
        0:  result := 'eAutoPos_Manual';
        1:  result := 'eAutoPos_TopLeft';
        2:  result := 'eAutoPos_CenterLeft';
        3:  result := 'eAutoPos_BottomLeft';
        4:  result := 'eAutoPos_TopCenter';
        5:  result := 'eAutoPos_CenterCenter';
        6:  result := 'eAutoPos_BottomCenter';
        7:  result := 'eAutoPos_TopRight';
        8:  result := 'eAutoPos_CenterRight';
        9:  result := 'eAutoPos_BottomRight';
    else
        result := 'Invalid';
    end;
end;


function StrFromObjectId(ObjectId: TObjectId): String;
begin
    case ObjectId of
        0:  Result := 'eNoObject';
        1:  Result := 'eArcObject';
        2:  Result := 'ePadObject';
        3:  Result := 'eViaObject';
        4:  Result := 'eTrackObject';
        5:  Result := 'eTextObject';
        6:  Result := 'eFillObject';
        7:  Result := 'eConnectionObject';
        8:  Result := 'eNetObject';
        9:  Result := 'eComponentObject';
        10: Result := 'ePolyObject';
        11: Result := 'eRegionObject';
        12: Result := 'eComponentBodyObject';
        13: Result := 'eDimensionObject';
        14: Result := 'eCoordinateObject';
        15: Result := 'eClassObject';
        16: Result := 'eRuleObject';
        17: Result := 'eFromToObject';
        18: Result := 'eDifferentialPairObject';
        19: Result := 'eViolationObject';
        20: Result := 'eEmbeddedObject';
        21: Result := 'eEmbeddedBoardObject';
        22: Result := 'eSplitPlaneObject';
        23: Result := 'eTraceObject';
        24: Result := 'eSpareViaObject';
        25: Result := 'eBoardObject';
        26: Result := 'eBoardOutlineObject';
    else
        Result := 'Invalid object ID';
    end;
end;


{ wrapper for TweakDesignators for users familiar with using "Start" }
procedure Start;
begin
    TweakDesignators;
end;


{ Main procedure }
procedure TweakDesignators;
var
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;
    Designator              : IPCB_Text;
    PCBSystemOptions        : IPCB_SystemOptions;
    DRCSetting              : Boolean;
    tc_AutoPos              : TTextAutoposition; // Current AP setting
    MoveDistance            : Integer;
    MaxDistance             : Integer;
    AutoMoveResult          : Integer;
    RotatedAutoPos          : Boolean;
    status, i               : Integer;
    bEnableSelected         : Boolean;

begin
    iDebugLevel := cDEBUGLEVEL;
    // set version label
    LabelVersion.Caption := 'v' + cScriptVersion;

    // set AD build flag
    if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;

    if SelectCompAndDesignators then bEnableSelected := True else bEnableSelected := False;

    // Disables Online DRC during designator movement to improve speed
    PCBSystemOptions := PCBServer.SystemOptions;

    if PCBSystemOptions = Nil then Exit;

    DRCSetting                   := PCBSystemOptions.DoOnlineDRC;
    PCBSystemOptions.DoOnlineDRC := False;
    try
        bAbortScript := False;
        if not bEnableSelected then SelectedCheckBox.Enabled := False;
        TweakDesForm.ShowModal; // Show the settings dialogue (and resume script here after closed)
        if bAbortScript then Exit;

        // Notify the pcbserver that we will make changes (Start undo)
        PCBServer.PreProcess;
        ComponentIteratorHandle := Board.BoardIterator_Create;
        ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
        ComponentIteratorHandle.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
        ComponentIteratorHandle.AddFilter_Method(eProcessAll);

        if TweakDesForm.UnHideDesignatorsCheckBox.Checked then bUnHideDesignators := True
        else bUnHideDesignators := False;

        if TweakDesForm.LazyAutoMoveCheckBox.Checked then bLazyAutoMove := True
        else bLazyAutoMove := False;

        Component := ComponentIteratorHandle.FirstPCBObject;

        if TweakDesForm.SelectedCheckBox.Checked then
            while (Component <> Nil) do
                if not Component.Selected then Component := ComponentIteratorHandle.NextPCBObject
                else break; // Find the first selected comp if select only checked

        // Set the move distance to DB units converted from mils or mm
        if TweakDesForm.MMmilButton.Caption = 'mm' then MoveDistance := MMsToCoord(TweakDesForm.EditDistance.Text)
        else MoveDistance := MilsToCoord(TweakDesForm.EditDistance.Text);

        if bAutoMode then
        begin
            if TweakDesForm.MMmilButton.Caption = 'mm' then MaxDistance := MMsToCoord(TweakDesForm.EditMaxDistance.Text)
            else MaxDistance := MilsToCoord(TweakDesForm.EditMaxDistance.Text);
        end;

        while (Component <> Nil) do
        begin
            Component.BeginModify;

            // Show hidden designators?
            if bUnHideDesignators = True then Component.NameOn := True;

            tc_AutoPos := Component.GetState_NameAutoPos; // Get current AP state

            // Get the designator handle
            Designator := Component.Name;

            // notify that the pcb object is going to be modified
            Designator.BeginModify;

            // Set text justification according to autoposition setting
            AutopositionJustify(Designator, tc_AutoPos);

            // notify that the pcb object is modified
            Designator.EndModify;

            if bAutoMode then
            begin
                AutoMoveResult := AutoMove(Designator, MaxDistance);

                if AutoMoveResult > 0 then
                begin
                    // set autoposition mode to Manual so it doesn't snap back to default position if component is rotated, etc.
                    Component.ChangeNameAutoPosition(eAutoPos_Manual);
                end
                else if bLazyAutoMove then
                begin
                    // if AutoMove failed and we're in lazy mode, treat max offset as fixed offset
                    AutoPosDeltaAdjust(Designator, MaxDistance, tc_AutoPos);
                    Component.ChangeNameAutoPosition(eAutoPos_Manual);
                end;

            end // bAutoMode
            else
            begin
                AutoPosDeltaAdjust(Designator, MoveDistance, tc_AutoPos);
                Component.ChangeNameAutoPosition(eAutoPos_Manual);
            end;

            if (not bAutoMode) or (bLazyAutoMove and (AutoMoveResult = 0)) then
            begin
            end;

            Designator.GraphicallyInvalidate;

            Component.EndModify;
            Component.GraphicallyInvalidate;

            // Get the next component handle
            Component := ComponentIteratorHandle.NextPCBObject;
            if TweakDesForm.SelectedCheckBox.Checked then
                while (Component <> Nil) do
                    if not Component.Selected then Component := ComponentIteratorHandle.NextPCBObject
                    else break; // Find the next selected comp if select only checked


        end; // end while

        // Notify the pcbserver that all changes have been made (Stop undo)
        PCBServer.PostProcess;

        // Refresh the screen (not needed with .GraphicallyInvalidate?)
        Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

        // Destroy the component handle
        Board.BoardIterator_Destroy(ComponentIteratorHandle);

        // save last-used values and presets
        if cUseConfigFile then ConfigFile_Write(ConfigFile_GetPath);

        // deselect designators
        for i := Board.SelectecObjectCount - 1 downto 0 do
        begin
            Designator := Board.SelectecObject[i];
            if (Designator.ObjectId <> eComponentObject) then Designator.SetState_Selected(False);
        end;

        Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);


    finally
        // Restore DRC setting
        PCBSystemOptions.DoOnlineDRC := DRCSetting;
    end;
end; { TweakDesignators }


{ programmatically, OnKeyPress fires before OnChange event and "catches" the key press }
procedure UserKeyPress(Sender : TObject; var Key : Char);
begin
    if (Sender = EditMaxDistance) and (ButtonAuto.Enabled) and (Ord(Key) = 13) then
    begin
        Key := #0; // catch and discard key press to avoid beep
        if ButtonAuto.Enabled then
        begin
            bAutoMode := True;
            TweakDesForm.Close;
        end;
    end
    else if (Sender <> EditMaxDistance) and (ButtonOK.Enabled) and (Ord(Key) = 13) then
    begin
        Key := #0; // catch and discard key press to avoid beep
        if ButtonOK.Enabled then
        begin
            bAutoMode := False;
            TweakDesForm.Close;
        end;
    end;
end; { UserKeyPress }


procedure ValidateOnChange(Sender : TObject);
var
    textbox : TEdit;
begin
    textbox := Sender;
    // ShowMessage(textbox.Text);
    if IsStringANum(textbox.Text) then
    begin
        if Sender <> EditDistance then EditDistance.Text := textbox.Text;

        ButtonOK.Enabled := True;
    end
    else ButtonOK.Enabled := False;

end; { ValidateOnChange }


procedure TTweakDesForm.ButtonAutoClick(Sender : TObject);
begin
    bAutoMode := True;
    TweakDesForm.Close;
end; { TTweakDesForm.ButtonAutoClick }


procedure TTweakDesForm.ButtonCancelClick(Sender : TObject);
begin
    bAbortScript := True;
    TweakDesForm.Close;
end; { TTweakDesForm.ButtonCancelClick }


procedure TTweakDesForm.ButtonInteractiveStartClick(Sender: TObject);
begin
    TweakDesForm.Visible := False; // hide the form because it's modal and sticks around until InteractivelyAutoposition finishes
    try
        InteractivelyAutoposition;
    finally
        bAbortScript := True;
        TweakDesForm.Close; // actually close the invisible form
    end;
end;


procedure TTweakDesForm.ButtonOKClick(Sender : TObject);
begin
    bAutoMode := False;
    TweakDesForm.Close;
end; { TTweakDesForm.ButtonOKClick }


procedure TTweakDesForm.MMmilButtonClick(Sender : TObject);
var
    TempString : string;
    MaxString  : string;
begin
    TempString := EditDistance.Text;
    if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

    MaxString := EditMaxDistance.Text;
    if (LastDelimiter(',.', MaxString) <> 0) then MaxString[LastDelimiter(',.', MaxString)] := DecimalSeparator;

    if MMmilButton.Caption = 'mil' then
    begin
        MMmilButton.Caption := 'mm';
        if IsStringANum(EditDistance.Text) then EditDistance.Text       := CoordToMMs(MilsToCoord(StrToFloat(TempString)));
        if IsStringANum(EditMaxDistance.Text) then EditMaxDistance.Text := CoordToMMs(MilsToCoord(StrToFloat(MaxString)));
    end
    else
    begin
        MMmilButton.Caption := 'mil';
        if IsStringANum(EditDistance.Text) then EditDistance.Text       := CoordToMils(MMsToCoord(StrToFloat(TempString)));
        if IsStringANum(EditMaxDistance.Text) then EditMaxDistance.Text := CoordToMils(MMsToCoord(StrToFloat(MaxString)));
    end;
    EditMaxDistance.Update;
    EditDistance.SetFocus;
    EditDistance.Update;
end; { TTweakDesForm.MMmilButtonClick }


procedure TTweakDesForm.EditDistanceChange(Sender : TObject);
begin

    if IsStringANum(EditDistance.Text) then
    begin
        EditDistance.Font.Color := clWindowText;
        ButtonOK.Enabled        := True;
    end
    else
    begin
        ButtonOK.Enabled        := False;
        EditDistance.Font.Color := clRed;
    end;
end; { TTweakDesForm.EditDistanceChange }


procedure TTweakDesForm.EditMaxDistanceChange(Sender : TObject);
begin

    if IsStringANum(EditMaxDistance.Text) then
    begin
        EditMaxDistance.Font.Color  := clWindowText;
        ButtonAuto.Enabled          := True;
    end
    else
    begin
        ButtonAuto.Enabled          := False;
        EditMaxDistance.Font.Color  := clRed;
    end;
end; { TTweakDesForm.EditMaxDistanceChange }


procedure TTweakDesForm.TweakDesFormShow(Sender : TObject);
begin
    // Set direction control hint
    Application.HintHidePause := 12000; // extend hint show time
    LazyAutoMoveCheckBox.Hint := 'ENABLED: if AutoMove REACHES Max offset without detecting an obstacle, will treat as fixed offset.' + sLineBreak +
        'DISABLED: if AutoMove move finds no obstacles BEFORE the specified Max offset, will abort offset and leave in autoposition.' + sLineBreak +
        'NOTE: AutoMove procedure is internally limited to 120 mil search range.';
    // read presets from file
    // LoadPresetListFromFile(0); // old file format deprecated
    if cUseConfigFile then ConfigFile_Read(ConfigFile_GetPath);
end; { TTweakDesForm.TweakDesFormShow }


procedure TTweakDesForm.LabelVersionClick(Sender : TObject);
begin
    About;
end;


function ConfigFile_GetPath(dummy : String = ''): String;
begin
    result := ClientAPI_SpecialFolder_AltiumApplicationData + '\' + cConfigFileName;
end;


procedure ConfigFile_Write(AFileName : String);
var
    IniFile: TIniFile;
begin
    IniFile := TIniFile.Create(AFileName);

    IniFile.WriteInteger('Window Position', 'Top', TweakDesForm.Top);
    IniFile.WriteInteger('Window Position', 'Left', TweakDesForm.Left);

    IniFile.WriteString('Presets', 'Preset1', tPreset1.Text);
    IniFile.WriteString('Presets', 'Preset2', tPreset2.Text);
    IniFile.WriteString('Presets', 'Preset3', tPreset3.Text);
    IniFile.WriteString('Presets', 'Preset4', tPreset4.Text);
    IniFile.WriteString('Presets', 'Preset5', tPreset5.Text);
    IniFile.WriteString('Presets', 'Preset6', tPreset6.Text);
    IniFile.WriteString('Presets', 'Preset7', tPreset7.Text);
    IniFile.WriteString('Presets', 'Preset8', tPreset8.Text);

    IniFile.WriteString('Last Used', 'Units', MMmilButton.Caption);
    IniFile.WriteBool('Last Used', 'Selected Only', SelectedCheckBox.Checked);
    IniFile.WriteBool('Last Used', 'Unhide Designators', UnHideDesignatorsCheckBox.Checked);
    IniFile.WriteBool('Last Used', 'Lazy Offset', LazyAutoMoveCheckBox.Checked);
    IniFile.WriteString('Last Used', 'Auto Distance', EditMaxDistance.Text);
    IniFile.WriteString('Last Used', 'Fixed Distance', EditDistance.Text);

    IniFile.Free;
end;


procedure ConfigFile_Read(AFileName : String);
var
    IniFile: TIniFile;
begin
    if not FileExists(AFileName) then
    begin
        // ini file doesn't exist, try to fall back on older format file
        LoadPresetListFromFile(0);
        exit;
    end;

    IniFile := TIniFile.Create(AFileName);

    TweakDesForm.Top := IniFile.ReadInteger('Window Position', 'Top', TweakDesForm.Top);
    TweakDesForm.Left := IniFile.ReadInteger('Window Position', 'Left', TweakDesForm.Left);

    tPreset1.Text := IniFile.ReadString('Presets', 'Preset1', tPreset1.Text);
    tPreset2.Text := IniFile.ReadString('Presets', 'Preset2', tPreset2.Text);
    tPreset3.Text := IniFile.ReadString('Presets', 'Preset3', tPreset3.Text);
    tPreset4.Text := IniFile.ReadString('Presets', 'Preset4', tPreset4.Text);
    tPreset5.Text := IniFile.ReadString('Presets', 'Preset5', tPreset5.Text);
    tPreset6.Text := IniFile.ReadString('Presets', 'Preset6', tPreset6.Text);
    tPreset7.Text := IniFile.ReadString('Presets', 'Preset7', tPreset7.Text);
    tPreset8.Text := IniFile.ReadString('Presets', 'Preset8', tPreset8.Text);

    MMmilButton.Caption                 := IniFile.ReadString('Last Used', 'Units', MMmilButton.Caption);
    SelectedCheckBox.Checked            := IniFile.ReadBool('Last Used', 'Selected Only', SelectedCheckBox.Checked);
    UnHideDesignatorsCheckBox.Checked   := IniFile.ReadBool('Last Used', 'Unhide Designators', UnHideDesignatorsCheckBox.Checked);
    LazyAutoMoveCheckBox.Checked        := IniFile.ReadBool('Last Used', 'Lazy Offset', LazyAutoMoveCheckBox.Checked);
    EditMaxDistance.Text                := IniFile.ReadString('Last Used', 'Auto Distance', EditMaxDistance.Text);

    // Main input field needs to be set last because changing some other values trigger it
    EditDistance.Text                   := IniFile.ReadString('Last Used', 'Fixed Distance', EditDistance.Text);

    IniFile.Free;
end;
