{ ****************************************************************************** }
{ * See README.md for release info and documentation
{ ****************************************************************************** }

const
    //status bar command typedefs
    cStatusBar_Panel1               =  1;
    cStatusBar_Panel2               =  2;
    cStatusBar_Panel3               =  3;
    cStatusBar_Panel4               =  4;
    cStatusBar_SetDefault           = -1;
    cStatusBar_WindowCaption        = -2;
    cStatusBar_Push                 = -3;
    cStatusBar_Pop                  = -4;
    cStatusBar_ProgressBarStart     = -5;
    cStatusBar_ProgressBarStop      = -6;
    cStatusBar_ProgressBarStep      = -7;
    cStatusBar_ComplexOpBegin       = -8;
    cStatusBar_ComplexOpEnd         = -9;
    cStatusBar_UndeterminedOpBegin  = -10;
    cStatusBar_UndeterminedOpEnd    = -11;

    cESC                = -1;
    cAltKey             = 1; // don't use when selecting component. Okay when clicking location.
    cShiftKey           = 2; // don't use it for selecting component or location. Makes funny selection stuff happen
    cCtrlKey            = 3; // available for use during component and location selection
    cConfigFileName     = 'QuickSilkSettings.ini';
    cScriptTitle        = 'QuickSilk';
    cScriptVersion      = '1.09';
    cDEBUGLEVEL         = 0;

var
    Board                           : IPCB_Board;
    IsAtLeastAD19                   : Boolean;
    iDebugLevel                     : Integer;
    bAutoMode                       : Boolean;
    bEnableAnyAngle                 : Boolean;
    bEnableFormSelectableCheck      : Boolean;
    bExtraOffsets                   : Boolean;
    bForbidLocalSettings            : Boolean;
    bIgnoreCBChange                 : Boolean;
    bLazyAutoMove                   : Boolean;
    bPersistentMode                 : Boolean;
    bUnHideDesignators              : Boolean;
    CompKeySet                      : TObjectSet; // keyboard modifiers used during component selection
    ESTOP                           : Boolean;
    iClearanceMode                  : Integer;
    IsCompSelectable                : Boolean;
    IsTextSelectable                : Boolean;
    iViaClearanceMode               : Integer;
    LocKeySet                       : TObjectSet; // keyboard key modifiers <alt> <shift> <cntl>
    PresetFilePath                  : String;
    PresetList                      : TStringList;
    TEXTEXPANSION, BODYEXPANSION    : TCoord;
    PADEXPANSION, CUTOUTEXPANSION   : TCoord;
    VIAEXPANSION, DEFAULTEXPANSION  : TCoord;
    gvCurrentPercentCount           : Integer; // used for progress status bar
    gvMarquee                       : Boolean; // used for progress status bar
    gvOldPercent                    : Integer; // used for progress status bar
    gvTotalPercentCount             : Integer; // used for progress status bar


procedure   _GUI; forward;
procedure   _QuickSilk; forward;
procedure   About; forward;
function    AutoMove(var NameOrComment : IPCB_Text; ParentOnly : Boolean = True; StartDist : TCoord = 200000; ForceAutoPos : TTextAutoposition = eAutoPos_Manual) : TCoord; forward;
function    AutoPosDeltaGetMax(var NameOrComment : IPCB_Text; autoPos : TTextAutoposition; bAnyAngleFlag : Boolean = False) : TCoord; forward;
procedure   AutoPosDeltaAdjust(var NameOrComment : IPCB_Text; MoveDistance : TCoord; autoPos : TTextAutoposition; bAnyAngleFlag : Boolean = False; SideOffset : Integer = 0); forward;
function    AutopositionJustify(var NameOrComment : IPCB_Text; const tc_AutoPos : TTextAutoposition) : TTextAutoposition; forward;
procedure   BothInitialCheck(var status : Integer); forward;
procedure   ChangeTextUnits(Units : TUnit); forward;
function    ConfigFile_GetPath(dummy : String = '') : String; forward;
procedure   ConfigFile_Read(AFileName : String); forward;
procedure   ConfigFile_Write(AFileName : String); forward;
function    CountValidInSelected : Integer; forward;
function    DeselectInvalid : Integer; forward;
function    DocumentIsPCB : Boolean; forward;
function    ESTOP_Assert(dummy : Boolean = False); forward;
function    ESTOP_ReadAndClear(dummy : Boolean = False) : Boolean; forward;
function    GetComponentAtCursor(const sPrompt : TString) : IPCB_Primitive; forward;
function    GetLayerSet(SlkLayer: Integer; ObjID: Integer) : TV7_LayerSet; forward;
function    GetMinDesignatorClearance(var Comp : IPCB_Component) : TCoord; forward;
function    GetObjPoly(Obj: IPCB_ObjectClass; Expansion: TCoord = 0; ALayer : TV7_Layer = eNoLayer) : IPCB_GeometricPolygon; forward;
function    GetRelativeAutoposition(var Comp : IPCB_Component; const loc_x, loc_y : TCoord) : TTextAutoposition; forward;
function    GUI_ForLoopStart(StatusMsg : String; LoopCount : Integer) : Integer; forward;
procedure   GUI_LoopEnd(dummy : Boolean = False); forward;
procedure   GUI_LoopProgress(StatusMsg : String = ''); forward;
procedure   InteractivelyAutoposition; forward;
function    IsForbiddenVia(Silk : IPCB_Text; ViaObj : IPCB_ObjectClass) : Boolean; forward;
function    IsOverlapping(Text: IPCB_ObjectClass; Obj2: IPCB_ObjectClass; ObjID : Integer = eNoObject) : Boolean; forward;
procedure   IsRuleViolation(ObjectIDSet : TObjectSet; RuleNameStr : String; Clearance : TCoord); forward;
function    IsSelectableCheck(bWarnComp : Boolean = False; bWarnText : Boolean = False) : Boolean; forward;
function    IsTextOverObj(Text : IPCB_Text; ObjID : Integer; Filter_Size : Integer; ParentOnly : Boolean = False; StrictRegions : Boolean = False) : Boolean; forward;
function    IsValidPlacement(Silkscreen : IPCB_Text; ParentOnly : Boolean = False) : Boolean; forward;
procedure   LoadPresetListFromFile(const dummy : Integer); forward;
procedure   ProcessMain(dummy : Boolean = False); forward;
function    RotateTextToAngle(var Text : IPCB_Text; const Angle : Double; const Normalize : Boolean = False; const Ortho : Boolean = False) : Double; forward;
function    SelectCompAndDesignators(dummy : Boolean = False) : Boolean; forward;
procedure   SetAutopositionLocation(var Comp : IPCB_Component; const tc_Autopos : TTextAutoposition; bDesignator : Boolean = True); forward;
procedure   SetButtonEnableStates(EnableState : Boolean; bFilteredOnly : Boolean = False); forward;
function    SilkViolatesRule(Silkscreen : IPCB_Text; ObjectIDSet : TObjectSet) : Boolean; forward;
procedure   TweakDesignators; forward;
procedure   UpdateConstants(dummy : Boolean = False); forward;
procedure   UpdateViaRule(dummy : Boolean = False); forward;
procedure   TQuickSilkForm.ButtonAutoClick(Sender : TObject); forward;
procedure   TQuickSilkForm.ButtonCancelClick(Sender : TObject); forward;
procedure   TQuickSilkForm.ButtonInteractiveStartClick(Sender : TObject); forward;
procedure   TQuickSilkForm.ButtonOKClick(Sender : TObject); forward;
procedure   TQuickSilkForm.ButtonSaveConfigClick(Sender : TObject); forward;
procedure   TQuickSilkForm.CheckBoxLocalSettingsClick(Sender: TObject); forward;
procedure   TQuickSilkForm.ClearanceEnterKey(Sender : TObject; var Key : Char); forward;
procedure   TQuickSilkForm.ConfigClick(Sender : TObject); forward;
procedure   TQuickSilkForm.InputValueChange(Sender : TObject); forward;
procedure   TQuickSilkForm.LabelClearanceMouseEnter(Sender: TObject); forward;
procedure   TQuickSilkForm.LabelClearanceMouseLeave(Sender: TObject); forward;
procedure   TQuickSilkForm.LabelVersionClick(Sender : TObject); forward;
procedure   TQuickSilkForm.MMmilButtonClick(Sender : TObject); forward;
procedure   TQuickSilkForm.PresetButtonClicked(Sender : TObject); forward;
procedure   TQuickSilkForm.PresetValueChange(Sender : TObject); forward;
procedure   TQuickSilkForm.QuickSilkFormShow(Sender : TObject); forward;
procedure   TQuickSilkForm.RuleCheckClick(Sender : TObject); forward;
procedure   TQuickSilkForm.UserKeyPress(Sender : TObject; var Key : Char); forward;
procedure   AddMessage(MessageClass, MessageText: String); forward;
function    CalculateCentroid(const contour : IPCB_Contour; out CentroidX : TCoord; out CentroidY : TCoord) : Boolean; forward;
procedure   ClientDeSelectAll(dummy : Boolean = False); forward;
procedure   ClientZoomRedraw(dummy : Boolean = False); forward;
procedure   ClientZoomSelected(dummy : Boolean = False); forward;
function    CoordToDisplayStr(coords : TCoord) : String; forward;
function    CoordToDisplayX(coords : TCoord) : String; forward;
function    CoordToDisplayY(coords : TCoord) : String; forward;
procedure   CopyTextFormatFromTo(SourceText : IPCB_Text; TargetText : IPCB_Text); forward;
function    DebugContourInfo(contour : IPCB_Contour) : TStringList; forward;
function    DebugGeometricPolygonInfo(poly : IPCB_GeometricPolygon) : TStringList; forward;
function    DebugLevelStr(dummy : String = '') : String; forward;
procedure   DebugMessage(const ShowLevel : Integer; const msg : WideString; const Caption : String = 'Confirm or Cancel Debug'); forward;
function    FolderIsReadOnly(const AFolderPath : String) : Boolean; forward;
function    GetComponentAreaMils(Comp : IPCB_Component) : Int64; forward;
function    GetComponentBodyLargest(Comp : IPCB_Component) : IPCB_ComponentBody; forward;
function    GetComponentBodyLayerSet(Comp : IPCB_Component) : TV7_LayerSet; forward;
function    GetDesignator(var Comp : IPCB_Component) : IPCB_Primitive; forward;
function    GetIteratorCount(Iter : IPCB_BoardIterator) : Integer; forward;
function    GetSelectedAssyTextCount(dummy : Boolean = False) : Integer; forward;
function    GetSelectedComponentCount(dummy : Boolean = False) : Integer; forward;
procedure   Inspect_IPCB_Text(var Text : IPCB_Text3; const MyLabel : string = ''); forward;
function    IsSameSide(Obj1: IPCB_ObjectClass; Obj2: IPCB_ObjectClass) : Boolean; forward;
function    IsStringANum(Text : string) : Boolean; forward;
function    IsViaOnBottomSide(ViaObj : IPCB_Via) : Boolean; forward;
function    IsViaOnTopSide(ViaObj : IPCB_Via) : Boolean; forward;
function    IsViaTentedForSilk(Silk : IPCB_Text; ViaObj : IPCB_Via) : Boolean; forward;
procedure   NormalizeSelectedWithJustification; forward;
function    NormalizeText(var Text : IPCB_Text) : Boolean; forward;
procedure   ReportFootprintNames_Selected(header : String = 'Footprint Names:'); forward;
procedure   ReportFootprintNames_All(header : String = 'Footprint Names:'); forward;
function    RoundCoordAuto(coords : TCoord) : TCoord; forward;
function    RoundCoords(coords : TCoord; round_mult : Double; units : TUnit) : TCoord; forward;
function    SelectAllComponents(dummy : Boolean = False) : Integer; forward;
function    StrFromAutoPos(eAutoPos: TTextAutoposition) : String; forward;
function    StrFromObjectId(ObjectId: TObjectId) : String; forward;
function    StrFromObjectIdSet(ObjectIdSet : TObjectSet) : String; forward;
procedure   MyStatusBar_SetState(Index : Integer; const S : WideString); forward;
function    MyStatusBar_GetState(Index : Integer) : Widestring; forward;
procedure   MyStatusBar_SetStateDefault(dummy : Boolean = False); forward;
procedure   MyStatusBar_PushStatus(dummy : Boolean = False); forward;
procedure   MyStatusBar_PopStatus(dummy : Boolean = False); forward;
function    MyPercentActive(dummy : Boolean = False) : Boolean; forward;
function    MyMarqueeActive(dummy : Boolean = False) : Boolean; forward;
function    MyPercent_GetTotal(dummy : Boolean = False) : Integer; forward;
procedure   MyPercent_Init(const InitialString : String ; TotalCount : Integer); forward;
procedure   MyPercent_Finish(dummy : Boolean = False); forward;
procedure   MyPercent_UpdateByNumber(AmountToIncrement : Integer, StatusMsg : String = ''); forward;
procedure   MyPercent_Update(StatusMsg : String = ''); forward;
procedure   MyPercent_BeginUndeterminedOperation(const InitialString : String); forward;
procedure   MyPercent_EndUndeterminedOperation(dummy : Boolean = False); forward;
procedure   MyPercent_BeginComplexOperation(const InitialString : String); forward;
procedure   MyPercent_EndComplexOperation(dummy : Boolean = False); forward;


{ GUI launcher, named this way to sort to the top of process list }
procedure   _GUI;
begin
    if not DocumentIsPCB then exit;
    if not DocumentIsPCB_Utils then exit;
    //ProcessMain;

    QuickSilkForm.FormStyle := fsStayOnTop;
    QuickSilkForm.Show;
end;

{ wrapper for InteractivelyAutoposition that sorts at the top of the list }
procedure   _QuickSilk;
begin
    InteractivelyAutoposition;
end;

procedure   About;
var
    MsgText : string;
begin
    MsgText := '"' + cScriptTitle + '" script version ' + cScriptVersion + sLineBreak +
        sLineBreak +
        'Use "_GUI" to configure clearances and adjust all or selected autopositioned designators' + sLineBreak +
        sLineBreak +
        'Use "_QuickSilk" to interactively place Designator or Comment for individual components.' + sLineBreak +
        sLineBreak +
        'Updated versions and documentation may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries' + sLineBreak +
        '/tree/master/Scripts - PCB/QuickSilk' + sLineBreak +
        sLineBreak +
        'Settings save location:' + sLineBreak +
        ConfigFile_GetPath;

    ShowInfo(MsgText, 'About');
end; { About }

function    AutoMove(var NameOrComment : IPCB_Text; ParentOnly : Boolean = True; StartDist : TCoord = 200000; ForceAutoPos : TTextAutoposition = eAutoPos_Manual) : TCoord;
const
    //cSEARCHDIST         = 1200000; // not used since `AutoPosDeltaGetMax` // don't make this too big or it leaves room to jump into valid placement on other side of part
    cMINSEARCHDIST      = 50000; // [coords] 5 mils min distance step to try when searching for open area
var
    MoveDist, TotalDist : TCoord;
    NameOrCommentAP     : TTextAutoposition;
    SearchDist          : TCoord;
    CurSearchDist       : TCoord;
    bFirstPass          : Boolean;
    bRunoff             : Boolean;
    bDefaultValid       : Boolean;
    bAnyAngleFlag       : Boolean;
    bValidPlacement     : Boolean;
    TextTypeStr         : String;
    SideOffsets         : array[0..12]; // make sure element count and LastOffsetIndex are in sync
    SideOffset          : Integer;
    OffsetIndex         : Integer;
    LastOffsetIndex     : Integer;
    MinDist             : TCoord;
begin
    MinDist := 5000; // [Coord] 0.5mil min distance step to try when fine tuning
    //SearchDist := MIN(SearchDist, cSEARCHDIST);
    bFirstPass := False;
    bRunoff := False;
    bValidPlacement := False;
    bDefaultValid := True;

    SideOffsets := [0, 1, -1, 2, -2, 3, -3, 4, -4, 5, -5, 6, -6]; // excessive number of offsets will increase time to fail dramatically
    OffsetIndex := 0;
    LastOffsetIndex := 12;

    if not (NameOrComment <> nil and NameOrComment.InComponent) then
    begin
        Result := 0;
        Exit;
    end;
    TotalDist := 0;

    bAnyAngleFlag := (ForceAutoPos <> eAutoPos_Manual) and bEnableAnyAngle;

    if bAnyAngleFlag then NameOrCommentAP := ForceAutoPos
    else if NameOrComment.IsDesignator then NameOrCommentAP := NameOrComment.Component.NameAutoPosition
    else NameOrCommentAP := NameOrComment.Component.CommentAutoPosition;

    if (NameOrCommentAP = eAutoPos_Manual) or (NameOrCommentAP = eAutoPos_CenterCenter) then
    begin
        if NameOrComment.IsDesignator then TextTypeStr := 'Designator' else TextTypeStr := 'Comment';
        DebugMessage(1, TextTypeStr + ' for component ' + NameOrComment.Component.Name.Text + ' is not Autopositioned. Automove canceled.');
        Result := 0;
        Exit;
    end;

    // TODO: based on autoposition and bAnyAngleFlag, derive SearchDist from component bounding box ((size * 0.65) rounded up to the nearest 10 mils)
    // since this will indicate the farthest you could possibly move without coming out the other side (easier with bAnyAngleFlag TRUE)

    // get max search distance from context
    SearchDist := AutoPosDeltaGetMax(NameOrComment, NameOrCommentAP, bAnyAngleFlag);
    StartDist := MIN(StartDist, cMINSEARCHDIST * 8);
    CurSearchDist := SearchDist;

    BeginHourGlass;

    // special case when avoiding objects outside parent: try moving up to parent ignoring others first, and see if that passes
    if not ParentOnly then
    begin
        EndHourGlass; // end before recursive call

        // call AutoMove recursively but only avoiding parent objects
        Result := AutoMove(NameOrComment, True, StartDist, ForceAutoPos);

        if (Result > 0) and IsValidPlacement(NameOrComment, ParentOnly) then
        begin
            // if Result is positive then it found a parent-only solution - if it still passes when evaluating outside the parent, we're done
            exit;
        end
        else if Result > 0 then
        begin
            // if there was a parent-only solution, we can reduce the search space (say, at least 5 mils larger and a multiple of 10 mils)
            SearchDist := ((Result + 50000 + 99999) div 100000) * 100000;
            CurSearchDist := SearchDist;
            DebugMessage(2, 'Adjusted search range to ' + CoordToDisplayStr(CurSearchDist));

            // if result is positive but there is not a complete solution, try offset nudging against parent comp (note that this is a gamble)
            BeginHourGlass; // resume hourglass after recursive call if didn't exit
            repeat
                //TotalDist := 0;

                // transform offset array [0, 1, -1, 2, -2, 3, -3, 4, -4, 5, -5, 6, -6] to [1, -2, 3, -4, 5, -6, 7, -8, 9, -10, 11, -12, 6]
                // to save calls to AutoPosDeltaAdjust() and end at no offset
                if OffsetIndex < LastOffsetIndex then
                begin
                    if SIGN(SideOffsets[OffsetIndex]) > 0 then SideOffset := -SideOffsets[OffsetIndex] * 2 else SideOffset := -SideOffsets[OffsetIndex] * 2 + 1;
                end
                else SideOffset := -SideOffsets[OffsetIndex];

                // try next offset
                AutoPosDeltaAdjust(NameOrComment, 0, NameOrCommentAP, bAnyAngleFlag, SideOffset);
                bValidPlacement := IsValidPlacement(NameOrComment, ParentOnly);

                DebugMessage(1, 'Adjusting offset.' + sLineBreak +
                        'SideOffset=' + IntToStr(SideOffsets[OffsetIndex] + SideOffset) + sLineBreak +
                        'Valid Placement=' + BoolToStr(bValidPlacement, True));
                if iDebugLevel >= 2 then NameOrComment.GraphicallyInvalidate;
                if iDebugLevel >= 2 then Application.ProcessMessages;

                Inc(OffsetIndex);
            until bValidPlacement or (OffsetIndex > LastOffsetIndex);

            if bValidPlacement then
            begin
                EndHourGlass; // end hourglass before exit
                exit; // note that result will just be move amount toward parent
            end;

            // restore to start position
            AutoPosDeltaAdjust(NameOrComment, -Result, NameOrCommentAP, bAnyAngleFlag);
            Result := 0;

            DebugMessage(1, 'Returned to start position.');
            if (iDebugLevel > 0) and (cDEBUGLEVEL >= 2) then iDebugLevel := MAX(iDebugLevel, cDEBUGLEVEL); // reset debug level unless it was 0
            if iDebugLevel >= 2 then NameOrComment.GraphicallyInvalidate;
            if iDebugLevel >= 2 then Application.ProcessMessages;

            OffsetIndex := 0;
        end;

    end;

    if not IsValidPlacement(NameOrComment, ParentOnly) then
    begin
        bDefaultValid := False;
        StartDist := cMINSEARCHDIST * 4;
        DebugMessage(1, 'Initial placement interferes. Starting move distance at ' + CoordToDisplayStr(StartDist) + sLineBreak +
                'CurSearchDist=' + CoordToDisplayStr(CurSearchDist));
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
                //CurSearchDist := SearchDist div 2; // runoff shouldn't be a problem with SearchDist derived from bounding rectangle
                MoveDist := cMINSEARCHDIST * 2;
                DebugMessage(2, 'Runaway detected. Resetting to start position.' + sLineBreak +
                        'New MoveDist=' + CoordToDisplayStr(MoveDist) + sLineBreak +
                        'CurSearchDist=' + CoordToDisplayStr(CurSearchDist) + sLineBreak +
                        'bFirstPass=' + BoolToStr(bFirstPass, True) + sLineBreak +
                        'ParentOnly=' + BoolToStr(ParentOnly, True) + sLineBreak +
                        'bRunuff=' + BoolToStr(bRunoff, True) + sLineBreak +
                        'SideOffset=' + IntToStr(SideOffsets[OffsetIndex]) + sLineBreak +
                        'Valid Placement=' + BoolToStr(bDefaultValid, True));
            end
            else
            begin
                bFirstPass := False;
                // if ParentOnly mode unless extra offsets is enabled, we're done, otherwise set MoveDist to 0 to trigger offset step
                if ParentOnly or (not bExtraOffsets) then break else MoveDist := 0;
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
            DebugMessage(2, 'Total move distance=' + CoordToDisplayStr(TotalDist) + sLineBreak +
                    'Moved by MoveDist=' + CoordToDisplayStr(MoveDist) + sLineBreak +
                    'CurSearchDist=' + CoordToDisplayStr(CurSearchDist) + sLineBreak +
                    'bFirstPass=' + BoolToStr(bFirstPass, True) + sLineBreak +
                    'ParentOnly=' + BoolToStr(ParentOnly, True) + sLineBreak +
                    'bRunuff=' + BoolToStr(bRunoff, True) + sLineBreak +
                    'SideOffset=' + IntToStr(SideOffsets[OffsetIndex]) + sLineBreak +
                    'Valid Placement=True');
            MoveDist := ABS(MoveDist);  // always move positive after pass
        end
        else
        begin
            DebugMessage(2, 'Total move distance=' + CoordToDisplayStr(TotalDist) + sLineBreak +
                    'Failed move by MoveDist=' + CoordToDisplayStr(MoveDist) + sLineBreak +
                    'CurSearchDist=' + CoordToDisplayStr(CurSearchDist) + sLineBreak +
                    'bFirstPass=' + BoolToStr(bFirstPass, True) + sLineBreak +
                    'ParentOnly=' + BoolToStr(ParentOnly, True) + sLineBreak +
                    'bRunuff=' + BoolToStr(bRunoff, True) + sLineBreak +
                    'SideOffset=' + IntToStr(SideOffsets[OffsetIndex]) + sLineBreak +
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
                    if bRunoff then CurSearchDist := SearchDist div 2; // after first pass backward, reduce search space to optimize time
                    MoveDist := ABS(MoveDist) div 2; // Start moving in the positive direction and halve distance
                end;

                // if moves are too small to likely be productive...
                if ABS(MoveDist) < cMINSEARCHDIST then
                    if ParentOnly or (not bExtraOffsets) then break
                    else
                    begin
                        // if not ParentOnly, step through lateral offsets in case a nudge will give a passing solution
                        if OffsetIndex < LastOffsetIndex then
                        begin
                            // undo previous moves and offset
                            AutoPosDeltaAdjust(NameOrComment, -TotalDist, NameOrCommentAP, bAnyAngleFlag, -SideOffsets[OffsetIndex]);
                            TotalDist := 0;

                            // setup next offset
                            Inc(OffsetIndex);
                            AutoPosDeltaAdjust(NameOrComment, 0, NameOrCommentAP, bAnyAngleFlag, SideOffsets[OffsetIndex]);
                            DebugMessage(1, 'Proceeding to next offset.' + sLineBreak + 'SideOffset=' + IntToStr(SideOffsets[OffsetIndex]));
                            if (iDebugLevel > 0) and (cDEBUGLEVEL >= 2) then iDebugLevel := MAX(iDebugLevel, cDEBUGLEVEL); // reset debug level unless it was 0
                            if iDebugLevel >= 2 then NameOrComment.GraphicallyInvalidate;
                            if iDebugLevel >= 2 then Application.ProcessMessages;

                            // reset loop for a fresh pass
                            MoveDist := StartDist;
                            bRunoff := False;
                        end;
                    end;
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
        AutoPosDeltaAdjust(NameOrComment, -TotalDist, NameOrCommentAP, bAnyAngleFlag, -SideOffsets[OffsetIndex]);
        DebugMessage(2, 'Failed to find passing placement within ' + CoordToDisplayStr(TotalDist) + '. Move attempt cancelled.');
        TotalDist := 0;
    end;

    Result := TotalDist;

    EndHourGlass;
end;

function    AutoPosDeltaGetMax(var NameOrComment : IPCB_Text; autoPos : TTextAutoposition; bAnyAngleFlag : Boolean = False) : TCoord;
var
    BRdX, BRdY : TCoord;
    rect : TCoordRect;
    taller : Boolean;
    TempRotation : Double;
    Comp : IPCB_Component;
    CompBRect : TCoordRect;
begin
    Result := 0;
    Comp := NameOrComment.Component;
    if Comp = nil then exit;

    TempRotation := Comp.Rotation;

    if bAnyAngleFlag and (TempRotation <> 0) then
    begin
        Comp.BeginModify;
        Comp.Rotation := 0;
        Comp.EndModify;
    end;

    // Top|Bottom Left|Right autopos behaves differently for strings that are height>width
    rect := NameOrComment.BoundingRectangle;
    taller := (rect.Top - rect.Bottom) > (rect.Right - rect.Left);

    CompBRect := Comp.BoundingRectangleNoNameComment;
    BRdX := CompBRect.Right - CompBRect.Left;
    BRdY := CompBRect.Top - CompBRect.Bottom;
    DebugMessage(2, 'Parent component ' + Comp.Name.Text + ' BoundingRectangleNoNameComment' + sLineBreak +
            'Left: ' + CoordToDisplayX(CompBRect.Left) + sLineBreak +
            'Right: ' + CoordToDisplayX(CompBRect.Right) + sLineBreak +
            'Top: ' + CoordToDisplayY(CompBRect.Top) + sLineBreak +
            'Bottom: ' + CoordToDisplayY(CompBRect.Bottom) + sLineBreak +
            'Width: ' + CoordToDisplayStr(BRdX) + sLineBreak +
            'Height: ' + CoordToDisplayStr(BRdY));

    Case autoPos of
        eAutoPos_CenterRight:   Result := BRdX;
        eAutoPos_TopCenter:     Result := BRdY;
        eAutoPos_CenterLeft:    Result := BRdX;
        eAutoPos_BottomCenter:  Result := BRdY;
        eAutoPos_TopLeft:       if taller then Result := BRdX else Result := BRdY;
        eAutoPos_TopRight:      if taller then Result := BRdX else Result := BRdY;
        eAutoPos_BottomLeft:    if taller then Result := BRdX else Result := BRdY;
        eAutoPos_BottomRight:   if taller then Result := BRdX else Result := BRdY;
    end;

    if bAnyAngleFlag and (TempRotation <> 0) then
    begin
        Comp.BeginModify;
        Comp.Rotation := TempRotation;
        Comp.EndModify;
    end;

    // round up to multiple of 10 mils, plus add the 20 mils that autoposition stands off the bounding rectangle
    // PERFORMANCE: Note that this assumes text could traverse all the way across the bounding rectangle before hitting something
    // but that should only be possible if the footprint's primitives are offset from the bounding rectangle (an edge connector, for example)
    Result := (((Result + 99999) div 100000) * 100000) + 200000;
end;

procedure   AutoPosDeltaAdjust(var NameOrComment : IPCB_Text; MoveDistance : TCoord; autoPos : TTextAutoposition; bAnyAngleFlag : Boolean = False; SideOffset : Integer = 0);
const
    cOFFSET = 50000; // [coords] 5 mils might be finer than necessary but is conservative
var
    dx, dy, d: Integer;
    flipx: Integer;
    rect : TCoordRect;
    taller : Boolean;
    TempRotation : Double;
    Comp : IPCB_Component;
begin
    if bAnyAngleFlag then
    begin
        Comp := NameOrComment.Component;
        TempRotation := Comp.Rotation;

        Comp.BeginModify;
        Comp.Rotation := 0;
        Comp.EndModify;
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
        begin
            dx := -d * flipx;
            dy := SideOffset * cOFFSET; // offset direction doesn't really matter, this is brute force
        end
        eAutoPos_TopCenter:
        begin
            dx := SideOffset * cOFFSET;
            dy := -d;
        end
        eAutoPos_CenterLeft:
        begin
            dx := d * flipx;
            dy := SideOffset * cOFFSET;
        end
        eAutoPos_BottomCenter:
        begin
            dx := SideOffset * cOFFSET;
            dy := d;
        end
        eAutoPos_TopLeft:
        begin
            dx := SideOffset * cOFFSET;
            dy := -d;
        end
        eAutoPos_TopRight:
        begin
            dx := SideOffset * cOFFSET;
            dy := -d;
        end
        eAutoPos_BottomLeft:
        begin
            dx := SideOffset * cOFFSET;
            dy := d;
        end
        eAutoPos_BottomRight:
        begin
            dx := SideOffset * cOFFSET;
            dy := d;
        end
    end;

    if taller then
    begin
        if (autoPos = eAutoPos_TopLeft) or (autoPos = eAutoPos_BottomLeft) then
        begin
            dx := d * flipx;
            dy := SideOffset * cOFFSET;
        end
        else if (autoPos = eAutoPos_TopRight) or (autoPos = eAutoPos_BottomRight) then
        begin
            dx := -d * flipx;
            dy := SideOffset * cOFFSET;
        end;
    end;

    if bAnyAngleFlag then
    begin
        NameOrComment.BeginModify;
        NameOrComment.MoveByXY(dx, dy);
        NameOrComment.EndModify;

        Comp.BeginModify;
        Comp.Rotation := TempRotation;
        Comp.EndModify;
    end
    else
    begin
        NameOrComment.BeginModify;
        NameOrComment.MoveByXY(dx, dy);
        NameOrComment.EndModify;
    end;
end;

{ function to transform text justification based on IPCB_Text autoposition and rotation }
function    AutopositionJustify(var NameOrComment : IPCB_Text; const tc_AutoPos : TTextAutoposition) : TTextAutoposition;
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
procedure   BothInitialCheck(var status : Integer);
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

procedure   ChangeTextUnits(Units : TUnit);
var
    i           : Integer;
    ControlList : TObjectList;
    EditControl : TObject;
    TempString  : String;
    EditString  : String;
begin
    ControlList := CreateObject(TObjectList);
    ControlList.OwnsObjects := False; // required to not throw invalid pointer errors when list is freed

    ControlList.Add(tPreset1);
    ControlList.Add(tPreset2);
    ControlList.Add(tPreset3);
    ControlList.Add(tPreset4);
    ControlList.Add(tPreset5);
    ControlList.Add(tPreset6);
    ControlList.Add(tPreset7);
    ControlList.Add(tPreset8);
    ControlList.Add(tClearanceText);
    ControlList.Add(tClearanceBody);
    ControlList.Add(tClearancePad);
    ControlList.Add(tClearanceCutout);
    ControlList.Add(tClearanceVia);
    ControlList.Add(tClearanceDefault);
    ControlList.Add(EditMaxDistance);
    ControlList.Add(EditDistance);

    EditString := EditDistance.Text;

    for i := 0 to ControlList.Count - 1 do
    begin
        EditControl := ControlList[i];
        if EditControl = nil then continue;

        // to work around presets triggering EditControl changes
        if EditControl = EditDistance then EditControl.Text := EditString;

        TempString := EditControl.Text;
        if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

        if Units = eMetric then
        begin
            if IsStringANum(EditControl.Text) then EditControl.Text := CoordToMMs(MilsToCoord(StrToFloat(TempString)));
        end
        else
        begin
            if IsStringANum(EditControl.Text) then EditControl.Text := CoordToMils(MMsToCoord(StrToFloat(TempString)));
        end;
    end;

end;

function    ConfigFile_GetPath(dummy : String = '') : String;
begin
    Result := ExtractFilePath(GetRunningScriptProjectName) + cConfigFileName;
    if (not FileExists(Result)) or bForbidLocalSettings then Result := IncludeTrailingPathDelimiter(SpecialFolder_AltiumApplicationData) + cConfigFileName;
end;

procedure   ConfigFile_Read(AFileName : String);
var
    IniFile             : TIniFile;
    LocalSettingsFile   : String;
    ConfigDebugCaption  : String;
    SettingsDebugFile   : String;
    SettingsDebugList   : TStringList;
begin
    LocalSettingsFile := ExtractFilePath(GetRunningScriptProjectName) + cConfigFileName;

    // set CheckBoxLocalSettings.Checked to true if local settings file exists
    if FileExists(LocalSettingsFile) then
    begin
        // FileIsReadOnly doesn't seem to work correctly
        if FileIsReadOnly(LocalSettingsFile) or bForbidLocalSettings then
        begin
            ShowWarning('Local settings file or script folder is read-only:' + sLineBreak + LocalSettingsFile);
            bForbidLocalSettings := True;
            CheckBoxLocalSettings.Enabled := False;
        end
        else
        begin
            bForbidLocalSettings := False;
            bIgnoreCBChange := True;
            CheckBoxLocalSettings.Checked := True;
        end;
    end;

    // Check for old MoveAPdesignators2 file if the provided file doesn't exist
    if not FileExists(AFileName) then
    begin
        AFileName := ClientAPI_SpecialFolder_AltiumApplicationData + '\MoveAPdesignators2Settings.ini';
        if not FileExists(AFileName) then
        begin
            // ini file doesn't exist, try to fall back on even older format file
            LoadPresetListFromFile(0);
            exit;
        end;
    end;

    IniFile := TIniFile.Create(AFileName);
    try
        QuickSilkForm.Top := IniFile.ReadInteger('Window Position', 'Top', QuickSilkForm.Top);
        QuickSilkForm.Left := IniFile.ReadInteger('Window Position', 'Left', QuickSilkForm.Left);

        tPreset1.Text := IniFile.ReadString('Presets', 'Preset1', tPreset1.Text);
        tPreset2.Text := IniFile.ReadString('Presets', 'Preset2', tPreset2.Text);
        tPreset3.Text := IniFile.ReadString('Presets', 'Preset3', tPreset3.Text);
        tPreset4.Text := IniFile.ReadString('Presets', 'Preset4', tPreset4.Text);
        tPreset5.Text := IniFile.ReadString('Presets', 'Preset5', tPreset5.Text);
        tPreset6.Text := IniFile.ReadString('Presets', 'Preset6', tPreset6.Text);
        tPreset7.Text := IniFile.ReadString('Presets', 'Preset7', tPreset7.Text);
        tPreset8.Text := IniFile.ReadString('Presets', 'Preset8', tPreset8.Text);

        CheckBoxPersistent.Checked := IniFile.ReadBool('Config', 'Persistent Placement Mode', CheckBoxPersistent.Checked);
        CheckBoxAnyAngle.Checked := IniFile.ReadBool('Config', 'Any-Angle Placement', CheckBoxAnyAngle.Checked);
        CheckBoxExtraOffsets.Checked := IniFile.ReadBool('Config', 'Try Extra Offsets', CheckBoxExtraOffsets.Checked);
        RadioGroupParentOnly.ItemIndex := IniFile.ReadInteger('Config', 'Clearance Checking Mode', RadioGroupParentOnly.ItemIndex);
        ComboBoxViaRule.ItemIndex := IniFile.ReadInteger('Config', 'Via Clearance Mode', ComboBoxViaRule.ItemIndex);

        tClearanceText.Text := IniFile.ReadString('Clearance', 'Text Clearance', tClearanceText.Text);
        tClearanceBody.Text := IniFile.ReadString('Clearance', 'Component Body Clearance', tClearanceBody.Text);
        tClearancePad.Text := IniFile.ReadString('Clearance', 'Pad Clearance', tClearancePad.Text);
        tClearanceCutout.Text := IniFile.ReadString('Clearance', 'Cutout Region Clearance', tClearanceCutout.Text);
        tClearanceVia.Text := IniFile.ReadString('Clearance', 'Via Clearance', tClearanceVia.Text);
        tClearanceDefault.Text := IniFile.ReadString('Clearance', 'Default Clearance', tClearanceDefault.Text);

        MMmilButton.Caption                 := IniFile.ReadString('Last Used', 'Units', MMmilButton.Caption);
        UnHideDesignatorsCheckBox.Checked   := IniFile.ReadBool('Last Used', 'Unhide Designators', UnHideDesignatorsCheckBox.Checked);
        LazyAutoMoveCheckBox.Checked        := IniFile.ReadBool('Last Used', 'Lazy Offset', LazyAutoMoveCheckBox.Checked);
        CheckBoxAutoParentOnly.Checked      := IniFile.ReadBool('Last Used', 'Parent-Only AutoMove', CheckBoxAutoParentOnly.Checked);
        EditMaxDistance.Text                := IniFile.ReadString('Last Used', 'Auto Distance', EditMaxDistance.Text);

        // Main input field needs to be set last because changing some other values trigger it
        EditDistance.Text                   := IniFile.ReadString('Last Used', 'Fixed Distance', EditDistance.Text);

        bPersistentMode := CheckBoxPersistent.Checked;
        bEnableAnyAngle := CheckBoxAnyAngle.Checked;
        bExtraOffsets := CheckBoxExtraOffsets.Checked;
        iClearanceMode := RadioGroupParentOnly.ItemIndex;
        //iViaClearanceMode := ComboBoxViaRule.ItemIndex; // done in UpdateViaRule instead

        UpdateViaRule; // update display of via clearance UI depending on selection

        UpdateConstants; // update all clearance constants based on units

        if iDebugLevel > 0 then
        begin
            SettingsDebugList := CreateObject(TStringList);

            if FileExists(AFileName) then
            begin
                SettingsDebugList.LoadFromFile(AFileName);
                ConfigDebugCaption := 'Confirm Settings read from file';
                SettingsDebugList.Insert(0, AFileName);
                DebugMessage(1, SettingsDebugList.Text, ConfigDebugCaption);
            end
            else DebugMessage(1, 'No settings file located. Defaults used.');

            SettingsDebugFile := ChangeFileExt(ConfigFile_GetPath,'.ini') + '_debug.ini';
            ConfigFile_Write(SettingsDebugFile);
            SettingsDebugList.LoadFromFile(SettingsDebugFile);
            DeleteFile(SettingsDebugFile);

            ConfigDebugCaption := 'Confirm Settings used in operation';
            DebugMessage(1, SettingsDebugList.Text, ConfigDebugCaption);

        end;

    finally
        IniFile.Free;
    end;
end;

procedure   ConfigFile_Write(AFileName : String);
var
    IniFile: TIniFile;
begin
    IniFile := TIniFile.Create(AFileName);
    try
        IniFile.WriteInteger('Window Position', 'Top', QuickSilkForm.Top);
        IniFile.WriteInteger('Window Position', 'Left', QuickSilkForm.Left);

        IniFile.WriteString('Presets', 'Preset1', tPreset1.Text);
        IniFile.WriteString('Presets', 'Preset2', tPreset2.Text);
        IniFile.WriteString('Presets', 'Preset3', tPreset3.Text);
        IniFile.WriteString('Presets', 'Preset4', tPreset4.Text);
        IniFile.WriteString('Presets', 'Preset5', tPreset5.Text);
        IniFile.WriteString('Presets', 'Preset6', tPreset6.Text);
        IniFile.WriteString('Presets', 'Preset7', tPreset7.Text);
        IniFile.WriteString('Presets', 'Preset8', tPreset8.Text);

        IniFile.WriteBool('Config', 'Persistent Placement Mode', CheckBoxPersistent.Checked);
        IniFile.WriteBool('Config', 'Any-Angle Placement', CheckBoxAnyAngle.Checked);
        IniFile.WriteBool('Config', 'Try Extra Offsets', CheckBoxExtraOffsets.Checked);
        IniFile.WriteInteger('Config', 'Clearance Checking Mode', RadioGroupParentOnly.ItemIndex);
        IniFile.WriteInteger('Config', 'Via Clearance Mode', ComboBoxViaRule.ItemIndex);

        IniFile.WriteString('Clearance', 'Text Clearance', tClearanceText.Text);
        IniFile.WriteString('Clearance', 'Component Body Clearance', tClearanceBody.Text);
        IniFile.WriteString('Clearance', 'Pad Clearance', tClearancePad.Text);
        IniFile.WriteString('Clearance', 'Cutout Region Clearance', tClearanceCutout.Text);
        IniFile.WriteString('Clearance', 'Via Clearance', tClearanceVia.Text);
        IniFile.WriteString('Clearance', 'Default Clearance', tClearanceDefault.Text);

        IniFile.WriteString('Last Used', 'Units', MMmilButton.Caption);
        IniFile.WriteBool('Last Used', 'Unhide Designators', UnHideDesignatorsCheckBox.Checked);
        IniFile.WriteBool('Last Used', 'Lazy Offset', LazyAutoMoveCheckBox.Checked);
        IniFile.WriteBool('Last Used', 'Parent-Only AutoMove', CheckBoxAutoParentOnly.Checked);
        //IniFile.WriteString('Last Used', 'Auto Distance', EditMaxDistance.Text);
        IniFile.WriteString('Last Used', 'Fixed Distance', EditDistance.Text);

    finally
        IniFile.Free;
    end;
end;

function    CountValidInSelected : Integer;
var
    i               : Integer;
    Obj             : IPCB_ObjectClass;
    DesignatorList  : TStringList;
begin
    Result := 0;
    if Board.SelectecObjectCount <= 0 then exit;

    // use a TStringList to automatically avoid duplicates
    DesignatorList := CreateObject(TStringList); // this will be a list of object address strings as it's just for counting uniques
    DesignatorList.Duplicates := dupIgnore;
    DesignatorList.Sorted := True; // needed for duplicate detection

    // Count components and Designators without deselecting them
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Obj := Board.SelectecObject[i];
        if (Obj.ObjectId = eComponentObject) and not Obj.Name.IsHidden then
        begin
            if not (InSet(Obj.NameAutoPosition, MkSet(eAutoPos_Manual, eAutoPos_CenterCenter))) then
                DesignatorList.Add(IntToStr(Obj.Name.I_ObjectAddress));
        end;
        if ((Obj.ObjectId = eTextObject) and Obj.IsDesignator and not Obj.IsHidden) then
        begin
            if not (InSet(Obj.Component.NameAutoPosition, MkSet(eAutoPos_Manual, eAutoPos_CenterCenter))) then
                DesignatorList.Add(IntToStr(Obj.I_ObjectAddress));
        end;
    end;

    Result := DesignatorList.Count;
end;

function    DeselectInvalid : Integer;
var
    i               : Integer;
    InvalidCount    : Integer;
    ComponentCount  : Integer;
    Obj             : Integer;
begin
    Result := 0;

    // deselect everything but components
    for i := Board.SelectecObjectCount - 1 downto 0 do
    begin
        Obj := Board.SelectecObject[i];
        if Obj.ObjectId <> eComponentObject then Obj.Selected := False;
    end;
    ComponentCount := Board.SelectecObjectCount;

    // now deselect components that can't be manipulated because their designators are manual or center autopositions
    for i := Board.SelectecObjectCount - 1 downto 0 do
    begin
        Obj := Board.SelectecObject[i];
        if InSet(Obj.NameAutoPosition, MkSet(eAutoPos_Manual, eAutoPos_CenterCenter)) then Board.SelectecObject[i].Selected := False;
    end;
    InvalidCount := ComponentCount - Board.SelectecObjectCount;

    Result := InvalidCount;
end;

function    DocumentIsPCB : Boolean;
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

function    ESTOP_Assert(dummy : Boolean = False);
begin
    ESTOP := True;
end;

function    ESTOP_ReadAndClear(dummy : Boolean = False) : Boolean;
begin
    Result := ESTOP;
    ESTOP := False;
end;

function    GetComponentAtCursor(const sPrompt : TString) : IPCB_Primitive;
var
    x, y            : TCoord;
    Comp, PrevComp  : IPCB_Component;
    Area, PrevArea  : Int64;
    Iter            : IPCB_BoardIterator;
    BRect           : TCoordRect;
    VisibleLayerSet : TV7_LayerSet;
begin
    CompKeySet := MkSet();
    Result := eNoObject;

    VisibleLayerSet := MkSet();
    if Board.LayerIsDisplayed(eTopLayer) then VisibleLayerSet := SetUnion(VisibleLayerSet, MkSet(eTopLayer));
    if Board.LayerIsDisplayed(eBottomLayer) then VisibleLayerSet := SetUnion(VisibleLayerSet, MkSet(eBottomLayer));

    //Application.ProcessMessages; // doesn't appear to be necessary
    Screen.Cursor := crHandPoint;
    try
        if Board.ChooseLocation(x, y, sPrompt) then  // false = ESC Key is pressed or right-clicked to cancel
        begin
            //   read modifier keys just as/after the "pick" mouse click
            if ControlKeyDown then CompKeySet := MkSet(cCtrlKey);
            //if ShiftKeyDown   then CompKeySet := SetUnion(CompKeySet, MkSet(cShiftKey));

            Result := Board.GetObjectAtXYAskUserIfAmbiguous(x, y, MkSet(eComponentObject), VisibleLayerSet, eEditAction_Focus);         // eEditAction_DontCare
            if (Result = Nil) then Result := eNoObject;

            // look again, component might be locked (has to iterate all components on board)
            if Result = eNoObject then
            begin
                Iter := Board.BoardIterator_Create;
                Iter.AddFilter_ObjectSet(MkSet(eComponentObject));
                Iter.AddFilter_LayerSet(VisibleLayerSet);
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
                    Comp := Iter.NextPCBObject;
                end;
                Board.BoardIterator_Destroy(Iter);
            end;
        end
        else Result := cESC;
    finally
        Screen.Cursor := crDefault;
    end;
end;

// Returns correct layer set given an ObjectId
function    GetLayerSet(SlkLayer: Integer; ObjID: Integer) : TV7_LayerSet;
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

function    GetMinDesignatorClearance(var Comp : IPCB_Component) : TCoord;
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

    DebugMessage(1, 'MinDistance: ' + CoordToDisplayStr(MinDistance));

    // Return the minimum distance found
    Result := MinDistance;
end;

// Get GeometricPolygon using PCBServer.PCBContourMaker
function    GetObjPoly(Obj: IPCB_ObjectClass; Expansion: TCoord = 0; ALayer : TV7_Layer = eNoLayer) : IPCB_GeometricPolygon;
var
    Poly : IPCB_GeometricPolygon;
    OldRect : TCoordRect;
    NewContour : IPCB_Contour;
    ViaExpansion : TCoord;
    ViaRadius : TCoord;
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
    else if Obj.ObjectId = eFillObject then
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
    else if Obj.ObjectId = eViaObject then
    begin
        // Function  MakeContour(APrim : IPCB_Primitive; AExpansion : Integer; ALayer : TV6_Layer) : IPCB_GeometricPolygon;
        if ALayer <> eNoLayer then
        begin
            // 0 = Waived; 1 = Pad (tented OK); 2 = From Pad; 3 = Hole (tented OK); 4 = From Hole
            if (iViaClearanceMode = 3) or (iViaClearanceMode = 4) then Expansion := Expansion - ((Obj.SizeOnLayer(ALayer) - Obj.HoleSize) / 2);
            ViaRadius := Obj.SizeOnLayer(ALayer) / 2; // could possibly leave a tiny dot if doesn't round to zero
            ViaExpansion := MAX(Expansion, -ViaRadius);
        end
        else
        begin
            ALayer := Obj.Layer;
            if (iViaClearanceMode = 3) or (iViaClearanceMode = 4) then Expansion := Expansion - ((Obj.Size - Obj.HoleSize) / 2);
            ViaRadius := Obj.Size / 2; // could possibly leave a tiny dot if doesn't round to zero
            ViaExpansion := MAX(Expansion, -ViaRadius);
        end;

        Poly := PCBServer.PCBContourMaker.MakeContour(Obj, ViaExpansion, ALayer);
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

function    GetRelativeAutoposition(var Comp : IPCB_Component; const loc_x, loc_y : TCoord) : TTextAutoposition;
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
    if bEnableAnyAngle then
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

function    GUI_ForLoopStart(StatusMsg : String; LoopCount : Integer) : Integer;
begin
    Result := LoopCount;
    MyPercent_Init(StatusMsg, LoopCount);
    BeginHourGlass;
end;

procedure   GUI_LoopEnd(dummy : Boolean = False);
begin
    EndHourGlass;
    MyPercent_Finish;
end;

procedure   GUI_LoopProgress(StatusMsg : String = '');
begin
    MyPercent_Update(StatusMsg);
end;

procedure   InteractivelyAutoposition;
var
    x, y                : TCoord;
    ModList             : TStringList;
    Comp                : IPCB_Component;
    NameOrComment       : IPCB_Text;
    bDesignator         : Boolean;
    bLocationFlag       : Boolean;
    sPrompt             : String;
    tc_Autopos          : TTextAutoposition;
    ParentOnly          : Boolean;
    MoveDist            : TCoord;
    OldSnapX, OldSnapY  : TCoord;
begin
    iDebugLevel := cDEBUGLEVEL;
    // set AD build flag
    if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;

    // Get the document
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    ConfigFile_Read(ConfigFile_GetPath);

    // Make it work for Pads, Vias, Strings, Polygons, Dimensions and coordinates
    //ASetOfObjects  := MkSet(ePadObject, eViaObject, eTextObject, ePolyObject, eRegionObject, eDimensionObject, eCoordinateObject);

    if cDEBUGLEVEL > 0 then
    begin
        ModList := CreateObject(TStringList);
        ModList.Delimiter := '|';
    end;

    Comp := Nil;
    LocKeySet := MkSet();
    CompKeySet := MkSet();

    // save original snap grid sizes
    OldSnapX := Board.SnapGridSizeX;
    OldSnapY := Board.SnapGridSizeY;

    // reduce grid size
    if (Board.DisplayUnit xor 1) = eImperial then Board.SnapGridSize := 50000 else Board.SnapGridSize := 39370; // don't believe the SDK, SnapGridSize takes TCoord, not double

    // TODO: figure out a way to detect whether hotspot snapping is on. In the meantime, small grid is probably sufficient
    // Client.SendMessage('PCB:DocumentPreferences', 'ObjectGuideSnapEnabled=False|ElectricalGridEnabled=False' , 255, Client.CurrentView);

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
                if InSet(cAltKey, LocKeySet) then ModList.Add('ALT');
                if InSet(cShiftKey, LocKeySet) then ModList.Add('SHIFT');
                if InSet(cCtrlKey, LocKeySet) then ModList.Add('CTRL');
                DebugMessage(1, Format('Picked %s @ [%s, %s]; location [%s, %s]%sModifiers: %s', [ Comp.Name.Text, CoordToDisplayX(Comp.x), CoordToDisplayY(Comp.y), CoordToDisplayX(x), CoordToDisplayY(y), sLineBreak, ModList.DelimitedText ]));
            end;

            if bDesignator then NameOrComment := Comp.Name else NameOrComment := Comp.Comment;

            // get autoposition relative to component origin
            tc_Autopos := GetRelativeAutoposition(Comp, x, y);
            DebugMessage(2, 'Autoposition Chosen: ' + StrFromAutoPos(tc_Autopos) + sLineBreak + 'bEnableAnyAngle=' + BoolToStr(bEnableAnyAngle, True));

            // Set autoposition based on modifiers
            SetAutopositionLocation(Comp, tc_Autopos, bDesignator);

            if iClearanceMode = 1 then ParentOnly := True
            else if iClearanceMode = 2 then ParentOnly := False
            else if InSet(cCtrlKey, LocKeySet) then ParentOnly := False else ParentOnly := True; // hold CTRL to also avoid things that are outside parent component
            DebugMessage(3, 'Begin: IsValidPlacement=' + BoolToStr(IsValidPlacement(NameOrComment, ParentOnly), True));

            MoveDist := AutoMove(NameOrComment, ParentOnly, 200000, tc_Autopos);

            NormalizeText(NameOrComment);

            NameOrComment.GraphicallyInvalidate;

            // TODO: to implement simultaneous placement of designator and comment in the same octant, I'm out of hotkeys
            // possibility 1: add separate command, but then user is forced to start and stop script for different situations. Not great.
            // possibility 2: if the other one (comment or designator) is visible, initially hide it and place the current text. After success, unhide and see if they overlap
            // and have the same justification, implying that they were both placed in the same octant. In that case, unhide and autoposition both in that octant before trying to
            // Automove them both again since they will be offset to not interfere. eAutoPos_CenterLeft, eAutoPos_TopCenter, eAutoPos_BottomCenter, eAutoPos_CenterRight
            // (depending on aspect ratio) need special handling to nudge them together to be TEXTEXPANSION apart. All other autopositions need
            // to move the one that's closer to the component first, then second can be moved with reduced search area.
            // QUESTION: should order that user positions them affect which is closer to the parent component, or do we just follow autoposition behavior of
            // always placing  Designator above Comment?
            // QUESTION: what about cases where they overlap but aren't placed in the same octant? Just nudge as usual? Native autoposition only cares if they are in the same autoposition.

            if MoveDist > 0 then
            begin
                Comp.BeginModify;
                if bDesignator then Comp.ChangeNameAutoPosition(eAutoPos_Manual) else Comp.ChangeCommentAutoPosition(eAutoPos_Manual);
                Comp.EndModify;
                Comp.GraphicallyInvalidate;
                DebugMessage(1, 'AutoMove moved by ' + CoordToDisplayStr(MoveDist));
            end
            else
            begin
                ShowInfo('No valid placement found' + sLineBreak + 'Ignored other components = ' + BoolToStr(ParentOnly, True));
            end;

            DebugMessage(3, 'End: IsValidPlacement=' + BoolToStr(IsValidPlacement(NameOrComment, ParentOnly), True));

            if not bPersistentMode then Comp := Nil;  // don't clear Source if in persistent mode (i.e. wait for user to escape)

            bLocationFlag := False; // clear location flag to allow picking a new location

            PCBServer.PostProcess;
            Application.ProcessMessages; // appears to be the only necessary flush for cursor lag? Maybe just calling this once per loop is enough to keep things performant?
            Board.ViewManager_FullUpdate;
        end;

        // Get PCB location
        if Assigned(Comp) then
        begin
            if InSet(cCtrlKey, CompKeySet) then bDesignator := False else bDesignator := True; // hold CTRL to autoposition Comment instead

            if bPersistentMode then sPrompt := 'PERSISTENT MODE ON ' else sPrompt := '';
            if bDesignator then sPrompt := sPrompt + 'Place Designator around ' else sPrompt := sPrompt + 'Place Comment around ';

            if iClearanceMode = 1 then sPrompt := sPrompt + Comp.Name.Text + ' (Parent objects avoided (locked by config); ALT to rotate; ALT+RightClick to hide)'
            else if iClearanceMode = 2 then sPrompt := sPrompt + Comp.Name.Text + ' (All objects avoided (locked by config); ALT to rotate; ALT+RightClick to hide)'
            else sPrompt := sPrompt + Comp.Name.Text + ' (ALT to rotate; CTRL to avoid all objects; ALT + Right-Click to hide)';

            // PERFORMANCE: any mouse movement between selecting component and this point will not move the cursor. Application.ProcessMessages forces an update
            //Application.ProcessMessages; // doesn't appear to be necessary on my machine, but might be performance-based on the machine running script
            Screen.Cursor := crCross;
            try
                //Board.ViewManager_FullUpdate;
                if Board.ChooseLocation(x, y, sPrompt) then  // false = ESC Key is pressed or right-clicked to cancel
                begin
                    LocKeySet := MkSet(); // clear modifier keyset
                    // read modifier keys just as/after the "pick" mouse click
                    if AltKeyDown     then LocKeySet := MkSet(cAltKey);  // Plan: hold ALT to rotate orthogonally
                    //if ShiftKeyDown then LocKeySet := SetUnion(LocKeySet, MkSet(cShiftKey));
                    if ControlKeyDown then LocKeySet := SetUnion(LocKeySet, MkSet(cCtrlKey)); // Plan: hold CTRL to stop ignoring outside objects

                    bLocationFlag := True;
                end
                else
                begin
                    // if user held ALT when they canceled location pick with RMB, center and hide text
                    if AltKeyDown then
                    begin
                        SetAutopositionLocation(Comp, eAutoPos_CenterCenter, bDesignator);
                        Comp.BeginModify;
                        if bDesignator then Comp.NameOn := False else Comp.CommentOn := False;
                        Comp.EndModify;
                        Comp.GraphicallyInvalidate;
                    end;
                    Comp := Nil;    // user canceled picking location, time to get a new source component
                end;
            finally
                Screen.Cursor := crDefault;
            end;
        end;

        if not Assigned(Comp) then
        begin
            bLocationFlag := False;
            if bEnableAnyAngle then sPrompt := '(ANY-ANGLE MODE ON) ' else sPrompt := '(ANY-ANGLE MODE OFF) ';
            repeat
                Comp := GetComponentAtCursor(sPrompt + 'Choose Source Component (Hold CTRL to position Comment instead of Designator)');
                if ControlKeyDown then CompKeySet := MkSet(cCtrlKey); // CTRL key held down during component pick
            until Assigned(Comp) or (Comp = cEsc);
        end;
    until (Comp = cESC);

    // restore original snap grid sizes in case they were modified (not going to use try..finally for this since it's not that important)
    Board.SnapGridSizeX := OldSnapX;
    Board.SnapGridSizeY := OldSnapY;

end;

function    IsForbiddenVia(Silk : IPCB_Text; ViaObj : IPCB_ObjectClass) : Boolean;
begin
    Result := False;
    if ViaObj = nil then exit;
    if ViaObj.ObjectId <> eViaObject then exit;

    // ignore vias that aren't present under silkscreen at all
    if (Silk.Layer = eTopOverlay) and not IsViaOnTopSide(ViaObj) then exit;
    if (Silk.Layer = eBottomOverlay) and not IsViaOnBottomSide(ViaObj) then exit;

    // 0 = Waived; 1 = Pad (tented OK); 2 = From Pad; 3 = Hole (tented OK); 4 = From Hole
    if iViaClearanceMode = 0 then exit
    else if (iViaClearanceMode = 1) or (iViaClearanceMode = 3) then if IsViaTentedForSilk(Silk, ViaObj) then exit;

    Result := True;
end;

// Checks if text object overlaps other object
function    IsOverlapping(Text: IPCB_ObjectClass; Obj2: IPCB_ObjectClass; ObjID : Integer = eNoObject) : Boolean;
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

    // Continue if allowed via
    if (Obj2.ObjectId = eViaObject) and not IsForbiddenVia(Text, Obj2) then
    begin
        result := False;
        Exit;
    end;

    Expansion := DEFAULTEXPANSION;
    case Obj2.ObjectId of
        eComponentObject:   Expansion := BODYEXPANSION;
        eTextObject:        Expansion := TEXTEXPANSION;
        ePadObject:         Expansion := PADEXPANSION;
        eViaObject:         Expansion := VIAEXPANSION;
        eRegionObject:      if Obj2.Kind = eRegionKind_Cutout then Expansion := CUTOUTEXPANSION;
    end;

    // special handling to handle cutouts in strict regions mode because I decided to make those their own rules
    // to allow different clearances and actions have consequences
    if (ObjID = eConnectionObject) and (Obj2.ObjectId = eRegionObject) and (Obj2.Kind <> eRegionKind_Cutout) then exit
    else if (ObjID = eRegionObject) and (Obj2.ObjectId = eRegionObject) and (Obj2.Kind = eRegionKind_Cutout) then exit;

    // Get geometric polygons for both objects
    TextPoly := GetObjPoly(Text);
    if Obj2.ObjectId = eViaObject then
    begin
        if Text.Layer = eTopOverlay then ObjPoly := GetObjPoly(Obj2, Expansion, eTopLayer)
        else if Text.Layer = eBottomOverlay then ObjPoly := GetObjPoly(Obj2, Expansion, eBottomLayer)
        else ObjPoly := GetObjPoly(Obj2, Expansion); // should not get here
    end
    else ObjPoly := GetObjPoly(Obj2, Expansion);

    DebugMessage(3, 'TextPoly Contours' + sLineBreak + DebugGeometricPolygonInfo(TextPoly).Text);
    DebugMessage(3, Obj2.Identifier + sLineBreak + 'ObjPoly Contours' + sLineBreak + DebugGeometricPolygonInfo(ObjPoly).Text);

    // IPCB_ContourUtilities Function  GeometricPolygonsTouch(AGeometricPolygon : IPCB_GeometricPolygon; BGeometricPolygon : IPCB_GeometricPolygon) : Boolean;
    result := PCBServer.PCBContourUtilities.GeometricPolygonsTouch(TextPoly, ObjPoly);
end;

procedure   IsRuleViolation(ObjectIDSet : TObjectSet; RuleNameStr : String; Clearance : TCoord);
var
    Text            : IPCB_Component;
    TextIter        : IPCB_BoardIterator;
    Designator      : IPCB_Text;
    ProcessedCount  : Integer;
    TotalCount      : Integer;
    ClearanceString : String;
    ProgressInt     : Integer;

begin
    ProcessedCount := 0;
    ProgressInt := 0;

    Client.SendMessage('PCB:DeSelect', 'Scope=All' , 255, Client.CurrentView);

    TextIter := Board.BoardIterator_Create;
    TextIter.AddFilter_ObjectSet(MkSet(eTextObject));
    TextIter.AddFilter_LayerSet(MkSet(eTopOverlay, eBottomOverlay));
    TextIter.AddFilter_Method(eProcessAll);

    TotalCount := GetIteratorCount(TextIter);

    Text := TextIter.FirstPCBObject;

    BeginHourGlass;

    GetWorkspace.DM_MessagesManager.ClearMessages();
    GetWorkspace.DM_ShowMessageView();

    AddMessage('QuickSilk Clearance Check', 'Started clearance check against ' + RuleNameStr);
    AddMessage('QuickSilk Status', Format('%d of %d text objects processed (%0.1f%%)', [ProcessedCount, TotalCount, (ProcessedCount / TotalCount) * 100]));

    GUI_ForLoopStart('Processing ' + IntToStr(TotalCount) + ' text objects', TotalCount);
    ESTOP_ReadAndClear; // clear E-STOP before starting
    while (Text <> Nil) and not ESTOP_ReadAndClear do
    begin
        GUI_LoopProgress(Format('Processed %d of %d text objects', [ProcessedCount, TotalCount]));
        Inc(ProcessedCount);
        // skip text that isn't a designator or comment
        if not (Text.IsDesignator or Text.IsComment) then
        begin
            Text := TextIter.NextPCBObject;
            continue;
        end;

        if SilkViolatesRule(Text, ObjectIDSet) then
        begin
            Text.Selected := True;
            Text.GraphicallyInvalidate;
        end;

        Text := TextIter.NextPCBObject;

        // throttle updates for performance
        if ((ProcessedCount / TotalCount) * 100) >= (ProgressInt + 1) then
        begin
            Inc(ProgressInt);
            AddMessage('QuickSilk Status', Format('%d of %d text objects processed (%d%%)', [ProcessedCount, TotalCount, ProgressInt]));
        end;

    end;
    Board.BoardIterator_Destroy(TextIter);
    GUI_LoopEnd;

    Client.SendMessage('PCB:Zoom', 'Action=Selected' , 255, Client.CurrentView);  // zoom on the selected text

    if Clearance > 0 then ClearanceString := ' within ' + CoordToDisplayStr(Clearance) + ' of ' else ClearanceString := ' within configured clearance of ';
    ShowInfo('Selected ' + IntToStr(Board.SelectecObjectCount) +  ' text objects' + ClearanceString + RuleNameStr);

    EndHourGlass;
end;

function    IsSelectableCheck(bWarnComp : Boolean = False; bWarnText : Boolean = False) : Boolean;
var
    checkComp   : Boolean;
    checkText   : Boolean;
    Iter        : IPCB_BoardIterator;
    Obj         : IPCB_ObjectClass;
    tempBool    : Boolean;
begin
    Result := False;
    checkComp := True;
    checkText := True;

    if not Assigned(IsCompSelectable) then IsCompSelectable := False;
    if not Assigned(IsTextSelectable) then IsTextSelectable := False;

    Iter := Board.BoardIterator_Create;
    Iter.AddFilter_ObjectSet(MkSet(eComponentObject, eTextObject));
    Iter.AddFilter_LayerSet(AllLayers);
    Iter.AddFilter_Method(eProcessAll);

    Obj := Iter.FirstPCBObject;
    while (Obj <> nil) do
    begin
        if (Obj.ObjectId = eComponentObject) and (checkComp) then
        begin
            tempBool := Obj.Selected;
            Obj.Selected := True;
            if Obj.Selected = True then IsCompSelectable := True;
            Obj.Selected := tempBool;
            checkComp := False;
        end
        else if (Obj.ObjectId = eTextObject) and (checkText) then
        begin
            tempBool := Obj.Selected;
            Obj.Selected := True;
            if Obj.Selected = True then IsTextSelectable := True;
            Obj.Selected := tempBool;
            checkText := False;
        end;

        if not (checkComp or checkText) then break;

        Obj := Iter.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(Iter);

    // returns results for enabled warnings i.e. warning-enabled checks passed
    if bWarnComp and bWarnText then Result := (IsCompSelectable and IsTextSelectable)
    else Result := (bWarnComp and IsCompSelectable) or (bWarnText and IsTextSelectable);

    if (bWarnComp and bWarnText) and not (IsCompSelectable or IsTextSelectable) then ShowWarning('Components and Texts are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.')
    else if bWarnComp and not IsCompSelectable then ShowWarning('Components are not selectable.')
    else if bWarnText and not IsTextSelectable then ShowWarning('Texts are not selectable. Clearance check needs "Texts" enabled in selection filter.');
end;

// search area for objects of given ObjectId and check to see if Text is too close
function    IsTextOverObj(Text : IPCB_Text; ObjID : Integer; Filter_Size : Integer; ParentOnly : Boolean = False; StrictRegions : Boolean = False) : Boolean;
var
    Iterator : IPCB_SpatialIterator;
    Obj : IPCB_ObjectClass;
    Rect : TCoordRect;
    RectL, RectR, RectB, RectT : TCoord;
    RegIter : Boolean; // Regular Iterator
    Name : TPCBString;
    DebugString : WideString;
    StrictObjID : Integer;
begin
    // hack to flag cutout regions differently without adding yet another argument (use eConnectionObject because it'll never be used normally)
    StrictObjID := eNoObject;
    if StrictRegions then
    begin
        case ObjID of
            eConnectionObject   : StrictObjID := ObjID;
            eRegionObject       : StrictObjID := ObjID;
        end;
    end;

    if ObjID = eConnectionObject then ObjID := eRegionObject; // always convert

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

        if iDebugLevel >= 4 then DebugString := Format('Left: %s, Right: %s, Top: %s, Bot: %s', [CoordToDisplayX(RectL), CoordToDisplayX(RectR), CoordToDisplayY(RectT), CoordToDisplayY(RectB)]) else DebugString := '';
        DebugMessage(4, StrFromObjectId(ObjID) + ' Spatial Iterator Area' + sLineBreak + DebugString);

        Iterator := Board.SpatialIterator_Create;
        Iterator.AddFilter_ObjectSet(MkSet(ObjID));
        if ObjID = eComponentBodyObject then Iterator.AddFilter_LayerSet(GetComponentBodyLayerSet(Text.Component))
        else Iterator.AddFilter_LayerSet(GetLayerSet(Text.Layer, ObjID));

        Iterator.AddFilter_Area(RectL, RectB, RectR, RectT);
        RegIter := False;
    end;

    // reset to StrictObjID now that we've used iterator
    ObjID := StrictObjID;

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
            // check clearances (if ObjID is anything but `eNoObject`, strict regions is implied
            if IsOverlapping(Text, Obj, ObjID) then
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
function    IsValidPlacement(Silkscreen : IPCB_Text; ParentOnly : Boolean = False) : Boolean;
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
    // same-side via overlap detection
    else if IsTextOverObj(Silkscreen, eViaObject, FILTERSIZE, ParentOnly) then
    begin
        DebugMessage(3, 'eViaObject check failed');
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
procedure   LoadPresetListFromFile(const dummy : Integer);
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
        //SelectedCheckBox.Checked          := PresetList[10];
        UnHideDesignatorsCheckBox.Checked := PresetList[11];
        EditDistance.Text                 := PresetList[0]; // Main input field needs to be set last because setting each preset updates it
    end
    else
    begin
        // if preset file didn't exist at all, just exit (older file format deprecated)
        exit;
    end;
end; { LoadPresetListFromFile }

{ Main procedure }
procedure   ProcessMain(dummy : Boolean = False);
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
    InvalidCount            : Integer;
    ProcessedCount          : Integer;
    SkippedCount            : Integer;
    SelectedCount           : Integer;
begin
    ProcessedCount := 0;
    SkippedCount := 0;
    if not Assigned(iDebugLevel) then iDebugLevel := cDEBUGLEVEL;

    // set AD build flag
    if not Assigned(IsAtLeastAD19) then if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;

    // Disables Online DRC during designator movement to improve speed
    PCBSystemOptions := PCBServer.SystemOptions;

    if PCBSystemOptions = Nil then Exit;

    DRCSetting                   := PCBSystemOptions.DoOnlineDRC;
    PCBSystemOptions.DoOnlineDRC := False;
    try
        ComponentIteratorHandle := Board.BoardIterator_Create;
        ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
        ComponentIteratorHandle.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
        ComponentIteratorHandle.AddFilter_Method(eProcessAll);

        if QuickSilkForm.UnHideDesignatorsCheckBox.Checked then bUnHideDesignators := True
        else bUnHideDesignators := False;

        if QuickSilkForm.LazyAutoMoveCheckBox.Checked then bLazyAutoMove := True
        else bLazyAutoMove := False;

        // Set the move distance to DB units converted from mils or mm
        if QuickSilkForm.MMmilButton.Caption = 'mm' then MoveDistance := MMsToCoord(QuickSilkForm.EditDistance.Text)
        else MoveDistance := MilsToCoord(QuickSilkForm.EditDistance.Text);

        if bAutoMode then
        begin
            if QuickSilkForm.MMmilButton.Caption = 'mm' then MaxDistance := MMsToCoord(QuickSilkForm.EditMaxDistance.Text)
            else MaxDistance := MilsToCoord(QuickSilkForm.EditMaxDistance.Text);
        end;

        // if only processing selected components
        if QuickSilkForm.RadioGroupSelectionScope.ItemIndex = 1 then
        begin
            if not IsSelectableCheck(True, True) then exit;
            SelectCompAndDesignators;
            SelectedCount := GetSelectedComponentCount;
            InvalidCount := DeselectInvalid; // deselect invalid objects and get number of components that can't be processed

            // warn the user if some selected components were ineligible
            if (SelectedCount = 0) or (SelectedCount = InvalidCount) then
            begin
                ShowError('No components with autopositioned designators selected');
                exit;
            end
            else if InvalidCount > 0 then
            begin
                ShowWarning(IntToStr(InvalidCount) + ' components automatically deselected because they have Manual or Center autoposition settings.');
            end;
        end;

        Component := ComponentIteratorHandle.FirstPCBObject;

        if QuickSilkForm.RadioGroupSelectionScope.ItemIndex = 1 then
            while (Component <> Nil) do
                if not Component.Selected then Component := ComponentIteratorHandle.NextPCBObject
                else break; // Find the first selected comp if "selected only" checked

        // Notify the pcbserver that we will make changes (Start undo)
        PCBServer.PreProcess;

        while (Component <> Nil) do
        begin
            // if processing all components
            if RadioGroupSelectionScope.ItemIndex = 0 then
            begin
                Inc(ProcessedCount);
                // if component is ineligible because autoposition is manual or center, skip it
                if InSet(Component.NameAutoPosition, MkSet(eAutoPos_Manual, eAutoPos_CenterCenter)) then
                begin
                    Inc(SkippedCount);
                    Component := ComponentIteratorHandle.NextPCBObject;
                    continue;
                end;
            end;

            Component.BeginModify;

            Component.Selected := False;

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
                AutoMoveResult := AutoMove(Designator, CheckBoxAutoParentOnly.Checked);

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
            if QuickSilkForm.RadioGroupSelectionScope.ItemIndex = 1 then
                while (Component <> Nil) do
                    if not Component.Selected then Component := ComponentIteratorHandle.NextPCBObject
                    else break; // Find the next selected comp if "selected only" checked

        end; // end while

        // Notify the pcbserver that all changes have been made (Stop undo)
        PCBServer.PostProcess;

        // if processed all component, report final stats
        if QuickSilkForm.RadioGroupSelectionScope.ItemIndex = 0 then
        begin
            if (ProcessedCount = SkippedCount) then ShowInfo('No components modified. All designators have manual or center autopositions.')
            else if SkippedCount = 0 then ShowInfo(IntToStr(ProcessedCount) + ' components modified.')
            else ShowInfo(IntToStr(ProcessedCount - SkippedCount) + ' components modified.' + sLineBreak +
                    'Skipped ' + IntToStr(SkippedCount) + ' components whose designator was not autopositioned to a side');
        end
        else ShowInfo(IntToStr(SelectedCount - InvalidCount) + ' components modified.');

        // Destroy the component handle
        Board.BoardIterator_Destroy(ComponentIteratorHandle);

        Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

    finally
        // Restore DRC setting
        PCBSystemOptions.DoOnlineDRC := DRCSetting;
    end;
end; { ProcessMain }

{ rotates IPCB_Text object to specific angle, optionally normalizing it to be right-reading, optionally rotating 90° CCW }
function    RotateTextToAngle(var Text : IPCB_Text; const Angle : Double; const Normalize : Boolean = False; const Ortho : Boolean = False) : Double;
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
function    SelectCompAndDesignators(dummy : Boolean = False) : Boolean;
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

procedure   SetAutopositionLocation(var Comp : IPCB_Component; const tc_Autopos : TTextAutoposition; bDesignator : Boolean = True);
var
    NameOrComment       : IPCB_Text;
    Ortho               : Boolean;
    TempRotation        : Double;
    AfterRotation       : Double;
begin
    TempRotation := 0;

    if bDesignator then NameOrComment := Comp.Name else NameOrComment := Comp.Comment;

    // if ALT key was used, set Ortho flag to rotate 90 deg CCW
    if InSet(cAltKey, LocKeySet) then Ortho := True else Ortho := False;

    // in any-angle mode, temporarily set to zero rotation
    if (Comp.Rotation <> 0) and bEnableAnyAngle then
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
        // after using autoposition, set to manual if any-angle mode is enabled
        if bEnableAnyAngle then Comp.ChangeNameAutoposition(eAutoPos_Manual);
    end
    else
    begin
        if not Comp.CommentOn then Comp.CommentOn := True;
        Comp.ChangeCommentAutoposition(tc_Autopos);
        // after using autoposition, set to manual if any-angle mode is enabled
        if bEnableAnyAngle then Comp.ChangeCommentAutoposition(eAutoPos_Manual);
    end;

    // restore original rotation
    if (TempRotation <> 0) then Comp.Rotation := TempRotation;

    Comp.EndModify;
    Comp.GraphicallyInvalidate;
end;

procedure   SetButtonEnableStates(EnableState : Boolean; bFilteredOnly : Boolean = False);
begin
    // controls that only care whether processing can be run
    ButtonAuto.Enabled              := EnableState;
    ButtonOK.Enabled                := EnableState;
    ButtonPreset1.Enabled           := EnableState;
    ButtonPreset2.Enabled           := EnableState;
    ButtonPreset3.Enabled           := EnableState;
    ButtonPreset4.Enabled           := EnableState;
    ButtonPreset5.Enabled           := EnableState;
    ButtonPreset6.Enabled           := EnableState;
    ButtonPreset7.Enabled           := EnableState;
    ButtonPreset8.Enabled           := EnableState;

    // controls that only care if numbers/settings are invalid
    if not bFilteredOnly then
    begin
        ButtonInteractiveStart.Enabled  := EnableState;
        ButtonSaveConfig.Enabled        := EnableState;
        MMmilButton.Enabled             := EnableState;
        if bForbidLocalSettings then CheckBoxLocalSettings.Enabled := False else CheckBoxLocalSettings.Enabled := EnableState;
        ButtonSaveConfig.Caption := '&SAVE';
    end;
end;

{ function to check if silkscreen violates a set of object types }
function    SilkViolatesRule(Silkscreen : IPCB_Text; ObjectIDSet : TObjectSet) : Boolean;
const
    FILTERSIZE = 200000;
var
    ValidObjectSet  : TObjectSet;
begin
    Result := True;

    ValidObjectSet := MkSet(eTextObject, eComponentBodyObject, ePadObject, eConnectionObject, eViaObject, eArcObject, eTrackObject, eFillObject, eRegionObject);

    if not SubSet(ObjectIDSet, ValidObjectSet) then
    begin
        ShowError(StrFromObjectIdSet(SetDifference(ObjectIDSet, ValidObjectSet)) + ' ObjectId(s) not supported.' + sLineBreak +
                'Valid objects: ' + StrFromObjectIdSet(ValidObjectSet) + sLineBreak +
                'Note that "eConnectionObject" actually means cutout region in this script.');
        Result := False;
        ESTOP_Assert;
        exit;
    end;

    // Overlap Detection
    if InSet(eTextObject, ObjectIDSet) and IsTextOverObj(Silkscreen, eTextObject, FILTERSIZE) then
    begin
        DebugMessage(3, 'FAIL' + sLineBreak + Silkscreen.Text + ' is within ' + CoordToDisplayStr(TEXTEXPANSION) + ' of other text.');
        Exit;
    end
    else if InSet(eComponentBodyObject, ObjectIDSet) and IsTextOverObj(Silkscreen, eComponentBodyObject, FILTERSIZE) then
    begin
        DebugMessage(3, 'FAIL' + sLineBreak + Silkscreen.Text + ' is within ' + CoordToDisplayStr(BODYEXPANSION) + ' of a component body.');
        Exit;
    end
    else if InSet(ePadObject, ObjectIDSet) and IsTextOverObj(Silkscreen, ePadObject, FILTERSIZE) then
    begin
        DebugMessage(3, 'FAIL' + sLineBreak + Silkscreen.Text + ' is within ' + CoordToDisplayStr(PADEXPANSION) + ' of a pad.');
        Exit;
    end
    // note that `eConnectionObject` actually means a cutout region (hack to distinguish from non-cutout regions downstream)
    else if InSet(eConnectionObject, ObjectIDSet) and IsTextOverObj(Silkscreen, eConnectionObject, FILTERSIZE, False, True) then
    begin
        DebugMessage(3, 'FAIL' + sLineBreak + Silkscreen.Text + ' is within ' + CoordToDisplayStr(CUTOUTEXPANSION) + ' of a cutout region.');
        Exit;
    end
    else if InSet(eViaObject, ObjectIDSet) and IsTextOverObj(Silkscreen, eViaObject, FILTERSIZE) then
    begin
        DebugMessage(3, 'FAIL' + sLineBreak + Silkscreen.Text + ' is within ' + CoordToDisplayStr(VIAEXPANSION) + ' of a via.');
        Exit;
    end
    else if InSet(eRegionObject, ObjectIDSet) and IsTextOverObj(Silkscreen, eRegionObject, FILTERSIZE, False, True) then
    begin
        DebugMessage(3, 'FAIL' + sLineBreak + Silkscreen.Text + ' is within ' + CoordToDisplayStr(DEFAULTEXPANSION) + ' of a regular region.');
        Exit;
    end
    else if InSet(eArcObject, ObjectIDSet) and IsTextOverObj(Silkscreen, eArcObject, FILTERSIZE) then
    begin
        DebugMessage(3, 'FAIL' + sLineBreak + Silkscreen.Text + ' is within ' + CoordToDisplayStr(DEFAULTEXPANSION) + ' of an arc.');
        Exit;
    end
    else if InSet(eTrackObject, ObjectIDSet) and IsTextOverObj(Silkscreen, eTrackObject, FILTERSIZE) then
    begin
        DebugMessage(3, 'FAIL' + sLineBreak + Silkscreen.Text + ' is within ' + CoordToDisplayStr(DEFAULTEXPANSION) + ' of a track.');
        Exit;
    end
    else if InSet(eFillObject, ObjectIDSet) and IsTextOverObj(Silkscreen, eFillObject, FILTERSIZE) then
    begin
        DebugMessage(3, 'FAIL' + sLineBreak + Silkscreen.Text + ' is within ' + CoordToDisplayStr(DEFAULTEXPANSION) + ' of a fill.');
        Exit;
    end
    // none of the previous checks failed
    else
    begin
        DebugMessage(3, 'PASS' + sLineBreak + Silkscreen.Text + ' is NOT within ' + CoordToDisplayStr(BODYEXPANSION) + ' of component body.');
        Result := False;
    end;

end;

// keep procedure name for users that might have this on their shortcuts
procedure   TweakDesignators;
begin
    _GUI;
end;

procedure   UpdateConstants(dummy : Boolean = False);
begin
    case MMmilButton.Caption of
        'mil':
        begin
            StringToCoordUnit(tClearanceText.Text, TEXTEXPANSION, eImperial);
            StringToCoordUnit(tClearanceBody.Text, BODYEXPANSION, eImperial);
            StringToCoordUnit(tClearancePad.Text, PADEXPANSION, eImperial);
            StringToCoordUnit(tClearanceCutout.Text, CUTOUTEXPANSION, eImperial);
            StringToCoordUnit(tClearanceVia.Text, VIAEXPANSION, eImperial);
            StringToCoordUnit(tClearanceDefault.Text, DEFAULTEXPANSION, eImperial);
        end;
        'mm':
        begin
            StringToCoordUnit(tClearanceText.Text, TEXTEXPANSION, eMetric);
            StringToCoordUnit(tClearanceBody.Text, BODYEXPANSION, eMetric);
            StringToCoordUnit(tClearancePad.Text, PADEXPANSION, eMetric);
            StringToCoordUnit(tClearanceCutout.Text, CUTOUTEXPANSION, eMetric);
            StringToCoordUnit(tClearanceVia.Text, VIAEXPANSION, eMetric);
            StringToCoordUnit(tClearanceDefault.Text, DEFAULTEXPANSION, eMetric);
        end;
        else
        begin
            // invalid
        end;
    end;
end;

procedure   UpdateViaRule(dummy : Boolean = False);
begin
    iViaClearanceMode := ComboBoxViaRule.ItemIndex;

    // 0 = Waived; 1 = Pad (tented OK); 2 = From Pad; 3 = Hole (tented OK); 4 = From Hole
    case iViaClearanceMode of
        0: begin
            tClearanceVia.Enabled := False;
            LabelClearanceVia.Font.Color := clWindowText;
            LabelClearanceVia.Enabled := False;
        end;
        else begin
            tClearanceVia.Enabled := True;
            LabelClearanceVia.Enabled := True;
            LabelClearanceVia.Font.Color := clBlue;
        end;
    end;
end;

procedure   TQuickSilkForm.ButtonAutoClick(Sender : TObject);
begin
    bAutoMode := True;
    ConfigFile_Write(ConfigFile_GetPath);
    ButtonSaveConfig.Caption := 'SAVED';
    ProcessMain;
end; { TQuickSilkForm.ButtonAutoClick }

procedure   TQuickSilkForm.ButtonCancelClick(Sender : TObject);
begin
    QuickSilkForm.Close;
end; { TQuickSilkForm.ButtonCancelClick }

procedure   TQuickSilkForm.ButtonInteractiveStartClick(Sender : TObject);
begin
    ConfigFile_Write(ConfigFile_GetPath);
    ButtonSaveConfig.Caption := 'SAVED';

    InteractivelyAutoposition;
end;

procedure   TQuickSilkForm.ButtonOKClick(Sender : TObject);
begin
    bAutoMode := False;
    ConfigFile_Write(ConfigFile_GetPath);
    ButtonSaveConfig.Caption := 'SAVED';
    ProcessMain;
end; { TQuickSilkForm.ButtonOKClick }

procedure   TQuickSilkForm.ButtonSaveConfigClick(Sender : TObject);
begin
    ConfigFile_Write(ConfigFile_GetPath);
    ButtonSaveConfig.Caption := 'SAVED';
end;

procedure   TQuickSilkForm.CheckBoxLocalSettingsClick(Sender: TObject);
var
    LocalSettingsFile : String;
begin
    // to avoid re-triggering
    if bIgnoreCBChange then
    begin
        bIgnoreCBChange := False;
        exit;
    end;

    LocalSettingsFile := ExtractFilePath(GetRunningScriptProjectName) + cConfigFileName;

    if Sender.Checked then
    begin
        if FolderIsReadOnly(ExtractFilePath(GetRunningScriptProjectName)) then
        begin
            ShowError('Unable to use local settings. No write access to script folder:' + sLineBreak + ExtractFilePath(GetRunningScriptProjectName));

            bForbidLocalSettings := True;
            bIgnoreCBChange := True;
            Sender.Checked := False;
            Sender.Enabled := False;
            exit;
        end;

        if ConfirmOKCancel('Current configuration settings will be saved to:' + sLineBreak + LocalSettingsFile + sLineBreak + '') then
        begin
            if LocalSettingsFile = ConfigFile_GetPath then DeleteFile(LocalSettingsFile);
            ConfigFile_Write(LocalSettingsFile);
        end
        else
        begin
            bIgnoreCBChange := True;
            Sender.Checked := False;
        end;
    end
    else
    begin
        if ConfirmOKCancel('Local configuration settings will be moved to:' + sLineBreak + SpecialFolder_AltiumApplicationData + '\' + cConfigFileName) then
        begin
            if LocalSettingsFile = ConfigFile_GetPath then DeleteFile(LocalSettingsFile);
            if FileExists(LocalSettingsFile) then
            begin
                ShowError('Unable to delete local settings file:' + sLineBreak + LocalSettingsFile + sLineBreak + 'Local settings will be loaded next time script is started as long as local settings still exist.');
                bForbidLocalSettings := True;
                Sender.Enabled := False;
            end;

            ConfigFile_Write(ConfigFile_GetPath);
        end
        else
        begin
            bIgnoreCBChange := True;
            Sender.Checked := True;
        end;
    end;
end;

procedure   TQuickSilkForm.ClearanceEnterKey(Sender : TObject; var Key : Char);
begin
    if (Ord(Key) = 13) then
    begin
        Key := #0; // eat the enter keypress to avoid beep
        QuickSilkForm.ActiveControl := ButtonSaveConfig; // jump to save button
    end;
end;

procedure   TQuickSilkForm.ConfigClick(Sender : TObject);
var
    cbItem : Integer;
begin
    ButtonSaveConfig.Caption := '&SAVE';
    if Sender = ComboBoxViaRule then UpdateViaRule;
end;

procedure   TQuickSilkForm.InputValueChange(Sender : TObject);
begin

    if IsStringANum(Sender.Text) then
    begin
        Sender.Font.Style := 0;
        Sender.Font.Color := clWindowText;
        SetButtonEnableStates(True);
        UpdateConstants;
    end
    else
    begin
        Sender.Font.Style := MkSet(fsBold, fsItalic, fsUnderline);
        Sender.Font.Color := clRed;
        SetButtonEnableStates(False);
    end;
end; { TQuickSilkForm.EditClearanceChange }

procedure   TQuickSilkForm.LabelClearanceMouseEnter(Sender: TObject);
begin
    if Sender.Enabled = False then exit;
    if Sender = LabelClearanceAll then Sender.Font.Style := MkSet(fsBold, fsUnderline) else Sender.Font.Style := MkSet(fsUnderline);
end;

procedure   TQuickSilkForm.LabelClearanceMouseLeave(Sender: TObject);
begin
    if Sender = LabelClearanceAll then Sender.Font.Style := MkSet(fsBold) else Sender.Font.Style := 0;
end;

procedure   TQuickSilkForm.LabelVersionClick(Sender : TObject);
begin
    About;
end;

procedure   TQuickSilkForm.MMmilButtonClick(Sender : TObject);
begin
    if MMmilButton.Caption = 'mil' then
    begin
        MMmilButton.Caption := 'mm';
        ChangeTextUnits(eMetric);
    end
    else
    begin
        MMmilButton.Caption := 'mil';
        ChangeTextUnits(eImperial);
    end;
    EditMaxDistance.Update;
    EditDistance.SetFocus;
    EditDistance.Update;

    ButtonSaveConfig.Caption := '&SAVE';
end; { TQuickSilkForm.MMmilButtonClick }

procedure   TQuickSilkForm.PresetButtonClicked(Sender : TObject);
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
    ConfigFile_Write(ConfigFile_GetPath);
    ButtonSaveConfig.Caption := 'SAVED';
    ProcessMain;
end; { PresetButtonClicked }

procedure   TQuickSilkForm.PresetValueChange(Sender : TObject);
var
    textbox : TEdit;
begin
    if IsStringANum(Sender.Text) then
    begin
        Sender.Font.Style := 0;
        Sender.Font.Color := clWindowText;
        EditDistance.Text := Sender.Text;
        SetButtonEnableStates(True);
    end
    else
    begin
        Sender.Font.Style := MkSet(fsBold, fsItalic, fsUnderline);
        Sender.Font.Color := clRed;
        SetButtonEnableStates(False);
    end;

end; { PresetValueChange }

procedure   TQuickSilkForm.QuickSilkFormShow(Sender : TObject);
begin
    // Checks if current document is a PCB kind if not, exit.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then
    begin
        ShowError('This script must be run from a PCB document.');
        exit;
    end;

    iDebugLevel := cDEBUGLEVEL;

    // set version label
    LabelVersion.Caption := 'About v' + cScriptVersion;

    // set AD build flag
    if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;

    // Set direction control hint
    Application.HintHidePause := 12000; // extend hint show time
    LazyAutoMoveCheckBox.Hint := 'ENABLED: if AutoMove moves more than the size of the part without detecting an obstacle, will treat as fixed offset.' + sLineBreak +
        'DISABLED: if AutoMove moves more than the size of the part without detecting an obstacle, it will abort move and leave in autoposition.';

    LabelClearanceVia.Hint := 'Via clearance. Click to select violating. Clearance modes:' + sLineBreak +
        'Waived: Via clearance is ignored' + sLineBreak +
        'Pad (tented OK): Via clearance is from via pad, ignoring tented vias (tented by checkbox, not negative expansion)' + sLineBreak +
        'From Pad: Via clearance is from via pad, no vias are ignored' + sLineBreak +
        'Hole (tented OK): Via clearance is from via hole, ignoring tented vias (tented by checkbox, not negative expansion)' + sLineBreak +
        'From Hole: Via clearance is from via hole, no vias are ignored';

    ConfigFile_Read(ConfigFile_GetPath);

    // put after initial config read so we can load local settings file if it exists, but then force non-local save location
    if FolderIsReadOnly(ExtractFilePath(GetRunningScriptProjectName)) then
    begin
        bForbidLocalSettings := True;
        CheckBoxLocalSettings.Enabled := False;
        CheckBoxLocalSettings.Caption := 'Local Config Read-only';
    end
    else bForbidLocalSettings := False;

    IsSelectableCheck(False, True);

    if SelectCompAndDesignators then RadioGroupSelectionScope.ItemIndex := 1 else RadioGroupSelectionScope.ItemIndex := 0;
end; { TQuickSilkForm.QuickSilkFormShow }

procedure   TQuickSilkForm.RuleCheckClick(Sender : TObject);
var
    RuleIndex : Integer;
begin
    if not IsSelectableCheck(False, True) then exit;

    if      Sender = LabelClearanceAll      then RuleIndex := 0
    else if Sender = LabelClearanceText     then RuleIndex := 1
    else if Sender = LabelClearanceBody     then RuleIndex := 2
    else if Sender = LabelClearancePad      then RuleIndex := 3
    else if Sender = LabelClearanceCutout   then RuleIndex := 4
    else if Sender = LabelClearanceVia      then RuleIndex := 5
    else if Sender = LabelClearanceDefault  then RuleIndex := 6;

    // uncomment to save config when running clearance checks, but it seems like manual save would be preferred so you can experiment
    //ConfigFile_Write(ConfigFile_GetPath);
    //ButtonSaveConfig.Caption := 'SAVED';

    case RuleIndex of
        0 : IsRuleViolation(MkSet(eTextObject, eComponentBodyObject, ePadObject, eConnectionObject, eArcObject, eTrackObject, eFillObject, eRegionObject), 'any object', 0);
        1 : IsRuleViolation(MkSet(eTextObject), Sender.Caption, TEXTEXPANSION);
        2 : IsRuleViolation(MkSet(eComponentBodyObject), Sender.Caption, BODYEXPANSION);
        3 : IsRuleViolation(MkSet(ePadObject), Sender.Caption, PADEXPANSION);
        4 : IsRuleViolation(MkSet(eConnectionObject), Sender.Caption, CUTOUTEXPANSION);
        5 : IsRuleViolation(MkSet(eViaObject), Sender.Caption, VIAEXPANSION);
        6 : IsRuleViolation(MkSet(eArcObject, eTrackObject, eFillObject, eRegionObject), Sender.Caption, DEFAULTEXPANSION);
    end;
end;

{ programmatically, OnKeyPress fires before OnChange event and "catches" the key press }
procedure   TQuickSilkForm.UserKeyPress(Sender : TObject; var Key : Char);
begin
    if (Sender = EditMaxDistance) and (ButtonAuto.Enabled) and (Ord(Key) = 13) then
    begin
        // irrelevant since Max distance control is hidden
        Key := #0; // catch and discard key press to avoid beep
        if ButtonAuto.Enabled then
        begin
            bAutoMode := True;
            ConfigFile_Write(ConfigFile_GetPath);
            ButtonSaveConfig.Caption := 'SAVED';
            ProcessMain;
        end;
    end
    else if (Sender <> EditMaxDistance) and (ButtonOK.Enabled) and (Ord(Key) = 13) then
    begin
        Key := #0; // catch and discard key press to avoid beep
        if ButtonOK.Enabled then
        begin
            bAutoMode := False;
            ConfigFile_Write(ConfigFile_GetPath);
            ButtonSaveConfig.Caption := 'SAVED';
            ProcessMain;
        end;
    end;
end; { UserKeyPress }

{ ************** UTILITY FUNCTIONS ************** }

procedure   AddMessage(MessageClass, MessageText: String);
begin
    // https://www.altium.com/ru/documentation/altium-nexus/wsm-api-types-and-constants/#Image%20Index%20Table
    // [!!!] 66 index for debug info
    GetWorkspace.DM_MessagesManager.BeginUpdate();
    GetWorkspace.DM_MessagesManager.AddMessage(MessageClass, MessageText, 'QuickSilk Script', GetWorkspace.DM_FocusedDocument.DM_FileName, '', '', 75, MessageClass = 'QuickSilk Status');
    GetWorkspace.DM_MessagesManager.EndUpdate();
    GetWorkspace.DM_MessagesManager.UpdateWindow();
end;

function    CalculateCentroid(const contour : IPCB_Contour; out CentroidX : TCoord; out CentroidY : TCoord) : Boolean;
var
    iPoint: Integer;
    xRunningSum, yRunningSum: Double;
begin
    Result := False;
    if contour.Count < 3 then exit;

    xRunningSum := 0.00000000001; // force double type
    yRunningSum := 0.00000000001; // force double type

    for iPoint := 0 to contour.Count - 1 do
    begin
        xRunningSum := xRunningSum + (contour.x(iPoint) - xRunningSum) / (iPoint + 1);
        yRunningSum := yRunningSum + (contour.y(iPoint) - yRunningSum) / (iPoint + 1);
    end;

    // Round to the nearest 10 units
    CentroidX := Round(xRunningSum / 10) * 10;
    CentroidY := Round(yRunningSum / 10) * 10;

    Result := True;
end;

procedure   ClientDeSelectAll(dummy : Boolean = False);
begin
    Client.SendMessage('PCB:DeSelect', 'Scope=All' , 255, Client.CurrentView);
end;

procedure   ClientZoomRedraw(dummy : Boolean = False);
begin
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
end;

procedure   ClientZoomSelected(dummy : Boolean = False);
begin
    Client.SendMessage('PCB:Zoom', 'Action=Selected' , 255, Client.CurrentView);
end;

function    CoordToDisplayStr(coords : TCoord) : String;
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

function    CoordToDisplayX(coords : TCoord) : String;
begin
    result := CoordUnitToString(coords - Board.XOrigin, Board.DisplayUnit xor 1);
end;

function    CoordToDisplayY(coords : TCoord) : String;
begin
    result := CoordUnitToString(coords - Board.YOrigin, Board.DisplayUnit xor 1);
end;

procedure   CopyTextFormatFromTo(SourceText : IPCB_Text; TargetText : IPCB_Text);
begin
    if SourceText = nil then exit;
    if TargetText = nil then exit;

    TargetText.Width                        := SourceText.Width;
    TargetText.UseTTFonts                   := SourceText.UseTTFonts;
    TargetText.UseInvertedRectangle         := SourceText.UseInvertedRectangle;
    TargetText.TTFTextWidth                 := SourceText.TTFTextWidth;
    TargetText.TTFTextHeight                := SourceText.TTFTextHeight;
    TargetText.TTFOffsetFromInvertedRect    := SourceText.TTFOffsetFromInvertedRect;
    TargetText.TTFInvertedTextJustify       := SourceText.TTFInvertedTextJustify;
    TargetText.TextKind                     := SourceText.TextKind;
    TargetText.Size                         := SourceText.Size;
    TargetText.Italic                       := SourceText.Italic;
    TargetText.InvRectWidth                 := SourceText.InvRectWidth;
    TargetText.InvRectHeight                := SourceText.InvRectHeight;
    TargetText.InvertedTTTextBorder         := SourceText.InvertedTTTextBorder;
    TargetText.Inverted                     := SourceText.Inverted;
    TargetText.FontName                     := SourceText.FontName;
    TargetText.FontID                       := SourceText.FontID;
    TargetText.Bold                         := SourceText.Bold;
    TargetText.BarCodeYMargin               := SourceText.BarCodeYMargin;
    TargetText.BarCodeXMargin               := SourceText.BarCodeXMargin;
    TargetText.BarCodeShowText              := SourceText.BarCodeShowText;
    TargetText.BarCodeRenderMode            := SourceText.BarCodeRenderMode;
    TargetText.BarCodeMinWidth              := SourceText.BarCodeMinWidth;
    TargetText.BarCodeKind                  := SourceText.BarCodeKind;
    TargetText.BarCodeInverted              := SourceText.BarCodeInverted;
    TargetText.BarCodeFullWidth             := SourceText.BarCodeFullWidth;
    TargetText.BarCodeFullHeight            := SourceText.BarCodeFullHeight;
    TargetText.BarCodeFontName              := SourceText.BarCodeFontName;
    TargetText.Layer                        := SourceText.Layer;

    TargetText.GraphicallyInvalidate;
end;

function    DebugContourInfo(contour : IPCB_Contour) : TStringList;
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

function    DebugGeometricPolygonInfo(poly : IPCB_GeometricPolygon) : TStringList;
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

function    DebugLevelStr(dummy : String = '') : String;
begin
    Result := '-------------------------  Debug Level: ' + IntToStr(iDebugLevel) + '  -------------------------' + sLineBreak;
end;

procedure   DebugMessage(const ShowLevel : Integer; const msg : WideString; const Caption : String = 'Confirm or Cancel Debug');
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

function    FolderIsReadOnly(const AFolderPath : String) : Boolean;
var
    TempFile: String;
    FileHandle: Integer;
begin
    Result := True;
    TempFile := IncludeTrailingPathDelimiter(AFolderPath) + 'temp.tmp';
    FileHandle := FileCreate(TempFile);
    if FileHandle > 0 then
    begin
        FileClose(FileHandle);
        DeleteFile(TempFile);
        Result := False;
    end;
end;

function    GetComponentAreaMils(Comp : IPCB_Component) : Int64;
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

function    GetComponentBodyLargest(Comp : IPCB_Component) : IPCB_ComponentBody;
var
    GIter : IPCB_GroupIterator;
    Prim : IPCB_Primitive;
    Area : Int64;
begin
    Result := nil;
    // Create group iterator
    GIter := Comp.GroupIterator_Create;
    GIter.AddFilter_ObjectSet(MkSet(eComponentBodyObject));

    // Try to cast the first element to a primitive
    Prim := GIter.FirstPCBObject;

    while (Prim <> nil) do
    begin
        if not Assigned(Area) then Area := Prim.Area;
        // only return layer of body with largest area, which means we have to check them all
        if Prim.Area > Area then
        begin
            Area := Prim.Area;
            Result := Prim;
        end;

        // Move to the next Primitive in the group
        Prim := GIter.NextPCBObject;
    end;
    Comp.GroupIterator_Destroy(GIter);

    if Result <> nil then DebugMessage(3, StrFromObjectId(Result.ObjectId) + ' with largest area detected on layer ' + Layer2String(Result.Layer) + sLineBreak + Result.Identifier);
end;

function    GetComponentBodyLayerSet(Comp : IPCB_Component) : TV7_LayerSet;
var
    Prim : IPCB_Primitive;
begin
    Result := MkSet(Comp.Layer);

    Prim := GetComponentBodyLargest(Comp);

    if Prim <> nil then Result := MkSet(Prim.Layer);
end;

function    GetDesignator(var Comp : IPCB_Component) : IPCB_Primitive;
{ Return the first .Designator special string associated with a component }
var
    GIter           : IPCB_GroupIterator;
    Text            : IPCB_Primitive;
begin
    Result := nil;
    if Comp = nil then exit;
    if Comp.ObjectId <> eComponentObject then exit;

    // Create an iterator set to loop through all text primitives
    GIter := Comp.GroupIterator_Create;
    GIter.AddFilter_ObjectSet(MkSet(eTextObject));

    Text := GIter.FirstPCBObject;
    while (Text <> nil) do
    begin
        // Check if the Text is .Designator special string
        if Text.UnderlyingString = '.Designator' then
        begin
            Result := Text;
            Break;
        end;

        Text := GIter.NextPCBObject;
    end;
    Comp.GroupIterator_Destroy(GIter);
end;

function    GetIteratorCount(Iter : IPCB_BoardIterator) : Integer;
var
    count : Integer;
    obj   : IPCB_ObjectClass;
begin
    count := 0;

    obj := Iter.FirstPCBObject;
    while obj <> nil do
    begin
        Inc(count);
        obj := Iter.NextPCBObject;
    end;
    result := count;
end;

function    GetSelectedAssyTextCount(dummy : Boolean = False) : Integer;
var
    i       : Integer;
begin
    Result := 0;
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        if Board.SelectecObject[i].ObjectId = eTextObject then
            if (Board.SelectecObject[i].InComponent) and (Board.SelectecObject[i].UnderlyingString = '.Designator') then Inc(Result);
    end;
end;

function    GetSelectedComponentCount(dummy : Boolean = False) : Integer;
var
    i       : Integer;
begin
    Result := 0;
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        if Board.SelectecObject[i].ObjectId = eComponentObject then Inc(Result);
    end;
end;

procedure   Inspect_IPCB_Text(var Text : IPCB_Text3; const MyLabel : string = '');
{ IPCB_Text inspector for debugging }
var
    AD19DebugStr : String;
begin
    AD19DebugStr := Format('%s : %s', ['AdvanceSnapping',  BoolToStr(Text.AdvanceSnapping, True)]) + sLineBreak +
            Format('%s : %s', ['SnapPointX',  CoordToDisplayX(Text.SnapPointX)]) + sLineBreak +
            Format('%s : %s', ['SnapPointY',  CoordToDisplayY(Text.SnapPointY)]);

    ShowInfo('DEBUGGING: ' + MyLabel + sLineBreak +
                '------------------------------' + sLineBreak +
                AD19DebugStr + sLineBreak +
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
                Format('%s : %s', ['Used',  BoolToStr(Text.Used, True)]) + sLineBreak +
                Format('%s : %s', ['UserRouted',  BoolToStr(Text.UserRouted, True)]) + sLineBreak +
                Format('%s : %s', ['ViewableObjectID',  IntToStr(Text.ViewableObjectID)]) + sLineBreak +
                Format('%s : %s', ['Width',  CoordToDisplayStr(Text.Width)]) + sLineBreak +
                Format('%s : %s', ['WordWrap',  BoolToStr(Text.WordWrap, True)]) + sLineBreak
                , 'Confirm IPCB_Text Info (partial)');
end;

function    IsSameSide(Obj1: IPCB_ObjectClass; Obj2: IPCB_ObjectClass) : Boolean;
// Check if two layers are the on the same side of the board. Handle different layer names.
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

function    IsStringANum(Text : string) : Boolean;
var
    i        : Integer;
    dotCount : Integer;
    hyphenCount : Integer;
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

    // Test for more than one hyphen
    hyphenCount := 0;
    ChSet    := MkSet(Ord('-'));
    for i    := 1 to Length(Text) do
        if InSet(Ord(Text[i]), ChSet) then inc(hyphenCount);

    // Test for more than one dot or comma
    dotCount := 0;
    ChSet    := MkSet(Ord('.'), Ord(','));
    for i    := 1 to Length(Text) do
        if InSet(Ord(Text[i]), ChSet) then inc(dotCount);

    if (dotCount > 1) or (hyphenCount > 1) then Result := False;
end; { IsStringANum }

function    IsViaOnBottomSide(ViaObj : IPCB_Via) : Boolean;
begin
    if ViaObj = nil then exit;
    Result := (ViaObj.LowLayer = eTopLayer) or (ViaObj.HighLayer = eTopLayer);
end;

function    IsViaOnTopSide(ViaObj : IPCB_Via) : Boolean;
begin
    if ViaObj = nil then exit;
    Result := (ViaObj.LowLayer = eTopLayer) or (ViaObj.HighLayer = eTopLayer);
end;

function    IsViaTentedForSilk(Silk : IPCB_Text; ViaObj : IPCB_Via) : Boolean;
begin
    if ViaObj = nil then exit;
    if ViaObj.ObjectId <> eViaObject then exit;
    Result := False;

    // Check if Silk is on TopOverlay and ViaObj is tented on top
    if (Silk.Layer = eTopOverlay) and IsViaOnTopSide(ViaObj) and ViaObj.IsTenting_Top then
        Result := True;

    // Check if Silk is on BottomOverlay and ViaObj is tented on bottom
    if (Silk.Layer = eBottomOverlay) and IsViaOnBottomSide(ViaObj) and ViaObj.IsTenting_Bottom then
        Result := True;
end;

procedure   NormalizeSelectedWithJustification;
{ normalizes selected text objects to be right-reading while preserving their justification }
var
    i       : Integer;
    Prim    : IPCB_Primitive;
begin
    if not DocumentIsPCB then exit;

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim := Board.SelectecObject[i];
        if Prim.ObjectId <> eTextObject then continue;

        NormalizeText(Prim);
    end;
end;

function    NormalizeText(var Text : IPCB_Text) : Boolean;
{ normalizes IPCB_Text object to be right-reading }
var
    OldAngle, NewAngle      : Double;
    OldJustify, NewJustify  : TTextAutoposition;
begin
    // AD19+ functions used for normalization
    Result := False;

    // coerce angle to 0 ~ 360
    OldAngle := (Text.Rotation mod 360 + 360) mod 360; // technically an integer operation but close enough
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

procedure   ReportFootprintNames_Selected(header : String = 'Footprint Names:');
var
    i           : Integer;
    Comp        : IPCB_Component;
    PatternList : TStringList;
begin
    if Board.SelectecObjectCount = 0 then
    begin
        ShowInfo('Nothing selected');
        exit;
    end;

    PatternList := CreateObject(TStringList);
    PatternList.Duplicates := dupIgnore;
    PatternList.Sorted := True; // needed for duplicate detection

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Comp := Board.SelectecObject[i];
        if Comp = nil then continue
        else if Comp.ObjectId <> eComponentObject then continue;
        PatternList.Add(Comp.Pattern);
    end;

    // watch the length or the message will be larger than the screen
    if PatternList.Count > 40 then ShowInfo(header + sLineBreak + PatternList.DelimitedText)
    else ShowInfo(header + sLineBreak + PatternList.Text);
end;

procedure   ReportFootprintNames_All(header : String = 'Footprint Names:');
var
    Iter        : IPCB_BoardIterator;
    Comp        : IPCB_Component;
    PatternList : TStringList;
begin
    PatternList := CreateObject(TStringList);
    PatternList.Duplicates := dupIgnore;
    PatternList.Sorted := True; // needed for duplicate detection

    Iter := Board.BoardIterator_Create;
    Iter.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iter.AddFilter_LayerSet(MkSet(eTopLayer, eBottomLayer));
    Iter.AddFilter_Method(eProcessAll);

    Comp := Iter.FirstPCBObject;
    while Comp <> nil do
    begin
        PatternList.Add(Comp.Pattern);
        Comp := Iter.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(Iter);

    // watch the length or the message will be larger than the screen
    if PatternList.Count > 40 then ShowInfo(header + sLineBreak + PatternList.DelimitedText)
    else ShowInfo(header + sLineBreak + PatternList.Text);
end;

function    RoundCoordAuto(coords : TCoord) : TCoord;
begin
    if not (Assigned(gvMetricUnits) and Assigned(gvRES_METRIC) and Assigned(gvRES_IMPERIAL)) then
    begin
        ShowError('function RoundCoordAuto(coords) requires global variables: gvMetricUnits, gvRES_METRIC, gvRES_IMPERIAL');
        Result := coords;
        exit;
    end;
    if gvMetricUnits then Result := RoundCoords(coords, gvRES_METRIC, eMetric)
    else Result := RoundCoords(coords, gvRES_IMPERIAL, eImperial);
end;

function    RoundCoords(coords : TCoord; round_mult : Double; units : TUnit) : TCoord;
begin
    case units of
        eImperial: begin
            Result := MilsToCoord(Round(CoordToMils(coords) / round_mult) * round_mult);
        end;
        eMetric: begin
            Result := MMsToCoord(Round(CoordToMMs(coords) / round_mult) * round_mult);
        end;
        else begin
            Result := coords; // invalid
        end;
    end;
end;

function    SelectAllComponents(dummy : Boolean = False) : Integer;
var
    Iter        : IPCB_BoardIterator;
    Comp        : IPCB_Component;
begin
    Result := 0;

    Iter := Board.BoardIterator_Create;
    Iter.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iter.AddFilter_LayerSet(MkSet(eTopLayer, eBottomLayer));
    Iter.AddFilter_Method(eProcessAll);

    Comp := Iter.FirstPCBObject;
    while Comp <> nil do
    begin
        Comp.Selected := True;
        Comp.GraphicallyInvalidate;
        Comp := Iter.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(Iter);

    Result := Board.SelectecObjectCount;
end;

function    StrFromAutoPos(eAutoPos: TTextAutoposition) : String;
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

function    StrFromObjectId(ObjectId: TObjectId) : String;
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

function    StrFromObjectIdSet(ObjectIdSet : TObjectSet) : String;
var
    ObjectId : Integer;
    ObjectIdList : TStringList;
begin
    Result := '';
    if ObjectIdSet = MkSet() then exit;

    ObjectIdList := CreateObject(TStringList);

    for ObjectId := 0 to 26 do
    begin
        if InSet(ObjectId, ObjectIdSet) then ObjectIdList.Add(StrFromObjectId(ObjectId));
    end;

    Result := ObjectIdList.DelimitedText;
end;

procedure   MyStatusBar_SetState(Index : Integer; const S : WideString);
begin
    Client.GUIManager.StatusBarManager.SetState(Index, S);
end;

function    MyStatusBar_GetState(Index : Integer) : Widestring;
begin
    Result := Client.GUIManager.StatusBarManager.GetState(Index);
end;

procedure   MyStatusBar_SetStateDefault(dummy : Boolean = False);
begin
    Client.GUIManager.StatusBarManager.SetState(cStatusBar_SetDefault,'');
end;

procedure   MyStatusBar_PushStatus(dummy : Boolean = False);
begin
    Client.GUIManager.StatusBarManager.SetState(cStatusBar_Push,'');
end;

procedure   MyStatusBar_PopStatus(dummy : Boolean = False);
begin
    Client.GUIManager.StatusBarManager.SetState(cStatusBar_Pop,'');
end;

function    MyPercentActive(dummy : Boolean = False) : Boolean;
begin
    Result := gvMarquee = False;
end;

function    MyMarqueeActive(dummy : Boolean = False) : Boolean;
begin
    Result := gvMarquee;
end;

function    MyPercent_GetTotal(dummy : Boolean = False) : Integer;
begin
    Result := -1;
    if MyPercentActive then
    begin
        Result := gvTotalPercentCount;
    end;
end;

procedure   MyPercent_Init(const InitialString : String ; TotalCount : Integer);
begin
    MyStatusBar_PushStatus;
    MyStatusBar_SetState(cStatusBar_ProgressBarStart, InitialString);
    gvTotalPercentCount     := TotalCount;
    gvCurrentPercentCount   := 0;
    gvOldPercent            := 0;
    gvMarquee               := False;
end;

procedure   MyPercent_Finish(dummy : Boolean = False);
begin
    //if MyPercentActive then
    //begin
        //MyStatusBar_SetState(cStatusBar_ProgressBarStop,'');
        //MyStatusBar_PopStatus;
    //end;

    MyStatusBar_SetState(cStatusBar_ProgressBarStop,'');
    MyStatusBar_PopStatus;
    MyStatusBar_SetState(cStatusBar_SetDefault,'');
end;

procedure   MyPercent_UpdateByNumber(AmountToIncrement : Integer, StatusMsg : String = '');
begin
    if MyPercentActive then
    begin
        gvCurrentPercentCount := gvCurrentPercentCount + AmountToIncrement - 1;
        MyPercent_Update(StatusMsg);
    end;
end;

procedure   MyPercent_Update(StatusMsg : String = '');
Var
    ThisPercent  : Integer;
    i            : Integer;
begin
    if MyPercentActive then
    begin
        Inc(gvCurrentPercentCount);

        if gvTotalPercentCount = 0 then ThisPercent := 100
        else
            ThisPercent := Round( (100.0 * gvCurrentPercentCount) /
                                  (1.0   * gvTotalPercentCount)  );
        if ThisPercent > 100 then
            ThisPercent := 100;
        for i := gvOldPercent to ThisPercent - 1 do
        begin
            if StatusMsg <> '' then MyStatusBar_SetState(cStatusBar_Panel2, Format('%s (%d%%)', [StatusMsg, ThisPercent]));
            MyStatusBar_SetState(cStatusBar_ProgressBarStep,'');
        end;
        gvOldPercent := ThisPercent;
    end;
end;

procedure   MyPercent_BeginUndeterminedOperation(const InitialString : String);
begin
    MyStatusBar_PushStatus;
    MyStatusBar_SetState(cStatusBar_UndeterminedOpBegin, InitialString);

    gvMarquee := True;
end;

procedure   MyPercent_EndUndeterminedOperation(dummy : Boolean = False);
begin
    if MyMarqueeActive then
    begin
        MyStatusBar_SetState(cStatusBar_UndeterminedOpEnd, '');
        MyStatusBar_PopStatus;
    end;
end;

procedure   MyPercent_BeginComplexOperation(const InitialString : String);
begin
    { status bar is not involved, so no need to push/pop percent stack}
    MyStatusBar_SetState(cStatusBar_ComplexOpBegin, InitialString);
end;

procedure   MyPercent_EndComplexOperation(dummy : Boolean = False);
begin
    MyStatusBar_SetState(cStatusBar_ComplexOpEnd, '');
end;
