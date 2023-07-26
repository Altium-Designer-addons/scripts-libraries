{ Created by:  Ryan Rutledge }
{ For documentation see README.md }

const
    cMaxMechLayers          = 1024;
    cScriptTitle            = 'AssemblyTextPrep';
    cConfigFileName         = 'AssemblyTextPrepConfig.ini';
    cScriptVersion          = '0.81';
    cDEBUGLEVEL             = 0;

    DEBUGEXPANSION          = 0; // leave at -1 to disable
    // font defaults for hidden settings
    cSTROKE_RATIO           = 1.1111; // desired ratio of stroke font actual height to `size` parameter. 1.1111 will give stroke width of 1/9th of size (1 + 1/9)
    cTEXTSTEPIMPERIAL       = 1; // [mils] text size will be rounded to this
    cTEXTSTEPMETRIC         = 0.025; // [mm] text size will be rounded to this
    cSTROKEWIDTHSTEP_MIL    = 0.1; // [mils] stroke width will be rounded to this
    cSTROKEWIDTHSTEP_MM     = 0.01; // [mm] stroke width will be rounded to this

var
    ASSY_LAYER_TOP          : TV7_Layer;
    ASSY_LAYER_BOT          : TV7_Layer;
    ASSY_LAYERS_STR         : String;
    bForbidLocalSettings    : Boolean;
    bIgnoreCBChange         : Boolean;
    IsTextSelectable        : Boolean;
    IsCompSelectable        : Boolean;
    Board                   : IPCB_Board;
    iDebugLevel             : Integer;
    IsAtLeastAD19           : Boolean;
    MLS                     : IPCB_MasterLayerStack;
    MLP                     : IPCB_MechanicalLayerPairs;
    TEXTHEIGHTMAX           : TCoord;
    TEXTHEIGHTMIN           : TCoord;
    TEXTHEIGHTNOM           : TCoord;
    STROKE_RATIO            : Double;
    STROKEWIDTHSTEP         : TCoord;
    TEXTSTEPSIZE            : TCoord;
    TEXTSTEPIMPERIAL        : TCoord;
    TEXTSTEPMETRIC          : TCoord;
    ASPECT_RATIO_TOL        : Double; // height:width ratio must exceed this to trigger rotation in Best Fit mode


procedure   _GUI; forward;
procedure   About; forward;
function    AddAssyTextToCompFromStyle(var Comp : IPCB_Component; const StyleText : IPCB_Text) : Boolean; forward;
function    AddFreeTextToComp(var Comp : IPCB_Component; var Text : IPCB_Text) : Boolean; forward;
procedure   AdjustDesignatorPositions(const CenterStrategyIndex : Integer; const OrientationIndex : Integer; const Resize : Boolean; const Normalize : Boolean); forward;
function    CalculateCentroid(const contour : IPCB_Contour; out CentroidX : TCoord; out CentroidY : TCoord) : Boolean; forward;
procedure   ChangeTextUnits(Units : TUnit); forward;
procedure   ClientDeSelectAll(dummy : Boolean = False); forward;
procedure   ClientZoomRedraw(dummy : Boolean = False); forward;
procedure   ClientZoomSelected(dummy : Boolean = False); forward;
function    ConfigFile_GetPath(dummy : String = '') : String; forward;
procedure   ConfigFile_Read(AFileName : String); forward;
procedure   ConfigFile_Write(AFileName : String); forward;
function    CoordToStr(Coords : TCoord) : String; forward;
function    CoordToX(Coords : TCoord) : String; forward;
function    CoordToY(Coords : TCoord) : String; forward;
procedure   CopyTextFormatFromTo(SourceText : IPCB_Text; TargetText : IPCB_Text); forward;
function    DebugContourInfo(contour : IPCB_Contour) : TStringList; forward;
function    DebugGeometricPolygonInfo(poly : IPCB_GeometricPolygon) : TStringList; forward;
function    DebugLevelStr(dummy : String = '') : String; forward;
procedure   DebugMessage(const ShowLevel : Integer; const msg : WideString; const Caption : String = 'Confirm or Cancel Debug'); forward;
procedure   DeselectInvalidComponents(dummy : Boolean = False); forward;
procedure   DeselectValidComponents(dummy : Boolean = False); forward;
procedure   DesignatorAutoAdjust; forward;
procedure   DesignatorOrthoResize; forward;
procedure   DesignatorResize; forward;
function    DocumentIsPCB : Boolean; forward;
function    GetAnyDesignator(dummy : Boolean = False) : IPCB_Text; forward;
function    GetAssyLayer(ALayer : TLayer) : TV7_Layer; forward;
function    GetBRectOnLayer_Pad(Pad : IPCB_Pad; ALayer : TV7_Layer) : TCoordRect; forward;
function    GetComponent(var Text : IPCB_Primitive) : IPCB_Component; forward;
function    GetComponentBodyLargest(Comp : IPCB_Component) : IPCB_ComponentBody; forward;
function    GetComponentBounds_Body(Comp : IPCB_Component; out CentroidX : TCoord; out CentroidY : TCoord; out box_width : TCoord; out box_height : TCoord) : Tnteger; forward;
function    GetComponentBounds_Pads(Comp : IPCB_Component; out CentroidX : TCoord; out CentroidY : TCoord; out box_width : TCoord; out box_height : TCoord) : Tnteger; forward;
function    GetComponentBounds_Simple(const Comp : IPCB_Component; out CentroidX : TCoord; out CentroidY : TCoord; out box_width : TCoord; out box_height : TCoord) : Integer; forward;
function    GetDesignator(var Comp : IPCB_Component) : IPCB_Primitive; forward;
function    GetObjPoly(Obj: IPCB_ObjectClass, Expansion: TCoord = 0) : IPCB_GeometricPolygon; forward;
function    GetSelectedAssyTextCount(dummy : Boolean = False) : Integer; forward;
function    GetSelectedComponentCount(dummy : Boolean = False) : Integer; forward;
function    GetSelectedInvalidCount(dummy : Boolean = False) : Integer; forward;
procedure   InitialCheckGUI(var status : Integer); forward;
procedure   InitialCheckSelectBoth(var status : Integer); forward;
procedure   InitialCheckSelectComponents(var status : Integer); forward;
procedure   InitialCheckSelectDesignators(var status : Integer); forward;
procedure   Inspect_IPCB_Text(var Text : IPCB_Text3; const MyLabel : string = ''); forward;
function    IsSelectableCheck(var bCanSelectComp : Boolean; var bCanSelectText : Boolean); forward;
function    IsStringANum(Text : string) : Boolean; forward;
function    IsViolating(Text : IPCB_ObjectClass) : Boolean; forward;
procedure   NormalizeSelectedWithJustification; forward;
function    NormalizeText(var Text : IPCB_Text) : Boolean; forward;
procedure   ReportFootprintNames(header : String = 'Footprint Names:'); forward;
procedure   ResetDesignatorPositions; forward;
procedure   ResetDesignatorPositionsNorm; forward;
procedure   ResetDesignatorPositionsNormOrtho; forward;
procedure   ResetDesignatorPositionsOrtho; forward;
procedure   ResizeText(var Text : IPCB_Text; const target_width : TCoord; const target_height : TCoord); forward;
function    RotateTextToAngle(var Text : IPCB_Text; const Angle : Double; const Normalize : Boolean = False; const Ortho : Boolean = False) : Double; forward;
function    SelectAllComponents(dummy : Boolean = False) : Integer; forward;
procedure   SelectBoth; forward;
procedure   SelectComponents; forward;
procedure   SelectDesignators; forward;
procedure   SetButtonEnableStates(EnableState : Boolean); forward;
procedure   TAssemblyTextPrepForm.AssemblyTextPrepFormCreate(Sender: TObject); forward;
procedure   TAssemblyTextPrepForm.AssemblyTextPrepFormMouseEnter(Sender: TObject); forward;
procedure   TAssemblyTextPrepForm.AssemblyTextPrepFormShow(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonAddDesignatorsClick(Sender: TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonAutoAdjustClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonCancelClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonCheckSelectedClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonNormalizeAnyTextClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonNormalizeDesignatorClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonProcessClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonResetCenterClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonResetOriginClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonResizeNoMoveClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonSaveConfigClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonSelectBothClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonSelectComponentsClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonSelectDesignatorsClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonSelectMissingClick(Sender: TObject); forward;
procedure   TAssemblyTextPrepForm.ButtonZoomSelectedClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.CheckBoxLocalSettingsClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.ConfigClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.InputValueChange(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.LabelVersionClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.MMmilButtonClick(Sender : TObject); forward;
procedure   TAssemblyTextPrepForm.UserKeyPress(Sender : TObject; var Key : Char); forward;
procedure   UpdateConstants(dummy : Boolean = False); forward;


{ Main GUI }
procedure _GUI;
var
    status          : Integer;
begin
    status := 0;
    if not DocumentIsPCB then exit;

    InitialCheckGUI(status);
    //if status <> 0 then exit;

    //AssemblyTextPrepForm.ShowModal; // Show the GUI
    AssemblyTextPrepForm.FormStyle := fsStayOnTop;
    AssemblyTextPrepForm.Show;
end;


{ About information }
procedure About;
var
    MsgText : string;
begin
    MsgText := '"' + cScriptTitle + '" script version ' + cScriptVersion + sLineBreak +
        sLineBreak +
        'Use "_GUI" to manipulate assembly designators.' + sLineBreak +
        sLineBreak +
        'Updated versions and documentation may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries' + sLineBreak +
        '/tree/master/Scripts - PCB/AssemblyTextPrep' + sLineBreak +
        sLineBreak +
        'Settings save location:' + sLineBreak +
        ConfigFile_GetPath;

    ShowInfo(MsgText, 'About');
end; { About }


function AddAssyTextToCompFromStyle(var Comp : IPCB_Component; const StyleText : IPCB_Text) : Boolean;
var
    NewTextObj      : IPCB_Text;
    AssyLayer       : TV7_Layer;
begin
    Result := False;

    if Comp.Layer = eTopLayer then AssyLayer := ASSY_LAYER_TOP else AssyLayer := ASSY_LAYER_BOT;

    NewTextObj := PCBServer.PCBObjectFactory(eTextObject, eNoDimension, eCreate_Default);
    if StyleText = nil then
    begin
        // set some basic properties for newly created text object
        NewTextObj.AdvanceSnapping          := True;
        NewTextObj.XLocation                := Comp.x;
        NewTextObj.YLocation                := Comp.y;
        NewTextObj.TTFInvertedTextJustify   := eAutoPos_CenterCenter;
        NewTextObj.SnapPointX               := Comp.x;
        NewTextObj.SnapPointY               := Comp.y;
        NewTextObj.WordWrap                 := False;
        NewTextObj.Layer                    := AssyLayer;
        NewTextObj.UnderlyingString         := '.Designator';
        NewTextObj.FontID                   := 2; // Sans Serif stroke font
        NewTextObj.Size                     := TEXTHEIGHTNOM;   // sets the height of the text.
        NewTextObj.Width                    := TEXTHEIGHTNOM div 10;

        if Comp.Layer = eTopLayer then NewTextObj.MirrorFlag := False
        else NewTextObj.MirrorFlag := True;
    end
    else
    begin
        // copy properties of example assy designator
        CopyTextFormatFromTo(StyleText, NewTextObj);
        NewTextObj.XLocation                := Comp.x;
        NewTextObj.YLocation                := Comp.y;
        NewTextObj.TTFInvertedTextJustify   := eAutoPos_CenterCenter;
        NewTextObj.SnapPointX               := Comp.x;
        NewTextObj.SnapPointY               := Comp.y;
        NewTextObj.Layer                    := AssyLayer;
        NewTextObj.UnderlyingString         := '.Designator';

        if Comp.Layer = eTopLayer then NewTextObj.MirrorFlag := False
        else NewTextObj.MirrorFlag := True;
    end;

    Board.AddPCBObject(NewTextObj);

    Result := AddFreeTextToComp(Comp, NewTextObj)
end;


// returns false if text is already in a component
function AddFreeTextToComp(var Comp : IPCB_Component; var Text : IPCB_Text) : Boolean;
begin
    Result := False;
    if Text.InComponent then exit;
    Comp.BeginModify;
    Comp.AddPCBObject(Text);
    Comp.EndModify;
    Comp.GraphicallyInvalidate;
    Result := True;
end;


{ Main function to select both components and assembly designators for selected objects, then reset .Designator positions }
procedure AdjustDesignatorPositions(const CenterStrategyIndex : Integer; const OrientationIndex : Integer; const Resize : Boolean; const Normalize : Boolean);
var
    i                       : Integer;
    status                  : Integer;
    Comp                    : IPCB_Component;
    Text                    : IPCB_Text3;
    Area1, Area2            : Integer;
    XOffset, YOffset        : TCoord;
    box_width, box_height   : TCoord;
    box_width2, box_height2 : TCoord;
    BRect                   : TCoordRect;
    CentroidX, CentroidY    : TCoord;
    CentroidX2, CentroidY2  : TCoord;
    target_rotation         : Integer;
    Ortho                   : Boolean;
    AutoRotate              : Boolean;
    InvalidCount            : Integer;
begin
    SelectBoth;

    InvalidCount := GetSelectedInvalidCount;

    if InvalidCount > 0 then
        if ConfirmNoYes(IntToStr(InvalidCount) + ' components without .Designator special strings found.' + sLineBreak +
                'Do you want to abort and select only components missing designators?') then
        begin
            DeselectValidComponents;

            // deselect any non-components
            i := 0;
            while i < Board.SelectecObjectCount do
            begin
                Text := Board.SelectecObject[i];
                if (Text.ObjectId <> eComponentObject) then Text.SetState_Selected(False)
                else i := i + 1; // advance iterator if current object remains selected
            end;

            ClientZoomSelected;
            exit;
        end
        else DeselectInValidComponents;

    if Board.SelectecObjectCount = 0 then exit;

    SelectDesignators;

    if Board.SelectecObjectCount > 0 then Board.NewUndo else exit;

    // -1 = Don't rotate; 0 = Best Fit; 1 = Match Component; 2 = Orthogonal to Component
    case OrientationIndex of
        -1: begin
            AutoRotate := False;
            Ortho := False;
        end;
        0: begin
            AutoRotate := True;
            Ortho := False;
        end;
        1: begin
            AutoRotate := False;
            Ortho := False;
        end;
        2: begin
            AutoRotate := False;
            Ortho := True;
        end;
    end;

    // Notify the pcbserver that we will make changes (Start undo)
    PCBServer.PreProcess;
    try
        // at this point we have a selection of text objects
        // For each Text object selected:
        for i := 0 to Board.SelectecObjectCount - 1 do
        begin
            // use GetComponent to get the owner component followed by its XY position and rotation
            Text := Board.SelectecObject[i];
            Comp := GetComponent(Text);

            if Comp = nil then continue;  // skip if component not found

            if iDebugLevel >= 2 then Inspect_IPCB_Text(Text, 'BEFORE');

            Text.BeginModify;
            // need to turn on AdvanceSnapping (requires AD19+ but I'm not going to bother supporting old versions)
            Text.AdvanceSnapping := True;   // necessary for autoposition to work correctly (thanks, Brett Miller!)

            // set Text object's justification to center.
            Text.TTFInvertedTextJustify := eAutoPos_CenterCenter;

            // -1 = Don't move; 0 = Automatic; 1 = Center of Bounds; 2 = Component Body; 3 = Center of Pads; 4 = Footprint origin
            case CenterStrategyIndex of
                -1: begin
                    BRect := Comp.BoundingRectangleNoNameComment;
                    box_width  := BRect.Right - BRect.Left;
                    box_height := BRect.Top - BRect.Bottom;
                end;
                0: begin
                    Area1 := GetComponentBounds_Body(Comp, CentroidX, CentroidY, box_width, box_height);
                    Area2 := GetComponentBounds_Pads(Comp, CentroidX2, CentroidY2, box_width2, box_height2);
                    if Area2 > Area1 then
                    begin
                        CentroidX := CentroidX2;
                        CentroidY := CentroidY2;
                        box_width := box_width2;
                        box_height := box_height2;
                    end;
                    Text.SnapPointX := CentroidX;
                    Text.SnapPointY := CentroidY;
                end;
                1: begin
                    GetComponentBounds_Simple(Comp, CentroidX, CentroidY, box_width, box_height);
                    Text.SnapPointX := CentroidX;
                    Text.SnapPointY := CentroidY;
                end;
                2: begin
                    GetComponentBounds_Body(Comp, CentroidX, CentroidY, box_width, box_height);
                    Text.SnapPointX := CentroidX;
                    Text.SnapPointY := CentroidY;
                end;
                3: begin
                    GetComponentBounds_Pads(Comp, CentroidX, CentroidY, box_width, box_height);
                    Text.SnapPointX := CentroidX;
                    Text.SnapPointY := CentroidY;
                end;
                4: begin
                    GetComponentBounds_Pads(Comp, CentroidX, CentroidY, box_width, box_height);
                    CentroidX := Comp.x;
                    CentroidY := Comp.y;
                    Text.SnapPointX := CentroidX;
                    Text.SnapPointY := CentroidY;
                end;
            end;

            Text.EndModify;
            Text.GraphicallyInvalidate;

            if AutoRotate then
            begin
                if ((box_width * ASPECT_RATIO_TOL) / MAX(box_height, 1)) < 1  then Ortho := True else Ortho := False;
            end;

            // resize text to fit desired width
            if Resize then
            begin
                DebugMessage(1, Comp.Name.Text + ' bounding box info' + sLineBreak + 'box_width: ' + CoordToStr(box_width) + sLineBreak + 'box_height: ' + CoordToStr(box_height));

                // -1 = Don't rotate
                if OrientationIndex <> -1 then
                begin
                    // rotate text to match component
                    Text.BeginModify;
                    RotateTextToAngle(Text, Comp.Rotation, Normalize, Ortho);
                    Text.EndModify;
                    Text.GraphicallyInvalidate;
                    if Ortho then ResizeText(Text, box_height, box_width) else ResizeText(Text, box_width, box_height);
                end
                else
                begin
                    BRect := Comp.BoundingRectangleNoNameComment;
                    box_width := (BRect.Right - BRect.Left) * 0.001; // scale down to avoid overflow
                    box_height := (BRect.Top - BRect.Bottom) * 0.001;
                    box_width := Round(Sqrt(box_width*box_width + box_height*box_height)) * 1000; // use corner dist
                    ResizeText(Text, box_width, box_width);
                end;
            end
            else
            begin
                if OrientationIndex <> -1 then
                begin
                    // rotate text to match component
                    Text.BeginModify;
                    RotateTextToAngle(Text, Comp.Rotation, Normalize, Ortho);
                    Text.EndModify;
                    Text.GraphicallyInvalidate;
                end;
            end;

            if iDebugLevel >= 2 then Inspect_IPCB_Text(Text, 'AFTER');
        end;
    finally
        // Notify the pcbserver that all changes have been made (Stop undo)
        PCBServer.PostProcess;
    end;

    ClientZoomRedraw;

end;


function CalculateCentroid(const contour : IPCB_Contour; out CentroidX : TCoord; out CentroidY : TCoord) : Boolean;
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


procedure ChangeTextUnits(Units : TUnit);
var
    i           : Integer;
    ControlList : TObjectList;
    EditControl : TObject;
    TempString  : String;
    EditString  : String;
begin
    ControlList := CreateObject(TObjectList);
    ControlList.OwnsObjects := False; // required to not throw invalid pointer errors when list is freed

    ControlList.Add(EditSizeMin);
    ControlList.Add(EditSizeNom);
    ControlList.Add(EditSizeMax);

    for i := 0 to ControlList.Count - 1 do
    begin
        EditControl := ControlList[i];
        if EditControl = nil then continue;

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


procedure ClientDeSelectAll(dummy : Boolean = False);
begin
    Client.SendMessage('PCB:DeSelect', 'Scope=All' , 255, Client.CurrentView);
end;


procedure ClientZoomRedraw(dummy : Boolean = False);
begin
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
end;


procedure ClientZoomSelected(dummy : Boolean = False);
begin
    Client.SendMessage('PCB:Zoom', 'Action=Selected' , 255, Client.CurrentView);
end;


function ConfigFile_GetPath(dummy : String = '') : String;
begin
    Result := ExtractFilePath(GetRunningScriptProjectName) + cConfigFileName;
    if (not FileExists(Result)) or bForbidLocalSettings then Result := IncludeTrailingPathDelimiter(SpecialFolder_AltiumApplicationData) + cConfigFileName;
end;


procedure ConfigFile_Read(AFileName : String);
var
    IniFile             : TIniFile;
    LocalSettingsFile   : String;
    ConfigDebugCaption  : String;
    SettingsDebugFile   : String;
    SettingsDebugList   : TStringList;
    TempString          : String;
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

    IniFile := TIniFile.Create(AFileName);
    try
        AssemblyTextPrepForm.Top := IniFile.ReadInteger('Window Position', 'Top', AssemblyTextPrepForm.Top);
        AssemblyTextPrepForm.Left := IniFile.ReadInteger('Window Position', 'Left', AssemblyTextPrepForm.Left);

        EditSizeMin.Text            := IniFile.ReadString('Limits', 'Minimum Height', EditSizeMin.Text);
        EditSizeNom.Text            := IniFile.ReadString('Limits', 'Nominal Height', EditSizeNom.Text);
        EditSizeMax.Text            := IniFile.ReadString('Limits', 'Maximum Height', EditSizeMax.Text);
        EditAspectRatioTol.Text     := IniFile.ReadString('Limits', 'Aspect Ratio Threshold', EditAspectRatioTol.Text);

        MMmilButton.Caption         := IniFile.ReadString('Config', 'Units', MMmilButton.Caption);
        CheckBoxResize.Checked      := IniFile.ReadBool('Config', 'Resize Text', CheckBoxResize.Checked);
        CheckBoxNormalize.Checked   := IniFile.ReadBool('Config', 'Normalize Text', CheckBoxNormalize.Checked);
        rgCenterStrategy.ItemIndex  := IniFile.ReadInteger('Config', 'Centering Strategy', rgCenterStrategy.ItemIndex);
        rgOrientation.ItemIndex     := IniFile.ReadInteger('Config', 'Designator Orientation', rgOrientation.ItemIndex);

        TempString                  := IniFile.ReadString('Hidden Settings', 'stroke font height ratio', FloatToStr(cSTROKE_RATIO));
        if IsStringANum(TempString) then STROKE_RATIO := StrToFloat(TempString) else STROKE_RATIO := cSTROKE_RATIO;
        if STROKE_RATIO < 1.02 then STROKE_RATIO := 1.02; // 1.02 implies 1:50 stroke width:height
        TempString                  := IniFile.ReadString('Hidden Settings', 'mil unit font size step', FloatToStr(cTEXTSTEPIMPERIAL));
        if IsStringANum(TempString) then TEXTSTEPIMPERIAL := MilsToCoord(StrToFloat(TempString)) else TEXTSTEPIMPERIAL := MilsToCoord(cTEXTSTEPIMPERIAL);
        TempString                  := IniFile.ReadString('Hidden Settings', 'mm unit font size step', FloatToStr(cTEXTSTEPMETRIC));
        if IsStringANum(TempString) then TEXTSTEPMETRIC := MMsToCoord(StrToFloat(TempString)) else TEXTSTEPMETRIC := MilsToCoord(cTEXTSTEPMETRIC);

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


procedure ConfigFile_Write(AFileName : String);
var
    IniFile: TIniFile;
begin
    IniFile := TIniFile.Create(AFileName);
    try
        IniFile.WriteInteger('Window Position', 'Top', AssemblyTextPrepForm.Top);
        IniFile.WriteInteger('Window Position', 'Left', AssemblyTextPrepForm.Left);

        IniFile.WriteString('Limits', 'Minimum Height', EditSizeMin.Text);
        IniFile.WriteString('Limits', 'Nominal Height', EditSizeNom.Text);
        IniFile.WriteString('Limits', 'Maximum Height', EditSizeMax.Text);
        IniFile.WriteString('Limits', 'Aspect Ratio Threshold', EditAspectRatioTol.Text);

        IniFile.WriteString('Config', 'Units', MMmilButton.Caption);
        IniFile.WriteBool('Config', 'Resize Text', CheckBoxResize.Checked);
        IniFile.WriteBool('Config', 'Normalize Text', CheckBoxNormalize.Checked);
        IniFile.WriteInteger('Config', 'Centering Strategy', rgCenterStrategy.ItemIndex);
        IniFile.WriteInteger('Config', 'Designator Orientation', rgOrientation.ItemIndex);

        IniFile.WriteString('Hidden Settings', 'Warning', 'This section allows customization of things you may want to customize so they aren''t lost if the script updates. Use at own risk.');
        IniFile.WriteString('Hidden Settings', 'stroke font height ratio', FloatToStr(STROKE_RATIO));
        IniFile.WriteString('Hidden Settings', 'mil unit font size step', FloatToStr(CoordToMils(TEXTSTEPIMPERIAL)));
        IniFile.WriteString('Hidden Settings', 'mm unit font size step', FloatToStr(CoordToMMs(TEXTSTEPMETRIC)));
    finally
        IniFile.Free;
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


procedure CopyTextFormatFromTo(SourceText : IPCB_Text; TargetText : IPCB_Text);
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

{ Initial checks when .Designator strings are ostensibly selected }
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


procedure DeselectInvalidComponents(dummy : Boolean = False);
var
    i       : Integer;
    Comp    : IPCB_Component;
    Prim1   : IPCB_ObjectClass;
begin
    for i := Board.SelectecObjectCount - 1 downto 0 do
    begin
        Comp := Board.SelectecObject[i];
        Prim1 := GetDesignator(Comp);
        if Prim1 <> nil then continue;
        Comp.Selected := False;
        Comp.GraphicallyInvalidate;
    end;
end;


procedure DeselectValidComponents(dummy : Boolean = False);
var
    i       : Integer;
    Comp    : IPCB_Component;
    Prim1   : IPCB_ObjectClass;
begin
    for i := Board.SelectecObjectCount - 1 downto 0 do
    begin
        Comp := Board.SelectecObject[i];
        Prim1 := GetDesignator(Comp);
        if Prim1 = nil then continue;
        Comp.Selected := False;
        Comp.GraphicallyInvalidate;
    end;
end;


{ wrapper call to AdjustDesignatorPositions that will resize text and orient it for best fit}
procedure DesignatorAutoAdjust;
begin
    AdjustDesignatorPositions(0, 0, True, True);
end;


{ wrapper call to AdjustDesignatorPositions that will resize text and orient it orthogonal to component rotation}
procedure DesignatorOrthoResize;
begin
    AdjustDesignatorPositions(0, 2, True, False);
end;


{ wrapper call to AdjustDesignatorPositions that will exactly match component rotation and resize text }
procedure DesignatorResize;
begin
    AdjustDesignatorPositions(0, 1, True, False);
end;


function DocumentIsPCB : Boolean;
begin
    // set AD build flag
    if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;

    iDebugLevel := cDEBUGLEVEL;

    // Checks if current document is a PCB kind if not, show error and return false.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then
    begin
        ShowError('This script must be run from a PCB document.');
        Result := False;
    end
    else Result := True;
end;


function GetAnyDesignator(dummy : Boolean = False) : IPCB_Text;
var
    i, count    : Integer;
    Iter        : IPCB_BoardIterator;
    Comp        : IPCB_Component;
    Prim1       : IPCB_Primitive;
begin
    Result := nil;

    // first pass will only consider current selection
    for count := 0 to 1 do
    begin
        SelectBoth;
        // see if there is a designator among the selected items
        for i := 0 to Board.SelectecObjectCount - 1 do
        begin
            Prim1 := Board.SelectecObject[i];
            if Prim1.ObjectId = eTextObject then
            begin
                Result := Prim1;
                exit;
            end;
        end;

        // if we're here then extend search to all components
        SelectAllComponents;
    end;
end;


function GetAssyLayer(ALayer : TLayer) : TV7_Layer;
var
    Iter        : IPCB_BoardIterator;
    Comp        : IPCB_Component;
    Prim1       : IPCB_Primitive;
begin
    Result := eNoLayer;

    Iter := Board.BoardIterator_Create;
    Iter.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iter.AddFilter_LayerSet(MkSet(ALayer));
    Iter.AddFilter_Method(eProcessAll);

    Comp := Iter.FirstPCBObject;
    while Comp <> nil do
    begin
        Prim1 := GetDesignator(Comp);
        if Prim1 <> nil then
        begin
            Result := Prim1.Layer;
            break;
        end;
        Comp := Iter.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(Iter);
end;


// not a very performance function but I don't know a better way to get a proper on-layer bounding rectangle that rotation doesn't screw up
function GetBRectOnLayer_Pad(Pad : IPCB_Pad; ALayer : TV7_Layer) : TCoordRect;
var
    contour             : IPCB_Contour;
    iPoint              : Integer;
    maxX, maxY          : TCoord;
    minX, minY          : TCoord;
    poly                : IPCB_GeometricPolygon;
begin
    Result := nil;
    if Pad = nil then exit;

    Result := Pad.BoundingRectangleOnLayer(ALayer);

    poly := PCBServer.PCBContourMaker.MakeContour(Pad, 0, ALayer);
    if (poly = nil) or (poly.Count < 1) then exit;
    contour := poly.Contour(0);
    if contour.Count < 3 then exit; // less than 3 points is not valid contour

    // so far so good, initialize custom bounding box coordinates
    minX := 2147483647;
    minY := 2147483647;
    maxX := -2147483647;
    maxY := -2147483647;

    for iPoint := 0 to contour.Count - 1 do
    begin
        if contour.x(iPoint) < minX then minX := contour.x(iPoint);
        if contour.y(iPoint) < minY then minY := contour.y(iPoint);
        if contour.x(iPoint) > maxX then maxX := contour.x(iPoint);
        if contour.y(iPoint) > maxY then maxY := contour.y(iPoint);
    end;

    Result.Left := minX;
    Result.Bottom := minY;
    Result.Right := maxX;
    Result.Top := maxY;
end;


{ Return the parent component of a text string }
function GetComponent(var Text : IPCB_Primitive) : IPCB_Component;
begin
    if (Text = nil) or (not Text.InComponent) then
        Result := nil
    else
        Result := Text.Component;
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
    Comp.GroupIterator_Destroy(GIter);

    if Result <> nil then DebugMessage(3, StrFromObjectId(Result.ObjectId) + ' with largest area detected on layer ' + Layer2String(Result.Layer) + sLineBreak + Result.Identifier);
end;


{ Function to calculate the bounding box dimensions of a component's largest 3D body. Returns area in square mils. }
function GetComponentBounds_Body(Comp : IPCB_Component; out CentroidX : TCoord; out CentroidY : TCoord; out box_width : TCoord; out box_height : TCoord) : Tnteger;
var
    body                : IPCB_ComponentBody;
    BRect               : TCoordRect;
    contour             : IPCB_Contour;
    poly                : IPCB_GeometricPolygon;
    radRotation         : Double;
    rotation            : Double;
    temp_cx, temp_cy    : TCoord; // zero-rotation centroid location
begin
    Result := 0;
    if Comp = nil then exit;

    CentroidX := Comp.x;
    CentroidY := Comp.y;

    rotation := Comp.Rotation;  // save original component rotation
    Comp.BeginModify;
    Comp.Rotation := 0;         // rotate component to zero to calculate bounding box
    Comp.EndModify;

    body := GetComponentBodyLargest(Comp);
    if body = nil then exit;
    poly := PCBServer.PCBContourMaker.MakeContour(body, 0, Comp.Layer);
    if (poly = nil) or (poly.Count < 1) then exit;
    contour := poly.Contour(0);
    if contour.Count < 1 then exit;

    CalculateCentroid(contour, temp_cx, temp_cy);

    BRect := Body.BoundingRectangle;

    box_width  := BRect.Right - BRect.Left;
    box_height := BRect.Top - BRect.Bottom;

    // return to original rotation
    Comp.BeginModify;
    Comp.Rotation := rotation;
    Comp.EndModify;

    radRotation := rotation * PI / 180; // convert to radians

    // calculate where centroid will be in original rotation
    CentroidX := Round((Cos(radRotation) * (temp_cx - Comp.x) - Sin(radRotation) * (temp_cy - Comp.y) + Comp.x) / 10) * 10;
    CentroidY := Round((Sin(radRotation) * (temp_cx - Comp.x) + Cos(radRotation) * (temp_cy - Comp.y) + Comp.y) / 10) * 10;

    // return area in square mils
    Result := Round(box_width * 0.0001) * Round(box_height * 0.0001);
end;


{ Function to calculate the bounding box dimensions enclosing a component's pads. Returns area in square mils. }
function GetComponentBounds_Pads(Comp : IPCB_Component; out CentroidX : TCoord; out CentroidY : TCoord; out box_width : TCoord; out box_height : TCoord) : Tnteger;
var
    Pad                 : IPCB_Pad;
    PadBoundary         : TCoordRect;
    PadIterator         : IPCB_GroupIterator;
    Poly                : IPCB_GeometricPolygon;
    minX, minY          : TCoord;
    maxX, maxY          : TCoord;
    temp_cx, temp_cy    : TCoord; // zero-rotation centroid location
    rotation            : Double;
    radRotation         : Double;
begin
    Result := 0;
    if Comp = nil then exit;

    // Initialize bounding box coordinates
    minX := 2147483647;
    minY := 2147483647;
    maxX := -2147483647;
    maxY := -2147483647;

    rotation := Comp.Rotation;  // save original component rotation
    Comp.BeginModify;
    Comp.Rotation := 0;         // rotate component to zero to calculate bounding box
    Comp.EndModify;

    // Create iterator for pads only
    PadIterator := Comp.GroupIterator_Create;
    try
        PadIterator.AddFilter_ObjectSet(MkSet(ePadObject));

        Pad := PadIterator.FirstPCBObject;
        while (Pad <> nil) do
        begin
            case Pad.ShapeOnLayer(Comp.Layer) of
                eRectangular: begin
                    PadBoundary := Pad.BoundingRectangleOnLayer(Comp.Layer);
                    if PadBoundary.Left < minX then minX := PadBoundary.Left;
                    if PadBoundary.Bottom < minY then minY := PadBoundary.Bottom;
                    if PadBoundary.Right > maxX then maxX := PadBoundary.Right;
                    if PadBoundary.Top > maxY then maxY := PadBoundary.Top;
                end;
                else begin
                    PadBoundary := GetBRectOnLayer_Pad(Pad, Comp.Layer);
                    if PadBoundary.Left < minX then minX := PadBoundary.Left;
                    if PadBoundary.Bottom < minY then minY := PadBoundary.Bottom;
                    if PadBoundary.Right > maxX then maxX := PadBoundary.Right;
                    if PadBoundary.Top > maxY then maxY := PadBoundary.Top;
                end;
            end;

            Pad := PadIterator.NextPCBObject;
        end;
    finally
        Comp.GroupIterator_Destroy(PadIterator);
    end;

    // Calculate the bounding box dimensions
    box_width  := maxX - minX;
    box_height := maxY - minY;

    // return to original rotation
    Comp.BeginModify;
    Comp.Rotation := rotation;
    Comp.EndModify;

    radRotation := rotation * PI / 180; // convert to radians

    temp_cx := minX + (box_width div 2);
    temp_cy := minY + (box_height div 2);

    // calculate where centroid will be in original rotation
    CentroidX := Round((Cos(radRotation) * (temp_cx - Comp.x) - Sin(radRotation) * (temp_cy - Comp.y) + Comp.x) / 10) * 10;
    CentroidY := Round((Sin(radRotation) * (temp_cx - Comp.x) + Cos(radRotation) * (temp_cy - Comp.y) + Comp.y) / 10) * 10;

    // return area in square mils
    Result := Round(box_width * 0.0001) * Round(box_height * 0.0001);
end;


{ Function to calculate the bounding box dimensions of a component's simple bounding rectangle. Returns area in square mils. }
function GetComponentBounds_Simple(const Comp : IPCB_Component; out CentroidX : TCoord; out CentroidY : TCoord; out box_width : TCoord; out box_height : TCoord) : Integer;
var
    Area                : Integer;
    BRect               : TCoordRect;
    temp_cx, temp_cy    : TCoord; // zero-rotation centroid location
    rotation            : Double;
    radRotation         : Double;
begin
    Result := 0;
    if Comp = nil then exit;

    rotation := Comp.Rotation;  // save original component rotation
    Comp.BeginModify;
    Comp.Rotation := 0;         // rotate component to zero to calculate bounding box size
    Comp.EndModify;

    BRect := Comp.BoundingRectangleNoNameComment;

    box_width  := BRect.Right - BRect.Left;
    box_height := BRect.Top - BRect.Bottom;

    // return to original rotation
    Comp.BeginModify;
    Comp.Rotation := rotation;
    Comp.EndModify;

    radRotation := rotation * PI / 180; // convert to radians

    temp_cx := BRect.Left + (box_width div 2);
    temp_cy := BRect.Bottom + (box_height div 2);

    // calculate where centroid will be in original rotation
    CentroidX := Round((Cos(radRotation) * (temp_cx - Comp.x) - Sin(radRotation) * (temp_cy - Comp.y) + Comp.x) / 10) * 10;
    CentroidY := Round((Sin(radRotation) * (temp_cx - Comp.x) + Cos(radRotation) * (temp_cy - Comp.y) + Comp.y) / 10) * 10;

    // return area in square mils
    Result := Round(box_width * 0.0001) * Round(box_height * 0.0001);
end;


{ Return the first .Designator special string associated with a component }
function GetDesignator(var Comp : IPCB_Component) : IPCB_Primitive;
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


function GetObjPoly(Obj: IPCB_ObjectClass, Expansion: TCoord = 0) : IPCB_GeometricPolygon;
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


function GetSelectedAssyTextCount(dummy : Boolean = False) : Integer;
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


function GetSelectedComponentCount(dummy : Boolean = False) : Integer;
var
    i       : Integer;
begin
    Result := 0;
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        if Board.SelectecObject[i].ObjectId = eComponentObject then Inc(Result);
    end;
end;


function GetSelectedInvalidCount(dummy : Boolean = False) : Integer;
begin
    Result := GetSelectedComponentCount - GetSelectedAssyTextCount;
end;


procedure InitialCheckGUI(var status : Integer);
var
    i               : Integer;
    LayerNameTop    : String;
    LayerNameBot    : String;
    TempLayerTop    : TV7_Layer;
    TempLayerBot    : TV7_Layer;
begin
    status := 0; // clear result status

    if not ((Board <> nil) or DocumentIsPCB) then
    begin
        status := 1;
        exit;
    end;

    iDebugLevel := cDEBUGLEVEL;

    IsSelectableCheck(IsCompSelectable, IsTextSelectable);
    if not (IsCompSelectable or IsTextSelectable) then ShowWarning('Components and Texts are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.')
    else if not IsCompSelectable then ShowWarning('Components are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.')
    else if not IsTextSelectable then ShowWarning('Texts are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.');

    MLS := Board.MasterLayerStack;
    MLP := Board.MechanicalPairs; // only needed if we can't derive assy layers from existing components

    if not Assigned(ASSY_LAYER_TOP) then ASSY_LAYER_TOP := eNoLayer;
    if not Assigned(ASSY_LAYER_BOT) then ASSY_LAYER_BOT := eNoLayer;

    TempLayerTop := GetAssyLayer(eTopLayer);
    TempLayerBot := GetAssyLayer(eBottomLayer);

    if TempLayerTop <> eNoLayer then ASSY_LAYER_TOP := TempLayerTop;
    if TempLayerBot <> eNoLayer then ASSY_LAYER_BOT := TempLayerBot;

    // if both assembly layer searches came up with nothing, show error, else use mech layer pairs to try to find missing from found
    if (ASSY_LAYER_TOP = eNoLayer) and (ASSY_LAYER_BOT = eNoLayer) then
    begin
        ShowError('Couldn''t determine any assembly layers from existing components:' + sLineBreak + ASSY_LAYERS_STR);
        ButtonAddDesignators.Enabled := False;
        status := 1;
    end
    else if (ASSY_LAYER_TOP = eNoLayer) then
    begin
        for i := 1 to cMaxMechLayers do
        begin
            // use MLS.GetMechanicalLayer(i) : IPCB_MechanicalLayer
            if MLP.PairDefined(ASSY_LAYER_BOT, MLS.GetMechanicalLayer(i).LayerID) then
            begin
                ASSY_LAYER_TOP := MLS.GetMechanicalLayer(i).LayerID;
                break;
            end;
        end;

        if ASSY_LAYER_TOP = eNoLayer then
        begin
            ShowError('Couldn''t determine top assembly layers from existing components or mech layer pairs:' + sLineBreak + ASSY_LAYERS_STR);
            status := 1;
        end;
    end
    else if (ASSY_LAYER_BOT = eNoLayer) then
    begin
        for i := 1 to cMaxMechLayers do
        begin
            if MLP.PairDefined(ASSY_LAYER_TOP, MLS.GetMechanicalLayer(i).LayerID) then
            begin
                ASSY_LAYER_BOT := MLS.GetMechanicalLayer(i).LayerID;
                break;
            end;
        end;

        if ASSY_LAYER_BOT = eNoLayer then
        begin
            ShowError('Couldn''t determine top assembly layers from existing components or mech layer pairs:' + sLineBreak + ASSY_LAYERS_STR);
            status := 1;
        end;
    end;

    if ASSY_LAYER_TOP = eNoLayer then LayerNameTop := 'N/A' else LayerNameTop := MLS.LayerObject(ASSY_LAYER_TOP).Name;
    if ASSY_LAYER_BOT = eNoLayer then LayerNameBot := 'N/A' else LayerNameBot := MLS.LayerObject(ASSY_LAYER_BOT).Name;

    //LayerObj := MLS.LayerObject(ASSY_LAYER_TOP).Name;
    ASSY_LAYERS_STR := Format('%s [%s] <----> %s [%s]', [LayerNameTop, cLayerStrings[ASSY_LAYER_TOP], LayerNameBot, cLayerStrings[ASSY_LAYER_BOT]]);

    LabelAssyLayerPair.Caption := ASSY_LAYERS_STR;
    LabelAssyLayerPair.Hint := LabelAssyLayerPair.Caption;
end;


{ Initial checks when a mix of components and .Designator strings are ostensibly selected }
procedure InitialCheckSelectBoth(var status : Integer);
var
    i                   : Integer;
    Prim1               : IPCB_Primitive;
    ComponentCount      : Integer;
    DesignatorCount     : Integer;
begin
    status := 0; // clear result status

    if not DocumentIsPCB then
    begin
        status := 1;
        exit;
    end;

    iDebugLevel := cDEBUGLEVEL;

    IsSelectableCheck(IsCompSelectable, IsTextSelectable);
    if not (IsCompSelectable or IsTextSelectable) then ShowWarning('Components and Texts are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.')
    else if not IsCompSelectable then ShowWarning('Components are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.')
    else if not IsTextSelectable then ShowWarning('Texts are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.');

    if not (IsCompSelectable and IsTextSelectable) then
    begin
        setatus := 1;
        exit;
    end;

    ComponentCount := 0;
    DesignatorCount := 0;

    // Count components and .Designator special strings without deselecting them
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if (Prim1.ObjectId = eComponentObject) then ComponentCount := ComponentCount + 1;
        if ((Prim1.ObjectId = eTextObject) and (Prim1.UnderlyingString = '.Designator')) then DesignatorCount := DesignatorCount + 1;
    end;

    if ((ComponentCount < 1) and (DesignatorCount < 1)) then
    begin
        if iDebugLevel > 0 then
        begin
            ShowError('Select at least 1 component or designator special string.' + sLineBreak +
                        '(designator string is ".Designator")' + sLineBreak + sLineBreak +
                        '-- Debugging Info --' + sLineBreak +
                        'ComponentCount: ' + IntToStr(ComponentCount) + sLineBreak +
                        'DesignatorCount: ' + IntToStr(DesignatorCount) + sLineBreak +
                        'Selected Object Count: ' + IntToStr(Board.SelectecObjectCount));
        end
        else ShowError('Select at least 1 component or designator special string.');

        status := 1;
        //exit; // don't exit, we want to do deselection
    end;

    if ((ComponentCount + DesignatorCount) <> Board.SelectecObjectCount) then
    begin
        // deselect anything that isn't a component or .Designator special string
        i := 0;
        while i < Board.SelectecObjectCount do
        begin
            Prim1 := Board.SelectecObject[i];
            if not ((Prim1.ObjectId = eComponentObject) or ((Prim1.ObjectId = eTextObject) and (Prim1.UnderlyingString = '.Designator'))) then Prim1.SetState_Selected(False)
            else i := i + 1; // advance iterator if current object remains selected
        end;
    end;

end;


procedure InitialCheckSelectComponents(var status : Integer);
var
    i               : Integer;
    Prim1           : IPCB_Primitive;
    DesignatorCount : Integer;
begin
    status := 0; // clear result status

    if not DocumentIsPCB then
    begin
        status := 1;
        exit;
    end;

    iDebugLevel := cDEBUGLEVEL;

    IsSelectableCheck(IsCompSelectable, IsTextSelectable);
    if not (IsCompSelectable or IsTextSelectable) then ShowWarning('Components and Texts are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.')
    else if not IsCompSelectable then ShowWarning('Components are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.')
    else if not IsTextSelectable then ShowWarning('Texts are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.');

    if not (IsCompSelectable and IsTextSelectable) then
    begin
        setatus := 1;
        exit;
    end;

    DesignatorCount := 0;

    // Count text objects without deselecting them
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if ((Prim1.ObjectId = eTextObject) and (Prim1.UnderlyingString = '.Designator')) then DesignatorCount := DesignatorCount + 1;
    end;

    if (DesignatorCount < 1) then
    begin
        if iDebugLevel > 0 then
        begin
            ShowError('Select at least 1 designator special string.' + sLineBreak +
                        '(designator string is ".Designator")' + sLineBreak + sLineBreak +
                        '-- Debugging Info --' + sLineBreak +
                        'DesignatorCount: ' + IntToStr(DesignatorCount) + sLineBreak +
                        'Selected Object Count: ' + IntToStr(Board.SelectecObjectCount));
        end
        else ShowError('Select at least 1 assembly designator' + sLineBreak + '(string contains ".Designator")');

        status := 1;
        exit;
    end;

    if (DesignatorCount <> Board.SelectecObjectCount) then
    begin
        // deselect anything that isn't a designator text object
        i := 0;
        while i < Board.SelectecObjectCount do
        begin
            Prim1 := Board.SelectecObject[i];
            if ((Prim1.ObjectId <> eTextObject) or (Prim1.UnderlyingString <> '.Designator')) then Prim1.SetState_Selected(False)
            else i := i + 1; // advance iterator if current object remains selected
        end;
    end;

end;


{ Initial checks when components are ostensibly selected }
procedure InitialCheckSelectDesignators(var status : Integer);
var
    i                   : Integer;
    Prim1               : IPCB_Primitive;
    ComponentCount      : Integer;
begin
    status := 0; // clear result status

    if not DocumentIsPCB then
    begin
        status := 1;
        exit;
    end;

    iDebugLevel := cDEBUGLEVEL;

    IsSelectableCheck(IsCompSelectable, IsTextSelectable);
    if not (IsCompSelectable or IsTextSelectable) then ShowWarning('Components and Texts are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.')
    else if not IsCompSelectable then ShowWarning('Components are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.')
    else if not IsTextSelectable then ShowWarning('Texts are not selectable. Make sure both "Components" and "Texts" are enabled in selection filter.');

    if not (IsCompSelectable and IsTextSelectable) then
    begin
        setatus := 1;
        exit;
    end;

    ComponentCount := 0;

    // Count components without deselecting them
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if (Prim1.ObjectId = eComponentObject) then ComponentCount := ComponentCount + 1;
    end;

    if (ComponentCount < 1) then
    begin
        if iDebugLevel > 0 then
        begin
            ShowError('Select at least 1 component.' + sLineBreak + sLineBreak +
                        '-- Debugging Info --' + sLineBreak +
                        'ComponentCount: ' + IntToStr(ComponentCount) + sLineBreak +
                        'Selected Object Count: ' + IntToStr(Board.SelectecObjectCount));
        end
        else ShowError('Select at least 1 component.');

        status := 1;
        exit;
    end;

    if (ComponentCount <> Board.SelectecObjectCount) then
    begin
        // deselect anything that isn't a component
        i := 0;
        while i < Board.SelectecObjectCount do
        begin
            Prim1 := Board.SelectecObject[i];
            if (Prim1.ObjectId <> eComponentObject) then Prim1.SetState_Selected(False)
            else i := i + 1; // advance iterator if current object remains selected
        end;
    end;

end;


{ IPCB_Text inspector for debugging }
procedure Inspect_IPCB_Text(var Text : IPCB_Text3; const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    AD19DebugStr := Format('%s : %s', ['AdvanceSnapping',  BoolToStr(Text.AdvanceSnapping, True)]) + sLineBreak +
            Format('%s : %s', ['SnapPointX',  CoordToX(Text.SnapPointX)]) + sLineBreak +
            Format('%s : %s', ['SnapPointY',  CoordToY(Text.SnapPointY)]);

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
                Format('%s : %s', ['MultilineTextHeight',  CoordToStr(Text.MultilineTextHeight)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextResizeEnabled',  BoolToStr(Text.MultilineTextResizeEnabled, True)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextWidth',  CoordToStr(Text.MultilineTextWidth)]) + sLineBreak +
                Format('%s : %s', ['ObjectId',  IntToStr(Text.ObjectId)]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString',  Text.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['PadCacheRobotFlag',  BoolToStr(Text.PadCacheRobotFlag, True)]) + sLineBreak +
                Format('%s : %s', ['Rotation',  FloatToStr(Text.Rotation)]) + sLineBreak +
                Format('%s : %s', ['Size',  CoordToStr(Text.Size)]) + sLineBreak +
                Format('%s : %s', ['Text',  Text.Text]) + sLineBreak +
                Format('%s : %s', ['TextKind',  IntToStr(Text.TextKind)]) + sLineBreak +
                Format('%s : %s', ['TTFInvertedTextJustify',  IntToStr(Text.TTFInvertedTextJustify)]) + sLineBreak +
                Format('%s : %s', ['TTFTextWidth',  CoordToStr(Text.TTFTextWidth)]) + sLineBreak +
                Format('%s : %s', ['TTFTextHeight',  CoordToStr(Text.TTFTextHeight)]) + sLineBreak +
                Format('%s : %s', ['UseTTFonts',  BoolToStr(Text.UseTTFonts, True)]) + sLineBreak +
                Format('%s : %s', ['Used',  BoolToStr(Text.Used, True)]) + sLineBreak +
                Format('%s : %s', ['UserRouted',  BoolToStr(Text.UserRouted, True)]) + sLineBreak +
                Format('%s : %s', ['ViewableObjectID',  IntToStr(Text.ViewableObjectID)]) + sLineBreak +
                Format('%s : %s', ['Width',  CoordToStr(Text.Width)]) + sLineBreak +
                Format('%s : %s', ['WordWrap',  BoolToStr(Text.WordWrap, True)]) + sLineBreak
                , 'Confirm IPCB_Text Info (partial)');
end;


function IsSelectableCheck(var bCanSelectComp : Boolean; var bCanSelectText : Boolean);
var
    checkComp   : Boolean;
    checkText   : Boolean;
    Iter        : IPCB_BoardIterator;
    Obj         : IPCB_ObjectClass;
    tempBool    : Boolean;
begin
    checkComp := True;
    checkText := True;

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
            if Obj.Selected = True then bCanSelectComp := True;
            Obj.Selected := tempBool;
            checkComp := False;
        end
        else if (Obj.ObjectId = eTextObject) and (checkText) then
        begin
            tempBool := Obj.Selected;
            Obj.Selected := True;
            if Obj.Selected = True then bCanSelectText := True;
            Obj.Selected := tempBool;
            checkText := False;
        end;

        if not (checkComp or checkText) then break;

        Obj := Iter.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(Iter);
end;


function IsStringANum(Text : string) : Boolean;
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


// Checks if text object overlaps other objects on same layer
function IsViolating(Text : IPCB_ObjectClass) : Boolean;
var
    TextPoly, ObjPoly       : IPCB_GeometricPolygon;
    GIter                   : IPCB_GroupIterator;
    SIter                   : IPCB_SpatialIterator;
    Obj                     : IPCB_Primitive;
    Expansion               : TCoord;
    SIterLeft, SIterBot     : TCoord;
    SIterRight, SIterTop    : TCoord;
begin
    Result := False;
    if Text.Component = nil then exit;

    if DEBUGEXPANSION = -1 then Expansion := Round(Text.Size / 50000) * 10000 else Expansion := DEBUGEXPANSION; // 1/5th text size seems like enough clearance

    TextPoly := GetObjPoly(Text); // get geometric polygon for text

    // use group iterator to loop through all primitives on Text layer
    GIter := Text.Component.GroupIterator_Create;
    GIter.AddFilter_LayerSet(MkSet(Text.Layer));
    GIter.AddFilter_ObjectSet(MkSet(eTextObject, eTrackObject, eArcObject, eFillObject, eRegionObject));

    Obj := GIter.FirstPCBObject;
    while (Obj <> nil) do
    begin
        // don't check text clearance against itself
        if Text.I_ObjectAddress = Obj.I_ObjectAddress then
        begin
            Obj := GIter.NextPCBObject;
            continue;
        end;

        // Get geometric polygon for other object
        ObjPoly := GetObjPoly(Obj, Expansion);

        // IPCB_ContourUtilities Function  GeometricPolygonsTouch(AGeometricPolygon : IPCB_GeometricPolygon; BGeometricPolygon : IPCB_GeometricPolygon) : Boolean;
        Result := PCBServer.PCBContourUtilities.GeometricPolygonsTouch(TextPoly, ObjPoly);

        if Result then break;

        Obj := GIter.NextPCBObject;
    end;
    Text.Component.GroupIterator_Destroy(GIter);

    // Create a spatial iterator to loop through free primitives on Text layer
    SIter := Board.SpatialIterator_Create;
    SIter.AddFilter_LayerSet(MkSet(Text.Layer));
    SIter.AddFilter_ObjectSet(MkSet(eTextObject, eTrackObject, eArcObject, eFillObject, eRegionObject));

    SIterLeft := Text.BoundingRectangle.Left - Expansion;
    SIterBot := Text.BoundingRectangle.Bottom - Expansion;
    SIterRight := Text.BoundingRectangle.Right + Expansion;
    SIterTop := Text.BoundingRectangle.Top + Expansion;

    SIter.AddFilter_Area(SIterLeft, SIterBot, SIterRight, SIterTop);

    Obj := SIter.FirstPCBObject;
    while (Obj <> nil) do
    begin
        // don't check text clearance against itself
        if Text.I_ObjectAddress = Obj.I_ObjectAddress then
        begin
            Obj := SIter.NextPCBObject;
            continue;
        end;

        // Get geometric polygon for other object
        ObjPoly := GetObjPoly(Obj, Expansion);

        // IPCB_ContourUtilities Function  GeometricPolygonsTouch(AGeometricPolygon : IPCB_GeometricPolygon; BGeometricPolygon : IPCB_GeometricPolygon) : Boolean;
        Result := PCBServer.PCBContourUtilities.GeometricPolygonsTouch(TextPoly, ObjPoly);

        if Result then break;

        Obj := SIter.NextPCBObject;
    end;
    Board.SpatialIterator_Destroy(SIter);

    if (Obj <> nil) and (iDebugLevel > 0) then
    begin
        DebugMessage(4, 'TextPoly Contours' + sLineBreak + DebugGeometricPolygonInfo(TextPoly).Text + sLineBreak + 'Result = ' + BoolToStr(Result, True));
        DebugMessage(4, Obj.Identifier + sLineBreak + 'ObjPoly Contours' + sLineBreak + DebugGeometricPolygonInfo(ObjPoly).Text);
    end;

end;


{ normalizes selected text objects to be right-reading while preserving their justification }
procedure NormalizeSelectedWithJustification;
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


procedure ReportFootprintNames(header : String = 'Footprint Names:');
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


{ wrapper call to AdjustDesignatorPositions that will exactly match component rotation }
procedure ResetDesignatorPositions;
begin
    AdjustDesignatorPositions(False, False, False, False);
end;


{ wrapper call to AdjustDesignatorPositions that will match component rotation but normalized to be right-reading }
procedure ResetDesignatorPositionsNorm;
begin
    AdjustDesignatorPositions(False, False, False, True);
end;


{ wrapper call to AdjustDesignatorPositions that will orient .Designator string orthogonal to component but normalized to be right-reading }
procedure ResetDesignatorPositionsNormOrtho;
begin
    AdjustDesignatorPositions(True, False, False, True);
end;


{ wrapper call to AdjustDesignatorPositions that will orient .Designator string orthogonal to component }
procedure ResetDesignatorPositionsOrtho;
begin
    AdjustDesignatorPositions(True, False, False, False);
end;


{ procedure to scale text to approximately fit within the selected width and height }
procedure ResizeText(var Text : IPCB_Text; const target_width : TCoord; const target_height : TCoord);
var
    temp_rotation   : double;
    width_ratio     : double;
    height_ratio    : double;
    planned_height  : TCoord;
    TTF_min_height  : TCoord;
    min_height      : TCoord;
    nom_height      : TCoord;
    max_height      : TCoord;
    init_height     : TCoord;
    excess_height   : TCoord;
    bStrokeFont     : Boolean; // get once to avoid accessing UseTTFonts property so much
begin
    bStrokeFont := Text.UseTTFonts = False;
    init_height := TEXTHEIGHTNOM; // set a consistent initial height for initial scale calculations
    target_height := MIN(target_height, TEXTHEIGHTMAX);
    temp_rotation := Text.Rotation;
    Text.BeginModify;
    Text.Rotation := 0;
    Text.Size := init_height;
    if bStrokeFont then Text.Width := Round(init_height * ((STROKE_RATIO - 1) / STROKEWIDTHSTEP)) * STROKEWIDTHSTEP;
    if Text.MultiLine then
    begin
        Text.MultiLine := False;
        Text.EndModify;
        Text.BeginModify;
        Text.MultiLine := True;
    end;
    Text.EndModify;
    if iDebugLevel >= 2 then Application.ProcessMessages;
    Text.GraphicallyInvalidate;

    if iDebugLevel >= 2 then Inspect_IPCB_Text(Text, 'Initial setup. Beginning resize.');

    // calculate ratio of target width to current width
    if bStrokeFont then width_ratio := target_width / (Text.TTFTextWidth - Text.Width) else width_ratio := target_width / Text.TTFTextWidth;;

    // calculate ratio of actual height to Text.Size
    if bStrokeFont then height_ratio := STROKE_RATIO else height_ratio := Text.TTFTextHeight / Text.Size;;

    min_height := Round((TEXTHEIGHTMIN / height_ratio) / STROKEWIDTHSTEP) * STROKEWIDTHSTEP;   // set min size correcting for actual height (round to STROKEWIDTHSTEP is intentional)
    nom_height := Round((TEXTHEIGHTNOM / height_ratio) / STROKEWIDTHSTEP) * STROKEWIDTHSTEP;   // set nom size correcting for actual height

    // multiply current size by width ratio to calculate what size would scale to target width (round to step size)
    planned_height := Round(Text.Size * width_ratio / TEXTSTEPSIZE) * TEXTSTEPSIZE;

    if planned_height > nom_height then
    begin
        excess_height := Round((planned_height - nom_height) / (2 * TEXTSTEPSIZE)) * TEXTSTEPSIZE; // scale by 0.5 between MAX and ABSMAX
        planned_height := nom_height + excess_height;
    end;

    Text.BeginModify;

    // limit planned height to target height or abs max height, whichever is smaller. (floor to step size)
    max_height := MIN(Floor((TEXTHEIGHTMAX / height_ratio) / TEXTSTEPSIZE) * TEXTSTEPSIZE, Floor((target_height / height_ratio) / TEXTSTEPSIZE) * TEXTSTEPSIZE);
    if planned_height > max_height then planned_height := max_height;

    Text.Size := MAX(min_height, planned_height);  // enforce a minimum height
    if bStrokeFont then Text.Width := Round(Text.Size * ((STROKE_RATIO - 1) / STROKEWIDTHSTEP)) * STROKEWIDTHSTEP;  // set stroke width, rounded to STROKEWIDTHSTEP
    Text.Rotation := temp_rotation; // undo temp rotation before checking clearance

    Text.EndModify;
    Text.GraphicallyInvalidate;

    if not IsViolating(Text) then
    begin
        DebugMessage(2, 'Resized text interferes: ' + BoolToStr(IsViolating(Text), True));
        exit;
    end
    else
    begin
        repeat
            planned_height := planned_height - (2 * TEXTSTEPSIZE); // simple solution is just to brute force 2 steps at a time

            Text.BeginModify;

            Text.Size := MAX(min_height, planned_height);  // enforce a minimum height
            if bStrokeFont then Text.Width := Round(Text.Size * ((STROKE_RATIO - 1) / STROKEWIDTHSTEP)) * STROKEWIDTHSTEP;  // set stroke width, rounded to STROKEWIDTHSTEP

            if Text.MultiLine then
            begin
                Text.MultiLine := False;
                Text.EndModify;
                Text.BeginModify;
                Text.MultiLine := True;
            end;

            Text.EndModify;
            //Text.GraphicallyInvalidate;
            if iDebugLevel >= 2 then Application.ProcessMessages;
            DebugMessage(3, 'Text.Size = ' + CoordToStr(Text.Size));

            if planned_height <= min_height then
            begin
                Text.EndModify;
                //Text.GraphicallyInvalidate;
                break;
            end;
        until (not IsViolating(Text));
    end;
    //if iDebugLevel >= 2 then Inspect_IPCB_Text(Text, 'After reduction loop.');

    DebugMessage(2, 'Resized text interferes: ' + BoolToStr(IsViolating(Text), True));
end;


{ rotates IPCB_Text object to specific angle, optionally normalizing it to be right-reading, optionally rotating 90° CW }
function RotateTextToAngle(var Text : IPCB_Text; const Angle : Double; const Normalize : Boolean = False; const Ortho : Boolean = False) : Double;
begin
    if Ortho then Angle := Angle + 270;

    // coerce angle to 0 ~ 360
    Angle := (Angle mod 360 + 360) mod 360; // technically an integer operation. TODO: make proper floating point mod throughout

    // rotate text to match Angle, based on how mirrored text reads from the flipside of the board
    case Text.GetState_Mirror of
        True: begin
            // mirrored text should be rotated to match layer flip behavior of text vs components
            if not Ortho then Angle := (Angle + 180) mod 360;
            if Normalize and (Angle >= 90) and (Angle < 270) then
                Text.Rotation := (Angle + 180) mod 360
            else
                Text.Rotation := Angle;
        end;
        False: begin
            if Normalize and (Angle > 90) and (Angle <= 270) then
                Text.Rotation := (Angle + 180) mod 360
            else
                Text.Rotation := Angle;
        end;
    end;

    Result := Text.Rotation;
end;


function SelectAllComponents(dummy : Boolean = False) : Integer;
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


{ Main function to select both components and assembly designators for selected objects }
procedure SelectBoth;
var
    i               : Integer;
    CompCount       : Integer;
    TextCount       : Integer;
    errText         : String;
    status          : Integer;
    Comp            : IPCB_Component;
    Text            : IPCB_Text;
    Prim1           : IPCB_Primitive;
begin
    InitialCheckSelectBoth(status);
    if status <> 0 then exit;

    CompCount := 0;
    TextCount := 0;

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if Prim1.ObjectId = eTextObject then
        begin
            TextCount := TextCount + 1;
            Comp := GetComponent(Prim1);
            if Comp <> nil then
            begin
                Comp.SetState_Selected(True);
                if Comp.GetState_Selected = True then CompCount := CompCount + 1;
            end;
        end
        else if Prim1.ObjectId = eComponentObject then
        begin
            CompCount := CompCount + 1;
            Text := GetDesignator(Prim1);
            if Text <> nil then
            begin
                Text.SetState_Selected(True);
                if Text.GetState_Selected = True then TextCount := TextCount + 1;
            end;
        end;
    end;

    ClientZoomRedraw;
end;


{ Main function to select components for selected assembly designators }
procedure SelectComponents;
var
    i               : Integer;
    status          : Integer;
    Comp            : IPCB_Component;
    Text            : IPCB_Primitive;
    InvalidCount    : Integer;
begin
    SelectBoth;

    InvalidCount := GetSelectedInvalidCount;

    if InvalidCount > 0 then
        if ConfirmNoYes(IntToStr(InvalidCount) + ' components without .Designator special strings found.' + sLineBreak +
                'Do you want to abort and select only components missing designators?') then
        begin
            DeselectValidComponents;
            ClientZoomSelected;
        end;

    // deselect assembly designators
    i := 0;
    while i < Board.SelectecObjectCount do
    begin
        Text := Board.SelectecObject[i];
        if (Text.ObjectId <> eComponentObject) then Text.SetState_Selected(False)
        else i := i + 1; // advance iterator if current object remains selected
    end;

    ClientZoomRedraw;
end;


{ Main function to select .Designator special strings for components }
procedure SelectDesignators;
var
    i               : Integer;
    status          : Integer;
    Comp            : IPCB_Component;
    CompCount       : Integer;
    Prim1           : IPCB_Primitive;
    InvalidCount    : Integer;
    DeselectID      : TObject;
begin
    DeselectID := eTextObject;
    SelectBoth;

    InvalidCount := GetSelectedInvalidCount;

    if InvalidCount > 0 then
        if ConfirmNoYes(IntToStr(InvalidCount) + ' components without .Designator special strings found.' + sLineBreak +
                'Do you want to abort and select only components missing designators?') then
        begin
            DeselectValidComponents;
            ClientZoomSelected;
            DeselectID := eComponentObject;
        end;

    // deselect components
    i := 0;
    while i < Board.SelectecObjectCount do
    begin
        Prim1 := Board.SelectecObject[i];
        if (Prim1.ObjectId <> eTextObject) then Prim1.SetState_Selected(False)
        else i := i + 1; // advance iterator if current object remains selected
    end;

    ClientZoomRedraw;
end;


procedure SetButtonEnableStates(EnableState : Boolean);
begin
    ButtonAutoAdjust.Enabled            := EnableState;
    ButtonNormalizeAnyText.Enabled      := EnableState;
    ButtonNormalizeDesignator.Enabled   := EnableState;
    ButtonProcess.Enabled               := EnableState;
    ButtonResetCenter.Enabled           := EnableState;
    ButtonResetOrigin.Enabled           := EnableState;
    ButtonResizeNoMove.Enabled          := EnableState;
    ButtonSaveConfig.Enabled            := EnableState;
    MMmilButton.Enabled                 := EnableState;

    if bForbidLocalSettings then CheckBoxLocalSettings.Enabled := False else CheckBoxLocalSettings.Enabled := EnableState;

    ButtonSaveConfig.Caption := '&SAVE';
end;


procedure TAssemblyTextPrepForm.AssemblyTextPrepFormCreate(Sender: TObject);
begin
    iDebugLevel := cDEBUGLEVEL;

    // set AD build flag
    if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;

    //ConfigFile_Read(ConfigFile_GetPath);
end;


procedure TAssemblyTextPrepForm.AssemblyTextPrepFormMouseEnter(Sender: TObject);
var
    status : Integer;
begin
    if (ASSY_LAYER_TOP = nil) and (GetAssyLayer(eTopLayer) <> nil) then InitialCheckGUI(status)
    else if (ASSY_LAYER_BOT = nil) and (GetAssyLayer(eBottomLayer) <> nil) then InitialCheckGUI(status);
end;


procedure TAssemblyTextPrepForm.AssemblyTextPrepFormShow(Sender : TObject);
begin

    LabelVersion.Caption := 'About v' + cScriptVersion;

    Application.HintHidePause := 12000; // extend hint show time

    rgCenterStrategy.Hint := 'Designator centering strategy. Affect resize fitting.' + sLineBreak +
        'Automatic: larger of component body outline or component pads outline' + sLineBreak +
        'Center of Bounds: center of component bounding rectangle.' + sLineBreak +
        'Component Body: centroid of component body outline.' + sLineBreak +
        'Centroid of Pads: center of rectangle that encloses component-layer pads.' + sLineBreak +
        'Footprint Origin: center on component origin. Resize using center of pads.';

    ConfigFile_Read(ConfigFile_GetPath); // moved to FormCreate

    UpdateConstants;

    // put after initial config read so we can load local settings file if it exists, but then force non-local save location
    if FolderIsReadOnly(ExtractFilePath(GetRunningScriptProjectName)) then
    begin
        bForbidLocalSettings := True;
        CheckBoxLocalSettings.Enabled := False;
        CheckBoxLocalSettings.Caption := 'Local Config Read-only';
    end
    else bForbidLocalSettings := False;

    //RadioGroupSelectionScope.Items[1] := 'Selecte&d Components (' + IntToStr(Board.SelectecObjectCount) + ')';
end; { TAssemblyTextPrepForm.AssemblyTextPrepFormShow }


procedure TAssemblyTextPrepForm.ButtonAddDesignatorsClick(Sender: TObject);
var
    i               : Integer;
    TextTemplate    : IPCB_Text;
    TargetList      : TInterfaceList;
    TextObj         : IPCB_Text;
    Comp            : IPCB_Component;
begin
    // Build a list of components we're going to process, before messing with selections
    TargetList := CreateObject(TInterfaceList);
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Comp := Board.SelectecObject[i];
        if Comp.ObjectId = eComponentObject then
        begin
            TextObj := GetDesignator(Comp); // get assy designator if it exists, else returns nil
            if TextObj = nil then TargetList.Add(Comp); // only add components without assy designator
        end;
    end;

    TextTemplate := GetAnyDesignator; // get an example assy designator from selection or board

    ClientDeSelectAll;

    PCBServer.PreProcess; // Start undo
    BeginHourGlass;
    for i := 0 to TargetList.Count - 1 do
    begin
        Comp := TargetList[i];
        if AddAssyTextToCompFromStyle(Comp, TextTemplate) then TextObj := GetDesignator(Comp);
        if TextObj <> nil then
        begin
            TextObj.Selected := True;
            TextObj.GraphicallyInvalidate;
        end;
    end;

    AdjustDesignatorPositions(0, 0, True, True);

    EndHourGlass;
    PCBServer.PostProcess;
end;


procedure TAssemblyTextPrepForm.ButtonAutoAdjustClick(Sender : TObject);
begin
    AdjustDesignatorPositions(0, 0, True, True);
end;


procedure TAssemblyTextPrepForm.ButtonCancelClick(Sender : TObject);
begin
    AssemblyTextPrepForm.Close;
end; { TAssemblyTextPrepForm.ButtonCancelClick }


procedure TAssemblyTextPrepForm.ButtonCheckSelectedClick(Sender : TObject);
var
    i   : Integer;
    Obj : IPCB_ObjectClass;
begin
    for i := Board.SelectecObjectCount - 1 downto 0 do
    begin
        Obj := Board.SelectecObject[i];
        //if Obj = nil then continue;
        if (Obj.ObjectId = eTextObject) and IsViolating(Obj) then continue;

        Obj.Selected := False;
        Obj.GraphicallyInvalidate;
    end;
    if Board.SelectecObjectCount > 0 then ShowWarning(IntToStr(Board.SelectecObjectCount) + ' violating text found');
end;


procedure TAssemblyTextPrepForm.ButtonNormalizeAnyTextClick(Sender : TObject);
begin
    NormalizeSelectedWithJustification;
end;


procedure TAssemblyTextPrepForm.ButtonNormalizeDesignatorClick(Sender : TObject);
begin
    SelectBoth;
    DeselectInvalidComponents;
    if not ((GetSelectedComponentCount > 0) and (GetSelectedAssyTextCount > 0)) then exit;
    SelectDesignators;
    NormalizeSelectedWithJustification;
end;


procedure TAssemblyTextPrepForm.ButtonProcessClick(Sender : TObject);
var
    Iter        : IPCB_BoardIterator;
    Comp        : IPCB_Component;
    CompCount   : Integer;
    mResponse   : Integer;
    status      : Integer;
begin
    BeginHourGlass;
    if rgSelectionScope.ItemIndex = 0 then
    begin
        ClientDeselectAll;
        SelectAllComponents;
        ClientZoomSelected;  // zoom on the selected components
        CompCount := Board.SelectecObjectCount;
        DeselectInvalidComponents;
    end
    else
    begin
        InitialCheckSelectBoth(status);
        if status <> 0 then exit;
        CompCount := Board.SelectecObjectCount;
        SelectBoth;
        SelectComponents;
        DeselectInvalidComponents;
    end;

    if Board.SelectecObjectCount < CompCount then
    begin
        EndHourGlass;
        if Board.SelectecObjectCount = 0 then
        begin
            if ConfirmNoYes('No valid components found. Do you want to select components missing .Designator special strings?') then mResponse := mrNo else mResponse := mrCancel;
        end
        else mResponse := ConfirmNoYesCancel(IntToStr(CompCount - Board.SelectecObjectCount) +
                ' components were deselected because they are missing .Designator special strings. Continue?' + sLineBreak +
                'YES to continue | NO to cancel operation and select invalid components.');
        case mResponse of
            mrCancel : exit;
            mrNo: begin
                ClientDeSelectAll;
                BeginHourGlass;
                SelectAllComponents;
                DeselectValidComponents;
                ClientZoomSelected;
                EndHourGlass;
                ReportFootprintNames('Footprints missing .Designator special strings:');
                exit;
            end;
        end;
    end;
    // process selected components using GUI settings
    AdjustDesignatorPositions(rgCenterStrategy.ItemIndex, rgOrientation.ItemIndex, CheckBoxResize.Checked, CheckBoxNormalize.Checked);

    EndHourGlass;

    ConfigFile_Write(ConfigFile_GetPath);
end; { TAssemblyTextPrepForm.ButtonProcessClick }


// reset position to automatic center, don't change rotation or resize
procedure TAssemblyTextPrepForm.ButtonResetCenterClick(Sender : TObject);
begin
    AdjustDesignatorPositions(0, -1, False, False);
end;


// reset position to origin, don't change rotation or resize
procedure TAssemblyTextPrepForm.ButtonResetOriginClick(Sender : TObject);
begin
    AdjustDesignatorPositions(4, -1, False, False);
end;


// resize to fit within bounds (in development)
procedure TAssemblyTextPrepForm.ButtonResizeNoMoveClick(Sender : TObject);
begin
    AdjustDesignatorPositions(-1, -1, True, False);
end;


procedure TAssemblyTextPrepForm.ButtonSaveConfigClick(Sender : TObject);
begin
    ConfigFile_Write(ConfigFile_GetPath);
    ButtonSaveConfig.Caption := 'SAVED';
end;


procedure TAssemblyTextPrepForm.ButtonSelectBothClick(Sender : TObject);
begin
    SelectBoth;
end;


procedure TAssemblyTextPrepForm.ButtonSelectComponentsClick(Sender : TObject);
begin
    SelectComponents;
end;


procedure TAssemblyTextPrepForm.ButtonSelectDesignatorsClick(Sender : TObject);
begin
    SelectDesignators;
end;


procedure TAssemblyTextPrepForm.ButtonSelectMissingClick(Sender: TObject);
begin
    ClientDeSelectAll;
    BeginHourGlass;
    SelectAllComponents;
    DeselectValidComponents;
    ClientZoomSelected;
    EndHourGlass;

    if Board.SelectecObjectCount = 0 then ShowInfo('No components missing .Designator strings.')
    else ReportFootprintNames('Footprints missing .Designator special strings:');
end;


procedure TAssemblyTextPrepForm.ButtonZoomSelectedClick(Sender : TObject);
begin
    ClientZoomSelected;  // zoom on the selected components
end;


procedure TAssemblyTextPrepForm.CheckBoxLocalSettingsClick(Sender : TObject);
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


procedure TAssemblyTextPrepForm.ConfigClick(Sender : TObject);
begin
    ButtonSaveConfig.Caption := '&SAVE';
end;


procedure TAssemblyTextPrepForm.InputValueChange(Sender : TObject);
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
end; { TAssemblyTextPrepForm.InputValueChange }


procedure TAssemblyTextPrepForm.LabelVersionClick(Sender : TObject);
begin
    About;
end;


procedure TAssemblyTextPrepForm.MMmilButtonClick(Sender : TObject);
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

    ButtonSaveConfig.Caption := '&SAVE';
end; { TAssemblyTextPrepForm.MMmilButtonClick }


{ programmatically, OnKeyPress fires before OnChange event and "catches" the key press }
procedure TAssemblyTextPrepForm.UserKeyPress(Sender : TObject; var Key : Char);
begin
    if (Ord(Key) = 13) then
    begin
        Key := #0; // eat the enter keypress to avoid beep
        AssemblyTextPrepForm.ActiveControl := ButtonSaveConfig; // jump to save button
    end;
end; { UserKeyPress }


procedure UpdateConstants(dummy : Boolean = False);
begin
    case MMmilButton.Caption of
        'mil':
        begin
            StringToCoordUnit(EditSizeMin.Text, TEXTHEIGHTMIN, eImperial);
            StringToCoordUnit(EditSizeNom.Text, TEXTHEIGHTNOM, eImperial);
            StringToCoordUnit(EditSizeMax.Text, TEXTHEIGHTMAX, eImperial);
            TEXTSTEPSIZE := TEXTSTEPIMPERIAL;
            STROKEWIDTHSTEP := MilsToCoord(cSTROKEWIDTHSTEP_MIL);
        end;
        'mm':
        begin
            StringToCoordUnit(EditSizeMin.Text, TEXTHEIGHTMIN, eMetric);
            StringToCoordUnit(EditSizeNom.Text, TEXTHEIGHTNOM, eMetric);
            StringToCoordUnit(EditSizeMax.Text, TEXTHEIGHTMAX, eMetric);
            TEXTSTEPSIZE := TEXTSTEPMETRIC;
            STROKEWIDTHSTEP := MMsToCoord(cSTROKEWIDTHSTEP_MM);
        end;
        else
        begin
            // invalid
        end;
    end;

    TEXTHEIGHTMIN := MAX(10000, TEXTHEIGHTMIN); // protect from invalid input
    TEXTHEIGHTNOM := MAX(10000, TEXTHEIGHTNOM); // protect from invalid input
    TEXTHEIGHTMAX := MAX(10000, TEXTHEIGHTMAX); // protect from invalid input

    ASPECT_RATIO_TOL := StrToFloat(EditAspectRatioTol.Text);
end;
