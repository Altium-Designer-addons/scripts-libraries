{ Created by:  Ryan Rutledge }
{ For documentation see README.md }

var
    Board : IPCB_Board;
    IsAtLeastAD19 : Boolean;

const
    DebuggingEnabled = False;
    ScriptVersion = '1.8';
    ScriptTitle = 'SelectAssyDesignators';
    MinDesignatorSize = 100000; // minimum assembly designator size for resizing in Altium coordinate units (100000 = 10 mils)


procedure About; forward;
procedure AdjustDesignatorPositions(const Normalize : Boolean = False; const Ortho : Boolean = False; const Resize : Boolean = False); forward;
procedure BothInitialCheck(var status : Integer); forward;
procedure CompInitialCheck(var status : Integer); forward;
procedure DesignatorInitialCheck(var status : Integer); forward;
procedure DesignatorOrthoResize; forward;
procedure DesignatorResize; forward;
procedure GetBoundingBox(const Comp : IPCB_Component; out box_width : TCoord; out box_height : TCoord); forward;
function GetComponent(var Text : IPCB_Primitive) : IPCB_Component; forward;
function GetDesignator(var Comp : IPCB_Component) : IPCB_Primitive; forward;
procedure Inspect_IPCB_Text(var Text : IPCB_Text3; const MyLabel : string = ''); forward;
function NormalizeText(var Text : IPCB_Text) : Boolean; forward;
procedure NormalizeSelectedWithJustification; forward;
procedure ResetDesignatorPositions; forward;
procedure ResetDesignatorPositionsNorm; forward;
procedure ResetDesignatorPositionsNormOrtho; forward;
procedure ResetDesignatorPositionsOrtho; forward;
procedure ResizeText(var Text : IPCB_Text; const width : TCoord; const height : TCoord); forward;
function RotateTextToAngle(var Text : IPCB_Text; const Angle : Double; const Normalize : Boolean = False; const Ortho : Boolean = False) : Double; forward;
procedure SelectBoth; forward;
procedure SelectComponents; forward;
procedure SelectDesignators; forward;


{ About information }
procedure About;
var
    MsgText : string;
begin
    MsgText := '"' + ScriptTitle + '" script version ' + ScriptVersion + sLineBreak + sLineBreak +
        'Updated versions may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries';

    ShowInfo(MsgText, 'About');
end;


{ Main function to select both components and assembly designators for selected objects, then reset .Designator positions }
procedure AdjustDesignatorPositions(const Normalize : Boolean = False; const Ortho : Boolean = False; const Resize : Boolean = False);
var
    i           : Integer;
    status      : Integer;
    Comp        : IPCB_Component;
    Text        : IPCB_Text3;
    XOffset, YOffset        : TCoord;
    box_width, box_height   : TCoord;
    target_rotation         : Integer;
begin
    SelectBoth;

    SelectDesignators;

    if Board.SelectecObjectCount > 0 then Board.NewUndo else exit;

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

            Text.BeginModify;
            try
                if DebuggingEnabled then
                begin
                	if i < 3 then Inspect_IPCB_Text(Text, 'BEFORE');
                end;

                // resize text to fit desired width
                if Resize then
                begin
                    GetBoundingBox(Comp, box_width, box_height);
                    //ShowInfo('box_width: ' + IntToStr(box_width) + sLineBreak +
                                //'box_height: ' + IntToStr(box_height), 'Bounding Box Info');
                    if Ortho then ResizeText(Text, box_height, box_width) else ResizeText(Text, box_width, box_height);

                end;

                // need to turn on AdvanceSnapping (requires AD19+ but I'm not going to bother supporting old versions)
                Text.AdvanceSnapping := True;   // necessary for autoposition to work correctly (thanks, Brett Miller!)

                // set Text object's justification to center.
                Text.TTFInvertedTextJustify := eAutoPos_CenterCenter;

                // move text to component's position using `Text.SnapPointX` and `Text.SnapPointY` (goodbye Text.MoveToXY()!)
                Text.SnapPointX := Comp.x;
                Text.SnapPointY := Comp.y;

                // rotate text to match component
                RotateTextToAngle(Text, Comp.Rotation, Normalize, Ortho);

                if DebuggingEnabled then
                begin
                	if i < 3 then Inspect_IPCB_Text(Text, 'AFTER');
                end;

            finally
                Text.EndModify;
            end;

            Text.GraphicallyInvalidate;
        end;
    finally
        // Notify the pcbserver that all changes have been made (Stop undo)
        PCBServer.PostProcess;
    end;

    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

end;


{ Initial checks when a mix of components and .Designator strings are ostensibly selected }
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

    // Count components and .Designator special strings without deselecting them
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if (Prim1.ObjectId = eComponentObject) then ComponentCount := ComponentCount + 1;
        if ((Prim1.ObjectId = eTextObject) and (Prim1.UnderlyingString = '.Designator')) then DesignatorCount := DesignatorCount + 1;
    end;

    if ((ComponentCount < 1) and (DesignatorCount < 1)) then
    begin
        if DebuggingEnabled then
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
        exit;
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


{ Initial checks when components are ostensibly selected }
procedure CompInitialCheck(var status : Integer);
var
    i                   : Integer;
    Prim1               : IPCB_Primitive;
    ComponentCount      : Integer;
begin
    status := 0; // clear result status

    // Checks if current document is a PCB kind if not, exit.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

    ComponentCount := 0;

    // Count components without deselecting them
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if (Prim1.ObjectId = eComponentObject) then ComponentCount := ComponentCount + 1;
    end;

    if (ComponentCount < 1) then
    begin
        if DebuggingEnabled then
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


{ Initial checks when .Designator strings are ostensibly selected }
procedure DesignatorInitialCheck(var status : Integer);
var
    i               : Integer;
    Prim1           : IPCB_Primitive;
    DesignatorCount : Integer;
begin
    status := 0; // clear result status

    // Checks if current document is a PCB kind if not, exit.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

    DesignatorCount := 0;

    // Count text objects without deselecting them
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if ((Prim1.ObjectId = eTextObject) and (Prim1.UnderlyingString = '.Designator')) then DesignatorCount := DesignatorCount + 1;
    end;

    if (DesignatorCount < 1) then
    begin
        if DebuggingEnabled then
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


{ wrapper call to AdjustDesignatorPositions that will resize text and orient it orthogonal to component rotation}
procedure DesignatorOrthoResize;
begin
    AdjustDesignatorPositions(False, True, True);
end;


{ wrapper call to AdjustDesignatorPositions that will exactly match component rotation and resize text }
procedure DesignatorResize;
begin
    AdjustDesignatorPositions(False, False, True);
end;


{ Function to calculate the bounding box dimensions of a component's pads }
procedure GetBoundingBox(const Comp : IPCB_Component; out box_width : TCoord; out box_height : TCoord);
var
    Pad         : IPCB_Pad;
    PadBoundary : TCoordRect;
    PadIterator : IPCB_GroupIterator;
    minX, minY  : TCoord;
    maxX, maxY  : TCoord;
    rotation    : Integer;
begin
    // Initialize bounding box coordinates
    minX := 2147483647;
    minY := 2147483647;
    maxX := -2147483647;
    maxY := -2147483647;


    rotation := Comp.Rotation;  // save original component rotation
    Comp.Rotation := 0;         // rotate component to zero to calculate bounding box

    // Create iterator for pads only
    PadIterator := Comp.GroupIterator_Create;
    try
        PadIterator.AddFilter_ObjectSet(MkSet(ePadObject));

        Pad := PadIterator.FirstPCBObject;
        while (Pad <> nil) do
        begin
            PadBoundary := Pad.BoundingRectangleOnLayer(Comp.Layer);
            if PadBoundary.Left < minX then minX := PadBoundary.Left;
            if PadBoundary.Bottom < minY then minY := PadBoundary.Bottom;
            if PadBoundary.Right > maxX then maxX := PadBoundary.Right;
            if PadBoundary.Top > maxY then maxY := PadBoundary.Top;

            Pad := PadIterator.NextPCBObject;
        end;
    finally
        Comp.GroupIterator_Destroy(PadIterator);
    end;

    // Calculate the bounding box dimensions
    box_width  := maxX - minX;
    box_height := maxY - minY;

    // return to original rotation
    Comp.Rotation := rotation;

end;


{ Return the parent component of a text string }
function GetComponent(var Text : IPCB_Primitive) : IPCB_Component;
begin
    if (Text = nil) or (not Text.InComponent) then
        Result := nil
    else
        Result := Text.Component;
end;


{ Return the first .Designator special string associated with a component }
function GetDesignator(var Comp : IPCB_Component) : IPCB_Primitive;
var
    TextIterator    : IPCB_GroupIterator;
    Text            : IPCB_Primitive;
begin
    if Comp = nil then exit;

    Result := nil;

    // Create an iterator set to loop through all text primitives
    TextIterator := Comp.GroupIterator_Create;
    TextIterator.AddFilter_ObjectSet(MkSet(eTextObject));

    Text := TextIterator.FirstPCBObject;
    while (Text <> nil) do
    begin
        // Check if the Text is .Designator special string
        if Text.UnderlyingString = '.Designator' then
        begin
            Result := Text;
            Break;
        end;

        Text := TextIterator.NextPCBObject;
    end;

    Comp.GroupIterator_Destroy(TextIterator);

end;


{ IPCB_Text inspector for debugging }
procedure Inspect_IPCB_Text(var Text : IPCB_Text3; const MyLabel : string = '');
var
    AD19DebugStr : String;
begin
    AD19DebugStr := Format('%s : %s', ['AdvanceSnapping',  BoolToStr(Text.AdvanceSnapping, True)]) + sLineBreak +
            Format('%s : %s', ['SnapPointX',  IntToStr(Text.SnapPointX)]) + sLineBreak +
            Format('%s : %s', ['SnapPointY',  IntToStr(Text.SnapPointY)]);

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
                Format('%s : %s', ['MultilineTextHeight',  IntToStr(Text.MultilineTextHeight)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextResizeEnabled',  BoolToStr(Text.MultilineTextResizeEnabled, True)]) + sLineBreak +
                Format('%s : %s', ['MultilineTextWidth',  IntToStr(Text.MultilineTextWidth)]) + sLineBreak +
                Format('%s : %s', ['ObjectId',  IntToStr(Text.ObjectId)]) + sLineBreak +
                Format('%s : %s', ['ObjectIDString',  Text.ObjectIDString]) + sLineBreak +
                Format('%s : %s', ['PadCacheRobotFlag',  BoolToStr(Text.PadCacheRobotFlag, True)]) + sLineBreak +
                Format('%s : %s', ['Text',  Text.Text]) + sLineBreak +
                Format('%s : %s', ['TextKind',  IntToStr(Text.TextKind)]) + sLineBreak +
                Format('%s : %s', ['TTFInvertedTextJustify',  IntToStr(Text.TTFInvertedTextJustify)]) + sLineBreak +
                Format('%s : %s', ['UseTTFonts',  BoolToStr(Text.UseTTFonts, True)]) + sLineBreak +
                Format('%s : %s', ['Used',  BoolToStr(Text.Used, True)]) + sLineBreak +
                Format('%s : %s', ['UserRouted',  BoolToStr(Text.UserRouted, True)]) + sLineBreak +
                Format('%s : %s', ['ViewableObjectID',  IntToStr(Text.ViewableObjectID)]) + sLineBreak +
                Format('%s : %s', ['WordWrap',  BoolToStr(Text.WordWrap, True)]) + sLineBreak
                , 'IPCB_Text Info (partial)');
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


{ normalizes selected text objects to be right-reading while preserving their justification }
procedure NormalizeSelectedWithJustification;
var
    i       : Integer;
    Prim    : IPCB_Primitive;
begin
    // Checks if current document is a PCB kind if not, exit.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

    // set AD build flag
    if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;

    // Count components and .Designator special strings without deselecting them
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim := Board.SelectecObject[i];
        if Prim.ObjectId <> eTextObject then continue;

        NormalizeText(Prim);
    end;
end;


{ wrapper call to AdjustDesignatorPositions that will exactly match component rotation }
procedure ResetDesignatorPositions;
begin
    AdjustDesignatorPositions(False);
end;


{ wrapper call to AdjustDesignatorPositions that will match component rotation but normalized to be right-reading }
procedure ResetDesignatorPositionsNorm;
begin
    AdjustDesignatorPositions(True);
end;


{ wrapper call to AdjustDesignatorPositions that will orient .Designator string orthogonal to component but normalized to be right-reading }
procedure ResetDesignatorPositionsNormOrtho;
begin
    AdjustDesignatorPositions(True, True);
end;


{ wrapper call to AdjustDesignatorPositions that will orient .Designator string orthogonal to component }
procedure ResetDesignatorPositionsOrtho;
begin
    AdjustDesignatorPositions(False, True);
end;


{ procedure to scale text to approximately fit within the selected width and height }
procedure ResizeText(var Text : IPCB_Text; const width : TCoord; const height : TCoord);
var
    width_ratio     : double;
    TTF_ratio       : double;
    planned_height  : TCoord;
    TTF_min_height  : TCoord;
begin
    if Text.UseTTFonts then
    begin
        width_ratio := width / Text.TTFTextWidth;
        TTF_ratio := Text.TTFTextHeight / Text.Size;
        planned_height := Round((Text.Size * width_ratio) / 10000) * 10000;
        if (planned_height * TTF_ratio) > height then planned_height := ((height / TTF_ratio) div 10000) * 10000;
        TTF_min_height := Round(((MinDesignatorSize * 1.1) / TTF_ratio) div 1000) * 1000;   // round to 0.1mil instead for TTF, and adjust to approximately match stroke font height
        Text.Size := Max(TTF_min_height, planned_height);  // enforce a minimum height
    end
    else
    begin
        width_ratio := width / (Text.TTFTextWidth - Text.Width);
        planned_height := Round(Text.Size * width_ratio / 10000) * 10000;
        if planned_height > height then planned_height := ((height * 0.9) div 10000) * 10000;    // scale down assuming stroke width is a tenth of character height
        Text.Size := Max(MinDesignatorSize, planned_height);  // enforce a minimum height
        Text.Width := Text.Size div 10;  // set stroke width to a tenth of the character height
    end;

end;


{ rotates IPCB_Text object to specific angle, optionally normalizing it to be right-reading, optionally rotating 90° CW }
function RotateTextToAngle(var Text : IPCB_Text; const Angle : Double; const Normalize : Boolean = False; const Ortho : Boolean = False) : Double;
begin
    if Ortho then Angle := Angle + 270;

    // coerce angle to 0 ~ 360
    Angle := (Angle mod 360 + 360) mod 360; // technically an integer operation. TODO: make proper floating point mod throughout

    // rotate text to match Angle, based on how mirrored text reads from the flipside of the board
    case Text.GetState_Mirror of
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


{ Main function to select both components and assembly designators for selected objects }
procedure SelectBoth;
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
    BothInitialCheck(status);
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

    if not ((CompCount > 0) and (TextCount > 0)) then
    begin
        errText := 'No Components and/or Text objects selected. Script will not work properly.' +
                sLineBreak + sLineBreak +
                Format('CompCount: %d; TextCount: %d', [ CompCount, TextCount ]) +
                sLineBreak + sLineBreak +
                'Make sure both "Components" and "Texts" are enabled in selection filter';
        ShowError(errText);
    end;

    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

end;


{ Main function to select components for selected assembly designators }
procedure SelectComponents;
var
    i       : Integer;
    status  : Integer;
    Comp    : IPCB_Component;
    Text    : IPCB_Primitive;
begin
    DesignatorInitialCheck(status);
    if status <> 0 then exit;

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Text := Board.SelectecObject[i];
        Comp := GetComponent(Text);
        if Comp <> nil then Comp.SetState_Selected(True);
    end;

    // deselect assembly designators
    i := 0;
    while i < Board.SelectecObjectCount do
    begin
        Text := Board.SelectecObject[i];
        if (Text.ObjectId <> eComponentObject) then Text.SetState_Selected(False)
        else i := i + 1; // advance iterator if current object remains selected
    end;

    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

end;


{ Main function to select .Designator special strings for components }
procedure SelectDesignators;
var
    i       : Integer;
    status  : Integer;
    Comp    : IPCB_Component;
    Prim1   : IPCB_Primitive;
begin
    CompInitialCheck(status);
    if status <> 0 then exit;

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Comp := Board.SelectecObject[i];
        Prim1 := GetDesignator(Comp);
        if Prim1 <> nil then Prim1.SetState_Selected(True);
    end;

    // deselect components
    i := 0;
    while i < Board.SelectecObjectCount do
    begin
        Prim1 := Board.SelectecObject[i];
        if (Prim1.ObjectId <> eTextObject) then Prim1.SetState_Selected(False)
        else i := i + 1; // advance iterator if current object remains selected
    end;

    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

end;
