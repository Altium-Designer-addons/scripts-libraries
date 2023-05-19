{ Created by:  Ryan Rutledge }
{ For documentation see README.md }

var
    Board             : IPCB_Board;

const
    DebuggingEnabled = False;
    ScriptVersion = '1.0';
    ScriptTitle = 'SelectAssyDesignators';


procedure AdjustDesignatorPositions(const Normalize : Boolean); forward;
procedure BothInitialCheck(var status : Integer); forward;
procedure CompInitialCheck(var status : Integer); forward;
procedure DesignatorInitialCheck(var status : Integer); forward;
function GetComponent(var Text : IPCB_Primitive) : IPCB_Component; forward;
function GetDesignator(var Comp : IPCB_Component) : IPCB_Primitive; forward;
procedure ResetDesignatorPositions; forward;
procedure ResetDesignatorPositionsNorm; forward;
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

    ShowMessage(MsgText);
end;


{ Main function to select both components and assembly designators for selected objects, then reset .Designator positions }
procedure AdjustDesignatorPositions(const Normalize : Boolean);
var
    i           : Integer;
    status      : Integer;
    Comp        : IPCB_Component;
    Text        : IPCB_Text;
    XOffset, YOffset    : TCoord;
begin
    SelectBoth;

    SelectDesignators;

    if Board.SelectecObjectCount > 0 then Board.NewUndo else exit;

    // Notify the pcbserver that we will make changes (Start undo)
    PCBServer.PreProcess;

    // at this point we have a selection of text objects
    // For each Text object selected:
    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        // use GetComponent to get the owner component followed by its XY position and rotation
        Text := Board.SelectecObject[i];
        Comp := GetComponent(Text);

        if Comp = nil then continue;  // skip if component not found

        Text.BeginModify;

        // set Text object's justification to center
        Text.TTFInvertedTextJustify := eAutoPos_CenterCenter;

        // IPCB_Text MoveToXY method doesn't account for justification settings, so need to calculate offsets (based on 0 rotation)
        if Text.UseTTFonts then XOffset := Text.TTFTextWidth div 2 else XOffset := Text.TTFTextWidth div 2 - Text.Width;
        if Text.UseTTFonts then YOffset := Text.TTFTextHeight div 2 else YOffset := Text.TTFTextHeight div 2 - Text.Width;

        Text.Rotation := 0; // set text rotation to 0 to fit calculated offsets rather than trying to calculate all the permutations based on rotation

        // Needed to "refresh" Text rotation before calling MoveToXY method (Text.GraphicallyInvalidate didn't work)
        Text.EndModify;
        Text.BeginModify;

        // move text to component's position using `Text.MoveToXY()` then rotate to match component
        case Text.GetState_Mirror of
            True :
                begin
                    Text.MoveToXY(Comp.x + XOffset, Comp.y - YOffset);
                    if Normalize and (Comp.Rotation >= 90) and (Comp.Rotation < 270) then
                        Text.Rotation := (Comp.Rotation + 180) mod 360
                    else
                        Text.Rotation := Comp.Rotation;
                end;
            False :
                begin
                    Text.MoveToXY(Comp.x - XOffset, Comp.y - YOffset);
                    if Normalize and (Comp.Rotation > 90) and (Comp.Rotation <= 270) then
                        Text.Rotation := (Comp.Rotation + 180) mod 360
                    else
                        Text.Rotation := Comp.Rotation;
                end;
        end;

        Text.EndModify;

        Text.GraphicallyInvalidate;
    end;

    // Notify the pcbserver that all changes have been made (Stop undo)
    PCBServer.PostProcess;

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
            Showmessage('Select at least 1 component or designator special string.' + sLineBreak +
                        '(designator string is ".Designator")' + sLineBreak + sLineBreak +
                        '-- Debugging Info --' + sLineBreak +
                        'ComponentCount: ' + IntToStr(ComponentCount) + sLineBreak +
                        'DesignatorCount: ' + IntToStr(DesignatorCount) + sLineBreak +
                        'Selected Object Count: ' + IntToStr(Board.SelectecObjectCount));
        end
        else Showmessage('Select at least 1 component or designator special string.');

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
            Showmessage('Select at least 1 component.' + sLineBreak + sLineBreak +
                        '-- Debugging Info --' + sLineBreak +
                        'ComponentCount: ' + IntToStr(ComponentCount) + sLineBreak +
                        'Selected Object Count: ' + IntToStr(Board.SelectecObjectCount));
        end
        else Showmessage('Select at least 1 component.');

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
            Showmessage('Select at least 1 designator special string.' + sLineBreak +
                        '(designator string is ".Designator")' + sLineBreak + sLineBreak +
                        '-- Debugging Info --' + sLineBreak +
                        'DesignatorCount: ' + IntToStr(DesignatorCount) + sLineBreak +
                        'Selected Object Count: ' + IntToStr(Board.SelectecObjectCount));
        end
        else Showmessage('Select at least 1 assembly designator' + sLineBreak + '(string contains ".Designator")');

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


{ Main function to select both components and assembly designators for selected objects }
procedure SelectBoth;
var
    i       : Integer;
    status  : Integer;
    Comp    : IPCB_Component;
    Text    : IPCB_Text;
    Prim1   : IPCB_Primitive;
begin
    BothInitialCheck(status);
    if status <> 0 then exit;

    for i := 0 to Board.SelectecObjectCount - 1 do
    begin
        Prim1 := Board.SelectecObject[i];
        if Prim1.ObjectId = eTextObject then
        begin
            Comp := GetComponent(Prim1);
            if Comp <> nil then Comp.SetState_Selected(True);
        end
        else if Prim1.ObjectId = eComponentObject then
        begin
            Text := GetDesignator(Prim1);
            if Text <> nil then Text.SetState_Selected(True);
        end;
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
