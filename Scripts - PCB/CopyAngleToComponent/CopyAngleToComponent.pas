{ see README.md for documentation }

const
    ScriptVersion = '1.3';
    ScriptTitle = 'CopyAngleToComponent';


procedure About;
var
    MsgText : string;
begin
    MsgText := '"' + ScriptTitle + '" script version ' + ScriptVersion + sLineBreak + sLineBreak +
        'Updated versions may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries';

    ShowInfo(MsgText, 'About');
end;


procedure MainRoutine(Verbose : Boolean = True);
var
    Board                          : IPCB_Board; // document board object
    Prim                           : IPCB_Primitive;
    Arc                            : IPCB_Arc;
    Track                          : IPCB_Track; // track object
    Component                      : IPCB_Component; // component object
    Angle, HalfAngle               : Double;  // Angle of selected segment
    Length                         : Double;  // Length of selected segment
    BoardUnits                     : String;    // Current unit string mm/mils


begin
    Board := PCBServer.GetCurrentPCBBoard;
    try
        if not Assigned(Board) then  // check of active document
        begin
            ShowError('The Current Document is not a PCB Document.');
            Exit;
        end;

        Prim := Board.GetObjectAtCursor(MkSet(eTrackObject, eArcObject), AllLayers, 'Select a Track or Arc object:');
        if not Assigned(Prim) then
        begin
            ShowWarning('No track or arc was selected.');
            Exit;
        end;

        while Assigned(Prim) do
        begin

            case Prim.ObjectId of
                eTrackObject:
                begin
                    Track := Prim;
                    Track.Selected := True;

                    if Track.X1 = Track.X2 then Angle := 90
                    else if Track.Y1 = Track.Y2 then Angle := 0
                    else
                    begin
                        Angle := ArcTan((Track.Y2-Track.Y1)/(Track.X2-Track.X1))/(Pi)*180;
                        if Track.X2 < Track.X1 then Angle := Angle + 180;
                    end;

                    Angle := Angle - 360.0 * Floor(Angle / 360.0);

                    Length := Power(( Power(Abs((Track.X2-Track.X1)),2) + Power(Abs((Track.Y2-Track.Y1)),2) ), 1/2 );

                    if Verbose then ShowInfo(Format('Track is %s long, angle is %.3f°', [ CoordUnitToString(Length, Board.DisplayUnit xor 1), Angle ]));

                end;
                eArcObject:
                begin
                    Arc := Prim;
                    Arc.Selected := True;

                    if Arc.EndAngle >= Arc.StartAngle then HalfAngle := (Arc.StartAngle + Arc.EndAngle) / 2 else HalfAngle := (Arc.StartAngle + Arc.EndAngle + 360) / 2;

                    Angle := HalfAngle + 90;
                    Angle := Angle - 360.0 * Floor(Angle / 360.0);

                    if Verbose then ShowInfo(Format('Angle tangent to arc midpoint is %.3f°', [ Angle ]));

                end;
            end;

            Component := Board.GetObjectAtCursor(MkSet(eComponentObject), AllLayers, Format('Select a component to rotate to %.3f°:', [ Angle ]));
            if not Assigned(Component) then
            begin
                if Verbose then ShowInfo('No component was selected.');
                Exit;
            end;

            PCBServer.PreProcess;
            try
                Component.BeginModify;
                Component.Rotation := Angle;
                Component.GraphicallyInvalidate;
                Component.EndModify;
                Board.NewUndo();    // Update the Undo System

            finally

            PCBServer.PostProcess;  // Finalize the systems in the PCB Editor.

            end;

            // deselect primitive before moving to another one
            Prim.Selected := False;

            // repeat for another track or arc
            Prim := Board.GetObjectAtCursor(MkSet(eTrackObject, eArcObject), AllLayers, 'Select next Track or Arc object:');
        end;

    finally
        //Full PCB system update
        Board.ViewManager_FullUpdate;
        // Refresh PCB screen
        Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
    end;
end;


procedure CopyAngleToComponentSilent;
begin
    MainRoutine(False);
end;


procedure CopyAngleToComponent;
begin
    MainRoutine(True);
end;
