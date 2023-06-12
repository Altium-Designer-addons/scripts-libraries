
{==============================================================================}
{ ----------------------- Copy Angle to Component -----------------------------}
{------------------------------------------------------------------------------}
{  updated 2023-06-12 by Ryan Rutledge - some translation and UX tweaking      }
{      - don't show angles as negative numbers                                 }
{      - added support for arc midpoint angles                                 }
{==============================================================================}

const
    ScriptVersion = '1.2';
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


Procedure CopyAngleToComponent;
var
    Board                          : IPCB_Board; // document board object
    Prim                           : IPCB_Primitive;
    Arc                            : IPCB_Arc;
    Track                          : IPCB_Track; // track object
    Component                      : IPCB_Component; // component object
    Angle, HalfAngle               : Double;  // Angle of selected segment
    Length                         : Double;  // Length of selected segment
    BoardUnits                     : String;    // Current unit string mm/mils


Begin
    Board := PCBServer.GetCurrentPCBBoard;
    Try
        If Not Assigned(Board) Then  // check of active document
            Begin
                ShowError('The Current Document is not a PCB Document.');
                Exit;
            End;

        Prim := Board.GetObjectAtCursor(MkSet(eTrackObject, eArcObject), AllLayers, 'Select a Track or Arc object:');
        If Not Assigned(Prim) Then
            Begin
                ShowWarning('No track or arc was selected.');
                Exit;
            End;

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

                ShowInfo(Format('Track is %s long, angle is %.3f°', [ CoordUnitToString(Length, Board.DisplayUnit xor 1), Angle ]));

            end;
            eArcObject:
            begin
                Arc := Prim;
                Arc.Selected := True;

                if Arc.EndAngle >= Arc.StartAngle then HalfAngle := (Arc.StartAngle + Arc.EndAngle) / 2 else HalfAngle := (Arc.StartAngle + Arc.EndAngle + 360) / 2;

                Angle := HalfAngle + 90;
                Angle := Angle - 360.0 * Floor(Angle / 360.0);

                ShowInfo(Format('Angle tangent to arc midpoint is %.3f°', [ Angle ]));

            end;
        end;

        Component := Board.GetObjectAtCursor(MkSet(eComponentObject), AllLayers, Format('Select a component to rotate to %.3f°:', [ Angle ]));
        If Not Assigned(Component) Then
            Begin
                ShowInfo('No component was selected.');
                Exit;
            End;

        PCBServer.PreProcess;
        Try
            Component.BeginModify;
            Component.Rotation := Angle;
            Component.GraphicallyInvalidate;
            Component.EndModify;
            Board.NewUndo();    // Update the Undo System

        Finally

        PCBServer.PostProcess;  // Finalize the systems in the PCB Editor.

        End;

    Finally
        //Full PCB system update
        Board.ViewManager_FullUpdate;
        // Refresh PCB screen
        Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
    End;
End;
