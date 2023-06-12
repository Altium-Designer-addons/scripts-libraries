
{==============================================================================}
{ ----------------------- Copy Angle to Component -----------------------------}
{------------------------------------------------------------------------------}
{  updated 2023-06-12 by Ryan Rutledge - some translation and UX tweaking      }
{      - don't show angles as negative numbers                                 }
{==============================================================================}

const
    ScriptVersion = '1.1';
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
    Track                          : IPCB_Track; // track object
    Component                      : IPCB_Component; // component object
    Angle                          : Double;  // Angle of selected segment
    Length                         : Double;  // Length of selected segment
    BoardUnits                     : String;    // Current unit string mm/mils


Begin
    Board := PCBServer.GetCurrentPCBBoard;
    Try
        If Not Assigned(Board) Then  // check of active document
            Begin
                ShowMessage('The Current Document is not a PCB Document.');
                Exit;
            End;

        Track := Board.GetObjectAtCursor(MkSet(eTrackObject), AllLayers, 'Select a Track object:');
        If Not Assigned(Track) Then
            Begin
                ShowMessage('No track was selected.');
                Exit;
            End;

        Track.Selected := True;          // The selected segment is used as a first two
        Angle := (ArcTan( (Track.Y2-Track.Y1)/(Track.X2-Track.X1))/(Pi)*180);
        Angle := Angle - 360.0 * Floor(Angle / 360.0);

        Length := Power(( Power(Abs((Track.X2-Track.X1)),2) +
                       Power(Abs((Track.Y2-Track.Y1)),2) ), 1/2 ) / 10000;

        if ( (Board.DisplayUnit) = 0) then BoardUnits := 'mm'
                                   else BoardUnits := 'mils';

        ShowInfo(Format('Track is %.3f%s long, angle is %.3f°', [ Length, BoardUnits, Angle ]));

        Component := Board.GetObjectAtCursor(MkSet(eComponentObject), AllLayers, Format('Select a component to rotate to %.3f°:', [ Angle ]));
        If Not Assigned(Component) Then
            Begin
                ShowMessage('No component was selected.');
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
