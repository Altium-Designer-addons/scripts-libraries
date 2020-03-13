
{==============================================================================}
{ ----------------------- Copy Angle to Component -----------------------------}
{------------------------------------------------------------------------------}
{-  -}
{==============================================================================}

Procedure CopyAngleToComponent;
var
    Board                          : IPCB_Board; // document board object
    Track                          : IPCB_Track; // track object
    Component                      : IPCB_Component; // component object
    Angle                          : Extended;
                                     // Angle of selected segment
    Length                         : Extended;
                                     // Length of selected segment
    BoardUnits                     : String;
                                     // Current unit string mm/mils

Begin
 Try
 Board := PCBServer.GetCurrentPCBBoard;
 If Not Assigned(Board) Then  // check of active document
    Begin
       ShowMessage('The Current Document is not a PCB Document.');
       Exit;
    End;
                        // User is asked to select track segment
 Track := Board.GetObjectAtCursor(MkSet(eTrackObject),AllLayers,
                                                    eEditAction_Select);
 If Not Assigned(Track) Then
    Begin
       ShowMessage('Not a track object was selected.');
       Exit;
    End;

 Track.Selected := True;          // The selected segment is used as a first two
 Angle := (ArcTan( (Track.Y2-Track.Y1)/(Track.X2-Track.X1))/(Pi)*180);
 Length := Power(( Power(Abs((Track.X2-Track.X1)),2) +
                   Power(Abs((Track.Y2-Track.Y1)),2) ), 1/2 ) / 10000;

 if ( (Board.DisplayUnit) = 0) then BoardUnits := 'mm'
                               else BoardUnits := 'mils';

 ShowMessage('Uhel je ' + FloatToStrF(Angle,ffFixed, 3, 1)
          +'° a delka ' + FloatToStrF(Length,ffFixed, 10, 3) + BoardUnits );

 Component := Board.GetObjectAtCursor(MkSet(eComponentObject),AllLayers,
                                                    eEditAction_Select);
 If Not Assigned(Component) Then
    Begin
       ShowMessage('Not a component was selected.');
       Exit;
    End;

 Try

 PCBServer.PreProcess;
 PCBServer.SendMessageToRobots(Component.I_ObjectAddress, c_Broadcast,
                               PCBM_BeginModify , c_NoEventData);
 Component.Rotation := Angle;
 Board.NewUndo();
 // Update the Undo System
 PCBServer.SendMessageToRobots(Component.I_ObjectAddress, c_Broadcast,
                               PCBM_EndModify , c_NoEventData);

 PCBServer.SendMessageToRobots(Board.I_ObjectAddress, c_Broadcast,
                          PCBM_BoardRegisteration, Component.I_ObjectAddress);

 Finally

 // Finalize the systems in the PCB Editor.
 PCBServer.PostProcess;

 End;

 Finally
 //Full PCB system update
 Board.ViewManager_FullUpdate;
 // Refresh PCB screen
 Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
 End;
End;


