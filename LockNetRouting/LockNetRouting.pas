{==============================================================================}
{ ------------------- Lock/Unlock Net Routing on Board  -----------------------}
{------------------------------------------------------------------------------}
{-                                                                            -}
{-  This script locks/unlocks whole track which belongs to user selected      -}
{-  track. Lock or unlock function is selected by choosing startup procedure. -}
{-  You can lock/unlock multiple nets by clicking one by one. Script is       -}
{-  terminated by right click during selection or by selecting No Net track.  -}
{-                                                                            -}
{-  It is good to activate "Protect Locked Objects" in DXP->Preferences->     -}
{-  PCB editor->General to get real protection for routing.                   -}                  
{-                                                                            -}
{-  Startup procedure  :::  LockNet  :::  - it locks net                      -}
{-                     ::: UnLockNet :::  - it unlocks net                    -}
{-                                                                            -}
{-  CUSTOMIZATION can by done by direct edditing of CONSTANTS                 -}
{-                                                                            -}
{-  LockConnectedComponents - script locks/unlocks components connected by    -}
{-           at least one pad to the processed net when set to true           -}
{-                                                                            -}
{-     ---  Please report script issues to Petr.Tosovsky@edatools.cz   ---    -}
{-                                                                            -}
{-  Author: Petr Tosovsky, Retry, www.edatools.cz                             -}
{-                                                                            -}
{==============================================================================}
var
    Board                          : IPCB_Board; // document board object
    Iterator                       : IPCB_BoardIterator; // object iterator
    RefTrack;                      // reference track object selected by user
    Track;                         // temp track object
    Component                      : IPCB_Component; // temp component object
    ComponentIterator              : IPCB_GroupIterator; // comp pad searching
    Pad                            : IPCB_Pad;       // temp pad object
    ConnectedComponent             : Boolean;
{==============================================================================}
const
     LockConnectedComponents = True;

Procedure LockSelectedNet(Lock : Boolean);
Begin
 Try
 Board := PCBServer.GetCurrentPCBBoard;
 If Not Assigned(Board) Then  // check of active document
    Begin
       ShowMessage('The Current Document is not a PCB Document.');
       Exit;
    End;
                        // User is asked to select track segment
 RefTrack := Board.GetObjectAtCursor(MkSet(eTrackObject, eArcObject, eViaObject)
                               ,AllLayers, 'Choose track segment');
 If Not Assigned(RefTrack) Then
    Begin
       ShowMessage('Not a track object was selected.');
       Exit;
    End;

 While ((RefTrack <> Nil) AND (RefTrack.Net <> Nil)) Do
 Begin

    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject, eViaObject));
    Iterator.AddFilter_LayerSet (AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    PCBServer.PreProcess;
    Track := Iterator.FirstPCBObject;  // find track, arc, via object on board

    While (Track <> Nil) Do
    Begin
      If (Track.Net = RefTrack.Net) Then   // check if the track on selected net
       Begin
         Track.BeginModify;
         Track.Moveable := Not(Lock);      // lock/unlock object
         Track.EndModify;
       End;
      Track := Iterator.NextPCBObject;  // next object in the iteration
    End;

    Board.BoardIterator_Destroy(Iterator);

    If (LockConnectedComponents = True) Then    // customized component
    Begin                                       // locking/unlocking
    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iterator.AddFilter_LayerSet (AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    Component := Iterator.FirstPCBObject;

    While (Component <> Nil) Do     // search for connected components
    Begin
        ComponentIterator := Component.GroupIterator_Create;
        ComponentIterator.AddFilter_ObjectSet(MkSet(ePadObject));
        Pad := ComponentIterator.FirstPCBObject;
        While (Pad <> Nil) Do
        Begin
            ConnectedComponent := False;     // check all components pads
            If (Pad.Net = RefTrack.Net) Then
               Begin
                    ConnectedComponent := True;
                    Pad.Moveable := Not(Lock);
               End;
            Pad := ComponentIterator.NextPCBObject;
        End;
        If (ConnectedComponent = True) Then
           Begin
                Component.BeginModify;
                Component.Moveable := Not(Lock); // lock/unlock component
                Component.EndModify;
           End;
        Component := Iterator.NextPCBObject;
    End;

    Board.BoardIterator_Destroy(Iterator);
    End;

    Board.NewUndo();
    PCBServer.PostProcess;            // finish board change with Undo option
    RefTrack := Board.GetObjectAtCursor(MkSet(eTrackObject, eArcObject, eViaObject)
                               ,AllLayers, 'Choose track segment');
 End;

 Finally

   Board.ViewManager_FullUpdate;

   // Refresh PCB screen
   Client.CommandLauncher.LaunchCommand('PCB:Zoom', 'Action=Redraw' , 255,
                                                    Client.CurrentView);
 End;
End;
{==============================================================================}
Procedure UnLockNet;
Begin
   LockSelectedNet(False);
End;
{==============================================================================}
Procedure LockNet;
Begin
   LockSelectedNet(True);
End;
{==============================================================================}


