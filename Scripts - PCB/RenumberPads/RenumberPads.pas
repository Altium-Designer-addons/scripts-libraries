{==============================================================================}
{ ------------------------- Renumber Pads tool --------------------------------}
{------------------------------------------------------------------------------}
{-                                                                            -}
{-  Simple tool for renumbering of placed pads of a footprint in PCB library  -}
{-  or PCB document.                                                          -}
{-  You can select number index of first pad in window which appears after    -}
{-  running of the script. All other pads will be renumbered by Increment.    -}
{-  Script is terminateed by right click or Esc key during pad selection.     -}
{-                                                                            -}
{------------------------------------------------------------------------------}
{==============================================================================}

Var
    Board       : IPCB_Board;
    PadObject   : IPCB_Pad;

{..............................................................................}
procedure TRenumberPads.btnOKClick(Sender: TObject);
Var
    PadIndex     : Integer;
    PadIncrement : Integer;

Begin

     // Get requested first index number
    PadIndex := StrToInt(edFirstPadNumber.Text);
    PadIncrement := StrToInt(edPadIncrement.Text);
    RenumberPads.Visible := 0;

     // Ask user to select first pad object
    PadObject := Board.GetObjectAtCursor(MkSet(ePadObject), AllLayers,
                                      'Choose a pad');
    While PadObject <> 0 Do
    Begin
         // create undo for each pad index change
        PCBServer.PreProcess;
        PCBServer.SendMessageToRobots(PadObject.I_ObjectAddress, c_Broadcast,
                               PCBM_BeginModify , c_NoEventData);
         // change pad index
        PadObject.Name := PadIndex;
        PCBServer.SendMessageToRobots(PadObject.I_ObjectAddress, c_Broadcast,
                               PCBM_EndModify , c_NoEventData);
        PCBServer.PostProcess;

        PadIndex := PadIndex + PadIncrement;
         // ask user to select next pad in infinite loop
        PadObject := Board.GetObjectAtCursor(MkSet(ePadObject), AllLayers,
                                      'Choose a pad');
    End;

    Close;
End;



procedure TRenumberPads.btnCancelClick(Sender: TObject);
begin
     Close;
end;

procedure TRenumberPads.RenumberPadsCreate(Sender: TObject);
begin

     // Load current board
    If PCBServer = Nil Then
       Begin
            ShowMessage('Not a PCB or Footprint editor activated.');
            Exit;
       End;
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then
       Begin
            ShowMessage('Not a PCB or Footprint loaded.');
            Exit;
       End;

end;

