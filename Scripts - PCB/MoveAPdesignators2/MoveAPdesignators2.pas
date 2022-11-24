{******************************************************************************}
{ TweakAPDesignators Script - Taken from AdjustDesignators script              }
{   - written by Mattias Ericson, Omnisys Instruments AB                       }
{   - Modified by Tony Chilco, Yorkville Sound                                 }
{   - Modified by Ryan Rutledge, Pacific Bioscience Laboratories Products, Inc.}
{                                                                              }
{ Written for Altium Designer 10                                               }
{                                                                              }
{ This script will change auto-positioned designators to manual and move them  }
{ by a user-defined amount. Will operate on all or selected components  }
{ The direction of the movement depends on the current autoposition status     }
{                                                                              }
{ Note: Only works in millimeters at the moment.                               }
{                                                                              }
{ Change log:                                                                  }
{ Ver 1.0 - Initial release                                                    }
{ Ver 1.1 - Corrected post process position so undo will work                  }
{ Ver 1.2 - fixed bug with mirrored designators moving the wrong direction     }
{                                                                              }

{******************************************************************************}

Var
     Board                          : IPCB_Board;
     UnHideDesignators, AbortScript : Boolean;

{..............................................................................}
Procedure TweakDesignators;
Var
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;
    Designator              : IPCB_Text;
    PCBSystemOptions        : IPCB_SystemOptions;
    DRCSetting              : boolean;
    tc_AutoPos              : TTextAutoposition; // Current AP setting
    DesignatorXmove         : Integer;           // Distance to move
begin
     // Verify that the document is a PcbDoc
     //if PCBServer.GetCurrentPCBBoard = Nil Then  Begin
     //  Exit;
     //End;

    // Checks if current document is a PCB kind if not, exit.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

     // Disables Online DRC during designator movement to improve speed
     PCBSystemOptions := PCBServer.SystemOptions;

     If PCBSystemOptions = Nil Then Exit;

     DRCSetting := PCBSystemOptions.DoOnlineDRC;
     PCBSystemOptions.DoOnlineDRC := false;
     try

        // Notify the pcbserver that we will make changes (Start undo)
        PCBServer.PreProcess;
        ComponentIteratorHandle := Board.BoardIterator_Create;
        ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
        ComponentIteratorHandle.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
        ComponentIteratorHandle.AddFilter_Method(eProcessAll);

        AbortScript:= False;
        TweakDesForm.ShowModal;      // Show the settings dialogue
        If AbortScript Then Exit;

        IF TweakDesForm.UnHideDesignatorsCheckBox.Checked Then UnHideDesignators:= True
        else UnHideDesignators:= False;

        Component := ComponentIteratorHandle.FirstPCBObject;

        If TweakDesForm.SelectedCheckbox.Checked Then  while (Component <> Nil) Do
          If NOT Component.Selected Then Component := ComponentIteratorHandle.NextPCBObject
          else break;    // Find the first selected comp if select only checked

        // Set the move distance to DB units converted from mils or mm
        If TweakDesForm.MMmilButton.Caption = 'MM' then   DesignatorXmove := MMsToCoord(TweakDesForm.AdjustAmtEdit.Text)
        else DesignatorXmove := MilsToCoord(TweakDesForm.AdjustAmtEdit.Text);

        while (Component <> Nil) Do
        begin
            Component.BeginModify;

             //Show hidden designators?
             if UnHideDesignators = true then
                Component.NameOn := true;

            tc_AutoPos:= Component.GetState_NameAutoPos; // Get current AP state

            // Set AP to manual
            Component.SetState_NameAutoPos(eAutoPos_Manual);

            // Get the designator handle
            Designator := Component.Name;

            // notify that the pcb object is going to be modified
            //PCBServer.SendMessageToRobots(Designator.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);
            Component.Name.BeginModify;

            Case Designator.GetState_Mirror of
                true:       // If designator is mirrored, we will assume that X-axis movement should be reversed
                    // Move designator depending on current AP setting
                    Case tc_AutoPos of
                      eAutoPos_Manual:;
                      eAutoPos_TopLeft:
                      begin
                          Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation - DesignatorXmove);
                          Designator.TTFInvertedTextJustify := eAutoPos_BottomRight;
                      end;
                      eAutoPos_CenterLeft:
                      begin
                          Designator.MoveToXY(Designator.Xlocation + DesignatorXmove, Designator.Ylocation);
                          Designator.TTFInvertedTextJustify := eAutoPos_CenterLeft;
                      end;
                      eAutoPos_BottomLeft:
                      begin
                          Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation + DesignatorXmove);
                          Designator.TTFInvertedTextJustify := eAutoPos_TopRight;
                      end;
                      eAutoPos_TopCenter:
                      begin
                          Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation - DesignatorXmove);
                          Designator.TTFInvertedTextJustify := eAutoPos_BottomCenter;
                      end;
                      eAutoPos_BottomCenter:
                      begin
                          Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation + DesignatorXmove);
                          Designator.TTFInvertedTextJustify := eAutoPos_TopCenter;
                      end;
                      eAutoPos_TopRight:
                      begin
                          Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation - DesignatorXmove);
                          Designator.TTFInvertedTextJustify := eAutoPos_BottomLeft;
                      end;
                      eAutoPos_CenterRight:
                      begin
                          Designator.MoveToXY(Designator.Xlocation - DesignatorXmove, Designator.Ylocation);
                          Designator.TTFInvertedTextJustify := eAutoPos_CenterRight;
                      end;
                      eAutoPos_BottomRight:
                      begin
                          Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation + DesignatorXmove);
                          Designator.TTFInvertedTextJustify := eAutoPos_BottomLeft;
                      end;
                      eAutoPos_CenterCenter:Designator.TTFInvertedTextJustify := eAutoPos_CenterCenter;
                    End; {case tc_AutoPos}
                false:
                    // Move designator depending on current AP setting
                    Case tc_AutoPos of
                        eAutoPos_Manual:;
                        eAutoPos_TopLeft:
                        begin
                            Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation - DesignatorXmove);
                            Designator.TTFInvertedTextJustify := eAutoPos_BottomRight;
                        end;
                        eAutoPos_CenterLeft:
                        begin
                            Designator.MoveToXY(Designator.Xlocation + DesignatorXmove, Designator.Ylocation);
                            Designator.TTFInvertedTextJustify := eAutoPos_CenterRight;
                        end;
                        eAutoPos_BottomLeft:
                        begin
                            Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation + DesignatorXmove);
                            Designator.TTFInvertedTextJustify := eAutoPos_TopRight;
                        end;
                        eAutoPos_TopCenter:
                        begin
                            Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation - DesignatorXmove);
                            Designator.TTFInvertedTextJustify := eAutoPos_BottomCenter;
                        end;
                        eAutoPos_BottomCenter:
                        begin
                            Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation + DesignatorXmove);
                            Designator.TTFInvertedTextJustify := eAutoPos_TopCenter;
                        end;
                        eAutoPos_TopRight:
                        begin
                            Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation - DesignatorXmove);
                            Designator.TTFInvertedTextJustify := eAutoPos_BottomLeft;
                        end;
                        eAutoPos_CenterRight:
                        begin
                            Designator.MoveToXY(Designator.Xlocation - DesignatorXmove, Designator.Ylocation);
                            Designator.TTFInvertedTextJustify := eAutoPos_CenterLeft;
                        end;
                        eAutoPos_BottomRight:
                        begin
                            Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation + DesignatorXmove);
                            Designator.TTFInvertedTextJustify := eAutoPos_TopLeft;
                        end;
                        eAutoPos_CenterCenter:Designator.TTFInvertedTextJustify := eAutoPos_CenterCenter;
                    End; {case tc_AutoPos}
            End; {case Designator.Layer}

            // notify that the pcb object is modified
            // PCBServer.SendMessageToRobots(Designator.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);
            Component.Name.EndModify;
            Designator.GraphicallyInvalidate;
            Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Designator.I_ObjectAddress);

            Component.EndModify;

          // Get the next component handle
            Component := ComponentIteratorHandle.NextPCBObject;
            If TweakDesForm.SelectedCheckbox.Checked Then  while (Component <> Nil) Do
              If NOT Component.Selected Then Component := ComponentIteratorHandle.NextPCBObject
              else break;  // Find the next selected comp if select only checked


     End;

        // Notify the pcbserver that all changes have been made (Stop undo)
        PCBServer.PostProcess;

        //Refresh the screen (not needed with .GraphicallyInvalidate)
        // Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

        // Destroy the component handle
        Board.BoardIterator_Destroy(ComponentIteratorHandle);

    finally
           // Restore DRC setting
           PCBSystemOptions.DoOnlineDRC :=  DRCSetting;
    end;
end;


procedure TTweakDesForm.OKButtonClick(Sender: TObject);
begin
  TweakDesForm.Close;
end;


procedure TTweakDesForm.MMmilButtonClick(Sender: TObject);
begin
  If MMmilButton.Caption = 'MM' then Begin
    MMmilButton.Caption:= 'Mil';
    AdjustAmtEdit.Text:=Trunc(0.5 + 10000 * AdjustAmtEdit.Text / 25.4)/10;
  End else Begin
    MMmilButton.Caption:= 'MM';
    AdjustAmtEdit.Text:= Trunc(0.5 + 100 * AdjustAmtEdit.Text * 0.0254)/100;
  End;
  AdjustAmtEdit.Update;
end;


procedure TTweakDesForm.CancelButtonClick(Sender: TObject);
begin
  TweakDesForm.Close;
  AbortScript:= True;
end;

