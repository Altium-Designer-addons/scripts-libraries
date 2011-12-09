{******************************************************************************}
{ TweakAPDesignators Script - Taken from AdjustDesignators script              }
{   - written by Mattias Ericson, Omnisys Instruments AB                       }
{   - Modified by Tony Chilco, Yorkville Sound                                 }
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
{                                                                              }

{******************************************************************************}

Var
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
     if PCBServer.GetCurrentPCBBoard = Nil Then  Begin
       Exit;
     End;

     // Disables Online DRC during designator movement to improve speed
     PCBSystemOptions := PCBServer.SystemOptions;

     If PCBSystemOptions = Nil Then Exit;

     DRCSetting := PCBSystemOptions.DoOnlineDRC;
     PCBSystemOptions.DoOnlineDRC := false;
     try

        // Notify the pcbserver that we will make changes
        PCBServer.PreProcess;
        ComponentIteratorHandle := PCBServer.GetCurrentPCBBoard.BoardIterator_Create;
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

        while (Component <> Nil) Do
        begin

             //Show hidden designators?
             if UnHideDesignators = true then
                Component.NameOn := true;

            // Get the designator handle
            Designator := Component.Name;

            // notify that the pcb object is going to be modified
            PCBServer.SendMessageToRobots(Designator.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);

            // Set the move distance to DB units converted from mils or mm
            If TweakDesForm.MMmilButton.Caption = 'MM' then   DesignatorXmove := MMsToCoord(TweakDesForm.AdjustAmtEdit.Text)
            else DesignatorXmove := MilsToCoord(TweakDesForm.AdjustAmtEdit.Text);


           tc_AutoPos:= Component.GetState_NameAutoPos; // Get current AP state

            // Set AP to manual
            Component.ChangeNameAutoposition  :=   eAutoPos_Manual;

            // Move designator depending on current AP setting
            Case tc_AutoPos of
              eAutoPos_Manual:;
              eAutoPos_TopLeft:Designator.Ylocation := Designator.Ylocation - DesignatorXmove;
              eAutoPos_CenterLeft:Designator.xlocation := Designator.xlocation + DesignatorXmove;
              eAutoPos_BottomLeft:Designator.Ylocation := Designator.Ylocation + DesignatorXmove;
              eAutoPos_TopCenter:Designator.Ylocation := Designator.Ylocation - DesignatorXmove;
              eAutoPos_BottomCenter:Designator.Ylocation := Designator.Ylocation + DesignatorXmove;
              eAutoPos_TopRight:Designator.Ylocation := Designator.Ylocation - DesignatorXmove;
              eAutoPos_CenterRight:Designator.xlocation := Designator.xlocation - DesignatorXmove;
              eAutoPos_BottomRight:Designator.Ylocation := Designator.Ylocation + DesignatorXmove;
              eAutoPos_CenterCenter:;
            End; {case tc_AutoPos}

           // notify that the pcb object is modified
            PCBServer.SendMessageToRobots(Designator.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);

          // Get the next component handle
            Component := ComponentIteratorHandle.NextPCBObject;
            If TweakDesForm.SelectedCheckbox.Checked Then  while (Component <> Nil) Do
              If NOT Component.Selected Then Component := ComponentIteratorHandle.NextPCBObject
              else break;  // Find the next selected comp if select only checked


        // Notify the pcbserver that all changes have been made
        PCBServer.PostProcess;

     End;
        //Refresh the screen
        Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

        // Destroy the component handle
        PCBServer.GetCurrentPCBBoard.BoardIterator_Destroy(ComponentIteratorHandle);

    finally
           // Restore DRC setting
           PCBSystemOptions.DoOnlineDRC :=  DRCSetting;
    end;
end;

{..............................................................................}

