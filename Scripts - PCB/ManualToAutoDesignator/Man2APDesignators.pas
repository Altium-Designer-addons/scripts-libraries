{******************************************************************************}
{ Man2APDesignators Script - Taken from AdjustDesignators script              }
{   - written by Mattias Ericson, Omnisys Instruments AB                       }
{   - Modified by Tony Chilco, Yorkville Sound                                 }
{                                                                              }
{ Written for Altium Designer 10                                               }
{                                                                              }
{ This script will change manual designators to auto-positioned Will operate   }
{ on all or selected components  }
{                                                                              }
{                                                                              }
{ Change log:                                                                  }
{ Ver 1.0 - Initial release                                                    }
{                                                                              }

{******************************************************************************}

Var
     UnHideDesignators, AbortScript : Boolean;

{..............................................................................}
Procedure Man2AutoDesignators;
Var
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;
    Designator              : IPCB_Text;
    PCBSystemOptions        : IPCB_SystemOptions;
    DRCSetting              : boolean;
    tc_AutoPos              : TTextAutoposition;       // Current AP setting
    DesignatorXoffset, DesignatorYoffset : Integer;    // Distance from origin
    AutoX, AutoY, AutoSet   : Integer;
    SomeText : String;
    CenterX, CenterY        : Long;
    CenterDX, CenterDY      : Long;
    R                       : TCoordRect;
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
        APDesForm.ShowModal;      // Show the settings dialogue
        If AbortScript Then Exit;

        IF APDesForm.UnHideDesignatorsCheckBox.Checked Then UnHideDesignators:= True
        else UnHideDesignators:= False;

        Component := ComponentIteratorHandle.FirstPCBObject;

        If APDesForm.SelectedCheckbox.Checked Then  while (Component <> Nil) Do
          If NOT Component.Selected Then Component := ComponentIteratorHandle.NextPCBObject
          else break;    // Find the first selected comp if select only checked

        while (Component <> Nil) Do
        begin

             //Show hidden designators?
             if UnHideDesignators = true then
                Component.NameOn := true;

            // Get the designator handle
            Designator := Component.Name;

            tc_AutoPos:= Component.GetState_NameAutoPos; // Get current AP state
            If tc_AutoPos = eAutoPos_Manual Then Begin

            // Determine Designator position relative to the part origin
            R := Component.BoundingRectangleNoNameComment;
            CenterX:=  (R.right - R.left)/2 + R.left;
            CenterY:=  (R.top - R.bottom)/2 + R.bottom;

            R := Designator.BoundingRectangle;
            CenterDX:=  (R.right - R.left)/2 + R.left;
            CenterDY:=  (R.top - R.bottom)/2 + R.bottom;

            DesignatorXoffset:= CenterX - CenterDX;
            DesignatorYoffset:= CenterY - CenterDY;

            (*ShowMessage(Designator.Text);
            ShowMessage(Designator.Size);
            ShowMessage(DesignatorXoffset);
            ShowMessage(DesignatorYoffset);
              *)
            If Abs(DesignatorXoffset)<Designator.Size Then AutoX:= 20
              else If DesignatorXoffset>0 Then AutoX:= 10
                else AutoX:= 30;
            If Abs(DesignatorYoffset)<Designator.Size Then AutoY:= 2
              else If DesignatorYoffset>0 Then AutoY:= 1
                else AutoY:= 3;
            AutoSet:=  AutoX + AutoY;
            //ShowMessage(AutoSet);

            // notify that the pcb object is going to be modified
            PCBServer.SendMessageToRobots(Designator.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);

            // Move designator depending on current AP setting
            Case AutoSet of
              13:tc_AutoPos:= eAutoPos_TopLeft;
              12:tc_AutoPos:= eAutoPos_CenterLeft;
              11:tc_AutoPos:= eAutoPos_BottomLeft;
              23:tc_AutoPos:= eAutoPos_TopCenter;
              21:tc_AutoPos:= eAutoPos_BottomCenter;
              33:tc_AutoPos:= eAutoPos_TopRight;
              32:tc_AutoPos:= eAutoPos_CenterRight;
              31:tc_AutoPos:= eAutoPos_BottomRight;
              22:tc_AutoPos:= eAutoPos_CenterCenter;
            End; {case AutoSet}

           // Set AP to manual
            Component.ChangeNameAutoposition  :=   tc_AutoPos;
          // notify that the pcb object is modified
            PCBServer.SendMessageToRobots(Designator.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);

            End;
          // Get the next component handle
            Component := ComponentIteratorHandle.NextPCBObject;
            If APDesForm.SelectedCheckbox.Checked Then  while (Component <> Nil) Do
              If NOT Component.Selected Then Component := ComponentIteratorHandle.NextPCBObject
              else break;  // Find the next selected comp if select only checked


     End;

        // Notify the pcbserver that all changes have been made
        PCBServer.PostProcess;

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

