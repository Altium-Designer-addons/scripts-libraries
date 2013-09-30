// Soldermask Barrel Relief for VIAs
// Author: Randy Clemmons
// September 8, 2013

// Barrel Relief Equals Drill Hole Size + Relief
// PCB Fabricators typically request Drill + 5mil when Via Hole > 13 mils

// In Altium Soldermask Expansion is Based on the Pad Size.
// That works great for Pads, but not so good for Vias.

// Calculate the Soldermask Expansion for Barrel Relief
// Soldermask Expansion = ((PadSize - (DrillSize + Relief))/2)*-1

// reference material "AddPCBObject method"
// http://wiki.altium.com/display/ADOH/PCB+API+System+Interfaces
// Note we convert values in Mils to internal coordinates
// using the MilsToCoord function. All PCB objects locations and sizes
// have internal coordinate units where 1 mil = 10000 internal units

// Debug Code removed from main loop
// ShowMessage (DrillSize);
// ShowMessage (PadSize);
// ShowMessage (Via.SolderMaskExpansion);

Var

 Board : IPCB_Board;
 BarrelRelief : Varaint;  // Desired Via Barrel Soldermask Relief in mils
 Threshold : Varaint;     // Via Hole Size Threshold for applying Barrel Reliefs
 SelectedVias : Boolean;  // Via Hole Size Threshold for applying Barrel Reliefs

Procedure Start;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then
   begin
        ShowMessage('Active Window is Not a .PcbDoc File');
        exit;
   end;

   // Use Show Mode to allow user to Edit and Select Vias
   FormViaBarrelRelief.Show;

end;

procedure BarrelReliefVias(BarrelRelief, Threshold);

Var

 Via : IPCB_Primitive;
 Iterator : IPCB_BoardIterator;

 PCBSystemOptions        : IPCB_SystemOptions;
 DRCSetting              : boolean;

 ViaNumber : Variant;
 DrillSize : Variant;
 PadSize : Variant;

 ExpandMask : Variant;
 PadCache : TPadCache;

Begin

// Start of User Inputs //

// Desired Via Barrel Soldermask Relief in mils
// Enter a Large Negative Number i.e. -99 to Fully Tent Vias
BarrelRelief := StrToFloat(txtRelief.Text);
// Via HoleSize Threshold in mils for Barrel Reliefs
Threshold := StrToFloat(txtThreshold.Text);

// End of User Inputs //

// Initialize the systems in the PCB Editor !!!
PCBServer.PreProcess;

 // Disables Online DRC during designator movement to improve speed
PCBSystemOptions := PCBServer.SystemOptions;

If PCBSystemOptions = Nil Then Exit;

DRCSetting := PCBSystemOptions.DoOnlineDRC;
PCBSystemOptions.DoOnlineDRC := false;

 ViaNumber := 0;
 Board := PCBServer.GetCurrentPCBBoard;
 If Board = Nil Then Exit;

 Iterator := Board.BoardIterator_Create;
 Iterator.AddFilter_ObjectSet(MkSet(eViaObject));
 Iterator.AddFilter_LayerSet(AllLayers);
 Iterator.AddFilter_Method(eProcessAll);

 Via := Iterator.FirstPCBObject;
 While (Via <> Nil) Do

    Begin

      DrillSize := Via.HoleSize;

      If (DrillSize / 10000) > Threshold then

         Begin
               PadSize := Via.Size;

               // Calculate Soldermask Expansion for desired Barrel Relief
               ExpandMask := ((PadSize / 10000 - (DrillSize / 10000 + BarrelRelief)) / 2) * -1;

               // Use a pad cache to set Via soldermask expansion values
               // Use a Large Negative Numbers to Fully Tent Vias
               PadCache := Via.GetState_Cache;
               PadCache.SolderMaskExpansion := MilsToCoord(ExpandMask);
               PadCache.SolderMaskExpansionValid := eCacheManual;
               // Assign the pad cache to the via
               Via.SetState_Cache := padCache;

               // Remove Complete Tenting from Both Sides for Barrel Relief
               Via.SetState_IsTenting_Top(False);
               Via.SetState_IsTenting_Bottom(False);

               Inc(ViaNumber);
         End;

     Via := Iterator.NextPCBObject;

 End;

 Board.BoardIterator_Destroy(Iterator);

 PCBServer.PostProcess;
 //  Refresh  PCB  screen
 Client.SendMessage('PCB:Zoom',  'Action=Redraw'  ,  255,  Client.CurrentView);

 // lblResults.Caption : = " ";
lblResults.Caption : = 'Relieved Vias: Qty = ' +  IntToStr(ViaNumber);

 // Restore DRC setting
 PCBSystemOptions.DoOnlineDRC :=  DRCSetting;

 End;

procedure BarrelReliefSelectedVias(BarrelRelief, Threshold);

Var

 Via : IPCB_Primitive;
 Iterator : IPCB_BoardIterator;

 PCBSystemOptions        : IPCB_SystemOptions;
 DRCSetting              : boolean;

 ViaNumber : Variant;
 DrillSize : Variant;
 PadSize : Variant;
 // BarrelRelief : Varaint;  // Desired Via Barrel Relief in mils
 // Threshold : Varaint;     // Via Drill Hole Size Threshold for applying Barrel Reliefs

 ExpandMask : Variant;
 PadCache : TPadCache;

Begin

// Start of User Inputs //

// Desired Via Barrel Soldermask Relief in mils
// Enter a Large Negative Number i.e. -99 to Fully Tent Vias
BarrelRelief := StrToFloat(txtRelief.Text);
// Via HoleSize Threshold in mils for Barrel Reliefs
Threshold := StrToFloat(txtThreshold.Text);

// End of User Inputs //

// Initialize the systems in the PCB Editor !!!
PCBServer.PreProcess;

// Disables Online DRC during designator movement to improve speed
PCBSystemOptions := PCBServer.SystemOptions;

If PCBSystemOptions = Nil Then Exit;

DRCSetting := PCBSystemOptions.DoOnlineDRC;
PCBSystemOptions.DoOnlineDRC := false;

 ViaNumber := 0;
 Board := PCBServer.GetCurrentPCBBoard;
 If Board = Nil Then Exit;

 Iterator := Board.BoardIterator_Create;
 Iterator.AddFilter_ObjectSet(MkSet(eViaObject));
 Iterator.AddFilter_LayerSet(AllLayers);
 Iterator.AddFilter_Method(eProcessAll);

 Via := Iterator.FirstPCBObject;
 While (Via <> Nil) Do

    Begin
    DrillSize := Via.HoleSize;

      If ((DrillSize / 10000) > Threshold) AND (Via.Selected = True) Then

         Begin
               PadSize := Via.Size;

               // Calculate Soldermask Expansion for desired Barrel Relief
               ExpandMask := ((PadSize / 10000 - (DrillSize / 10000 + BarrelRelief)) / 2) * -1;

               // Use a pad cache to set Via soldermask expansion values
               // Use a Large Negative Numbers to Fully Tent Vias
               PadCache := Via.GetState_Cache;
               PadCache.SolderMaskExpansion := MilsToCoord(ExpandMask);
               PadCache.SolderMaskExpansionValid := eCacheManual;
               // Assign the pad cache to the via
               Via.SetState_Cache := padCache;

               // Remove Complete Tenting from Both Sides for Barrel Relief
               Via.SetState_IsTenting_Top(False);
               Via.SetState_IsTenting_Bottom(False);

               Inc(ViaNumber);
         end;

        Via := Iterator.NextPCBObject;

 End;

 Board.BoardIterator_Destroy(Iterator);

 PCBServer.PostProcess;
 //  Refresh  PCB  screen
 Client.SendMessage('PCB:Zoom',  'Action=Redraw'  ,  255,  Client.CurrentView);

 // ShowMessage('Barrel Relief Vias with Hole Sizes > ' + VarToStr(Threshold) + 'mils :  Qty = ' +  IntToStr(ViaNumber));

 // lblResults.Caption : = "";
lblResults.Caption : = 'Relieved Selected Vias: Qty = ' +  IntToStr(ViaNumber);

 // Restore DRC setting
 PCBSystemOptions.DoOnlineDRC :=  DRCSetting;

End;


procedure TentSelectedVias(SelectedVias);

Var

 Via : IPCB_Primitive;
 Iterator : IPCB_BoardIterator;

 PCBSystemOptions        : IPCB_SystemOptions;
 DRCSetting              : boolean;
 ViaNumber : Variant;

 // BarrelRelief : Varaint;  // Desired Via Barrel Soldermask Relief in mils
 // Threshold : Varaint;     // Via Hole Size Threshold for applying Barrel Reliefs

Begin

// Initialize the systems in the PCB Editor !!!
PCBServer.PreProcess;

// Disables Online DRC during designator movement to improve speed
PCBSystemOptions := PCBServer.SystemOptions;

If PCBSystemOptions = Nil Then Exit;

DRCSetting := PCBSystemOptions.DoOnlineDRC;
PCBSystemOptions.DoOnlineDRC := false;

 ViaNumber := 0;
 Board := PCBServer.GetCurrentPCBBoard;
 If Board = Nil Then Exit;

 Iterator := Board.BoardIterator_Create;
 Iterator.AddFilter_ObjectSet(MkSet(eViaObject));
 Iterator.AddFilter_LayerSet(AllLayers);
 Iterator.AddFilter_Method(eProcessAll);

 Via := Iterator.FirstPCBObject;
 While (Via <> Nil) Do

    Begin

         If Via.Selected Then
            Begin
                 Via.SetState_IsTenting_Top(True);
                 Via.SetState_IsTenting_Bottom(True);
                 Inc(ViaNumber);
            End;

         Via := Iterator.NextPCBObject;

    End;

 Board.BoardIterator_Destroy(Iterator);

 PCBServer.PostProcess;
 //  Refresh  PCB  screen
 Client.SendMessage('PCB:Zoom',  'Action=Redraw'  ,  255,  Client.CurrentView);

 // ShowMessage('Barrel Relief Vias with Hole Sizes > ' + VarToStr(Threshold) + 'mils :  Qty = ' +  IntToStr(ViaNumber));

 // lblResults.Caption : = "";
lblResults.Caption : = 'Tented Selected Vias: Qty = ' +  IntToStr(ViaNumber);

 // Restore DRC setting
 PCBSystemOptions.DoOnlineDRC :=  DRCSetting;

End;


procedure TentAllVias(SelectedVias);

Var

 Via : IPCB_Primitive;
 Iterator : IPCB_BoardIterator;

 PCBSystemOptions        : IPCB_SystemOptions;
 DRCSetting              : boolean;
 ViaNumber : Variant;

Begin

// Initialize the systems in the PCB Editor !!!
PCBServer.PreProcess;

// Disables Online DRC during designator movement to improve speed
PCBSystemOptions := PCBServer.SystemOptions;

If PCBSystemOptions = Nil Then Exit;

DRCSetting := PCBSystemOptions.DoOnlineDRC;
PCBSystemOptions.DoOnlineDRC := false;

 ViaNumber := 0;
 Board := PCBServer.GetCurrentPCBBoard;
 If Board = Nil Then Exit;

 Iterator := Board.BoardIterator_Create;
 Iterator.AddFilter_ObjectSet(MkSet(eViaObject));
 Iterator.AddFilter_LayerSet(AllLayers);
 Iterator.AddFilter_Method(eProcessAll);

 Via := Iterator.FirstPCBObject;
 While (Via <> Nil) Do

    Begin
         Via.SetState_IsTenting_Top(True);
         Via.SetState_IsTenting_Bottom(True);
         Inc(ViaNumber);
         Via := Iterator.NextPCBObject;
    End;

 Board.BoardIterator_Destroy(Iterator);

 PCBServer.PostProcess;
 //  Refresh  PCB  screen
 Client.SendMessage('PCB:Zoom',  'Action=Redraw'  ,  255,  Client.CurrentView);

 // ShowMessage('Barrel Relief Vias with Hole Sizes > ' + VarToStr(Threshold) + 'mils :  Qty = ' +  IntToStr(ViaNumber));

 // lblResults.Caption : = "";
lblResults.Caption : = 'Tented All Vias: Qty = ' +  IntToStr(ViaNumber);

 // Restore DRC setting
 PCBSystemOptions.DoOnlineDRC :=  DRCSetting;

End;

procedure TFormViaBarrelRelief.btnCancelClick(Sender: TObject);
begin
     Close;
end;


procedure TFormViaBarrelRelief.btnReliefClick(Sender: TObject);
begin
     // Desired Via Barrel Soldermask Relief in mils
     // Entering a Large Negative Number i.e. -99 will Tent Vias
     BarrelRelief := StrToFloat(txtRelief.Text);

     // Via HoleSize Threshold in mils for Barrel Reliefs
     Threshold := StrToFloat(txtThreshold.Text);

     if chkSelected.Checked = False Then BarrelReliefVias(BarrelRelief, Threshold);
     if chkSelected.Checked = True Then BarrelReliefSelectedVias(BarrelRelief, Threshold);
end;

procedure TFormViaBarrelRelief.btnTentClick(Sender: TObject);
begin
     if chkSelected.Checked = True Then TentSelectedVias(True);
     if chkSelected.Checked = False Then TentAllVias(False);
end;


