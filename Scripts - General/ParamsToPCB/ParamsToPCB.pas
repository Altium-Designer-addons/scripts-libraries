{..............................................................................}
{ Summary   This scripts can be used to add component parameters to the PCB.   }
{                                                                              }
{           Component parameters will be added to footprint.                   }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}




var
   Board         : IPCB_Board;
   FlatHierarchy : IDocument;
   MechPairs     : TStringList;
   MechSingles   : TStringList;


function GetFirstLayerName(Pair : String) : String;
var
   pos : Integer;
begin
   Pos := AnsiPos(' <----> ', Pair);
   SetLength(Pair, Pos - 1);

   Result := Pair;
end;



function GetSecondLayerName(Pair : String) : String;
var
   pos : Integer;
begin
   Pos := AnsiPos(' <----> ', Pair);
   Delete(Pair, 1, Pos  + 7);

   Result := Pair;
end;



procedure TFormParamsToPCB.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;



procedure TFormParamsToPCB.FormMechLayerDesignatorsShow(Sender: TObject);
var
   LayerPair : TMechanicalLayerPair;
   Layers    : TLayer;
   i, j      : Integer; 

begin
   if Board.MechanicalPairs.Count = 0 then
   begin
      GroupBoxLayer.Caption := 'Choose Mech Layer:';

      GroupBoxTop.Enabled := False;
      GroupBoxTop.Visible := False;

      GroupBoxLayers.Enabled := False;
      GroupBoxLayers.Visible := False;

      RadioButtonSingle.Checked := True;

      for i := 1 to 32 do
         if Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].MechanicalLayerEnabled then
         begin
            ComboBoxLayers.Items.Add(Board.LayerName(ILayer.MechanicalLayer(i)));
            if comboBoxLayers.Items.Count = 1 then
               ComboBoxLayers.Text := ComboBoxLayers.Items[0];
         end;
   end
   else
   begin
      for i := 1 to 32 do
      begin
         for j := i + 1 to 32 do
            if Board.MechanicalPairs.PairDefined(ILayer.MechanicalLayer(i), ILayer.MechanicalLayer(j)) then
            begin
               MechPairs.Add(Board.LayerName(ILayer.MechanicalLayer(i)) + ' <----> ' + Board.LayerName(ILayer.MechanicalLayer(j)));
               ComboBoxLayers.Items.Add(Board.LayerName(ILayer.MechanicalLayer(i)) + ' <----> ' + Board.LayerName(ILayer.MechanicalLayer(j)));
               if comboBoxLayers.Items.Count = 1 then
               begin
                  ComboBoxLayers.Text := ComboBoxLayers.Items[0];
                  RadioButtonLayer1.caption := Board.LayerName(ILayer.MechanicalLayer(i));
                  RadioButtonLayer2.caption := Board.LayerName(ILayer.MechanicalLayer(j));
               end;
            end;

         // Here I need to fill in MechSingles, if user switches:
         if Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].MechanicalLayerEnabled then
            MechSingles.Add(Board.LayerName(ILayer.MechanicalLayer(i)));
      end;
   end;
end;



procedure TFormParamsToPCB.RadioButtonSingleClick(Sender: TObject);
var
   i : Integer;
begin
   if GroupBoxLayer.Caption <> 'Choose Mech Layer:' then
   begin
      GroupBoxTop.Enabled := False;
      GroupBoxTop.Visible := False;

      GroupBoxLayer.Caption := 'Choose Mech Layer:';

      ComboBoxLayers.Clear;

      for i := 0 to MechSingles.Count - 1 do
      begin
         ComboBoxLayers.Items.Add(MechSingles[i]);
      end;

      ComboBoxLayers.Text := ComboBoxLayers.Items[0];

   end;
end;



procedure TFormParamsToPCB.RadioButtonPairClick(Sender: TObject);
var
   i : integer;
begin
   if GroupBoxLayer.Caption <> 'Choose Mech Layer Pair:' then
   begin

      GroupBoxTop.Enabled := True;
      GroupBoxTop.Visible := True;

      RadioButtonLayer1.Enabled := True;
      RadioButtonLayer2.Enabled := True;

      GroupBoxLayer.Caption := 'Choose Mech Layer Pair:';

      ComboBoxLayers.Clear;

      for i := 0 to MechPairs.Count - 1 do
      begin
         ComboBoxLayers.Items.Add(MechPairs[i]);
      end;

      ComboBoxLayers.Text := ComboBoxLayers.Items[0];
      RadioButtonLayer1.Caption := GetFirstLayerName(ComboBoxLayers.Text);
      RadioButtonLayer2.Caption := GetSecondLayerName(ComboBoxLayers.Text);

   end;
end;



procedure TFormParamsToPCB.ComboBoxLayersChange(Sender: TObject);
begin
   if GroupBoxLayer.Caption = 'Choose Mech Layer Pair:' then
   begin
      RadioButtonLayer1.Caption := GetFirstLayerName(ComboBoxLayers.Text);
      RadioButtonLayer2.Caption := GetSecondLayerName(ComboBoxLayers.Text);
   end;
end;



procedure TFormParamsToPCB.ButtonOKClick(Sender: TObject);
var
   MechTop       : IPCB_LayerObject;
   MechBot       : IPCB_LayerObject;
   i, j, flag    : Integer;
   TextObj       : IPCB_Text;
   CompIterator  : IPCB_BoardIterator;
   PCBComponent  : IPCB_Component;

   ComponentNum  : Integer;
   Component     : IComponent;
   PartNum       : Integer;
   Part          : IPart;
   ParameterNum  : Integer;
   Parameter     : IParameter;

begin
   // This is the main one. This was hard to set up.
   // I hope it will not be so hard to finish.

   if ComboBoxParameters.Text = '' then
   begin
      ShowMessage('Please choose parameter name');
      exit;
   end;

   if RadioButtonSilk.Checked then
   begin
      MechTop := String2Layer('Top Overlay');
      MechBot := String2Layer('Bottom Overlay');
   end
   else
   begin
      if RadioButtonPair.Checked then
      begin
         for i := 1 to 32 do
         begin
            if (Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].Name = RadioButtonLayer1.Caption) then
               if RadioButtonLayer1.Checked then MechTop := ILayer.MechanicalLayer(i)
               else                              MechBot := ILayer.MechanicalLayer(i);
            if (Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].Name = RadioButtonLayer2.Caption) then
               if RadioButtonLayer2.Checked then MechTop := ILayer.MechanicalLayer(i)
               else                              MechBot := ILayer.MechanicalLayer(i);
         end;
      end
      else
      begin
         for i := 1 to 32 do
            if (Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)].Name = ComboBoxLayers.Text) then
            begin
               MechTop := ILayer.MechanicalLayer(i);
               MechBot := ILayer.MechanicalLayer(i);
               break;
            end;
      end;
   end;

   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      if Board.SelectecObject[i].ObjectID = eComponentObject then
      begin
         PCBComponent := Board.SelectecObject[i];

         For ComponentNum := 0 to FlatHierarchy.DM_ComponentCount - 1 do
         begin
            Component := FlatHierarchy.DM_Components(ComponentNum);

            if Component.DM_UniqueId = PCBComponent.SourceUniqueId then
            begin
               for ParameterNum := 0 to Component.DM_ParameterCount - 1 do
               begin
                  Parameter := Component.DM_Parameters(ParameterNum);

                  if Parameter.DM_Name = ComboBoxParameters.Text then
                  begin

                     // here we need to create new string and add it to component. we will place it in the same place component is in.
                     PCBServer.PreProcess;

                     TextObj := PCBServer.PCBObjectFactory(eTextObject, eNoDimension, eCreate_Default);

                     // notify that the pcb object is going to be modified
                     TextObj.BeginModify;

                     if PCBComponent.Layer = eTopLayer then
                        TextObj.Layer  := MechTop
                     else
                        TextObj.Layer  := MechBot;

                     TextObj.Text      := Parameter.DM_Value;
                     TextObj.Size      := PCBComponent.Name.Size;
                     TextObj.Width     := PCBComponent.Name.Width;

                     TextObj.XLocation := PCBComponent.BoundingRectangleNoNameComment.Left;
                     TextObj.YLocation := PCBComponent.BoundingRectangleNoNameComment.Bottom - TextObj.Size - 2 * TextObj.Width;

                     Board.AddPCBObject(TextObj);
                     PCBComponent.AddPCBObject(TextObj);

                     // notify that the pcb object has been modified
                     TextObj.EndModify;

                     Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, TextObj.I_ObjectAddress);

                     PcbServer.PostProcess;

                  end;
               end;
            end;
         end;
      end;
   end;

   Close;
end;



Procedure Start;
var

    Workspace         : IWorkspace;
    PCBProject        : IProject;
    ComponentNum      : Integer;
    Component         : IComponent;
    PartNum           : Integer;
    Part              : IPart;
    ParameterNum      : Integer;
    Parameter         : IParameter;

    Iterator          : IPCB_BoardIterator;
    PCBComponent      : IPCB_Component;

    Params            : TStringList;
    i                 : integer;

begin

   Board := PCBServer.GetCurrentPCBBoard;
   If Board = Nil Then
   Begin
      ShowMessage('Current Document is not a PCB Document');
      exit;
   end;

   if Board.SelectecObjectCount = 0 then
   Begin
      ShowMessage('Please select components whose parameters you wish to import');
      exit;
   end;

   Workspace   := GetWorkspace;
   PCBProject  := Workspace.DM_FocusedProject;

   If (PcbProject = nil) then
   Begin
      ShowMessage('Current Project is not a PCB Project');
      exit;
   end;

   If (AnsiUpperCase(ExtractFileExt(PCBProject.DM_ProjectFileName)) <> '.PRJPCB') then
   Begin
      ShowMessage('Current Project is not a PCB Project');
      exit;
   end;

   // Compile project
   PCBProject.DM_Compile;

   FlatHierarchy := PCBProject.DM_DocumentFlattened;

   // If we couldn't get the flattened sheet, then most likely the project has
   // not been compiled recently
   If (FlatHierarchy = Nil) Then
   Begin
      ShowMessage('Compile the Project before Running this script.');
      Exit;
   End;

   // Here I need to cycle through all selected objects and see what PCB component are selected.

   Params := TStringList.Create;

   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      if Board.SelectecObject[i].ObjectId = eComponentObject then
      begin
         PCBComponent := Board.SelectecObject[i];

         For ComponentNum := 0 to FlatHierarchy.DM_ComponentCount - 1 do
         begin
            Component := FlatHierarchy.DM_Components(ComponentNum);

            if Component.DM_UniqueId = PcbComponent.SourceUniqueId then
            begin
               for ParameterNum := 0 to Component.DM_ParameterCount - 1 do
               begin
                  Parameter := Component.DM_Parameters(ParameterNum);
                  Params.Values[Parameter.DM_Name] := Parameter.DM_Name;
               end;
            end;
         end;
      end;
   end;

   for i := 0 to Params.count - 1 do
   begin
      ComboBoxParameters.Items.Add(Params.ValueFromIndex[i]);
   end;

   MechPairs   := TStringList.Create;
   MechSingles := TStringList.Create;

   FormParamsToPCB.ShowModal;
end;




procedure TFormParamsToPCB.RadioButtonSilkClick(Sender: TObject);
begin
   if RadioButtonSilk.Checked then
   begin

      GroupBoxTop.Enabled := False;
      GroupBoxTop.Visible := False;

      GroupBoxLayers.Enabled := False;
      GroupBoxLayers.Visible := False;

      GroupBoxLayer.Enabled := False;
      GroupBoxLayer.Visible := False;
   end;
end;


procedure TFormParamsToPCB.RadioButtonMechClick(Sender: TObject);
begin
   if RadioButtonMech.Checked then
   begin
      if Board.MechanicalPairs.Count <> 0 then
      begin
         GroupBoxTop.Enabled := True;
         GroupBoxTop.Visible := True;

         GroupBoxLayers.Enabled := True;
         GroupBoxLayers.Visible := True;
      end;

      GroupBoxLayer.Enabled := True;
      GroupBoxLayer.Visible := True;
   end;
end;

