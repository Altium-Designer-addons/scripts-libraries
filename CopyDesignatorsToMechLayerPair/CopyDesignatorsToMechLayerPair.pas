{..............................................................................}
{ Summary   This scripts can be used to copy component designators to mech     }
{           layer or mech layer pair.                                          }
{                                                                              }
{           Designators will be the same as in main components, but they will  }
{           have '.Designator' text, and they will be part of component        }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}




var
   Board       : IPCB_Board;
   MechPairs   : TStringList;
   MechSingles : TStringList;


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



procedure TFormMechLayerDesignators.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;



procedure TFormMechLayerDesignators.FormMechLayerDesignatorsShow(Sender: TObject);
var
   LayerPair : TMechanicalLayerPair;
   Layers    : TLayer;
   i, j      : Integer; 

begin
   if Board.MechanicalPairs.Count = 0 then
   begin
      RadioButtonSingle.Checked := True;
      RadioButtonPair.Enabled := False;
      GroupBoxLayer.Caption := 'Choose Mech Layer:';

      RadioButtonLayer1.Enabled := False;
      RadioButtonLayer2.Enabled := False;

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



procedure TFormMechLayerDesignators.RadioButtonSingleClick(Sender: TObject);
var
   i : Integer;
begin
   if GroupBoxLayer.Caption <> 'Choose Mech Layer:' then
   begin
      RadioButtonLayer1.Enabled := False;
      RadioButtonLayer2.Enabled := False;

      RadioButtonLayer1.Caption := 'Single Layer';
      RadioButtonLayer2.Caption := 'Single Layer';

      GroupBoxLayer.Caption := 'Choose Mech Layer:';

      ComboBoxLayers.Clear;

      for i := 0 to MechSingles.Count - 1 do
      begin
         ComboBoxLayers.Items.Add(MechSingles[i]);
      end;

      ComboBoxLayers.Text := ComboBoxLayers.Items[0];

   end;
end;



procedure TFormMechLayerDesignators.RadioButtonPairClick(Sender: TObject);
var
   i : integer;
begin
   if GroupBoxLayer.Caption <> 'Choose Mech Layer Pair:' then
   begin
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



procedure TFormMechLayerDesignators.ComboBoxLayersChange(Sender: TObject);
begin
   if GroupBoxLayer.Caption = 'Choose Mech Layer Pair:' then
   begin
      RadioButtonLayer1.Caption := GetFirstLayerName(ComboBoxLayers.Text);
      RadioButtonLayer2.Caption := GetSecondLayerName(ComboBoxLayers.Text);
   end;
end;



procedure TFormMechLayerDesignators.ButtonOKClick(Sender: TObject);
var
   MechTop       : IPCB_LayerObject;
   MechBot       : IPCB_LayerObject;
   i, flag       : Integer;
   Primitive     : IPCB_Primitive;
   NewDesignator : IPCB_Text;
   CompIterator  : IPCB_BoardIterator;
begin
   // This is the main one. This was hard to set up.
   // I hope it will not be so hard to finish.

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

   // Now I need to cycle through all components, or only selected ones, and
   // I need to copy their designators to the mech layers defined.

   flag := 0;

   for i := 0 to Board.SelectecObjectCount - 1 do
   begin
      Primitive := Board.SelectecObject[i];
      if Primitive.ObjectID = eComponentObject then
      begin
         flag := 1;
         NewDesignator := Primitive.Name.Replicate;

         if Primitive.Layer = eTopLayer then NewDesignator.Layer := MechTop
         else                                NewDesignator.Layer := MechBot;

         NewDesignator.Text := '.Designator';

         Board.AddPCBObject(NewDesignator);
         Primitive.AddPCBObject(NewDesignator);
      end;
   end;

   if flag = 0 then
   begin
      // No selected components - make it for all components

      CompIterator := Board.BoardIterator_Create;
      CompIterator.AddFilter_ObjectSet(MkSet(eComponentObject));
      CompIterator.AddFilter_LayerSet(AllLayers);
      CompIterator.AddFilter_Method(eProcessAll);


      Primitive := CompIterator.FirstPCBObject;
      While (Primitive <> Nil) Do
      Begin
         NewDesignator := Primitive.Name.Replicate;

         if Primitive.Layer = eTopLayer then NewDesignator.Layer := MechTop
         else                                NewDesignator.Layer := MechBot;

         NewDesignator.Text := '.Designator';

         Board.AddPCBObject(NewDesignator);
         Primitive.AddPCBObject(NewDesignator);


         Primitive := CompIterator.NextPCBObject;
      End;
      Board.BoardIterator_Destroy(CompIterator);
   end;
   
   Close;
end;



Procedure Start;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   MechPairs   := TStringList.Create;
   MechSingles := TStringList.Create;

   FormMechLayerDesignators.ShowModal;
end;
