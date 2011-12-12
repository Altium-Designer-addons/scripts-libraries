{..............................................................................}
{ Summary   This scripts can be used to show or hide layers.                   }
{                                                                              }
{           it shows all layers in a form and by clicking you turn them on or  }
{           off. This is easier to use than "Board Layers and colors" dialog   }
{           because:                                                           }
{           - View is automativally updated and user can see the view when     }
{             clicking on checkboxes, so if something also is in a way it can  }
{             be turned off                                                    }
{           - This is not dialog so user can control PCB while the form is     }
{             shown.                                                           }
{                                                                              }
{           Limitation - it supports "only" up to 32 signal layers. If you     }
{                        need more - ask.                                      }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
var
   Board         : IPCB_Board;
   TheLayerStack : IPCB_LayerStack;



function CBFromInt(i : Integer) : TCheckBox;
begin
   case i of
       1 : Result := CheckBox1;
       2 : Result := CheckBox2;
       3 : Result := CheckBox3;
       4 : Result := CheckBox4;
       5 : Result := CheckBox5;
       6 : Result := CheckBox6;
       7 : Result := CheckBox7;
       8 : Result := CheckBox8;
       9 : Result := CheckBox9;
      10 : Result := CheckBox10;
      11 : Result := CheckBox11;
      12 : Result := CheckBox12;
      13 : Result := CheckBox13;
      14 : Result := CheckBox14;
      15 : Result := CheckBox15;
      16 : Result := CheckBox16;
      17 : Result := CheckBox17;
      18 : Result := CheckBox18;
      19 : Result := CheckBox19;
      20 : Result := CheckBox20;
      21 : Result := CheckBox21;
      22 : Result := CheckBox22;
      23 : Result := CheckBox23;
      24 : Result := CheckBox24;
      25 : Result := CheckBox25;
      26 : Result := CheckBox26;
      27 : Result := CheckBox27;
      28 : Result := CheckBox28;
      29 : Result := CheckBox29;
      30 : Result := CheckBox30;
      31 : Result := CheckBox29;
      32 : Result := CheckBox30;
   end;
end;



Procedure Layer2CB(CheckBox : TCheckBox, Tekst : String, Display : Boolean);
begin
   CheckBox.Enabled := True;
   CheckBox.Visible := True;
   CheckBox.Caption := Tekst;
   CheckBox.Checked := Display;
end;



Procedure CB2Layer(CheckBoxa : TCheckBox);
var
   LayerObj  : IPCB_LayerObject;
   MechLayer : IPCB_MechanicalLayer;
   i         : Integer;
begin

   if TabControlLayers.TabIndex = 0 then
   begin

      LayerObj := TheLayerStack.FirstLayer;
      i := 0;
      while (LayerObj <> nil) do
      begin
         if (i = ((CheckBoxa.Top - 30) / 20)) then break;
         Inc(i);
         LayerObj := TheLayerStack.NextLayer(LayerObj);
      end;

      LayerObj.IsDisplayed[Board] := CheckBoxa.Checked;
   end
   else if TabControlLayers.TabIndex = 1 then
   begin
      for i := 1 to 32 do
      begin
         MechLayer := TheLayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

         if (MechLayer.MechanicalLayerEnabled) and (MechLayer.Name = CheckBoxa.Caption) then
            MechLayer.IsDisplayed[Board] := CheckBoxa.Checked;
      end;
   end
   else
   begin
      LayerObj := Board.LayerStack.LayerObject_V7[String2Layer(CheckBoxa.Caption)];

      LayerObj.IsDisplayed[Board] := CheckBoxa.Checked;
   end;

   Board.ViewManager_UpdateLayerTabs;
   Board.ViewManager_FullUpdate;
end;



procedure TShowHideLayers.ShowHideLayersShow(Sender: TObject);
var
   LayerObj : IPCB_LayerObject;
   i        : Integer;
begin

   LayerObj := TheLayerStack.FirstLayer;
   i := 1;
   while (LayerObj <> nil) and (i <= 32) do
   begin

      Layer2CB(CBFromInt(i),  LayerObj.Name, LayerObj.IsDisplayed[Board]);

      Inc(i);
      LayerObj := TheLayerStack.NextLayer(LayerObj);
   end;
end;



procedure TShowHideLayers.TabControlLayersChange(Sender: TObject);
var
   LayerObj  : IPCB_LayerObject;
   MechLayer : IPCB_MechanicalLayer;
   i, j      : Integer;
   GetCB     : TCheckBox;
begin

   for i := 1 to 32 do
   begin
      GetCB := CBFromInt(i);
      GetCB.Checked := False;
      GetCB.Enabled := False;
      GetCB.Visible := False;
   end;

   if TabControlLayers.TabIndex = 0 then
   begin
      LayerObj := TheLayerStack.FirstLayer;
      i := 1;
      while (LayerObj <> nil) and (i <= 32) do
      begin
         Layer2CB(CBFromInt(i),  LayerObj.Name, LayerObj.IsDisplayed[Board]);
         Inc(i);
         LayerObj := TheLayerStack.NextLayer(LayerObj);
      end;
   end
   else if TabControlLayers.TabIndex = 1 then
   begin
      j := 0;
      for i := 1 to 32 do
      begin
         MechLayer := TheLayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

         if MechLayer.MechanicalLayerEnabled then
         begin
            inc(j);
            Layer2CB(CBFromInt(j), MechLayer.Name, MechLayer.IsDisplayed[Board]);
         end;
      end;
   end
   else
   begin
      // Here we take care of masks and other layers

      // Masks
      Layer2CB(CBFromInt(1),  'Top Solder Mask',    Board.LayerIsDisplayed[String2Layer('Top Solder Mask')]);
      Layer2CB(CBFromInt(2),  'Bottom Solder Mask', Board.LayerIsDisplayed[String2Layer('Bottom Solder Mask')]);

      Layer2CB(CBFromInt(4),  'Top Paste',          Board.LayerIsDisplayed[String2Layer('Top Paste')]);
      Layer2CB(CBFromInt(5),  'Bottom Paste',       Board.LayerIsDisplayed[String2Layer('Bottom Paste')]);

      // Overlay
      Layer2CB(CBFromInt(7),  'Top Overlay',        Board.LayerIsDisplayed[String2Layer('Top Overlay')]);
      Layer2CB(CBFromInt(8),  'Bottom Overlay',     Board.LayerIsDisplayed[String2Layer('Bottom Overlay')]);

      Layer2CB(CBFromInt(10), 'Drill Guide',        Board.LayerIsDisplayed[String2Layer('Drill Guide')]);
      Layer2CB(CBFromInt(11), 'Drill Drawing',      Board.LayerIsDisplayed[String2Layer('Drill Drawing')]);

      Layer2CB(CBFromInt(13), 'Multi Layer',        Board.LayerIsDisplayed[String2Layer('Multi Layer')]);
      Layer2CB(CBFromInt(14), 'Keep Out Layer',     Board.LayerIsDisplayed[String2Layer('Keep Out Layer')]);

   end;
end;



procedure TShowHideLayers.CheckBoxCopperClick(Sender: TObject);
var
   i        : Integer;
   CB       : TCheckBox;
   LayerObj : IPCB_LayerObject;
begin
   LayerObj := TheLayerStack.FirstLayer;
   i := 1;
   while (LayerObj <> nil) do
   begin
      LayerObj.IsDisplayed[Board] := CheckBoxCopper.Checked;

      if TabControlLayers.TabIndex = 0 then
      begin
         CB := CBFromInt(i);

         if CheckBoxCopper.Checked then
            CB.Checked := True
         else
            CB.Checked := False;
      end;

      Inc(i);
      LayerObj := TheLayerStack.NextLayer(LayerObj);
   end;


   Board.ViewManager_UpdateLayerTabs;
   Board.ViewManager_FullUpdate;
end;



procedure TShowHideLayers.CheckBoxMechClick(Sender: TObject);
var
   i         : Integer;
   MechLayer : IPCB_MechanicalLayer;
   CB        : TCheckBox;
begin
   for i := 1 to 32 do
   begin
      MechLayer := TheLayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

      if MechLayer.MechanicalLayerEnabled then
         MechLayer.IsDisplayed[Board] := CheckBoxMech.Checked;

      if TabControlLayers.TabIndex = 1 then
      begin
         CB := CBFromInt(i);

         if CheckBoxMech.Checked then
            CB.Checked := True
         else
            CB.Checked := False;
      end;
   end;

   Board.ViewManager_UpdateLayerTabs;
   Board.ViewManager_FullUpdate;
end;



procedure TShowHideLayers.CheckBoxOthersClick(Sender: TObject);
begin
   Board.LayerIsDisplayed[String2Layer('Top Solder Mask')]    := CheckBoxOthers.Checked;
   Board.LayerIsDisplayed[String2Layer('Bottom Solder Mask')] := CheckBoxOthers.Checked;

   Board.LayerIsDisplayed[String2Layer('Top Paste')]          := CheckBoxOthers.Checked;
   Board.LayerIsDisplayed[String2Layer('Bottom Paste')]       := CheckBoxOthers.Checked;

   Board.LayerIsDisplayed[String2Layer('Top Overlay')]        := CheckBoxOthers.Checked;
   Board.LayerIsDisplayed[String2Layer('Bottom Overlay')]     := CheckBoxOthers.Checked;

   Board.LayerIsDisplayed[String2Layer('Drill Guide')]        := CheckBoxOthers.Checked;
   Board.LayerIsDisplayed[String2Layer('Drill Drawing')]      := CheckBoxOthers.Checked;

   Board.LayerIsDisplayed[String2Layer('Multi Layer')]        := CheckBoxCopper.Checked;
   Board.LayerIsDisplayed[String2Layer('Keep Out Layer')]     := CheckBoxOthers.Checked;

   if TabControlLayers.TabIndex = 2 then
   begin
      CheckBox1.Checked  := CheckBoxOthers.Checked;
      CheckBox2.Checked  := CheckBoxOthers.Checked;

      CheckBox4.Checked  := CheckBoxOthers.Checked;
      CheckBox5.Checked  := CheckBoxOthers.Checked;

      CheckBox7.Checked  := CheckBoxOthers.Checked;
      CheckBox8.Checked  := CheckBoxOthers.Checked;

      CheckBox10.Checked := CheckBoxOthers.Checked;
      CheckBox11.Checked := CheckBoxOthers.Checked;

      CheckBox13.Checked := CheckBoxCopper.Checked;
      CheckBox14.Checked := CheckBoxOthers.Checked;
   end;

   Board.ViewManager_UpdateLayerTabs;
   Board.ViewManager_FullUpdate;
end;



procedure TShowHideLayers.CheckBox1Click(Sender: TObject);
begin
   CB2Layer(CheckBox1);
end;

procedure TShowHideLayers.CheckBox2Click(Sender: TObject);
begin
   CB2Layer(CheckBox2);
end;

procedure TShowHideLayers.CheckBox3Click(Sender: TObject);
begin
   CB2Layer(CheckBox3);
end;

procedure TShowHideLayers.CheckBox4Click(Sender: TObject);
begin
   CB2Layer(CheckBox4);
end;

procedure TShowHideLayers.CheckBox5Click(Sender: TObject);
begin
   CB2Layer(CheckBox5);
end;

procedure TShowHideLayers.CheckBox6Click(Sender: TObject);
begin
   CB2Layer(CheckBox6);
end;

procedure TShowHideLayers.CheckBox7Click(Sender: TObject);
begin
   CB2Layer(CheckBox7);
end;

procedure TShowHideLayers.CheckBox8Click(Sender: TObject);
begin
   CB2Layer(CheckBox8);
end;

procedure TShowHideLayers.CheckBox9Click(Sender: TObject);
begin
   CB2Layer(CheckBox9);
end;

procedure TShowHideLayers.CheckBox10Click(Sender: TObject);
begin
   CB2Layer(CheckBox10);
end;

procedure TShowHideLayers.CheckBox11Click(Sender: TObject);
begin
   CB2Layer(CheckBox11);
end;

procedure TShowHideLayers.CheckBox12Click(Sender: TObject);
begin
   CB2Layer(CheckBox12);
end;

procedure TShowHideLayers.CheckBox13Click(Sender: TObject);
begin
   CB2Layer(CheckBox13);
end;

procedure TShowHideLayers.CheckBox14Click(Sender: TObject);
begin
   CB2Layer(CheckBox14);
end;

procedure TShowHideLayers.CheckBox15Click(Sender: TObject);
begin
   CB2Layer(CheckBox15);
end;

procedure TShowHideLayers.CheckBox16Click(Sender: TObject);
begin
   CB2Layer(CheckBox16);
end;

procedure TShowHideLayers.CheckBox17Click(Sender: TObject);
begin
   CB2Layer(CheckBox17);
end;

procedure TShowHideLayers.CheckBox18Click(Sender: TObject);
begin
   CB2Layer(CheckBox18);
end;

procedure TShowHideLayers.CheckBox19Click(Sender: TObject);
begin
   CB2Layer(CheckBox19);
end;

procedure TShowHideLayers.CheckBox20Click(Sender: TObject);
begin
   CB2Layer(CheckBox20);
end;

procedure TShowHideLayers.CheckBox21Click(Sender: TObject);
begin
   CB2Layer(CheckBox21);
end;

procedure TShowHideLayers.CheckBox22Click(Sender: TObject);
begin
   CB2Layer(CheckBox22);
end;

procedure TShowHideLayers.CheckBox23Click(Sender: TObject);
begin
   CB2Layer(CheckBox23);
end;

procedure TShowHideLayers.CheckBox24Click(Sender: TObject);
begin
   CB2Layer(CheckBox24);
end;

procedure TShowHideLayers.CheckBox25Click(Sender: TObject);
begin
   CB2Layer(CheckBox25);
end;

procedure TShowHideLayers.CheckBox26Click(Sender: TObject);
begin
   CB2Layer(CheckBox26);
end;

procedure TShowHideLayers.CheckBox27Click(Sender: TObject);
begin
   CB2Layer(CheckBox27);
end;

procedure TShowHideLayers.CheckBox28Click(Sender: TObject);
begin
   CB2Layer(CheckBox28);
end;

procedure TShowHideLayers.CheckBox29Click(Sender: TObject);
begin
   CB2Layer(CheckBox29);
end;

procedure TShowHideLayers.CheckBox30Click(Sender: TObject);
begin
   CB2Layer(CheckBox30);
end;

procedure TShowHideLayers.CheckBox31Click(Sender: TObject);
begin
   CB2Layer(CheckBox31);
end;

procedure TShowHideLayers.CheckBox32Click(Sender: TObject);
begin
   CB2Layer(CheckBox32);
end;



Procedure Start;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   TheLayerStack := Board.LayerStack;
   if TheLayerStack = nil then exit;

   ShowHideLayers.Show;
end;



