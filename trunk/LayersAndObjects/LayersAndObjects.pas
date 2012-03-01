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
{           This one was made from scratch, because I wanted to remove tabs    }
{           from "ShowHideLayers" script. It shows layers in blocks that can   }
{           be shown or hidden. it has couple of more enhancements than        }
{           "ShowHidelayers" script:                                           }
{           - Support for all 48 copper layers (you will never manage to fit   }
{             then on screen)                                                  }
{           - you can disable group of layers by clicking in the upper right   }
{             corner of the group. If you disable the group you still see "All"}
{             and "Visible" checkboxes                                         }
{           - "Visible" checkbox allows you to disable the group from view     }
{             without changing checkbox values                                 }
{           - Click on Layer Color makes that Layer Current Layer              }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{ Save position to ini file added by Tony Chilco 2011/12/14                    }
{ Tab object resize with main dialogue added by Tony Chilco 2011/12/14         }
{..............................................................................}

{..............................................................................}


var
   Board            : IPCB_Board;
   TheLayerStack    : IPCB_LayerStack;
   OnShow           : Boolean;
   Refresh          : Boolean;
   UpdateFromCB     : Boolean;
   SwitchGrayAndOff : Boolean;

function Int2CBCopper(i : Integer) : TCheckBox;
begin
   case i of
       1 : Result := CBCopper1;
       2 : Result := CBCopper2;
       3 : Result := CBCopper3;
       4 : Result := CBCopper4;
       5 : Result := CBCopper5;
       6 : Result := CBCopper6;
       7 : Result := CBCopper7;
       8 : Result := CBCopper8;
       9 : Result := CBCopper9;
      10 : Result := CBCopper10;
      11 : Result := CBCopper11;
      12 : Result := CBCopper12;
      13 : Result := CBCopper13;
      14 : Result := CBCopper14;
      15 : Result := CBCopper15;
      16 : Result := CBCopper16;
      17 : Result := CBCopper17;
      18 : Result := CBCopper18;
      19 : Result := CBCopper19;
      20 : Result := CBCopper20;
      21 : Result := CBCopper21;
      22 : Result := CBCopper22;
      23 : Result := CBCopper23;
      24 : Result := CBCopper24;
      25 : Result := CBCopper25;
      26 : Result := CBCopper26;
      27 : Result := CBCopper27;
      28 : Result := CBCopper28;
      29 : Result := CBCopper29;
      30 : Result := CBCopper30;
      31 : Result := CBCopper31;
      32 : Result := CBCopper32;
      33 : Result := CBCopper33;
      34 : Result := CBCopper34;
      35 : Result := CBCopper35;
      36 : Result := CBCopper36;
      37 : Result := CBCopper37;
      38 : Result := CBCopper38;
      39 : Result := CBCopper39;
      40 : Result := CBCopper40;
      41 : Result := CBCopper41;
      42 : Result := CBCopper42;
      43 : Result := CBCopper43;
      44 : Result := CBCopper44;
      45 : Result := CBCopper45;
      46 : Result := CBCopper46;
      47 : Result := CBCopper47;
      48 : Result := CBCopper48;
   end;
end;

function Int2ShapeCopper(i : Integer) : TShape;
begin
   case i of
       1 : Result := ShapeCopper1;
       2 : Result := ShapeCopper2;
       3 : Result := ShapeCopper3;
       4 : Result := ShapeCopper4;
       5 : Result := ShapeCopper5;
       6 : Result := ShapeCopper6;
       7 : Result := ShapeCopper7;
       8 : Result := ShapeCopper8;
       9 : Result := ShapeCopper9;
      10 : Result := ShapeCopper10;
      11 : Result := ShapeCopper11;
      12 : Result := ShapeCopper12;
      13 : Result := ShapeCopper13;
      14 : Result := ShapeCopper14;
      15 : Result := ShapeCopper15;
      16 : Result := ShapeCopper16;
      17 : Result := ShapeCopper17;
      18 : Result := ShapeCopper18;
      19 : Result := ShapeCopper19;
      20 : Result := ShapeCopper20;
      21 : Result := ShapeCopper21;
      22 : Result := ShapeCopper22;
      23 : Result := ShapeCopper23;
      24 : Result := ShapeCopper24;
      25 : Result := ShapeCopper25;
      26 : Result := ShapeCopper26;
      27 : Result := ShapeCopper27;
      28 : Result := ShapeCopper28;
      29 : Result := ShapeCopper29;
      30 : Result := ShapeCopper30;
      31 : Result := ShapeCopper31;
      32 : Result := ShapeCopper32;
      33 : Result := ShapeCopper33;
      34 : Result := ShapeCopper34;
      35 : Result := ShapeCopper35;
      36 : Result := ShapeCopper36;
      37 : Result := ShapeCopper37;
      38 : Result := ShapeCopper38;
      39 : Result := ShapeCopper39;
      40 : Result := ShapeCopper40;
      41 : Result := ShapeCopper41;
      42 : Result := ShapeCopper42;
      43 : Result := ShapeCopper43;
      44 : Result := ShapeCopper44;
      45 : Result := ShapeCopper45;
      46 : Result := ShapeCopper46;
      47 : Result := ShapeCopper47;
      48 : Result := ShapeCopper48;
   end;
end;

function Int2CBMech(i : Integer) : TCheckBox;
begin
   case i of
       1 : Result := CBMech1;
       2 : Result := CBMech2;
       3 : Result := CBMech3;
       4 : Result := CBMech4;
       5 : Result := CBMech5;
       6 : Result := CBMech6;
       7 : Result := CBMech7;
       8 : Result := CBMech8;
       9 : Result := CBMech9;
      10 : Result := CBMech10;
      11 : Result := CBMech11;
      12 : Result := CBMech12;
      13 : Result := CBMech13;
      14 : Result := CBMech14;
      15 : Result := CBMech15;
      16 : Result := CBMech16;
      17 : Result := CBMech17;
      18 : Result := CBMech18;
      19 : Result := CBMech19;
      20 : Result := CBMech20;
      21 : Result := CBMech21;
      22 : Result := CBMech22;
      23 : Result := CBMech23;
      24 : Result := CBMech24;
      25 : Result := CBMech25;
      26 : Result := CBMech26;
      27 : Result := CBMech27;
      28 : Result := CBMech28;
      29 : Result := CBMech29;
      30 : Result := CBMech30;
      31 : Result := CBMech31;
      32 : Result := CBMech32;
   end;
end;

function Int2ShapeMech(i : Integer) : TShape;
begin
   case i of
       1 : Result := ShapeMech1;
       2 : Result := ShapeMech2;
       3 : Result := ShapeMech3;
       4 : Result := ShapeMech4;
       5 : Result := ShapeMech5;
       6 : Result := ShapeMech6;
       7 : Result := ShapeMech7;
       8 : Result := ShapeMech8;
       9 : Result := ShapeMech9;
      10 : Result := ShapeMech10;
      11 : Result := ShapeMech11;
      12 : Result := ShapeMech12;
      13 : Result := ShapeMech13;
      14 : Result := ShapeMech14;
      15 : Result := ShapeMech15;
      16 : Result := ShapeMech16;
      17 : Result := ShapeMech17;
      18 : Result := ShapeMech18;
      19 : Result := ShapeMech19;
      20 : Result := ShapeMech20;
      21 : Result := ShapeMech21;
      22 : Result := ShapeMech22;
      23 : Result := ShapeMech23;
      24 : Result := ShapeMech24;
      25 : Result := ShapeMech25;
      26 : Result := ShapeMech26;
      27 : Result := ShapeMech27;
      28 : Result := ShapeMech28;
      29 : Result := ShapeMech29;
      30 : Result := ShapeMech30;
      31 : Result := ShapeMech31;
      32 : Result := ShapeMech32;
   end;
end;

Function Str2CBOther(Name : String): TCheckBox;
begin
   Case Name of
      'Top Overlay'        : Result := CBTopOverlay;
      'Bottom Overlay'     : Result := CBBottomOverlay;
      'Top Solder Mask'    : Result := CBTopSolder;
      'Bottom Solder Mask' : Result := CBBottomSolder;
      'Top Paste'          : Result := CBTopPaste;
      'Bottom Paste'       : Result := CBBottomPaste;
      'Drill Guide'        : Result := CBDrillGuide;
      'Drill Drawing'      : Result := CBDrillDraw;
      'Multi Layer'        : Result := CBMultiLayer;
      'Keep Out Layer'     : Result := CBKeepOut;
   end;
end;

Function Str2ShapeOther(Name : String): TShape;
begin
   Case Name of
      'Top Overlay'        : Result := ShapeTopOverlay;
      'Bottom Overlay'     : Result := ShapeBottomOverlay;
      'Top Solder Mask'    : Result := ShapeTopSolder;
      'Bottom Solder Mask' : Result := ShapeBottomSolder;
      'Top Paste'          : Result := ShapeTopPaste;
      'Bottom Paste'       : Result := ShapeBottomPaste;
      'Drill Guide'        : Result := ShapeDrillGuide;
      'Drill Drawing'      : Result := ShapeDrillDraw;
      'Multi Layer'        : Result := ShapeMultiLayer;
      'Keep Out Layer'     : Result := ShapeKeepOut;
   end;
end;

function OtherEnabled(dummy : String) : Boolean;
begin
   Result := False;
   if      Board.LayerIsDisplayed[String2Layer('Top Overlay')]        then Result := True
   else if Board.LayerIsDisplayed[String2Layer('Bottom Overlay')]     then Result := True
   else if Board.LayerIsDisplayed[String2Layer('Top Solder Mask')]    then Result := True
   else if Board.LayerIsDisplayed[String2Layer('Bottom Solder Mask')] then Result := True
   else if Board.LayerIsDisplayed[String2Layer('Top Paste')]          then Result := True
   else if Board.LayerIsDisplayed[String2Layer('Bottom Paste')]       then Result := True
   else if Board.LayerIsDisplayed[String2Layer('Drill Guide')]        then Result := True
   else if Board.LayerIsDisplayed[String2Layer('Drill Drawing')]      then Result := True
   else if Board.LayerIsDisplayed[String2Layer('Keep Out Layer')]     then Result := True
   else if Board.LayerIsDisplayed[String2Layer('Multi Layer')]        then Result := True;

end;

function OtherDisabled(dummy : String) : Boolean;
begin
   Result := False;
   if      not Board.LayerIsDisplayed[String2Layer('Top Overlay')]        then Result := True
   else if not Board.LayerIsDisplayed[String2Layer('Bottom Overlay')]     then Result := True
   else if not Board.LayerIsDisplayed[String2Layer('Top Solder Mask')]    then Result := True
   else if not Board.LayerIsDisplayed[String2Layer('Bottom Solder Mask')] then Result := True
   else if not Board.LayerIsDisplayed[String2Layer('Top Paste')]          then Result := True
   else if not Board.LayerIsDisplayed[String2Layer('Bottom Paste')]       then Result := True
   else if not Board.LayerIsDisplayed[String2Layer('Drill Guide')]        then Result := True
   else if not Board.LayerIsDisplayed[String2Layer('Drill Drawing')]      then Result := True
   else if not Board.LayerIsDisplayed[String2Layer('Keep Out Layer')]     then Result := True
   else if not Board.LayerIsDisplayed[String2Layer('Multi Layer')]        then Result := True;
end;

procedure TFormLayersPanel.FormLayersPanelShow(Sender: TObject);
var
   LayerObj         : IPCB_LayerObject;
   MechLayer        : IPCB_MechanicalLayer;
   i, j             : Integer;
   GetCB            : TCheckBox;

   Enabled          : Boolean;
   Disabled         : Boolean;

   SignalsEnabled   : Boolean;
   SignalsDisabled  : Boolean;
   PlanesEnabled    : Boolean;
   PlanesDisabled   : Boolean;
   Signals          : IPCB_LayerSet;

   UnPairedEnabled  : Boolean;
   UnPairedDisabled : Boolean;
   PairedEnabled    : Boolean;
   PairedDisabled   : Boolean;

   Shape            : TShape;
begin
   // On Show We have to set up all checkboxes based on current view,
   // number of mech and copper layers, etc...
   Board := PCBServer.GetCurrentPCBBoard;
   TheLayerStack := Board.LayerStack;

   Enabled         := False;
   Disabled        := False;
   SignalsEnabled  := False;
   SignalsDisabled := False;
   PlanesEnabled   := False;
   PlanesDisabled  := False;
   Signals         := LayerSet.SignalLayers;

   GroupBoxObjects.Top  := 16;
   GroupBoxObjects.Left := 16;

   GroupBoxCopper.Top := GroupBoxObjects.Top + GroupBoxObjects.Height + 10;
   GroupBoxCopper.Left := GroupBoxObjects.Left;

   if TheLayerStack.LayersInStackCount <> TheLayerStack.SignalLayerCount then
   begin
      CBSignals.Enabled := True;
      CBSignals.Visible := True;
      CBPlanes.Enabled  := True;
      CBPlanes.Visible  := True;
   end;

   // Copper
   LayerObj := TheLayerStack.FirstLayer;
   i := 1;
   while (LayerObj <> nil) and (i <= 32) do
   begin
      if LayerObj.IsDisplayed[Board] then Enabled := True
      else                                Disabled := True;

      if Signals.Contains(LayerObj.V7_LayerID) then
      begin
         if LayerObj.IsDisplayed[Board] then SignalsEnabled := True
         else                                SignalsDisabled := True;
      end
      else
      begin
         if LayerObj.IsDisplayed[Board] then PlanesEnabled := True
         else                                PlanesDisabled := True;
      end;

      GetCB := Int2CBCopper(i);
      GetCB.Visible := True;
      GetCB.Enabled := True;
      GetCB.Caption := LayerObj.Name;
      GetCB.Width := FormLayersPanel.Canvas.TextWidth(LayerObj.Name) + 16;
      GetCB.Checked := LayerObj.IsDisplayed[Board];

      Shape := Int2ShapeCopper(i);
      Shape.Visible := True;
      Shape.Enabled := True;
      Shape.Brush.Color := Board.LayerColor[LayerObj.LayerID];

      Inc(i);
      LayerObj := TheLayerStack.NextLayer(LayerObj);
   end;

   if      Enabled  = False        then CBCopperAll.Checked := False
   else if Disabled = False        then CBCopperAll.Checked := True
   else                                 CBCopperAll.State   := cbGrayed;

   if      SignalsEnabled  = False then CBSignals.Checked := False
   else if SignalsDisabled = False then CBSignals.Checked := True
   else                                 CBSignals.State   := cbGrayed;

   if      PlanesEnabled  = False  then CBPlanes.Checked := False
   else if PlanesDisabled = False  then CBPlanes.Checked := True
   else                                 CBPlanes.State   := cbGrayed;


   // Now we need to modify size of copper group,
   // and mech groupbox position

   GetCB := Int2CBCopper(i);
   GroupBoxCopper.Height := GetCB.Top + 10;
   GroupBoxMech.Left := GroupBoxCopper.Left;
   GroupBoxMech.Top  := GroupBoxCopper.Top + GroupBoxCopper.Height + 10;

   Enabled  := False;
   Disabled := False;

   UnPairedEnabled  := False;
   UnPairedDisabled := False;
   PairedEnabled    := False;
   PairedDisabled   := False;

   j := 1;
   for i := 1 to 32 do
   begin
      MechLayer := TheLayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

      if MechLayer.MechanicalLayerEnabled then
      begin

         if MechLayer.LinkToSheet = False then
         begin
            if MechLayer.IsDisplayed[Board] then Enabled := True
            else                                 Disabled := True;

            if Board.MechanicalPairs.LayerUsed(ILayer.MechanicalLayer(j)) then
            begin
               if MechLayer.IsDisplayed[Board] then PairedEnabled := True
               else                                 PairedDisabled := True;
            end
            else
            begin
               if MechLayer.IsDisplayed[Board] then UnPairedEnabled := True
               else                                 UnPairedDisabled := True;
            end;
         end;

         GetCB := Int2CBMech(j);
         GetCB.Visible := True;
         if MechLayer.LinkToSheet = False then
            GetCB.Enabled := True;
         GetCB.Width := FormLayersPanel.Canvas.TextWidth(MechLayer.Name) + 16;
         GetCb.Caption := MechLayer.Name;
         GetCB.Checked := MechLayer.IsDisplayed[Board];

         Shape := Int2ShapeMech(j);
         Shape.Visible := True;
         Shape.Enabled := True;

         Shape.Brush.Color := PCBServer.SystemOptions.LayerColors[MechLayer.V7_LayerID];

         inc(j);
      end;
   end;

   if      Enabled  = False then CBMechAll.Checked := False
   else if Disabled = False then CBMechAll.Checked := True
   else                          CBMechAll.State   := cbGrayed;
     
   if      PairedEnabled  = False then CBPaired.Checked := False
   else if PairedDisabled = False then CBPaired.Checked := True
   else                                CBPaired.State   := cbGrayed;

   if      UnPairedEnabled  = False then CBUnPaired.Checked := False
   else if UnPairedDisabled = False then CBUnPaired.Checked := True
   else                                  CBUnPaired.State   := cbGrayed;


   // Now we need to modify size of mech group,
   // and other groupbox position

   if j = 1 then
      GroupBoxMech.Height := 48
   else
   begin
      GetCB := Int2CBMech(j-1);
      GroupBoxMech.Height := GetCB.Top + 30;
      if Board.MechanicalPairs.Count > 0 then
      begin
         CBPaired.Visible := True;
         CBPaired.Enabled := True;

         CBUnPaired.Visible := True;
         CBUnPaired.Enabled := True;
      end;
   end;
      
   GroupBoxOther.Left  := GroupBoxMech.Left;
   GroupBoxOther.Top   := GroupBoxMech.Top + GroupBoxMech.Height + 10;

   Enabled  := False;
   Disabled := False;

   CBTopOverlay.Checked := Board.LayerIsDisplayed[String2Layer('Top Overlay')];
   ShapeTopOverlay.Brush.Color := Board.LayerColor[String2Layer('Top Overlay')];

   CBBottomOverlay.Checked := Board.LayerIsDisplayed[String2Layer('Bottom Overlay')];
   ShapeBottomOverlay.Brush.Color := Board.LayerColor[String2Layer('Bottom Overlay')];

   CBTopSolder.Checked := Board.LayerIsDisplayed[String2Layer('Top Solder Mask')];
   ShapeTopSolder.Brush.Color := Board.LayerColor[String2Layer('Top Solder Mask')];

   CBBottomSolder.Checked := Board.LayerIsDisplayed[String2Layer('Bottom Solder Mask')];
   ShapeBottomSolder.Brush.Color := Board.LayerColor[String2Layer('Bottom Solder Mask')];

   CBTopPaste.Checked := Board.LayerIsDisplayed[String2Layer('Top Paste')];
   ShapeTopPaste.Brush.Color := Board.LayerColor[String2Layer('Top Paste')];

   CBBottomPaste.Checked := Board.LayerIsDisplayed[String2Layer('Bottom Paste')];
   ShapeBottomPaste.Brush.Color := Board.LayerColor[String2Layer('Bottom Paste')];

   CBDrillGuide.Checked := Board.LayerIsDisplayed[String2Layer('Drill Guide')];
   ShapeDrillGuide.Brush.Color := Board.LayerColor[String2Layer('Drill Guide')];

   CBDrillDraw.Checked := Board.LayerIsDisplayed[String2Layer('Drill Drawing')];
   ShapeDrillDraw.Brush.Color := Board.LayerColor[String2Layer('Drill Drawing')];

   CBMultiLayer.Checked := Board.LayerIsDisplayed[String2Layer('Multi Layer')];
   ShapeMultiLayer.Brush.Color := Board.LayerColor[String2Layer('Multi Layer')];

   CBKeepOut.Checked := Board.LayerIsDisplayed[String2Layer('Keep Out Layer')];
   ShapeKeepOut.Brush.Color := Board.LayerColor[String2Layer('Keep Out Layer')];


   if      OtherEnabled('')  = False then CBOtherAll.Checked := False
   else if OtherDisabled('') = False then CBOtherAll.Checked := True
   else                                   CBOtherAll.State   := cbGrayed;

   if (not CBTopOverlay.Checked) and (not CBBottomOverlay.Checked) then CBOverlay.Checked := False
   else if CBTopOverlay.Checked  and      CBBottomOverlay.Checked  then CBOverlay.Checked := True
   else                                                                 CBOverlay.State   := cbGrayed;

   if (not CBTopSolder.Checked) and (not CBBottomSolder.Checked) and (not CBTopPaste.Checked) and (not CBBottomPaste.Checked) then CBMask.Checked := False
   else if CBTopSolder.Checked  and      CBBottomSolder.Checked  and      CBTopPaste.Checked  and      CBBottomPaste.Checked  then CBMask.Checked := True
   else                                                                                                                            CBMask.State   := cbGrayed;

   if (not CBDrillDraw.Checked) and (not CBDrillGuide.Checked) then CBDrill.Checked := False
   else if CBDrillDraw.Checked  and      CBDrillGuide.Checked  then CBDrill.Checked := True
   else                                                             CBDrill.State   := cbGrayed;

   if (not CBKeepOut.Checked) and (not CBMultiLayer.Checked) then CBOther.Checked := False
   else if CBKeepOut.Checked  and      CBMultiLayer.Checked  then CBOther.Checked := True
   else                                                           CBOther.State   := cbGrayed;

   GroupBoxOther.Height := cbMultiLayer.Top + 30;
   FormLayersPanel.Width := 3 * GroupBoxCopper.Left + GroupBoxCopper.Width;
   FormLayersPanel.Height:= GroupBoxOther.Top + GroupBoxOther.Height + 50;

   // finally read are connection lines shown
   CBConnections.Checked := Board.LayerIsDisplayed[eConnectLayer];
end;

Procedure WriteToIniFile(AFileName : String);
Var
   IniFile : TIniFile;
Begin
   IniFile := TIniFile.Create(AFileName);

   IniFile.WriteInteger('Window', 'DialogueTop',    FormLayersPanel.Top);
   IniFile.WriteInteger('Window', 'DialogueHeight', FormLayersPanel.Height);
   IniFile.WriteInteger('Window', 'DialogueLeft',   FormLayersPanel.Left);
   IniFile.WriteInteger('Window', 'DialogueWidth',  FormLayersPanel.Width);
   IniFile.WriteBool   ('Window', 'ObjectsShown',   ImageArrowUpObjects.Enabled);
   IniFile.WriteBool   ('Window', 'CopperShown',    ImageArrowUpCopper.Enabled);
   IniFile.WriteBool   ('Window', 'MechShown',      ImageArrowUpMech.Enabled);
   IniFile.WriteBool   ('Window', 'OtherShown',     ImageArrowUpOther.Enabled);

   IniFile.Free;
End;

Procedure ReadFromIniFile(AFileName : String);
Var
   IniFile : TIniFile;
Begin
   IniFile := TIniFile.Create(AFileName);

   FormLayersPanel.Top        := IniFile.ReadInteger('Window', 'DialogueTop',    FormLayersPanel.Top);
   FormLayersPanel.Height     := IniFile.ReadInteger('Window', 'DialogueHeight', FormLayersPanel.Height);
   FormLayersPanel.Left       := IniFile.ReadInteger('Window', 'DialogueLeft',   FormLayersPanel.Left);
   FormLayersPanel.Width      := IniFile.ReadInteger('Window', 'DialogueWidth',  FormLayersPanel.Width);

   if IniFile.ReadBool('Window', 'ObjectsShown', True) = False then
   begin
      GroupBoxObjects.Height := 48;
      GroupBoxCopper.Top    := GroupBoxObjects.Top + GroupBoxObjects.Height + 10;
      GroupBoxMech.Top      := GroupBoxCopper.Top + GroupBoxCopper.Height + 10;
      GroupBoxOther.Top     := GroupBoxMech.Top + GroupBoxMech.Height + 10;

      ImageArrowUpObjects.Visible := False;
      ImageArrowUpObjects.Enabled := False;

      ImageArrowDownObjects.Visible := True;
      ImageArrowDownObjects.Enabled := True;
   end;

   if IniFile.ReadBool('Window', 'CopperShown', True) = False then
   begin
      GroupBoxCopper.Height := 48;
      GroupBoxMech.Top      := GroupBoxCopper.Top + GroupBoxCopper.Height + 10;
      GroupBoxOther.Top     := GroupBoxMech.Top + GroupBoxMech.Height + 10;

      ImageArrowUpCopper.Visible := False;
      ImageArrowUpCopper.Enabled := False;

      ImageArrowDownCopper.Visible := True;
      ImageArrowDownCopper.Enabled := True;
   end;

   if IniFile.ReadBool('Window', 'MechShown', True) = False then
   begin
      GroupBoxMech.Height := 48;
      GroupBoxOther.Top   := GroupBoxMech.Top + GroupBoxMech.Height + 10;

      ImageArrowUpMech.Visible := False;
      ImageArrowUpMech.Enabled := False;

      ImageArrowDownMech.Visible := True;
      ImageArrowDownMech.Enabled := True;
   end;

   if IniFile.ReadBool('Window', 'OtherShown', True) = False then
   begin
      GroupBoxOther.Height := 48;

      ImageArrowUpOther.Visible := False;
      ImageArrowUpOther.Enabled := False;

      ImageArrowDownOther.Visible := True;
      ImageArrowDownOther.Enabled := True;
   end;

   FormLayersPanel.Height:= GroupBoxOther.Top + GroupBoxOther.Height + 50;
   IniFile.Free;
End;

Procedure Start;
var
   iniFile : TIniFile;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   TheLayerStack := Board.LayerStack;
   if TheLayerStack = nil then exit;

   UpdateFromCB := False;
   Refresh      := False;

   FormlayersPanel.Show;

   ReadFromIniFile(ClientAPI_SpecialFolder_AltiumApplicationData + '\LayersPanelScriptData.ini');

   UpdateFromCB     := True;
   Refresh          := True;
   SwitchGrayAndOff := True;
end;

procedure TFormLayersPanel.FormLayersPanelClose(Sender: TObject; var Action: TCloseAction);
begin
   WriteToIniFile(ClientAPI_SpecialFolder_AltiumApplicationData + '\LayersPanelScriptData.ini');
end;

procedure TFormLayersPanel.FormLayersPanelResize(Sender: TObject);
begin
   if FormLayersPanel.Width < 254 then
      FormLayersPanel.Width := 254
   else
   begin
      GroupBoxObjects.Width := FormLayersPanel.Width - 50;
      GroupBoxCopper.Width  := FormLayersPanel.Width - 50;
      GroupBoxMech.Width    := FormLayersPanel.Width - 50;
      GroupBoxOther.Width   := FormLayersPanel.Width - 50;

      Image1.Width := GroupBoxCopper.Width - 2;
      Image2.Width := GroupBoxMech.Width - 2;
      Image3.Width := GroupBoxOther.Width - 2;
      Image4.Width := GroupBoxOther.Width - 2;

      ImageArrowUpCopper.Left    := GroupBoxCopper.Width - 21;
      ImageArrowDownCopper.Left  := GroupBoxCopper.Width - 21;
      ImageArrowUpMech.Left      := GroupBoxMech.Width - 21;
      ImageArrowDownMech.Left    := GroupBoxMech.Width - 21;
      ImageArrowUpOther.Left     := GroupBoxOther.Width - 21;
      ImageArrowDownOther.Left   := GroupBoxOther.Width - 21;
      ImageArrowUpObjects.Left   := GroupBoxOther.Width - 21;
      ImageArrowDownObjects.Left := GroupBoxOther.Width - 21;

      CBCopperAll.Left      := GroupBoxCopper.Width - 37;
      CBCopperSelected.Left := GroupBoxCopper.Width - 101;
      CBSignals.Left        := GroupBoxCopper.Width - 90;
      CBPlanes.Left         := GroupBoxCopper.Width - 87;

      CBMechAll.Left        := GroupBoxMech.Width - 37;
      CBMechSelected.Left   := GroupBoxMech.Width - 101;
      CBPaired.Left         := GroupBoxMech.Width - 57;
      CBUnPaired.Left       := GroupBoxMech.Width - 69;

      CBOtherAll.Left       := GroupBoxOther.Width - 37;
      CBOtherSelected.Left  := GroupBoxOther.Width - 101;
      CBOverlay.Left        := GroupBoxOther.Width - 63;
      CBMask.Left           := GroupBoxOther.Width - 49;
      CBDrill.Left          := GroupBoxOther.Width - 42;
      CBOther.Left          := GroupBoxOther.Width - 53;

      CBObjectsAll.Left     := GroupBoxOther.Width - 37;
      CBShowobjects.Left    := GroupBoxOther.Width - 101;    
   end;
end;

procedure TFormLayersPanel.ImageArrowUpCopperClick(Sender: TObject);
begin
   GroupBoxCopper.Height := 48;
   GroupBoxMech.Top      := GroupBoxCopper.Top + GroupBoxCopper.Height + 10;
   GroupBoxOther.Top     := GroupBoxMech.Top + GroupBoxMech.Height + 10;

   FormLayersPanel.Height:= GroupBoxOther.Top + GroupBoxOther.Height + 50;

   ImageArrowUpCopper.Visible := False;
   ImageArrowUpCopper.Enabled := False;

   ImageArrowDownCopper.Visible := True;
   ImageArrowDownCopper.Enabled := True;
end;

procedure TFormLayersPanel.ImageArrowDownCopperClick(Sender: TObject);
var
   i     : Integer;
   GetCB : TCheckBox;
   Flag  : Boolean;
   Height: Integer;
begin
   Flag := True;
   for i := 1 to 48 do
   begin
      GetCB := Int2CBCopper(i);
      if (GetCB.Visible = False) then
      begin
         Flag := False;
         break;
      end;
   end;

   Height := GetCB.Top + 10;

   if Flag then Height := Height + 20;

   GroupBoxCopper.Height := Height;
   GroupBoxMech.Top      := GroupBoxCopper.Top + GroupBoxCopper.Height + 10;
   GroupBoxOther.Top     := GroupBoxMech.Top + GroupBoxMech.Height + 10;

   FormLayersPanel.Height:= GroupBoxOther.Top + GroupBoxOther.Height + 50;

   ImageArrowUpCopper.Visible := True;
   ImageArrowUpCopper.Enabled := True;

   ImageArrowDownCopper.Visible := False;
   ImageArrowDownCopper.Enabled := False;
end;

procedure TFormLayersPanel.ImageArrowUpMechClick(Sender: TObject);
begin
   GroupBoxMech.Height := 48;
   GroupBoxOther.Top   := GroupBoxMech.Top + GroupBoxMech.Height + 10;

   FormLayersPanel.Height:= GroupBoxOther.Top + GroupBoxOther.Height + 50;

   ImageArrowUpMech.Visible := False;
   ImageArrowUpMech.Enabled := False;

   ImageArrowDownMech.Visible := True;
   ImageArrowDownMech.Enabled := True;

end;

procedure TFormLayersPanel.ImageArrowDownMechClick(Sender: TObject);
var
   i : Integer;
   GetCB : TCheckBox;
   Height : Integer;
   Flag : Boolean;

begin
   Flag := True;

   for i := 1 to 32 do
   begin
      GetCB := Int2CBMech(i);
      if (GetCB.Visible = False) then
      begin
         Flag := False;
         break;
      end;
   end;

   Height := GetCB.Top + 10;

   if Flag then  Height := Height + 20;
   if i = 1 then Height := 48;

   GroupBoxMech.Height := Height;
   GroupBoxOther.Top   := GroupBoxMech.Top + GroupBoxMech.Height + 10;

   FormLayersPanel.Height:= GroupBoxOther.Top + GroupBoxOther.Height + 50;

   ImageArrowUpMech.Visible := True;
   ImageArrowUpMech.Enabled := True;

   ImageArrowDownMech.Visible := False;
   ImageArrowDownMech.Enabled := False;

end;

procedure TFormLayersPanel.ImageArrowUpOtherClick(Sender: TObject);
begin
   GroupBoxOther.Height := 48;

   FormLayersPanel.Height:= GroupBoxOther.Top + GroupBoxOther.Height + 50;

   ImageArrowUpOther.Visible := False;
   ImageArrowUpOther.Enabled := False;

   ImageArrowDownOther.Visible := True;
   ImageArrowDownOther.Enabled := True;

end;

procedure TFormLayersPanel.ImageArrowDownOtherClick(Sender: TObject);
begin
   GroupBoxOther.Height := cbMultiLayer.Top + 30;

   FormLayersPanel.Height:= GroupBoxOther.Top + GroupBoxOther.Height + 50;

   ImageArrowDownOther.Visible := False;
   ImageArrowDownOther.Enabled := False;

   ImageArrowUpOther.Visible := True;
   ImageArrowUpOther.Enabled := True;

end;

procedure TFormLayersPanel.CBCopperSelectedClick(Sender: TObject);
Var
   LayerObj         : IPCB_LayerObject;
   i                : Integer;
   CB               : TCheckBox;
   Enabled          : Boolean;
   Disabled         : Boolean;
   SignalsEnabled   : Boolean;
   SignalsDisabled  : Boolean;
   PlanesEnabled    : Boolean;
   PlanesDisabled   : Boolean;
   Signals          : IPCB_LayerSet;
begin

   UpdateFromCB    := False;
   Enabled         := False;
   Disabled        := False;
   SignalsEnabled  := False;
   SignalsDisabled := False;
   PlanesEnabled   := False;
   PlanesDisabled  := False;
   Signals         := LayerSet.SignalLayers;

   if CBCopperSelected.Checked then
   begin
      LayerObj := TheLayerStack.FirstLayer;
      i := 1;

      while (LayerObj <> nil) do
      begin

         CB := Int2CBCopper(i);

         if CB.State = cbGrayed then
            CB.Checked := True;

         LayerObj.IsDisplayed[Board] := CB.Checked;

         if CB.Checked and Refresh then
         Begin
           Board.CurrentLayer := LayerObj.LayerID;
           Refresh := False;
         end;

         if Signals.Contains(LayerObj.V7_LayerID) then
         begin
            if LayerObj.IsDisplayed[Board] then SignalsEnabled := True
            else                                SignalsDisabled := True;
         end
         else
         begin
            if LayerObj.IsDisplayed[Board] then PlanesEnabled := True
            else                                PlanesDisabled := True;
         end;

         if LayerObj.IsDisplayed[Board] then Enabled := True
         else                                Disabled := True;

         Inc(i);
         LayerObj := TheLayerStack.NextLayer(LayerObj);
      end;

      if      Enabled  = False then CBCopperAll.Checked := False
      else if Disabled = False then CBCopperAll.Checked := True
      else                          CBCopperAll.State   := cbGrayed;

      if      SignalsEnabled  = False then CBSignals.Checked := False
      else if SignalsDisabled = False then CBSignals.Checked := True
      else                                 CBSignals.State   := cbGrayed;

      if      PlanesEnabled  = False then CBPlanes.Checked := False
      else if PlanesDisabled = False then CBPlanes.Checked := True
      else                                CBPlanes.State   := cbGrayed;

      Refresh := True;
   end
   else
   begin
      LayerObj := TheLayerStack.FirstLayer;
      i := 1;

      while (LayerObj <> nil) do
      begin
         if LayerObj.IsDisplayed[Board] then
         begin

            LayerObj.IsDisplayed[Board] := False;
            CB := Int2CBCopper(i);
            CB.State := cbGrayed;

         end;
         inc(i);
         LayerObj := TheLayerStack.NextLayer(LayerObj);
      end;
      CBCopperAll.Checked := False;
      CBSignals.Checked   := False;
      CBPlanes.Checked    := False;
   end;

   Board.ViewManager_FullUpdate;
   Board.ViewManager_UpdateLayerTabs;
   UpdateFromCB := True;
end;

procedure TFormLayersPanel.CBMechSelectedClick(Sender: TObject);
var
   i, j             : Integer;
   MechLayer        : IPCB_MechanicalLayer;
   CB               : TCheckBox;
   Enabled          : Boolean;
   Disabled         : Boolean;
   UnPairedEnabled  : Boolean;
   UnPairedDisabled : Boolean;
   PairedEnabled    : Boolean;
   PairedDisabled   : Boolean;
begin

   j := 1;

   UpdateFromCB     := False;
   Enabled          := False;
   Disabled         := False;
   UnPairedEnabled  := False;
   UnPairedDisabled := False;
   PairedEnabled    := False;
   PairedDisabled   := False;

   if CBMechSelected.Checked then
   begin
      for i := 1 to 32 do
      begin
         MechLayer := TheLayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

         if MechLayer.MechanicalLayerEnabled then
         begin
            if MechLayer.LinkToSheet = False then
            begin
               CB := Int2CBMech(j);
               if CB.State = cbGrayed then
                  CB.Checked := True;

               MechLayer.IsDisplayed[Board] := CB.Checked;

               if CB.Checked and Refresh then
               begin
                  Board.CurrentLayer := MechLayer.V7_LayerID;
                  Refresh := False;
               end;

               if MechLayer.IsDisplayed[Board] then Enabled := True
               else                                 Disabled := True;

               if Board.MechanicalPairs.LayerUsed(ILayer.MechanicalLayer(j)) then
               begin
                  if MechLayer.IsDisplayed[Board] then PairedEnabled := True
                  else                                 PairedDisabled := True;
               end
               else
               begin
                  if MechLayer.IsDisplayed[Board] then UnPairedEnabled := True
                  else                                 UnPairedDisabled := True;
               end;
            end;

            Inc(j);
         end;
      end;

      if      Enabled  = False         then CBMechAll.Checked  := False
      else if Disabled = False         then CBMechAll.Checked  := True
      else                                  CBMechAll.State    := cbGrayed;

      if      PairedEnabled  = False   then CBPaired.Checked   := False
      else if PairedDisabled = False   then CBPaired.Checked   := True
      else                                  CBPaired.State     := cbGrayed;

      if      UnPairedEnabled  = False then CBUnPaired.Checked := False
      else if UnPairedDisabled = False then CBUnPaired.Checked := True
      else                                  CBUnPaired.State   := cbGrayed;

      Refresh := True;
   end
   else
   begin
      j := 1;
      for i := 1 to 32 do
      begin
         MechLayer := TheLayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

         if MechLayer.MechanicalLayerEnabled then
         begin
            if MechLayer.IsDisplayed[Board] and (MechLayer.LinkToSheet = False) then
            begin
               MechLayer.IsDisplayed[Board] := False;
               CB := Int2CBMech(j);
               CB.State := cbGrayed;
            end;
            inc(j);
         end;
         CBMechAll.Checked  := False;
         CBPaired.Checked   := False;
         CBUnPaired.Checked := False;
      end;
   end;

   Board.ViewManager_FullUpdate;
   Board.ViewManager_UpdateLayerTabs;

   UpdateFromCB := True;
end;

procedure OtherSelectedON(Layer : String);
var
   CB : TCheckBox;
begin
   CB := Str2CBOther(Layer);

   if CB.State = cbGrayed then
      CB.Checked := True;

   Board.LayerIsDisplayed[String2Layer(Layer)] := CB.Checked;
   if Refresh and CB.Checked then
   begin
      Board.CurrentLayer := String2Layer(Layer);
      Refresh := False;
   end;
end;

procedure OtherSelectedOFF(Layer : String);
var
   CB : TCheckBox;
begin
   CB := Str2CBOther(Layer);

   if CB.Checked then
   begin
      CB.State := cbGrayed;
      Board.LayerIsDisplayed[String2Layer(Layer)] := False;
   end;
end;

procedure TFormLayersPanel.CBOtherSelectedClick(Sender: TObject);
begin
   UpdateFromCB := False;

   if CBOtherSelected.Checked then
   begin
      OtherSelectedON('Top Overlay');
      OtherSelectedON('Bottom Overlay');
      OtherSelectedON('Top Solder Mask');
      OtherSelectedON('Bottom Solder Mask');
      OtherSelectedON('Top Paste');
      OtherSelectedON('Bottom Paste');
      OtherSelectedON('Drill Guide');
      OtherSelectedON('Drill Drawing');
      OtherSelectedON('Keep Out Layer');
      OtherSelectedON('Multi Layer');

      if      OtherEnabled('')  = False then CBOtherAll.Checked := False
      else if OtherDisabled('') = False then CBOtherAll.Checked := True
      else                                   CBOtherAll.State   := cbGrayed;

      if (not CBTopOverlay.Checked) and (not CBBottomOverlay.Checked) then CBOverlay.Checked := False
      else if CBTopOverlay.Checked  and      CBBottomOverlay.Checked  then CBOverlay.Checked := True
      else                                                                 CBOverlay.State   := cbGrayed;

      if (not CBTopSolder.Checked) and (not CBBottomSolder.Checked) and (not CBTopPaste.Checked) and (not CBBottomPaste.Checked) then CBMask.Checked := False
      else if CBTopSolder.Checked  and      CBBottomSolder.Checked  and      CBTopPaste.Checked  and      CBBottomPaste.Checked  then CBMask.Checked := True
      else                                                                                                                            CBMask.State   := cbGrayed;

      if (not CBDrillDraw.Checked) and (not CBDrillGuide.Checked) then CBDrill.Checked := False
      else if CBDrillDraw.Checked  and      CBDrillGuide.Checked  then CBDrill.Checked := True
      else                                                             CBDrill.State   := cbGrayed;

      if (not CBKeepOut.Checked) and (not CBMultiLayer.Checked) then CBOther.Checked := False
      else if CBKeepOut.Checked  and      CBMultiLayer.Checked  then CBOther.Checked := True
      else                                                           CBOther.State   := cbGrayed;

      Refresh := True;
   end
   else
   begin
      OtherSelectedOFF('Top Overlay');
      OtherSelectedOFF('Bottom Overlay');
      OtherSelectedOFF('Top Solder Mask');
      OtherSelectedOFF('Bottom Solder Mask');
      OtherSelectedOFF('Top Paste');
      OtherSelectedOFF('Bottom Paste');
      OtherSelectedOFF('Drill Guide');
      OtherSelectedOFF('Drill Drawing');
      OtherSelectedOFF('Keep Out Layer');
      OtherSelectedOFF('Multi Layer');

      CBOtherAll.Checked := False;
      CBOverlay.Checked  := False;
      CBMask.Checked     := False;
      CBDrill.Checked    := False;
      CBOther.Checked    := False;
   end;

   UpdateFromCB := True;
   Board.ViewManager_FullUpdate;
   Board.ViewManager_UpdateLayerTabs;
end;

procedure TFormLayersPanel.CBCopperAllClick(Sender: TObject);
var
   LayerObj : IPCB_LayerObject;
   i        : integer;
   CB       : TCheckBox;
   Flag     : Boolean;
begin
   if (CBCopperAll.State <> cbGrayed) and UpdateFromCB then
   begin
      LayerObj     := TheLayerStack.FirstLayer;
      i            := 1;
      UpdateFromCB := False;
      Flag         := True;

      while (LayerObj <> nil) do
      begin
         CB := Int2CBCopper(i);

         LayerObj.IsDisplayed[Board] := CBCopperAll.Checked;
         CB.Checked := CBCopperAll.Checked;

         if CB.Checked and Flag then
            Board.CurrentLayer := LayerObj.LayerID;

         Flag := False;
         Inc(i);
         LayerObj := TheLayerStack.NextLayer(LayerObj);
      end;

      CBSignals.Checked := CBCopperAll.Checked;
      CBPlanes.Checked  := CBCopperAll.Checked;

      UpdateFromCB := True;
      Refresh := True;
      Board.ViewManager_FullUpdate;
      Board.ViewManager_UpdateLayerTabs;
   end;
end;

procedure TFormLayersPanel.CBMechAllClick(Sender: TObject);
var
   i, j      : Integer;
   MechLayer : IPCB_MechanicalLayer;
   CB        : TCheckBox;
   Flag      : Boolean;
begin
   if (CBMechAll.State <> cbGrayed) and UpdateFromCB then
   begin
      j            := 0;
      Flag         := True;
      UpdateFromCB := False;

      for i := 1 to 32 do
      begin
         MechLayer := TheLayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

         if MechLayer.MechanicalLayerEnabled then
         Begin
            Inc(j);

            if MechLayer.LinkToSheet = False then
            begin
               CB := Int2CBMech(j);
               MechLayer.IsDisplayed[Board] := CBMechAll.Checked;
               CB.Checked :=  CBMechAll.Checked;

               if Cb.Checked and Flag then
                  Board.CurrentLayer := MechLayer.V7_LayerID;

               Flag := False;
            end;
         end;
      end;

      CBPaired.Checked   := CBMechAll.Checked;
      CBUnPaired.Checked := CBMechAll.Checked;

      UpdateFromCB := True;
      Board.ViewManager_FullUpdate;
      Board.ViewManager_UpdateLayerTabs;
   end;
end;

procedure TFormLayersPanel.CBOtherAllClick(Sender: TObject);
begin
   if (CBOtherAll.State <> cbGrayed) and UpdateFromCB then
   begin
      Board.LayerIsDisplayed[String2Layer('Top Overlay')]        := CBOtherAll.Checked;
      Board.LayerIsDisplayed[String2Layer('Bottom Overlay')]     := CBOtherAll.Checked;
      Board.LayerIsDisplayed[String2Layer('Top Solder Mask')]    := CBOtherAll.Checked;
      Board.LayerIsDisplayed[String2Layer('Bottom Solder Mask')] := CBOtherAll.Checked;
      Board.LayerIsDisplayed[String2Layer('Top Paste')]          := CBOtherAll.Checked;
      Board.LayerIsDisplayed[String2Layer('Bottom Paste')]       := CBOtherAll.Checked;
      Board.LayerIsDisplayed[String2Layer('Drill Guide')]        := CBOtherAll.Checked;
      Board.LayerIsDisplayed[String2Layer('Drill Drawing')]      := CBOtherAll.Checked;
      Board.LayerIsDisplayed[String2Layer('Keep Out Layer')]     := CBOtherAll.Checked;
      Board.LayerIsDisplayed[String2Layer('Multi Layer')]        := CBOtherAll.Checked;

      UpdateFromCB := False;

      CBTopOverlay.Checked    := CBOtherAll.Checked;
      CBBottomOverlay.Checked := CBOtherAll.Checked;
      CBTopSolder.Checked     := CBOtherAll.Checked;
      CBBottomSolder.Checked  := CBOtherAll.Checked;
      CBTopPaste.Checked      := CBOtherAll.Checked;
      CBBottomPaste.Checked   := CBOtherAll.Checked;
      CBDrillGuide.Checked    := CBOtherAll.Checked;
      CBDrillDraw.Checked     := CBOtherAll.Checked;
      CBKeepOut.Checked       := CBOtherAll.Checked;
      CBMultiLayer.Checked    := CBOtherAll.Checked;
      CBOtherAll.Checked      := CBOtherAll.Checked;
      CBOverlay.Checked       := CBOtherAll.Checked;
      CBMask.Checked          := CBOtherAll.Checked;
      CBDrill.Checked         := CBOtherAll.Checked;
      CBOther.Checked         := CBOtherAll.Checked;

      UpdateFromCB := True;

      if CBOtherAll.Checked then
         Board.CurrentLayer := String2Layer('Top Overlay');

      Board.ViewManager_FullUpdate;
      Board.ViewManager_UpdateLayerTabs;
   end;
end;

procedure TFormLayersPanel.CBSignalsClick(Sender: TObject);
var
   LayerObj : IPCB_LayerObject;
   i        : integer;
   Flag     : Boolean;
   CB       : TCheckBox;
   Signals  : IPCB_LayerSet;
   Enabled  : Boolean;
   Disabled : Boolean;
begin
   if (CBSignals.State <> cbGrayed) and UpdateFromCB then
   begin
      LayerObj     := TheLayerStack.FirstLayer;
      i            := 1;
      Flag         := True;
      UpdateFromCB := False;
      Signals      := LayerSet.SignalLayers;
      Enabled      := False;
      Disabled     := False;

      while (LayerObj <> nil) do
      begin
         if Signals.Contains(LayerObj.V7_LayerID) then
         begin
            LayerObj.IsDisplayed[Board] := CBSignals.Checked;

            CB := Int2CBCopper(i);

            CB.Checked := CBSignals.Checked;

            if Flag and CB.Checked then
               Board.CurrentLayer := LayerObj.LayerID;

            Flag := False;
         end;

         if LayerObj.IsDisplayed[Board] then Enabled := True
         else                                Disabled := True;

         Inc(i);
         LayerObj := TheLayerStack.NextLayer(LayerObj);
      end;

      if      Enabled  = False then CBCopperAll.Checked := False
      else if Disabled = False then CBCopperAll.Checked := True
      else                          CBCopperAll.State   := cbGrayed;

      UpdateFromCB := True;
      Board.ViewManager_FullUpdate;
      Board.ViewManager_UpdateLayerTabs;
   end;
end;

procedure TFormLayersPanel.CBPlanesClick(Sender: TObject);
var
   LayerObj : IPCB_LayerObject;
   i        : integer;
   Flag     : Boolean;
   CB       : TCheckBox;
   Signals  : IPCB_LayerSet;
   Enabled  : Boolean;
   Disabled : Boolean;
begin
   if (CBPlanes.State <> cbGrayed) and UpdateFromCB then
   begin
      LayerObj     := TheLayerStack.FirstLayer;
      i            := 1;
      Flag         := True;
      UpdateFromCB := False;
      Signals      := LayerSet.SignalLayers;
      Enabled      := False;
      Disabled     := False;

      while (LayerObj <> nil) do
      begin
         if not Signals.Contains(LayerObj.V7_LayerID) then
         begin
            LayerObj.IsDisplayed[Board] := CBPlanes.Checked;

            CB := Int2CBCopper(i);

            CB.Checked := CBPlanes.Checked;

            if Flag and CB.Checked then
               Board.CurrentLayer := LayerObj.LayerID;

            Flag := False;
         end;

         if LayerObj.IsDisplayed[Board] then Enabled := True
         else                                Disabled := True;

         Inc(i);
         LayerObj := TheLayerStack.NextLayer(LayerObj);
      end;

      if      Enabled  = False then CBCopperAll.Checked := False
      else if Disabled = False then CBCopperAll.Checked := True
      else                          CBCopperAll.State   := cbGrayed;

      UpdateFromCB := True;
      Board.ViewManager_FullUpdate;
      Board.ViewManager_UpdateLayerTabs;
   end;
end;

procedure TFormLayersPanel.CBPairedClick(Sender: TObject);
var
   i, j             : Integer;
   MechLayer        : IPCB_MechanicalLayer;
   CB               : TCheckBox;
   Enabled          : Boolean;
   Disabled         : Boolean;
   UnPairedEnabled  : Boolean;
   UnPairedDisabled : Boolean;
   PairedEnabled    : Boolean;
   PairedDisabled   : Boolean;
begin
   if (CBPaired.State <> cbGrayed) and UpdateFromCB then
   begin
      j := 1;

      Refresh      := True;
      UpdateFromCB := False;
      Enabled      := False;
      Disabled     := False;

      for i := 1 to 32 do
      begin
         MechLayer := TheLayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

         if MechLayer.MechanicalLayerEnabled then
         begin
            if Board.MechanicalPairs.LayerUsed(ILayer.MechanicalLayer(j)) then
            begin
               MechLayer.IsDisplayed[Board] := CBPaired.Checked;

               CB := Int2CBMech(j);
               CB.Checked := CBPaired.Checked;

               if Refresh and CBPaired.Checked then
               begin
                  Board.CurrentLayer := MechLayer.V7_LayerID;
                  Refresh := False;
               end;
            end;

            if MechLayer.IsDisplayed[Board] then Enabled := True
            else                                 Disabled := True;

            Inc(j);
         end;
      end;

      if      Enabled  = False then CBMechAll.Checked  := False
      else if Disabled = False then CBMechAll.Checked  := True
      else                          CBMechAll.State    := cbGrayed;

      UpdateFromCB := True;
      Refresh := True;

      Board.ViewManager_FullUpdate;
      Board.ViewManager_UpdateLayerTabs;
   end;
end;

procedure TFormLayersPanel.CBUnPairedClick(Sender: TObject);
var
   i, j             : Integer;
   MechLayer        : IPCB_MechanicalLayer;
   CB               : TCheckBox;
   Enabled          : Boolean;
   Disabled         : Boolean;
   UnPairedEnabled  : Boolean;
   UnPairedDisabled : Boolean;
   PairedEnabled    : Boolean;
   PairedDisabled   : Boolean;
begin
   if (CBUnPaired.State <> cbGrayed) and UpdateFromCB then
   begin
      j := 1;

      Refresh      := True;
      UpdateFromCB := False;
      Enabled      := False;
      Disabled     := False;

      for i := 1 to 32 do
      begin
         MechLayer := TheLayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

         if MechLayer.MechanicalLayerEnabled then
         begin
            if not Board.MechanicalPairs.LayerUsed(ILayer.MechanicalLayer(j)) then
            begin
               MechLayer.IsDisplayed[Board] := CBUnPaired.Checked;

               CB := Int2CBMech(j);
               CB.Checked := CBUnPaired.Checked;

               if Refresh and CBUnPaired.Checked then
               begin
                  Board.CurrentLayer := MechLayer.V7_LayerID;
                  Refresh := False;
               end;
            end;

            if MechLayer.IsDisplayed[Board] then Enabled := True
            else                                 Disabled := True;

            Inc(j);
         end;
      end;

      if      Enabled  = False then CBMechAll.Checked  := False
      else if Disabled = False then CBMechAll.Checked  := True
      else                          CBMechAll.State    := cbGrayed;

      UpdateFromCB := True;
      Refresh := True;

      Board.ViewManager_FullUpdate;
      Board.ViewManager_UpdateLayerTabs;
   end;
end;

Procedure CopperClick(Nr : Integer);
Var
   LayerObj         : IPCB_LayerObject;
   i                : Integer;
   CB               : TCheckBox;
   Enabled          : Boolean;
   Disabled         : Boolean;
   SignalsEnabled   : Boolean;
   SignalsDisabled  : Boolean;
   PlanesEnabled    : Boolean;
   PlanesDisabled   : Boolean;
   Signals          : IPCB_LayerSet;
begin
   if UpdateFromCB then
   begin
      LayerObj := TheLayerStack.FirstLayer;
      i := 1;

      Enabled         := False;
      Disabled        := False;
      SignalsEnabled  := False;
      SignalsDisabled := False;
      PlanesEnabled   := False;
      PlanesDisabled  := False;
      Signals         := LayerSet.SignalLayers;

      while (LayerObj <> nil) do
      begin
         if i = Nr then
         begin
            CB := Int2CBCopper(Nr);

            LayerObj.IsDisplayed[Board] := CB.Checked;

            if CB.Checked then
              Board.CurrentLayer := LayerObj.LayerID;

            if Refresh Then
            begin
               Board.ViewManager_FullUpdate;
               Board.ViewManager_UpdateLayerTabs;
            end;
         end;

         if LayerObj.IsDisplayed[Board] then Enabled := True
         else                                Disabled := True;

         if Signals.Contains(LayerObj.V7_LayerID) then
         begin
            if LayerObj.IsDisplayed[Board] then SignalsEnabled := True
            else                                SignalsDisabled := True;
         end
         else
         begin
            if LayerObj.IsDisplayed[Board] then PlanesEnabled := True
            else                                PlanesDisabled := True;
         end;

         Inc(i);
         LayerObj := TheLayerStack.NextLayer(LayerObj);
      end;

      UpdateFromCB := False;

      if      Enabled  = False then CBCopperAll.Checked := False
      else if Disabled = False then CBCopperAll.Checked := True
      else                          CBCopperAll.State   := cbGrayed;

      if      SignalsEnabled  = False then CBSignals.Checked := False
      else if SignalsDisabled = False then CBSignals.Checked := True
      else                                 CBSignals.State   := cbGrayed;

      if      PlanesEnabled  = False then CBPlanes.Checked := False
      else if PlanesDisabled = False then CBPlanes.Checked := True
      else                                CBPlanes.State   := cbGrayed;

      UpdateFromCB := True;
   end;
end;

Procedure CopperCurrent(Nr : Integer);
Var
   LayerObj : IPCB_LayerObject;
   i        : Integer;
begin
   LayerObj := TheLayerStack.FirstLayer;
   i := 1;

   while (LayerObj <> nil) do
   begin
      if i = Nr then
      begin
         if LayerObj.IsDisplayed[Board] then
         begin
            Board.CurrentLayer := LayerObj.LayerID;

            Board.ViewManager_FullUpdate;
            Board.ViewManager_UpdateLayerTabs;
         end;

         break;
      end;
      Inc(i);
      LayerObj := TheLayerStack.NextLayer(LayerObj);
   end;
end;

Procedure MechClick(Nr : Integer);
var
   i, j             : Integer;
   MechLayer        : IPCB_MechanicalLayer;
   CB               : TCheckBox;
   Enabled          : Boolean;
   Disabled         : Boolean;
   UnPairedEnabled  : Boolean;
   UnPairedDisabled : Boolean;
   PairedEnabled    : Boolean;
   PairedDisabled   : Boolean;
begin
   if UpdateFromCB then
   begin
      j                := 0;
      Enabled          := False;
      Disabled         := False;
      UnPairedEnabled  := False;
      UnPairedDisabled := False;
      PairedEnabled    := False;
      PairedDisabled   := False;

      for i := 1 to 32 do
      begin
         MechLayer := TheLayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

         if MechLayer.MechanicalLayerEnabled then
         begin
            Inc(j);

            if j = Nr then
            begin
               CB := Int2CBMech(Nr);
               MechLayer.IsDisplayed[Board] := CB.Checked;

               if CB.Checked then
                  Board.CurrentLayer := MechLayer.V7_LayerID;

               if Refresh then
               begin
                  Board.ViewManager_FullUpdate;
                  Board.ViewManager_UpdateLayerTabs;
               end;
            end;

            if MechLayer.LinkToSheet = False then
            begin
               if MechLayer.IsDisplayed[Board] then Enabled := True
               else                                 Disabled := True;

               if Board.MechanicalPairs.LayerUsed(ILayer.MechanicalLayer(j)) then
               begin
                  if MechLayer.IsDisplayed[Board] then PairedEnabled := True
                  else                                 PairedDisabled := True;
               end
               else
               begin
                  if MechLayer.IsDisplayed[Board] then UnPairedEnabled := True
                  else                                 UnPairedDisabled := True;
               end;
            end;
         end;
      end;

      UpdateFromCB := False;

      if      Enabled  = False then CBMechAll.Checked := False
      else if Disabled = False then CBMechAll.Checked := True
      else                          CBMechAll.State   := cbGrayed;

      if      PairedEnabled  = False then CBPaired.Checked := False
      else if PairedDisabled = False then CBPaired.Checked := True
      else                                CBPaired.State   := cbGrayed;

      if      UnPairedEnabled  = False then CBUnPaired.Checked := False
      else if UnPairedDisabled = False then CBUnPaired.Checked := True
      else                                  CBUnPaired.State   := cbGrayed;

      UpdateFromCB := True;
   end;
end;

Procedure MechCurrent(Nr : Integer);
var
   i, j      : Integer;
   MechLayer : IPCB_MechanicalLayer;
begin
   j := 0;
   for i := 1 to 32 do
   begin
      MechLayer := TheLayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

      if MechLayer.MechanicalLayerEnabled then Inc(j);

      if j = Nr then
      begin
         if MechLayer.IsDisplayed[Board] then
         begin
            Board.CurrentLayer := MechLayer.V7_LayerID;

            Board.ViewManager_FullUpdate;
            Board.ViewManager_UpdateLayerTabs;
         end;

         break;
      end;
   end;
end;

Procedure OtherClick(Name : String);
var
   CB : TCheckBox;
begin
   if UpdateFromCB then
   begin
      CB := Str2CBOther(Name);
      Board.LayerIsDisplayed[String2Layer(Name)] := CB.Checked;

      if CB.Checked then
         Board.CurrentLayer := String2Layer(Name);

      if Refresh then
      begin
         Board.ViewManager_FullUpdate;
         Board.ViewManager_UpdateLayerTabs;
      end;

      UpdateFromCB := False;
      if      OtherEnabled('')  = False then CBOtherAll.Checked := False
      else if OtherDisabled('') = False then CBOtherAll.Checked := True
      else                                   CBOtherAll.State   := cbGrayed;

      if (Name = 'Top Overlay') or (Name = 'Bottom Overlay') then
      begin
         if (not CBTopOverlay.Checked) and (not CBBottomOverlay.Checked) then CBOverlay.Checked := False
         else if CBTopOverlay.Checked  and      CBBottomOverlay.Checked  then CBOverlay.Checked := True
         else                                                                 CBOverlay.State   := cbGrayed;
      end
      else if (Name = 'Top Solder Mask') or (Name = 'Bottom Solder Mask') or (Name = 'Top Paste') or (Name = 'Bottom Paste') then
      begin
         if (not CBTopSolder.Checked) and (not CBBottomSolder.Checked) and (not CBTopPaste.Checked) and (not CBBottomPaste.Checked) then CBMask.Checked := False
         else if CBTopSolder.Checked  and      CBBottomSolder.Checked  and      CBTopPaste.Checked  and      CBBottomPaste.Checked  then CBMask.Checked := True
         else                                                                                                                            CBMask.State   := cbGrayed;
      end
      else if (Name = 'Drill Guide') or (Name = 'Drill Drawing') then
      begin
         if (not CBDrillDraw.Checked) and (not CBDrillGuide.Checked) then CBDrill.Checked := False
         else if CBDrillDraw.Checked  and      CBDrillGuide.Checked  then CBDrill.Checked := True
         else                                                             CBDrill.State   := cbGrayed;
      end
      else if (Name = 'Multi Layer') or (Name = 'Keep Out Layer') then
      begin
         if (not CBKeepOut.Checked) and (not CBMultiLayer.Checked) then CBOther.Checked := False
         else if CBKeepOut.Checked  and      CBMultiLayer.Checked  then CBOther.Checked := True
         else                                                           CBOther.State   := cbGrayed;
      end;

      UpdateFromCB := True;
   end;
end;

Procedure OtherCurrent(Name : String);
begin
   if Board.LayerIsDisplayed[String2Layer(Name)] then
   begin
      Board.CurrentLayer := String2Layer(Name);
      
      Board.ViewManager_FullUpdate;
      Board.ViewManager_UpdateLayerTabs;
   end;
end;

procedure TFormLayersPanel.CBCopper1Click(Sender: TObject);
begin
   CopperClick(1);
end;

procedure TFormLayersPanel.CBCopper2Click(Sender: TObject);
begin
   CopperClick(2);
end;

procedure TFormLayersPanel.CBCopper3Click(Sender: TObject);
begin
   CopperClick(3);
end;

procedure TFormLayersPanel.CBCopper4Click(Sender: TObject);
begin
   CopperClick(4);
end;

procedure TFormLayersPanel.CBCopper5Click(Sender: TObject);
begin
   CopperClick(5);
end;

procedure TFormLayersPanel.CBCopper6Click(Sender: TObject);
begin
   CopperClick(6);
end;

procedure TFormLayersPanel.CBCopper7Click(Sender: TObject);
begin
   CopperClick(7);
end;

procedure TFormLayersPanel.CBCopper8Click(Sender: TObject);
begin
   CopperClick(8);
end;

procedure TFormLayersPanel.CBCopper9Click(Sender: TObject);
begin
   CopperClick(9);
end;

procedure TFormLayersPanel.CBCopper10Click(Sender: TObject);
begin
   CopperClick(10);
end;

procedure TFormLayersPanel.CBCopper11Click(Sender: TObject);
begin
   CopperClick(11);
end;

procedure TFormLayersPanel.CBCopper12Click(Sender: TObject);
begin
   CopperClick(12);
end;

procedure TFormLayersPanel.CBCopper13Click(Sender: TObject);
begin
   CopperClick(13);
end;

procedure TFormLayersPanel.CBCopper14Click(Sender: TObject);
begin
   CopperClick(14);
end;

procedure TFormLayersPanel.CBCopper15Click(Sender: TObject);
begin
   CopperClick(15);
end;

procedure TFormLayersPanel.CBCopper16Click(Sender: TObject);
begin
   CopperClick(16);
end;

procedure TFormLayersPanel.CBCopper17Click(Sender: TObject);
begin
   CopperClick(17);
end;

procedure TFormLayersPanel.CBCopper18Click(Sender: TObject);
begin
   CopperClick(18);
end;

procedure TFormLayersPanel.CBCopper19Click(Sender: TObject);
begin
   CopperClick(19);
end;

procedure TFormLayersPanel.CBCopper20Click(Sender: TObject);
begin
   CopperClick(20);
end;

procedure TFormLayersPanel.CBCopper21Click(Sender: TObject);
begin
   CopperClick(21);
end;

procedure TFormLayersPanel.CBCopper22Click(Sender: TObject);
begin
   CopperClick(22);
end;

procedure TFormLayersPanel.CBCopper23Click(Sender: TObject);
begin
   CopperClick(23);
end;

procedure TFormLayersPanel.CBCopper24Click(Sender: TObject);
begin
   CopperClick(24);
end;

procedure TFormLayersPanel.CBCopper25Click(Sender: TObject);
begin
   CopperClick(25);
end;

procedure TFormLayersPanel.CBCopper26Click(Sender: TObject);
begin
   CopperClick(26);
end;

procedure TFormLayersPanel.CBCopper27Click(Sender: TObject);
begin
   CopperClick(27);
end;

procedure TFormLayersPanel.CBCopper28Click(Sender: TObject);
begin
   CopperClick(28);
end;

procedure TFormLayersPanel.CBCopper29Click(Sender: TObject);
begin
   CopperClick(29);
end;

procedure TFormLayersPanel.CBCopper30Click(Sender: TObject);
begin
   CopperClick(30);
end;

procedure TFormLayersPanel.CBCopper31Click(Sender: TObject);
begin
   CopperClick(31);
end;

procedure TFormLayersPanel.CBCopper32Click(Sender: TObject);
begin
   CopperClick(32);
end;

procedure TFormLayersPanel.CBCopper33Click(Sender: TObject);
begin
   CopperClick(33);
end;

procedure TFormLayersPanel.CBCopper34Click(Sender: TObject);
begin
   CopperClick(34);
end;

procedure TFormLayersPanel.CBCopper35Click(Sender: TObject);
begin
   CopperClick(35);
end;

procedure TFormLayersPanel.CBCopper36Click(Sender: TObject);
begin
   CopperClick(36);
end;

procedure TFormLayersPanel.CBCopper37Click(Sender: TObject);
begin
   CopperClick(37);
end;

procedure TFormLayersPanel.CBCopper38Click(Sender: TObject);
begin
   CopperClick(38);
end;

procedure TFormLayersPanel.CBCopper39Click(Sender: TObject);
begin
   CopperClick(39);
end;

procedure TFormLayersPanel.CBCopper40Click(Sender: TObject);
begin
   CopperClick(40);
end;

procedure TFormLayersPanel.CBCopper41Click(Sender: TObject);
begin
   CopperClick(41);
end;

procedure TFormLayersPanel.CBCopper42Click(Sender: TObject);
begin
   CopperClick(42);
end;

procedure TFormLayersPanel.CBCopper43Click(Sender: TObject);
begin
   CopperClick(43);
end;

procedure TFormLayersPanel.CBCopper44Click(Sender: TObject);
begin
   CopperClick(44);
end;

procedure TFormLayersPanel.CBCopper45Click(Sender: TObject);
begin
   CopperClick(45);
end;

procedure TFormLayersPanel.CBCopper46Click(Sender: TObject);
begin
   CopperClick(46);
end;

procedure TFormLayersPanel.CBCopper47Click(Sender: TObject);
begin
   CopperClick(47);
end;

procedure TFormLayersPanel.CBCopper48Click(Sender: TObject);
begin
   CopperClick(48);
end;

procedure TFormLayersPanel.ShapeCopper1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(1);
end;

procedure TFormLayersPanel.ShapeCopper2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(2);
end;

procedure TFormLayersPanel.ShapeCopper3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(3);
end;

procedure TFormLayersPanel.ShapeCopper4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(4);
end;

procedure TFormLayersPanel.ShapeCopper5MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(5);
end;

procedure TFormLayersPanel.ShapeCopper6MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(6);
end;

procedure TFormLayersPanel.ShapeCopper7MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(7);
end;

procedure TFormLayersPanel.ShapeCopper8MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(8);
end;

procedure TFormLayersPanel.ShapeCopper9MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(9);
end;

procedure TFormLayersPanel.ShapeCopper10MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(10);
end;

procedure TFormLayersPanel.ShapeCopper11MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(11);
end;

procedure TFormLayersPanel.ShapeCopper12MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(12);
end;

procedure TFormLayersPanel.ShapeCopper13MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(13);
end;

procedure TFormLayersPanel.ShapeCopper14MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(14);
end;

procedure TFormLayersPanel.ShapeCopper15MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(15);
end;

procedure TFormLayersPanel.ShapeCopper16MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(16);
end;

procedure TFormLayersPanel.ShapeCopper17MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(17);
end;

procedure TFormLayersPanel.ShapeCopper18MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(18);
end;

procedure TFormLayersPanel.ShapeCopper19MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(19);
end;

procedure TFormLayersPanel.ShapeCopper20MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(20);
end;

procedure TFormLayersPanel.ShapeCopper21MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(21);
end;

procedure TFormLayersPanel.ShapeCopper22MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(22);
end;

procedure TFormLayersPanel.ShapeCopper23MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(23);
end;

procedure TFormLayersPanel.ShapeCopper24MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(24);
end;

procedure TFormLayersPanel.ShapeCopper25MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(25);
end;

procedure TFormLayersPanel.ShapeCopper26MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(26);
end;

procedure TFormLayersPanel.ShapeCopper27MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(27);
end;

procedure TFormLayersPanel.ShapeCopper28MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(28);
end;

procedure TFormLayersPanel.ShapeCopper29MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(29);
end;

procedure TFormLayersPanel.ShapeCopper30MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(30);
end;

procedure TFormLayersPanel.ShapeCopper31MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(31);
end;

procedure TFormLayersPanel.ShapeCopper32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(32);
end;

procedure TFormLayersPanel.ShapeCopper33MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(33);
end;

procedure TFormLayersPanel.ShapeCopper34MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(34);
end;

procedure TFormLayersPanel.ShapeCopper35MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(35);
end;

procedure TFormLayersPanel.ShapeCopper36MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(36);
end;

procedure TFormLayersPanel.ShapeCopper37MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(37);
end;

procedure TFormLayersPanel.ShapeCopper38MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(38);
end;

procedure TFormLayersPanel.ShapeCopper39MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(39);
end;

procedure TFormLayersPanel.ShapeCopper40MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(40);
end;

procedure TFormLayersPanel.ShapeCopper41MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(41);
end;

procedure TFormLayersPanel.ShapeCopper42MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(42);
end;

procedure TFormLayersPanel.ShapeCopper43MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(43);
end;

procedure TFormLayersPanel.ShapeCopper44MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(44);
end;

procedure TFormLayersPanel.ShapeCopper45MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(45);
end;

procedure TFormLayersPanel.ShapeCopper46MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(46);
end;

procedure TFormLayersPanel.ShapeCopper47MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(47);
end;

procedure TFormLayersPanel.ShapeCopper48MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   CopperCurrent(48);
end;

procedure TFormLayersPanel.CBMech1Click(Sender: TObject);
begin
   MechClick(1);
end;

procedure TFormLayersPanel.CBMech2Click(Sender: TObject);
begin
   MechClick(2);
end;

procedure TFormLayersPanel.CBMech3Click(Sender: TObject);
begin
   MechClick(3);
end;

procedure TFormLayersPanel.CBMech4Click(Sender: TObject);
begin
   MechClick(4);
end;

procedure TFormLayersPanel.CBMech5Click(Sender: TObject);
begin
   MechClick(5);
end;

procedure TFormLayersPanel.CBMech6Click(Sender: TObject);
begin
   MechClick(6);
end;

procedure TFormLayersPanel.CBMech7Click(Sender: TObject);
begin
   MechClick(7);
end;

procedure TFormLayersPanel.CBMech8Click(Sender: TObject);
begin
   MechClick(8);
end;

procedure TFormLayersPanel.CBMech9Click(Sender: TObject);
begin
   MechClick(9);
end;

procedure TFormLayersPanel.CBMech10Click(Sender: TObject);
begin
   MechClick(10);
end;

procedure TFormLayersPanel.CBMech11Click(Sender: TObject);
begin
   MechClick(11);
end;

procedure TFormLayersPanel.CBMech12Click(Sender: TObject);
begin
   MechClick(12);
end;

procedure TFormLayersPanel.CBMech13Click(Sender: TObject);
begin
   MechClick(13);
end;

procedure TFormLayersPanel.CBMech14Click(Sender: TObject);
begin
   MechClick(14);
end;

procedure TFormLayersPanel.CBMech15Click(Sender: TObject);
begin
   MechClick(15);
end;

procedure TFormLayersPanel.CBMech16Click(Sender: TObject);
begin
   MechClick(16);
end;

procedure TFormLayersPanel.CBMech17Click(Sender: TObject);
begin
   MechClick(17);
end;

procedure TFormLayersPanel.CBMech18Click(Sender: TObject);
begin
   MechClick(18);
end;

procedure TFormLayersPanel.CBMech19Click(Sender: TObject);
begin
   MechClick(19);
end;

procedure TFormLayersPanel.CBMech20Click(Sender: TObject);
begin
   MechClick(20);
end;

procedure TFormLayersPanel.CBMech21Click(Sender: TObject);
begin
   MechClick(21);
end;

procedure TFormLayersPanel.CBMech22Click(Sender: TObject);
begin
   MechClick(22);
end;

procedure TFormLayersPanel.CBMech23Click(Sender: TObject);
begin
   MechClick(23);
end;

procedure TFormLayersPanel.CBMech24Click(Sender: TObject);
begin
   MechClick(24);
end;

procedure TFormLayersPanel.CBMech25Click(Sender: TObject);
begin
   MechClick(25);
end;

procedure TFormLayersPanel.CBMech26Click(Sender: TObject);
begin
   MechClick(26);
end;

procedure TFormLayersPanel.CBMech27Click(Sender: TObject);
begin
   MechClick(27);
end;

procedure TFormLayersPanel.CBMech28Click(Sender: TObject);
begin
   MechClick(28);
end;

procedure TFormLayersPanel.CBMech29Click(Sender: TObject);
begin
   MechClick(29);
end;

procedure TFormLayersPanel.CBMech30Click(Sender: TObject);
begin
   MechClick(30);
end;

procedure TFormLayersPanel.CBMech31Click(Sender: TObject);
begin
   MechClick(31);
end;

procedure TFormLayersPanel.CBMech32Click(Sender: TObject);
begin
   MechClick(32);
end;

procedure TFormLayersPanel.ShapeMech1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(1);
end;

procedure TFormLayersPanel.ShapeMech2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(2);
end;

procedure TFormLayersPanel.ShapeMech3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(3);
end;

procedure TFormLayersPanel.ShapeMech4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(4);
end;

procedure TFormLayersPanel.ShapeMech5MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(5);
end;

procedure TFormLayersPanel.ShapeMech6MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(6);
end;

procedure TFormLayersPanel.ShapeMech7MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(7);
end;

procedure TFormLayersPanel.ShapeMech8MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(8);
end;

procedure TFormLayersPanel.ShapeMech9MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(9);
end;

procedure TFormLayersPanel.ShapeMech10MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(10);
end;

procedure TFormLayersPanel.ShapeMech11MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(11);
end;

procedure TFormLayersPanel.ShapeMech12MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(12);
end;

procedure TFormLayersPanel.ShapeMech13MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(13);
end;

procedure TFormLayersPanel.ShapeMech14MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(14);
end;

procedure TFormLayersPanel.ShapeMech15MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(15);
end;

procedure TFormLayersPanel.ShapeMech16MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(16);
end;

procedure TFormLayersPanel.ShapeMech17MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(17);
end;

procedure TFormLayersPanel.ShapeMech18MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(18);
end;

procedure TFormLayersPanel.ShapeMech19MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(19);
end;

procedure TFormLayersPanel.ShapeMech20MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(20);
end;

procedure TFormLayersPanel.ShapeMech21MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(21);
end;

procedure TFormLayersPanel.ShapeMech22MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(22);
end;

procedure TFormLayersPanel.ShapeMech23MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(23);
end;

procedure TFormLayersPanel.ShapeMech24MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(24);
end;

procedure TFormLayersPanel.ShapeMech25MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(25);
end;

procedure TFormLayersPanel.ShapeMech26MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(26);
end;

procedure TFormLayersPanel.ShapeMech27MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(27);
end;

procedure TFormLayersPanel.ShapeMech28MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(28);
end;

procedure TFormLayersPanel.ShapeMech29MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(29);
end;

procedure TFormLayersPanel.ShapeMech30MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(30);
end;

procedure TFormLayersPanel.ShapeMech31MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(31);
end;

procedure TFormLayersPanel.ShapeMech32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MechCurrent(32);
end;

procedure TFormLayersPanel.CBTopOverlayClick(Sender: TObject);
begin
   OtherClick('Top Overlay');
end;

procedure TFormLayersPanel.CBBottomOverlayClick(Sender: TObject);
begin
   OtherClick('Bottom Overlay');
end;

procedure TFormLayersPanel.CBTopSolderClick(Sender: TObject);
begin
   OtherClick('Top Solder Mask');
end;

procedure TFormLayersPanel.CBBottomSolderClick(Sender: TObject);
begin
   OtherClick('Bottom Solder Mask');
end;

procedure TFormLayersPanel.CBTopPasteClick(Sender: TObject);
begin
   OtherClick('Top Paste');
end;

procedure TFormLayersPanel.CBBottomPasteClick(Sender: TObject);
begin
   OtherClick('Bottom Paste');
end;

procedure TFormLayersPanel.CBDrillGuideClick(Sender: TObject);
begin
   OtherClick('Drill Guide');
end;

procedure TFormLayersPanel.CBDrillDrawClick(Sender: TObject);
begin
   OtherClick('Drill Drawing');
end;

procedure TFormLayersPanel.CBKeepOutClick(Sender: TObject);
begin
   OtherClick('Keep Out Layer');
end;

procedure TFormLayersPanel.CBMultiLayerClick(Sender: TObject);
begin
   OtherClick('Multi Layer');
end;

procedure TFormLayersPanel.ShapeTopOverlayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   OtherCurrent('Top Overlay');
end;

procedure TFormLayersPanel.ShapeBottomOverlayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   OtherCurrent('Bottom Overlay');
end;

procedure TFormLayersPanel.ShapeTopSolderMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   OtherCurrent('Top Solder Mask');
end;

procedure TFormLayersPanel.ShapeBottomSolderMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   OtherCurrent('Bottom Solder Mask');
end;

procedure TFormLayersPanel.ShapeTopPasteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   OtherCurrent('Top Paste');
end;

procedure TFormLayersPanel.ShapeBottomPasteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   OtherCurrent('Bottom Paste');
end;

procedure TFormLayersPanel.ShapeDrillGuideMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   OtherCurrent('Drill Guide');
end;

procedure TFormLayersPanel.ShapeDrillDrawMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   OtherCurrent('Drill Drawing');
end;

procedure TFormLayersPanel.ShapeKeepOutMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   OtherCurrent('Keep Out Layer');
end;

procedure TFormLayersPanel.ShapeMultiLayerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   OtherCurrent('Multi Layer');
end;

procedure TFormLayersPanel.CBOverlayClick(Sender: TObject);
begin
   if UpdateFromCB and (CBOverlay.State <> cbGrayed) then
   begin
      UpdateFromCB := False;

      Board.LayerIsDisplayed[String2Layer('Top Overlay')]    := CBOverlay.Checked;
      Board.LayerIsDisplayed[String2Layer('Bottom Overlay')] := CBOverlay.Checked;

      CBTopOverlay.Checked    := CBOverlay.Checked;
      CBBottomOverlay.Checked := CBOverlay.Checked;

      if CBOverlay.Checked then
         Board.CurrentLayer := String2Layer('Top Overlay');

      if      OtherEnabled('')  = False then CBOtherAll.Checked := False
      else if OtherDisabled('') = False then CBOtherAll.Checked := True
      else                                   CBOtherAll.State   := cbGrayed;

      Board.ViewManager_UpdateLayerTabs;
      Board.ViewManager_FullUpdate;

      UpdateFromCB := True;
   end;
end;

procedure TFormLayersPanel.CBMaskClick(Sender: TObject);
begin
   if UpdateFromCB and (CBMask.State <> cbGrayed) then
   begin
      UpdateFromCB := False;

      Board.LayerIsDisplayed[String2Layer('Top Solder Mask')]    := CBMask.Checked;
      Board.LayerIsDisplayed[String2Layer('Bottom Solder Mask')] := CBMask.Checked;
      Board.LayerIsDisplayed[String2Layer('Top Paste')]          := CBMask.Checked;
      Board.LayerIsDisplayed[String2Layer('Bottom Paste')]       := CBMask.Checked;

      CBTopSolder.Checked    := CBMask.Checked;
      CBBottomSolder.Checked := CBMask.Checked;
      CBTopPaste.Checked     := CBMask.Checked;
      CBBottomPaste.Checked  := CBMask.Checked;

      if CBMask.Checked then
         Board.CurrentLayer := String2Layer('Top Solder Mask');

      if      OtherEnabled('')  = False then CBOtherAll.Checked := False
      else if OtherDisabled('') = False then CBOtherAll.Checked := True
      else                                   CBOtherAll.State   := cbGrayed;

      Board.ViewManager_UpdateLayerTabs;
      Board.ViewManager_FullUpdate;

      UpdateFromCB := True;
   end;
end;

procedure TFormLayersPanel.CBDrillClick(Sender: TObject);
begin
   if UpdateFromCB and (CBDrill.State <> cbGrayed) then
   begin
      UpdateFromCB := False;

      Board.LayerIsDisplayed[String2Layer('Drill Guide')]    := CBDrill.Checked;
      Board.LayerIsDisplayed[String2Layer('Drill Drawing')]  := CBDrill.Checked;

      CBDrillGuide.Checked := CBDrill.Checked;
      CBDrillDraw.Checked  := CBDrill.Checked;

      if CBDrill.Checked then
         Board.CurrentLayer := String2Layer('Drill Guide');

      if      OtherEnabled('')  = False then CBOtherAll.Checked := False
      else if OtherDisabled('') = False then CBOtherAll.Checked := True
      else                                   CBOtherAll.State   := cbGrayed;

      Board.ViewManager_UpdateLayerTabs;
      Board.ViewManager_FullUpdate;

      UpdateFromCB := True;
   end;
end;

procedure TFormLayersPanel.CBOtherClick(Sender: TObject);
begin
   if UpdateFromCB and (CBOther.State <> cbGrayed) then
   begin
      UpdateFromCB := False;

      Board.LayerIsDisplayed[String2Layer('Keep Out Layer')] := CBOther.Checked;
      Board.LayerIsDisplayed[String2Layer('Multi Layer')]    := CBOther.Checked;

      CBKeepOut.Checked    := CBOther.Checked;
      CBMultiLayer.Checked := CBOther.Checked;

      if CBOther.Checked then
         Board.CurrentLayer := String2Layer('Keep Out Layer');

      if      OtherEnabled('')  = False then CBOtherAll.Checked := False
      else if OtherDisabled('') = False then CBOtherAll.Checked := True
      else                                   CBOtherAll.State   := cbGrayed;

      Board.ViewManager_UpdateLayerTabs;
      Board.ViewManager_FullUpdate;

      UpdateFromCB := True;
   end;
end;

procedure TFormLayersPanel.ImageArrowUpObjectsClick(Sender: TObject);
begin
   GroupBoxObjects.Height := 48;
   GroupBoxCopper.Top := GroupBoxObjects.Top + GroupBoxObjects.Height + 10;
   GroupBoxMech.Top   := GroupBoxCopper.Top + GroupBoxCopper.Height + 10;
   GroupBoxOther.Top  := GroupBoxMech.Top + GroupBoxMech.Height + 10;

   FormLayersPanel.Height:= GroupBoxOther.Top + GroupBoxOther.Height + 50;

   ImageArrowUpObjects.Visible := False;
   ImageArrowUpObjects.Enabled := False;

   ImageArrowDownObjects.Visible := True;
   ImageArrowDownObjects.Enabled := True;
end;

procedure TFormLayersPanel.ImageArrowDownObjectsClick(Sender: TObject);
begin
   GroupBoxObjects.Height := 196;
   GroupBoxCopper.Top := GroupBoxObjects.Top + GroupBoxObjects.Height + 10;
   GroupBoxMech.Top   := GroupBoxCopper.Top + GroupBoxCopper.Height + 10;
   GroupBoxOther.Top  := GroupBoxMech.Top + GroupBoxMech.Height + 10;

   FormLayersPanel.Height:= GroupBoxOther.Top + GroupBoxOther.Height + 50;

   ImageArrowUpObjects.Visible := True;
   ImageArrowUpObjects.Enabled := True;

   ImageArrowDownObjects.Visible := False;
   ImageArrowDownObjects.Enabled := False;
end;

procedure TFormLayersPanel.CBObjectsAllClick(Sender: TObject);
begin
   if Refresh then
   begin
      if SwitchGrayAndOff then
      begin
         SwitchGrayAndOff := False;
         if CBObjectsAll.State = cbUnchecked   then CBObjectsAll.State := cbGrayed
         else if CBObjectsAll.State = cbGrayed then CBObjectsAll.State := cbChecked
         else                                       CBObjectsAll.State := cbUnchecked;
      end
      else
      begin
         Refresh     := False;

         CBTracks.State        := CBObjectsAll.State;
         CBArcs.State          := CBObjectsAll.State;
         CBPads.State          := CBObjectsAll.State;
         CBVias.State          := CBObjectsAll.State;
         CBRegions.State       := CBObjectsAll.State;
         CBFills.State         := CBObjectsAll.State;
         CBStrings.State       := CBObjectsAll.State;
         CBComponents.State    := CBObjectsAll.State;
         CBPolygons.State      := CBObjectsAll.State;
         CB3DModels.State      := CBObjectsAll.State;
         CBDimensions.State    := CBObjectsAll.State;
         CBCoordinates.State   := CBObjectsAll.State;
         CBRooms.State         := CBObjectsAll.State;
         if CBObjectsAll.State = cbUnchecked then
            CBConnections.Checked := False
         else
            CBConnections.Checked := True;

         ResetParameters;
         AddStringParameter('Action', 'Redraw');
         RunProcess('PCB:Zoom');

         Refresh          := True;
         SwitchGrayAndOff := True;
      end;
   end;
end;

procedure TFormLayersPanel.CBShowObjectsClick(Sender: TObject);
begin
   CBTracks.Checked      := CBTracks.Checked;
   CBArcs.Checked        := CBArcs.Checked;
   CBPads.Checked        := CBPads.Checked;
   CBVias.Checked        := CBVias.Checked;
   CBRegions.Checked     := CBRegions.Checked;
   CBFills.Checked       := CBFills.Checked;
   CBStrings.Checked     := CBStrings.Checked;
   CBComponents.Checked  := CBComponents.Checked;
   CBPolygons.Checked    := CBPolygons.Checked;
   CB3DModels.Checked    := CB3DModels.Checked;
   CBDimensions.Checked  := CBDimensions.Checked;
   CBCoordinates.Checked := CBCoordinates.Checked;
   CBRooms.Checked       := CBRooms.Checked;
   CBConnections.Checked := CBConnections.Checked;
end;

Procedure ShowHideObjects(CBox : TCheckBox;);
var
   Prim     : IPCB_Primitive;
   ObjSet   : TObjectSet;
   Iterator : IPCB_BoardIterator;
   i,j,k : Integer;
   ObjType  : String;
   ViewType : String;
begin
   // Here is a small Algorythm to switch unchecked and grayed
   if UpdateFromCB then
   begin
      if CBox.Caption = 'Connection Lines' then
      begin
         Board.LayerIsDisplayed[eConnectLayer] := CBox.Checked;
         If Refresh then
         begin
            ResetParameters;
            AddStringParameter('Action', 'Redraw');
            RunProcess('PCB:Zoom');
            SwitchGrayAndOff := True;
         end;
      end
      else
      begin
         if SwitchGrayAndOff then
         begin
            SwitchGrayAndOff := False;
            if CBox.State = cbUnchecked   then CBox.State := cbGrayed
            else if CBox.State = cbGrayed then CBox.State := cbChecked
            else                               CBox.State := cbUnchecked;
         end
         else
         begin
            Case CBox.Caption of
               'Tracks'      : ObjType := 'TrackQuality';
               'Arcs'        : ObjType := 'ArcQuality';
               'Pads'        : ObjType := 'PadQuality';
               'Vias'        : ObjType := 'ViaQuality';
               'Regions'     : ObjType := 'SolidRegionQuality';
               'Fills'       : ObjType := 'FillQuality';
               'Strings'     : ObjType := 'StringQuality';
               'Components'  : ObjType := 'ComponentQuality';
               'Polygons'    : ObjType := 'PolygonQuality';
               '3D Models'   : ObjType := '3DBodyQuality';
               'Dimensions'  : ObjType := 'DimensionQuality';
               'Rooms'       : ObjType := 'RoomQuality';
               'Coordinates' : ObjType := 'CoordQuality';
            end;

            if      CBox.State = cbChecked then ViewType := 'Final'
            else if CBox.State = cbGrayed  then ViewType := 'Draft'
            else                                ViewType := 'Hidden';

            ResetParameters;
            AddStringParameter (ObjType, ViewType);
            RunProcess ('Pcb:SetupPreferences');

            If Refresh then
            begin
               ResetParameters;
               AddStringParameter('Action', 'Redraw');
               RunProcess('PCB:Zoom');
               SwitchGrayAndOff := True;
            end;
         end;
      end;
   end;
end;




procedure TFormLayersPanel.CBTracksClick(Sender: TObject);
begin
   ShowHideObjects(CBTracks);
end;

procedure TFormLayersPanel.CBArcsClick(Sender: TObject);
begin
   ShowHideObjects(CBArcs);
end;

procedure TFormLayersPanel.CBPadsClick(Sender: TObject);
begin
   ShowHideObjects(CBPads);
end;

procedure TFormLayersPanel.CBViasClick(Sender: TObject);
begin
   ShowHideObjects(CBVias);
end;

procedure TFormLayersPanel.CBRegionsClick(Sender: TObject);
begin
   ShowHideObjects(CBRegions);
end;

procedure TFormLayersPanel.CBFillsClick(Sender: TObject);
begin
   ShowHideObjects(CBFills);
end;

procedure TFormLayersPanel.CBStringsClick(Sender: TObject);
begin
   ShowHideObjects(CBStrings);
end;

procedure TFormLayersPanel.CBComponentsClick(Sender: TObject);
begin
   ShowHideObjects(CBComponents);
end;

procedure TFormLayersPanel.CBPolygonsClick(Sender: TObject);
begin
   ShowHideObjects(CBPolygons);
end;

procedure TFormLayersPanel.CB3DModelsClick(Sender: TObject);
begin
   ShowHideObjects(CB3Dmodels);
end;

procedure TFormLayersPanel.CBDimensionsClick(Sender: TObject);
begin
   ShowHideObjects(CBDimensions);
end;

procedure TFormLayersPanel.CBCoordinatesClick(Sender: TObject);
begin
   ShowHideObjects(CBCoordinates);
end;

procedure TFormLayersPanel.CBRoomsClick(Sender: TObject);
begin
   ShowHideObjects(CBRooms);
end;

procedure TFormLayersPanel.CBConnectionsClick(Sender: TObject);
begin
   ShowHideObjects(CBConnections);
end;
