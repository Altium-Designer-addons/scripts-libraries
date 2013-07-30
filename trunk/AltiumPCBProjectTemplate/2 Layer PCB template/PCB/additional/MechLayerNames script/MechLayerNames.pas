{..............................................................................}
{ Summary   This scripts can be used to copy mech layer names to a *.txt file. }
{                                                                              }
{           After that, layer names can be imported in another PcbDoc file     }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
Procedure ExportMechLayerInfo;
var
   Board      : IPCB_Board;
   MechLayer  : IPCB_MechanicalLayer;
   i          : Integer;
   SaveDialog : TSaveDialog;
   Flag       : Integer;
   FileName   : String;
   INIFile    : TIniFile;
   a          : TColor;

begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   SaveDialog        := TSaveDialog.Create(Application);
   SaveDialog.Title  := 'Save Mech Layer Names to *.ini file';
   SaveDialog.Filter := 'INI file (*.ini)|*.ini';

   Flag := SaveDialog.Execute;
   if (not Flag) then exit;

   FileName := SaveDialog.FileName;

   // Set file extension
   FileName := ChangeFileExt(FileName, '.ini');

   IniFile := TIniFile.Create(FileName);

   for i := 1 to 32 do
   begin

      MechLayer := Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];


      IniFile.WriteString('MechLayer' + IntToStr(i), 'Name',    MechLayer.Name);
      IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'Enabled', MechLayer.MechanicalLayerEnabled);
      IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'Show',    MechLayer.IsDisplayed[Board]);
      IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'Sheet',   MechLayer.LinkToSheet);
      IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'SLM',     MechLayer.DisplayInSingleLayerMode);
   // IniFile.WriteString('MechLayer' + IntToStr(i), 'Color',   Board.LayerColor[MechLayer.LayerID]);
   end;

end;



Procedure ImportMechLayerInfo;
var
   Board      : IPCB_Board;
   i          : Integer;
   OpenDialog : TOpenDialog;
   Flag       : Integer;
   FileName   : String;
   MechLayer  : IPCB_MechanicalLayer;
   INIFile    : TIniFile;

begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;


   OpenDialog        := TOpenDialog.Create(Application);
   OpenDialog.Title  := 'Import Mech Layer Names from *.ini file';
   OpenDialog.Filter := 'INI file (*.ini)|*.ini';

   Flag := OpenDialog.Execute;
   if (not Flag) then exit;

   FileName := OpenDialog.FileName;

   IniFile := TIniFile.Create(FileName);

   for i := 1 To 32 do
   begin

      MechLayer := Board.LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];


      if not MechLayer.MechanicalLayerEnabled then
         MechLayer.MechanicalLayerEnabled  := IniFile.ReadBool  ('MechLayer' + IntToStr(i), 'Enabled', True);


      MechLayer.Name                      := IniFile.ReadString('MechLayer' + IntToStr(i), 'Name',  '');
      MechLayer.LinkToSheet               := IniFile.ReadBool  ('MechLayer' + IntToStr(i), 'Sheet', False);
      MechLayer.DisplayInSingleLayerMode  := IniFile.ReadBool  ('MechLayer' + IntToStr(i), 'SLM',   False);
      MechLayer.IsDisplayed[Board]        := IniFile.ReadBool  ('MechLayer' + IntToStr(i), 'Show',  True);

   // if i < 17 then
   //   Board.LayerColor[MechLayer.LayerID] := IniFile.ReadString('MechLayer' + IntToStr(i), 'Color', '0');

   end;

   ResetParameters;
   AddStringParameter('LayerName','Next');
   RunProcess('PCB:SetCurrentLayer');

   ResetParameters;
   AddStringParameter('LayerName','Previous');
   RunProcess('PCB:SetCurrentLayer');

   ShowInfo('The names assigned to each layer have now been updated.');
end;
