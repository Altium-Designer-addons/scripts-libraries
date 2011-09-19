Procedure ExportMechLayerNames;
var
   Board      : IPCB_Board;
   i          : Integer;
   Lista      : TStringList;
   SaveDialog : TSaveDialog;
   Flag       : Integer;
   FileName   : String;

begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   SaveDialog        := TSaveDialog.Create(Application);
   SaveDialog.Title  := 'Save Mech Layer Names to *.txt file';
   SaveDialog.Filter := 'TXT file (*.txt)|*.txt';

   Flag := SaveDialog.Execute;
   if (not Flag) then exit;

   FileName := SaveDialog.FileName;

   // Set file extension
   FileName := ChangeFileExt(FileName, '.txt');

   Lista := TStringList.Create;

   for i := 1 to 32 do
   begin
      Lista.Add(Board.LayerName(ILayer.MechanicalLayer(i)));
   end;

   Lista.SaveToFile(FileName);
end;



Procedure ImportMechLayerNames;
var
   Board      : IPCB_Board;
   i          : Integer;
   Lista      : TStringList;
   OpenDialog : TOpenDialog;
   Flag       : Integer;
   FileName   : String;
   LayerObj   : IPCB_LayerObject;
   LS         : IPCB_LayerStack;

begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   LS := Board.LayerStack;

   OpenDialog        := TOpenDialog.Create(Application);
   OpenDialog.Title  := 'Import Mech Layer Names from *.txt file';
   OpenDialog.Filter := 'TXT file (*.txt)|*.txt';

   Flag := OpenDialog.Execute;
   if (not Flag) then exit;

   FileName := OpenDialog.FileName;

   Lista := TStringList.Create;

   Lista.LoadFromFile(FileName);

   for i := 1 To 32 do
   begin
      FileName := Lista[i-1];
      LayerObj := LS.LayerObject_V7[ILayer.MechanicalLayer(i)];
      LayerObj.Name := FileName;
   end;

   ResetParameters;
   AddStringParameter('LayerName','Next');
   RunProcess('PCB:SetCurrentLayer');

   ResetParameters;
   AddStringParameter('LayerName','Previous');
   RunProcess('PCB:SetCurrentLayer');

   ShowInfo('The names assigned to each layer have now been updated.');
end;
