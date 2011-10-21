Procedure SingleLayerWithConnectionlines;
var
   Board : IPCB_Board;
   Parameters : String;
   ProcessLauncher : IProcessLauncher;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   If Board = Nil then exit;

   Parameters := 'Apply=True|Expr=';

   if ILayer.IsSignalLayer(Board.CurrentLayer) then
      Parameters := Parameters + '(Layer = ''MultiLayer'') or ((Layer = ''' + Board.LayerName(Board.CurrentLayer) + ''') and (not IsComponent)) or IsConnection'
   else
      Parameters := Parameters + 'OnLayer(''' + Board.LayerName(Board.CurrentLayer) + ''')';

   Parameters := Parameters + '|Zoom=False|Select=False|Mask=True';

   ProcessLauncher := Client;
   ProcessLauncher.PostMessage('PCB:RunQuery', 'Clear', Length('Clear'), Client.CurrentView);
   ProcessLauncher.PostMessage('PCB:RunQuery', Parameters, Length(Parameters), Client.CurrentView);
end;
