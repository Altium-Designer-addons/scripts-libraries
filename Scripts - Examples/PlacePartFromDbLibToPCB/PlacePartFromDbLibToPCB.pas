{
  Original code from Allen GONG
  https://forum.live.altium.com/#posts/241580/746763
}

Procedure PlacePartFromDbLibToPCB;
var
  _PCBServer   : IPCB_ServerInterface;
  Board        : IPCB_Board;
  Component    : IPCB_Component;
begin
  _PCBServer := PCBServer;
  If _PCBServer = Nil Then
    Exit;
  Board:=_PCBServer.GetCurrentPCBBoard;
  if Board=nil then exit;
  component := _PCBServer.PCBObjectFactory(eComponentObject, eNoDimension,eCreate_Default);
  if component<>nil then
  
  begin
    Component.Board := Board;
    Component.LoadFromLibrary('SourceLibReference=103PP-303|FootPrint=2EDG2X6P-3R81-TH-W|SourceComponentLibrary=YourDBLIB.Dblib');
    Component.Layer := eV7_TopLayer;
    Component.SetState_XLocation(18500000);
    Component.SetState_YLocation(13100000);

    Board.AddPCBObject(Component);
    Board.GraphicalView_ZoomRedraw;
  end;
end;
