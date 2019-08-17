{ MechLayerNames.pas                                                              }
{ Summary   Used to export/import mech layer info to/from a text *.ini file.      }
{           Works on PcbDoc & PcbLib files.                                       }
{           Mechanical layer Names, Colours and MechanicalPairs can be            }
{           exported/imported to another PcbDoc/PcbLib file                       }
{                                                                                 }
{ Created by:    Petar Perisin                                                    }
{
 Modified by : B. Miller
 03/07/2017  : mod to fix layer names output, why was import okay ??
 23/02/2018  : added MechLayer Pairs & colours, still loads old ini files.

..................................................................................}
var
    Board       : IPCB_Board;
    LayerStack  : IPCB_LayerStack_V7;
    LayerObj_V7 : IPCB_LayerObject_V7;
    MechLayer   : IPCB_MechanicalLayer;
    MechPairs   : IPCB_MechanicalLayerPairs;
    FileName    : String;
    INIFile     : TIniFile;
    Flag        : Integer;
    i, j        : Integer;

{.................................................................................}
Procedure ExportMechLayerInfo;
var
    SaveDialog  : TSaveDialog;

begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;
    
    SaveDialog        := TSaveDialog.Create(Application);
    SaveDialog.Title  := 'Save Mech Layer Names to *.ini file';
    SaveDialog.Filter := 'INI file (*.ini)|*.ini';
    
    Flag := SaveDialog.Execute;
    if (not Flag) then exit;

    // Get file & set extension
    FileName := SaveDialog.FileName;
    FileName := ChangeFileExt(FileName, '.ini');
    IniFile := TIniFile.Create(FileName);

    LayerStack := Board.LayerStack_V7;
    MechPairs  := Board.MechanicalPairs;

    for i := 1 to 32 do
    begin
        MechLayer := LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

        IniFile.WriteString('MechLayer' + IntToStr(i), 'Name', Board.LayerName(ILayer.MechanicalLayer(i)) );    //MechLayer.Name);

        for j := 1 to 32 do
        begin
            if MechPairs.PairDefined(ILayer.MechanicalLayer(i), ILayer.MechanicalLayer(j)) then
                IniFile.WriteString('MechLayer' + IntToStr(i), 'Pair', Board.LayerName(ILayer.MechanicalLayer(j)) );
        end;
        IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'Enabled', MechLayer.MechanicalLayerEnabled);
        IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'Show',    MechLayer.IsDisplayed[Board]);
        IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'Sheet',   MechLayer.LinkToSheet);
        IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'SLM',     MechLayer.DisplayInSingleLayerMode);
        IniFile.WriteString('MechLayer' + IntToStr(i), 'Color',   ColorToString(Board.LayerColor[MechLayer.V6_LayerID]));
    end;
end;


Procedure ImportMechLayerInfo;
var
    PCBSysOpts : IPCB_SystemOptions;
    OpenDialog : TOpenDialog;
    MechLayer2 : IPCB_MechanicalLayer;
    MPairLayer : String;
    LColour    : TColor;

begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;
    PCBSysOpts := PCBServer.SystemOptions;
    If PCBSysOpts = Nil Then exit;

    OpenDialog        := TOpenDialog.Create(Application);
    OpenDialog.Title  := 'Import Mech Layer Names from *.ini file';
    OpenDialog.Filter := 'INI file (*.ini)|*.ini';

    Flag := OpenDialog.Execute;
    if (not Flag) then exit;

    FileName := OpenDialog.FileName;
    IniFile := TIniFile.Create(FileName);

    LayerStack := Board.LayerStack_V7;
    MechPairs  := Board.MechanicalPairs;

    For i := 1 To 32 do
    begin
        MechLayer := LayerStack.LayerObject_V7[ILayer.MechanicalLayer(i)];

        MechLayer.Name := IniFile.ReadString('MechLayer' + IntToStr(i), 'Name',  '');
        MPairLayer     := IniFile.ReadString('MechLayer' + IntToStr(i), 'Pair',  '');

    // remove existing mechpairs & add new ones.        
    // potentially new layer names in this file are of mech pair; only check parsed ones.
        for j := 1 to (i -1) do
        begin
            // remove pair including backwards ones !
            if MechPairs.PairDefined(ILayer.MechanicalLayer(j), ILayer.MechanicalLayer(i)) then
                MechPairs.RemovePair(ILayer.MechanicalLayer(j), ILayer.MechanicalLayer(i));
            if MechPairs.PairDefined(ILayer.MechanicalLayer(i), ILayer.MechanicalLayer(j)) then
                MechPairs.RemovePair(ILayer.MechanicalLayer(i), ILayer.MechanicalLayer(j));

            MechLayer2 := LayerStack.LayerObject_V7[ILayer.MechanicalLayer(j)];
            if (MPairLayer = MechLayer2.Name) and not MechPairs.PairDefined(ILayer.MechanicalLayer(j), ILayer.MechanicalLayer(i)) then
                MechPairs.AddPair(ILayer.MechanicalLayer(j), ILayer.MechanicalLayer(i));
        end;

        If Not MechLayer.MechanicalLayerEnabled then
            MechLayer.MechanicalLayerEnabled := IniFile.ReadBool('MechLayer' + IntToStr(i), 'Enabled', True);

        MechLayer.LinkToSheet              := IniFile.ReadBool('MechLayer' + IntToStr(i), 'Sheet', False);
        MechLayer.DisplayInSingleLayerMode := IniFile.ReadBool('MechLayer' + IntToStr(i), 'SLM',   False);
        MechLayer.IsDisplayed[Board]       := IniFile.ReadBool('MechLayer' + IntToStr(i), 'Show',  True);

        LColour := StringToColor( IniFile.ReadString('MechLayer' + IntToStr(i), 'Color', '0') );
        PCBSysOpts.LayerColors[ILayer.MechanicalLayer(i)] := LColour;
    end;

    ResetParameters;
    AddStringParameter('LayerName','Next');
    RunProcess('PCB:SetCurrentLayer');

    ResetParameters;
    AddStringParameter('LayerName','Previous');
    RunProcess('PCB:SetCurrentLayer');

    ShowInfo('The names & colours assigned to layers (& mech pairs) have been updated.');
end;
