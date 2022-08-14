{ MechLayerNames.pas                                                              }
{ Summary   Used to export/import mech layer info to/from a text *.ini file.      }
{           Works on PcbDoc & PcbLib files.                                       }
{           Mechanical layer Names, Colours and MechanicalPairs can be            }
{           exported/imported to another PcbDoc/PcbLib file                       }
{                                                                                 }
{ Created by:    Petar Perisin                                                    }
{
 Modified by : B. Miller
 Date        Ver  Comment
 03/07/2017  : mod to fix layer names output, why was import okay ??
 23/02/2018  : added MechLayer Pairs & colours, still loads old ini files.
 18/08/2019  : Layer colours above eMech16 are all cBlack so ignore.
 28/08/2019  : Try improve import default filepath.
 12/09/2019  : Layer tab display refresh without "flashing" & fix colours for all layers.
 18/09/2019  : Versioncheck & set max mech layers for AD19 & later
 19/09/2019 1.1  Export: Limit continuous sequential layer export listing to below AllLayerDataMax
                 Import: test the Layer Section Key exists.
 02/10/2019 1.2  AD19 mechlayer Kind with legacy conversion back to layer pair(s)
 15/10/2019 1.3  Support all (known) mech layer kinds & component layer pairs.
 16/02/2020 1.4  Add proc for AD ver >= 19; Convert MechLayer Kinds to Legacy mech style.
 01/07/2020 1.41 convert version major to int value to test.
 23/03/2021 1.42 Add bit more PairKind support; fix Legacy to remove all LayerPairKinds & LayerKinds
                 Export over an existing file with user Yes/No prompt.
 24/08/2021 1.43 Added UnPairCurrentMechLayer()

Until TMechanicalLayerPair is solved..
Can NOT correctly/completely export the Layer Pair but CAN import them.. so user has to edit the ini file
to set the top & bottom properties of the layer pair.

 Color strings are not right with AD21.
 AD21 Layer UI "Create CompLayer Pair" grabs the first free layers & then after you select target mech layer pair numbers..
      it leaves the defaults with same new mech layer names!!  At least they are not pairs!


Notes: Legacy fallback for AD17/18; drop ML "kind" & MLPairKind but retain pairings & names etc.
LayerKinds & LayerPairKinds are best guess & the associated text is user defined.

  TMechanicalLayerToKindItem
.............................................................................................
//UnPairCurrentMechLayer()
Process : RunScriptText
Text=Var B;C;I;P;M; begin B:=PCBServer.GetCurrentPCBBoard;P:=B.MechanicalPairs;C:=B.CurrentLayer;for I:=1 To 1024 do begin M:=LayerUtils.MechanicalLayer(I);if P.PairDefined(C, M) then begin P.RemovePair(C, M);ShowInfo('UnPaired Mech Layers :' + Layer2String(C) + ' & ' + Layer2String(M));break;end; end;B.ViewManager_UpdateLayerTabs;end;
..................................................................................}

const
    NoColour          = 'ncol';
    AD19VersionMajor  = 19;
    AD17MaxMechLayers = 32;       // scripting API has broken consts from TV6_Layer
    AD19MaxMechLayers = 1024;
    AllLayerDataMax   = 16;       // after this mech layer index only report the actual active layers.
    NoMechLayerKind   = 0;        // enum const does not exist for AD17/18
    ctTop             = 'Top';    // text used denote mech layer kind pairs.
    ctBottom          = 'Bottom';

var
    Board             : IPCB_Board;
    LayerStack        : IPCB_LayerStack_V7;
    LayerObj_V7       : IPCB_LayerObject_V7;
    MechLayer         : IPCB_MechanicalLayer;
    MLayerKind        : TMechanicalLayerKind;
    MLayerPairKind    : TMechanicalLayerPairKind;
    MLayerKindStr     : WideString;
    MLayerPairKindStr : WideString;
    MechLayerPairs    : IPCB_MechanicalLayerPairs;
    MechLayerPair     : TMechanicalLayerPair;       // IPCB_MechanicalLayerPairs.LayerPair(MechPairIdx)
    MechPairIdx       : integer;                    // index of above
    VerMajor          : WideString;
    MaxMechLayers     : integer;
    FileName      : String;
    INIFile       : TIniFile;
    Flag          : Integer;
    ML1, ML2      : integer;
    i, j          : Integer;
    LegacyMLS     : boolean;

function Version(const dummy : boolean) : TStringList;                     forward;
function LayerPairKindToStr(LPK : TMechanicalLayerPairKind) : WideString;  forward;
function LayerStrToPairKind(LPKS : WideString) : TMechanicalLayerPairKind; forward;
function LayerKindToStr(LK : TMechanicalLayerKind) : WideString;           forward;
function LayerStrToKind(LKS : WideString) : TMechanicalLayerKind;          forward;
{.........................................................................................................}

Procedure UnPairCurrentMechLayer;
var
    CurrLayer : TLayer;
begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

    VerMajor := Version(true).Strings(0);

    MaxMechLayers := AD17MaxMechLayers;
    if (StrToInt(VerMajor) >= AD19VersionMajor) then
    begin
        MaxMechLayers := AD19MaxMechLayers;
    end;

    MechLayerPairs := Board.MechanicalPairs;
    CurrLayer      := Board.CurrentLayer;

    for i := 1 To MaxMechLayers do
    begin
        ML1 := LayerUtils.MechanicalLayer(i);
        if MechLayerPairs.PairDefined(CurrLayer, ML1) then
        begin
             MechLayerPairs.RemovePair(CurrLayer, ML1);
             ShowInfo('UnPaired Mechanical Layers :' + Board.LayerName(CurrLayer) + ' and ' + Board.LayerName(ML1) );
             break;
        end;
    end;

    Board.ViewManager_UpdateLayerTabs;
end;

Procedure ExportMechLayerInfo;
var
    SaveDialog       : TSaveDialog;
    dConfirm         : boolean;
    slMechLayerPairs : TStringList;

begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

    VerMajor := Version(true).Strings(0);

    MaxMechLayers := AD17MaxMechLayers;
    LegacyMLS     := true;
    if (StrToInt(VerMajor) >= AD19VersionMajor) then
    begin
        LegacyMLS     := false;
        MaxMechLayers := AD19MaxMechLayers;
    end;

    SaveDialog        := TSaveDialog.Create(Application);
    SaveDialog.Title  := 'Save Mech Layer Names to *.ini file';
    SaveDialog.Filter := 'INI file (*.ini)|*.ini';
    FileName := ExtractFilePath(Board.FileName);
    SaveDialog.FileName := ChangeFileExt(FileName, '');

// UI prompt for file & set extension
    Flag := SaveDialog.Execute;
    if (Flag = 0) then exit;
    FileName := SaveDialog.FileName;
    FileName := ChangeFileExt(FileName, '.ini');

    if FileExists(Filename, false) then
    begin
       dConfirm := ConfirmNoYesWithCaption('File of that name already exists.. ','Overwrite ' + ExtractFileName(Filename) + ' ?');
       if dConfirm then
           DeleteFile(FileName)
       else
           exit;
    end;
    IniFile := TIniFile.Create(FileName);

    BeginHourGlass(crHourGlass);

    LayerStack     := Board.LayerStack_V7;
    MechLayerPairs := Board.MechanicalPairs;

    for i := 1 to MaxMechLayers do
    begin
        ML1 := LayerUtils.MechanicalLayer(i);
        MechLayer := LayerStack.LayerObject_V7[ML1];

        if (i <= AllLayerDataMax) or MechLayer.MechanicalLayerEnabled then
        begin
            MLayerKind := NoMechLayerKind;
            if not LegacyMLS then MLayerKind := MechLayer.Kind;
            MLayerKindStr := LayerKindToStr(MLayerKind);

            IniFile.WriteString('MechLayer' + IntToStr(i), 'Name',    Board.LayerName(ML1) );       // MechLayer.Name);
            IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'Enabled', MechLayer.MechanicalLayerEnabled);
            IniFile.WriteString('MechLayer' + IntToStr(i), 'Kind',    MLayerKindStr);
            IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'Show',    MechLayer.IsDisplayed[Board]);
            IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'Sheet',   MechLayer.LinkToSheet);
            IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'SLM',     MechLayer.DisplayInSingleLayerMode);
            IniFile.WriteString('MechLayer' + IntToStr(i), 'Color',   ColorToString(Board.LayerColor[ML1]) );

// if layer has valid "Kind", STILL need (our) explicit pairing to be set.
            for j := 1 to MaxMechLayers do
            begin
                ML2 := LayerUtils.MechanicalLayer(j);
                if MechLayerPairs.PairDefined(ML1, ML2) then
                begin
// can NOT determine the LayerPair layers from PairIndex because TMechanicalLayerPair wrapper is borked so can NOT determine the PairKinds.
// if only .PairDefined(,) had returned the PairIndex
// but then NO guarantee ML1 is comp top or bottom side!
                    MLayerPairKindStr := LayerPairKindToStr(NoMechLayerKind);
                    IniFile.WriteString('MechLayer' + IntToStr(i), 'Pair',     Board.LayerName(ML2) );
                    IniFile.WriteString('MechLayer' + IntToStr(i), 'PairKind', MLayerPairKindStr );
                end;
            end;
        end;
    end;
    IniFile.Free;
    EndHourGlass;
end;


Procedure ImportMechLayerInfo;
var
    PCBSysOpts         : IPCB_SystemOptions;
    OpenDialog         : TOpenDialog;
    MechLayer2         : IPCB_MechanicalLayer;
    MPairLayer         : WideString;
    MLayerKind2        : TMechanicalLayerKind;
    MLayerPairKind2    : TMechanicalLayerPairKind;
    MLayerKindStr2     : WideString;
    MLayerPairKindStr2 : WideString;
    LColour            : TColor;

begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;
    PCBSysOpts := PCBServer.SystemOptions;
    If PCBSysOpts = Nil Then exit;

    VerMajor := Version(true).Strings(0);

    MaxMechLayers := AD17MaxMechLayers;
    LegacyMLS     := true;
    if (StrToInt(VerMajor) >= AD19VersionMajor) then
    begin
        MaxMechLayers := AD19MaxMechLayers;
        LegacyMLS     := false;
    end;

    OpenDialog        := TOpenDialog.Create(Application);
    OpenDialog.Title  := 'Import Mech Layer Names from *.ini file';
    OpenDialog.Filter := 'INI file (*.ini)|*.ini';
//    OpenDialog.InitialDir := ExtractFilePath(Board.FileName);
    OpenDialog.FileName := '';
    Flag := OpenDialog.Execute;
    if (Flag = 0) then exit;

    FileName := OpenDialog.FileName;
    IniFile := TIniFile.Create(FileName);

    BeginHourGlass(crHourGlass);
    LayerStack     := Board.LayerStack_V7;
    MechLayerPairs := Board.MechanicalPairs;

    for i := 1 To MaxMechLayers do
    begin
        MLayerKind := NoMechLayerKind;
        ML1 := LayerUtils.MechanicalLayer(i);
        MechLayer := LayerStack.LayerObject_V7[ML1];

        if IniFile.SectionExists('MechLayer' + IntToStr(i)) then
        begin
            MechLayer.Name := IniFile.ReadString('MechLayer' + IntToStr(i), 'Name', 'eMech' + IntToStr(i));

//    allow turn Off -> ON only, default Off for missing entries
            If Not MechLayer.MechanicalLayerEnabled then
                MechLayer.MechanicalLayerEnabled := IniFile.ReadBool('MechLayer' + IntToStr(i), 'Enabled', False);

            MLayerKindStr                      := IniFile.ReadString('MechLayer' + IntToStr(i), 'Kind',     LayerKindToStr(NoMechLayerKind) );
            MPairLayer                         := IniFile.ReadString('MechLayer' + IntToStr(i), 'Pair',     '');
            MLayerPairKindStr                  := IniFile.ReadString('MechLayer' + IntToStr(i), 'PairKind', LayerPairKindToStr(NoMechLayerKind) );
            MechLayer.LinkToSheet              := IniFile.ReadBool  ('MechLayer' + IntToStr(i), 'Sheet',    False);
            MechLayer.DisplayInSingleLayerMode := IniFile.ReadBool  ('MechLayer' + IntToStr(i), 'SLM',      False);
            MechLayer.IsDisplayed[Board]       := IniFile.ReadBool  ('MechLayer' + IntToStr(i), 'Show',     True);
            LColour                            := IniFile.ReadString('MechLayer' + IntToStr(i), 'Color',    NoColour);
            if LColour <> NoColour then
                PCBSysOpts.LayerColors(ML1) := StringToColor( LColour);

            MLayerKind     := LayerStrToKind(MLayerKindStr);
            MLayerPairKind := LayerStrToPairKind(MLayerPairKindStr);
//    new "kind" pairs are treated individually by kind but are still a Pair
            if not LegacyMLS then
                MechLayer.Kind  := MLayerKind;

//    remove existing mechlayerpairs & add new ones.
//    potentially new layer names in this file are of mech layer pair; only check parsed ones.
            for j := 1 to (i - 1) do
            begin
                MechPairIdx        := -1;
                ML2                := LayerUtils.MechanicalLayer(j);
                MechLayer2         := LayerStack.LayerObject_V7(ML2);
                MLayerKindStr2     := IniFile.ReadString('MechLayer' + IntToStr(j), 'Kind',      LayerKindToStr(NoMechLayerKind) );
                MLayerPairKindStr2 := IniFile.ReadString('MechLayer' + IntToStr(j), 'PairKind',  LayerPairKindToStr(NoMechLayerKind) );
                MLayerKind2        := LayerStrToKind(MLayerKindStr2);
                MLayerPairKind2    := LayerStrToPairKind(MLayerPairKindStr2);

//        remove pair including backwards ones !
                if MechLayerPairs.PairDefined(ML2, ML1) then
                    MechLayerPairs.RemovePair(ML2, ML1);
                if MechLayerPairs.PairDefined(ML1, ML2) then
                    MechLayerPairs.RemovePair(ML1, ML2);

                if (MPairLayer = MechLayer2.Name) and not MechLayerPairs.PairDefined(ML2, ML1) then
                    MechPairIdx := MechLayerPairs.AddPair(ML2, ML1);    // yes, intentional (j, i) 

                if not LegacyMLS then
                begin
                    MechLayer2.Kind  := MLayerKind2;
                    if (MechPairIdx > -1) then
                    if MLayerPairKind > NoMechLayerKind then
                    if MLayerPairKind = MLayerPairKind2 then
                    begin
                        MechLayerPair := MechLayerPairs.LayerPair(MechPairIdx);
                        MechLayerPairs.LayerPairKind(MechPairIdx) := MLayerPairKind;
                    end;
                end;
            end;

        end; // section exists
    end;

    EndHourGlass;
    IniFile.Free;
    Board.ViewManager_UpdateLayerTabs;
    ShowInfo('Mechanical Layer Names & Colours (& pairs) updated.');
end;

Procedure ConvertMechLayerKindToLegacy;
begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;

    VerMajor := Version(true).Strings(0);

    MaxMechLayers := AD17MaxMechLayers;
    LegacyMLS     := true;
    MLayerKind := NoMechLayerKind;
    if (StrToInt(VerMajor) >= AD19VersionMajor) then
    begin
        MaxMechLayers := AD19MaxMechLayers;
        LegacyMLS     := false;
    end else
    begin
        ShowMessage('Requires AD19 or later to convert ');
        exit;
    end;

    LayerStack     := Board.LayerStack_V7;
    MechLayerPairs := Board.MechanicalPairs;

// Could check a "PairKind" pair does have legacy "Pair" set..
// MechLayerPairs.PairKind(index) ; need index ?? no solution.. can not do this.
    for I:= 0 to (MechLayerPairs.Count - 1) do
    begin
        MechLayerPair := MechLayerPairs.LayerPair(i);
        MechLayerPairs.SetState_LayerPairKind(i) := NoMechLayerKind;
    end;

    for i := 1 To MaxMechLayers do
    begin
        ML1 := LayerUtils.MechanicalLayer(i);
        MechLayer := LayerStack.LayerObject_V7[ML1];

        MechLayer.Kind := NoMechLayerKind;       //  'Not Set'
    end;

    Board.ViewManager_UpdateLayerTabs;
    ShowInfo('Converted Mechanical Layer Kinds To Legacy ..');
end;

{------------------------------------------------------------------------------------}
function LayerPairKindToStr(LPK : TMechanicalLayerPairKind) : WideString;
begin
    case LPK of
    NoMechLayerKind : Result := 'Not Set';            // single
    1               : Result := 'Assembly';
    2               : Result := 'Coating';
    3               : Result := 'Component Center';
    4               : Result := 'Component Outline';
    5               : Result := 'Courtyard';
    6               : Result := 'Designator';
    7               : Result := 'Dimensions';
    8               : Result := 'Glue Points';
    9               : Result := 'Gold Plating';
    10              : Result := 'Value';
    11              : Result := '3D Body';
    else              Result := 'Unknown'
    end;
end;

function LayerStrToPairKind(LPKS : WideString) : TMechanicalLayerPairKind;
var
    I : integer;
begin
    Result := -1;
    for I := 0 to 12 do
    begin
         if LayerPairKindToStr(I) = LPKS then
         begin
             Result := I;
             break;
         end;
    end;
end;

function LayerKindToStr(LK : TMechanicalLayerKind) : WideString;
begin
    case LK of
    NoMechLayerKind : Result := 'Not Set';            // single
    1               : Result := 'Assembly Top';
    2               : Result := 'Assembly Bottom';
    3               : Result := 'Assembly Notes';     // single
    4               : Result := 'Board';
    5               : Result := 'Coating Top';
    6               : Result := 'Coating Bottom';
    7               : Result := 'Component Center Top';
    8               : Result := 'Component Center Bottom';
    9               : Result := 'Component Outline Top';
    10              : Result := 'Component Outline Bottom';
    11              : Result := 'Courtyard Top';
    12              : Result := 'Courtyard Bottom';
    13              : Result := 'Designator Top';
    14              : Result := 'Designator Bottom';
    15              : Result := 'Dimensions';         // single
    16              : Result := 'Dimensions Top';
    17              : Result := 'Dimensions Bottom';
    18              : Result := 'Fab Notes';         // single
    19              : Result := 'Glue Points Top';
    20              : Result := 'Glue Points Bottom';
    21              : Result := 'Gold Plating Top';
    22              : Result := 'Gold Plating Bottom';
    23              : Result := 'Value Top';
    24              : Result := 'Value Bottom';
    25              : Result := 'V Cut';             // single
    26              : Result := '3D Body Top';
    27              : Result := '3D Body Bottom';
    28              : Result := 'Route Tool Path';   // single
    29              : Result := 'Sheet';             // single
    else              Result := 'Unknown'
    end;
end;

function LayerStrToKind(LKS : WideString) : TMechanicalLayerKind;
var
    I : integer;
begin
    Result := -1;
    for I := 0 to 30 do
    begin
         if LayerKindToStr(I) = LKS then
         begin
             Result := I;
             break;
         end;
    end;
end;

function Version(const dummy : boolean) : TStringList;
begin
    Result               := TStringList.Create;
    Result.Delimiter     := '.';
    Result.Duplicates    := dupAccept;
    Result.DelimitedText := Client.GetProductVersion;
end;

// unused fn
function GetMechLayerPairKind(LKS : WideString) : TMechanicalLayerPairKind;
// RootKind : basename of layer kind (Assembly Top, Coating, Courtyard etc)
var
    WPos : integer;
begin
    Result := -1;
// must contain top or bottom
    WPos := AnsiPos(ctTop,LKS);
    if WPos < 1 then
    begin
        WPos := AnsiPos(ctBottom,LKS);
        if WPos < 1 then exit;
    end;

// must contain PairKind
    for I := 0 to 12 do
    begin
         if ansipos(LayerPairKindToStr(I), LKS) = 1 then
         begin
             Result := I;
             break;
         end;
    end;
end;
