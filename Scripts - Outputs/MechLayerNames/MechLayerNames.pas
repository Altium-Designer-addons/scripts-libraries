{ MechLayerNames.pas                                                              }
{ Summary   Used to export/import mech layer info to/from a text *.ini file.      }
{           Works on PcbDoc & PcbLib files.                                       }
{           Mechanical layer Names, Colours and MechanicalPairs can be            }
{           exported/imported to another PcbDoc/PcbLib file                       }
{                                                                                 }
{ Created by:    Petar Perisin                                                    }
{
 Modified by : B. Miller

Until TMechanicalLayerPair is solved..
Export: Can ONLY guess the Layer PairKind from LayerKinds & only assume Top & Bottom..
        User has to check/edit the ini file for the layer pair & set the PairKind.
Import: Can import Kinds PairKinds Top is always assumed at lower layer index than Bottom (listed first)

Legacy fallback for AD17/18; drop ML "kind" & MLPairKind but retain pairings & names etc.

TBD:
  explicit definition of top & bottom of each pair.

Notes:
*1 Board.LayerColor() strings are not right with AD21.

 AD21 Layer UI "Create CompLayer Pair" grabs the first free layers & then after you select target mech layer pair numbers..
      it leaves the defaults with same new mech layer names!!  At least they are not pairs!



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
 22/08/2022 1.44 Export: Board-LayerColor() colours are stale/wrong*1, use PCB SystemOption. Add 'Board Shape' const.
                 Import: Altium rearranges layer names when creating a pair; Force our layer names
 23/08/2022 1.45 try simplify process flow for import. Add LayerID to export & best guess for PairKinds.


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
    cNumPairKinds     = 12;        // number of layerpair kinds (inc. "Not Set")

var
    PCBSysOpts        : IPCB_SystemOptions;
    Board             : IPCB_Board;
    LayerStack        : IPCB_LayerStack_V7;
    LayerObj_V7       : IPCB_LayerObject_V7;
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
    LegacyMLS     : boolean;

function Version(const dummy : boolean) : TStringList;                      forward;
function LayerPairKindToStr(LPK : TMechanicalLayerPairKind) : WideString;   forward;
function LayerStrToPairKind(LPKS : WideString) : TMechanicalLayerPairKind;  forward;
function LayerKindToStr(LK : TMechanicalLayerKind) : WideString;            forward;
function LayerStrToKind(LKS : WideString) : TMechanicalLayerKind;           forward;
function FindAllMechPairLayers(LayerStack : IPCB_LayerStack, MLPS : IPCB_MechanicalLayerPairs) : TStringList; forward;
function FindUsedPairKinds(MLPS : IPCB_MechanicalLayerPairs) : TStringList;                 forward;
function GuessLayerPairKind(MLayerKind : TMechanicalLayerKind) : TMechanicalLayerPairKind; forward;
{.........................................................................................................}

Procedure UnPairCurrentMechLayer;
var
    ML1       : integer;
    i         : Integer;
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
    MechLayer        : IPCB_MechanicalLayer;
    SaveDialog       : TSaveDialog;
    dConfirm         : boolean;
    slMechLayerPairs : TStringList;
    slUsedPairKinds  : TStringList;
    ML1, ML2         : integer;
    i, j             : Integer;
    sColour          : WideString;
    bHasPairKinds    : boolean;

begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;
    PCBSysOpts := PCBServer.SystemOptions;
    If PCBSysOpts = Nil Then exit;

    VerMajor := Version(true).Strings(0);

    MaxMechLayers := AD17MaxMechLayers;
    LegacyMLS     := true;
    bHasPairKinds := false;
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

    LayerStack       := Board.LayerStack_V7;
    MechLayerPairs   := Board.MechanicalPairs;
//    slMechLayerPairs := FindAllMechPairLayers(LayerStack, MechLayerPairs);
    slUsedPairKinds  := FindUsedPairKinds(MechLayerPairs);
    if slUsedPairKinds.Count > 0 then bHasPairKinds := true;

    for i := 1 to MaxMechLayers do
    begin
        ML1 := LayerUtils.MechanicalLayer(i);
        MechLayer := LayerStack.LayerObject_V7[ML1];

        if (i <= AllLayerDataMax) or MechLayer.MechanicalLayerEnabled then
        begin
            MLayerKind := NoMechLayerKind;
            if not LegacyMLS then
                MLayerKind := MechLayer.Kind;
            MLayerKindStr := LayerKindToStr(MLayerKind);

// Board.LayerColor[ML1] are wrong or stale.
            sColour := ColorToString( PCBSysOpts.LayerColors(ML1) );

            IniFile.WriteString('MechLayer' + IntToStr(i), 'Name',    Board.LayerName(ML1) );       // MechLayer.Name);
            IniFile.WriteString('MechLayer' + IntToStr(i), 'Layer',   IntToStr(ML1) );              // MechLayer.Layer);
            IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'Enabled', MechLayer.MechanicalLayerEnabled);
            IniFile.WriteString('MechLayer' + IntToStr(i), 'Kind',    MLayerKindStr);
            IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'Show',    MechLayer.IsDisplayed[Board]);
            IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'Sheet',   MechLayer.LinkToSheet);
            IniFile.WriteBool  ('MechLayer' + IntToStr(i), 'SLM',     MechLayer.DisplayInSingleLayerMode);
            IniFile.WriteString('MechLayer' + IntToStr(i), 'Color',   sColour);

// if layer has valid "Kind", STILL need (our) explicit pairing to be set.
            for j := 1 to MaxMechLayers do
            begin
                ML2 := LayerUtils.MechanicalLayer(j);
                if MechLayerPairs.PairDefined(ML1, ML2) then
                begin
// can NOT determine the LayerPair layers from PairIndex because TMechanicalLayerPair wrapper is borked so can NOT determine the PairKinds.
// if only .PairDefined(L1,L2) had returned the PairIndex instead of boolean.
// but then NO guarantee ML1 is comp top or bottom side!

// make a guess for PairKind from single layer Kind.
                    MLayerPairKindStr := LayerPairKindToStr(NoMechLayerKind);
// don't assume a pair kind unless they are already used in PcbDoc.
                    if bHasPairKinds then
                    begin
                        MLayerPairKindStr := LayerPairKindToStr( GuessLayerPairKind(MLayerKind) );
                        if (slUsedPairKinds.IndexOf(MLayerPairKindStr) < 0) then
                            MLayerPairKindStr := LayerPairKindToStr(NoMechLayerKind);
                    end;

                    IniFile.WriteString ('MechLayer' + IntToStr(i), 'Pair',      Board.LayerName(ML2) );
                    IniFile.WriteInteger('MechLayer' + IntToStr(i), 'PairLayer', ML2 );
                    IniFile.WriteString ('MechLayer' + IntToStr(i), 'PairKind',  MLayerPairKindStr );
                end;
            end;
        end;
    end;
    IniFile.Free;
    EndHourGlass;
    ShowMessage('Warning: LayerPair Kinds are ONLY a best guess. ' + #13
                +' Check the inifile PairKind values. ');
end;

Procedure ImportMechLayerInfo;
var
    OpenDialog         : TOpenDialog;
    MechLayer          : IPCB_MechanicalLayer;
    MechLayer2         : IPCB_MechanicalLayer;
    MPairLayer         : WideString;
    MLayerKind2        : TMechanicalLayerKind;
    MLayerPairKind2    : TMechanicalLayerPairKind;
    MLayerKindStr2     : WideString;
    MLayerPairKindStr2 : WideString;
    LayerName1         : WideString;
    LayerName2         : WideString;
    Pair2LID           : integer;
    LColour            : TColor;
    ML1, ML2           : integer;
    i, j               : Integer;

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

// remove any existing pairs connected to all layers listed in inifile.
// set all new layer names
    for i := 1 To MaxMechLayers do
    begin
        ML1 := LayerUtils.MechanicalLayer(i);

        if IniFile.SectionExists('MechLayer' + IntToStr(i)) then
        begin
            MechLayer := LayerStack.LayerObject_V7[ML1];
            LayerName1 := IniFile.ReadString('MechLayer' + IntToStr(i), 'Name', 'eMech' + IntToStr(i));
            MechLayer.Name := LayerName1;

            for j := (i + 1) to MaxMechLayers do
            begin
                if i = j then continue;

                ML2 := LayerUtils.MechanicalLayer(j);
//        remove any pair including backwards ones !
                if MechLayerPairs.PairDefined(ML2, ML1) then
                    MechLayerPairs.RemovePair(ML2, ML1);
                if MechLayerPairs.PairDefined(ML1, ML2) then
                    MechLayerPairs.RemovePair(ML1, ML2);
            end;
        end;
    end;

// add single settings & new pairs
    for i := 1 To MaxMechLayers do
    begin
        MLayerKind := NoMechLayerKind;
        ML1 := LayerUtils.MechanicalLayer(i);
        MechLayer := LayerStack.LayerObject_V7[ML1];

        if IniFile.SectionExists('MechLayer' + IntToStr(i)) then
        begin
            LayerName1 := IniFile.ReadString('MechLayer' + IntToStr(i), 'Name', 'eMech' + IntToStr(i));
            MechLayer.Name := LayerName1;

//    allow turn Off -> ON only, default Off for missing entries
            If Not MechLayer.MechanicalLayerEnabled then
                MechLayer.MechanicalLayerEnabled := IniFile.ReadBool ('MechLayer' + IntToStr(i), 'Enabled',   False);

            MLayerKindStr                      := IniFile.ReadString ('MechLayer' + IntToStr(i), 'Kind',      LayerKindToStr(NoMechLayerKind) );
            MPairLayer                         := IniFile.ReadString ('MechLayer' + IntToStr(i), 'Pair',      '');
            Pair2LID                           := IniFile.ReadInteger('MechLayer' + IntToStr(i), 'PairLayer', 0);
            MLayerPairKindStr                  := IniFile.ReadString ('MechLayer' + IntToStr(i), 'PairKind',  LayerPairKindToStr(NoMechLayerKind) );
            MechLayer.LinkToSheet              := IniFile.ReadBool   ('MechLayer' + IntToStr(i), 'Sheet',     False);
            MechLayer.DisplayInSingleLayerMode := IniFile.ReadBool   ('MechLayer' + IntToStr(i), 'SLM',       False);
            MechLayer.IsDisplayed[Board]       := IniFile.ReadBool   ('MechLayer' + IntToStr(i), 'Show',      True);
            LColour                            := IniFile.ReadString ('MechLayer' + IntToStr(i), 'Color',     NoColour);
            if LColour <> NoColour then
                PCBSysOpts.LayerColors(ML1) := StringToColor( LColour);

            MLayerKind     := LayerStrToKind(MLayerKindStr);
            MLayerPairKind := LayerStrToPairKind(MLayerPairKindStr);
//    new "kind" pairs is a separate property & single layers each have a Kind
            if not LegacyMLS then
                MechLayer.Kind  := MLayerKind;

//   if no key for any Pair then go around.
            if MPairLayer = '' then continue;

//    ignore already processed layers.
            for j := (i + 1) to MaxMechLayers do
            begin
                if i = j then continue;

                MLayerKind2     := NoMechLayerKind;
                MLayerPairKind2 := NoMechLayerKind;
                ML2             := LayerUtils.MechanicalLayer(j);
                MechLayer2      := LayerStack.LayerObject_V7(ML2);

                LayerName2         := IniFile.ReadString('MechLayer' + IntToStr(j), 'Name', 'Mechanical ' + IntToStr(j));
                MLayerKindStr2     := IniFile.ReadString('MechLayer' + IntToStr(j), 'Kind',      LayerKindToStr(NoMechLayerKind) );
                MLayerPairKindStr2 := IniFile.ReadString('MechLayer' + IntToStr(j), 'PairKind',  LayerPairKindToStr(NoMechLayerKind) );
                MLayerKind2        := LayerStrToKind(MLayerKindStr2);
                MLayerPairKind2    := LayerStrToPairKind(MLayerPairKindStr2);

//    if 2nd of the pair is not listed in initfile
//                if not IniFile.SectionExists('MechLayer' + IntToStr(j)) then
//                begin
//                     use new Layer number
//                end;

// does ML2 name (from file) match ML1 paired layer name
                MechPairIdx := -1;
                if (MPairLayer = LayerName2) and not MechLayerPairs.PairDefined(ML1, ML2) then
                    MechPairIdx := MechLayerPairs.AddPair(ML1, ML2);    // (i, j)       // index? to what FFS

                if not LegacyMLS then
                begin
                    MechLayer2.Kind := MLayerKind2;
                    if (MechPairIdx > -1) then
                    begin
                        MechLayerPair := MechLayerPairs.LayerPair(MechPairIdx);
                        MechLayerPairs.LayerPairKind(MechPairIdx) := MLayerPairKind;

                        if MLayerPairKind <> MLayerPairKind2 then
                        ShowMessage('mismatch pair kinds ' + LayerName1 + '---' + LayerName2);
                    end;
                end;

// Creating pairs automatically changes the names to Top & Bottom keywords first!
// Altium tries to force/dictate its naming convention Top Bottom first so rewrite our names.
                if (MechPairIdx > -1) then
                begin
                    MechLayer.Name  := LayerName1;
                    MechLayer2.Name := LayerName2;
                    break;
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
var
    MechLayer  : IPCB_MechanicalLayer;
    ML1        : integer;
    i          : Integer;

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
    for i:= 0 to (MechLayerPairs.Count - 1) do
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
    30              : Result := 'Board Shape';
    else              Result := 'Unknown'
    end;
end;

function LayerStrToKind(LKS : WideString) : TMechanicalLayerKind;
var
    I : integer;
begin
    Result := -1;
    for I := 0 to 31 do
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
//    Result.Duplicates    := dupAccept;   // requires .Sort
    Result.DelimitedText := Client.GetProductVersion;
end;

function FindAllMechPairLayers(LayerStack : IPCB_LayerStack, MLPS : IPCB_MechanicalLayerPairs) : TStringList;
// is this list always in the same order as MechanicalPairs ??
// no it is NOT & higher layer number can be top side layer !!
var
    MechLayer1    : IPCB_MechanicalLayer;
    MechLayer2    : IPCB_MechanicalLayer;
    ML1, ML2      : integer;
    i, j          : Integer;
begin
    Result := TStringList.Create;
    Result.StrictDelimiter := true;
    Result.Delimiter := '|';
    Result.NameValueSeparator := '=';

    for i := 1 to MaxMechLayers do
    begin
        ML1        := LayerUtils.MechanicalLayer(i);
        MechLayer1 := LayerStack.LayerObject_V7(ML1);

        if MechLayer1.MechanicalLayerEnabled then
        begin
            for j := (i + 1) to MaxMechLayers do
            begin
                ML2        := LayerUtils.MechanicalLayer(j);
                MechLayer2 := LayerStack.LayerObject_V7(ML2);
                if MechLayer2.MechanicalLayerEnabled then
                if MLPS.PairDefined(ML1, ML2) then
//                if (MLPS.LayerUsed(ML1) and MLPS.LayerUsed(ML2)) then
                    Result.Add(IntToStr(ML1) + '=' + IntToStr(ML2));
            end;
        end;
    end;
end;

function FindUsedPairKinds(MLPS : IPCB_MechanicalLayerPairs) : TStringList;
var
    i        : integer;
    PairKind : TMechanicalLayerPairKind;
begin
    Result := TStringList.Create;
    if LegacyMLS then exit;

    for i:= 0 to (MLPS.Count - 1) do
    begin
        PairKind := MLPS.LayerPairKind(i);
        Result.Add(LayerPairKindToStr(PairKind));
    end;
end;

function GuessLayerPairKind(MLayerKind : TMechanicalLayerKind) : TMechanicalLayerPairKind;
var
    MLayerKindStr : WideString;
    MLPK          : WideString;
    I             : integer;
begin
    Result        := NoMechLayerKind;
    MLayerKindStr := LayerKindToStr(MLayerKind);
    for I := 0 to cNumPairKinds do
    begin
        MLPK := LayerPairKindToStr(I);
        if (MLPK <> '') and (MLPK <> ' ') then
        if ansipos(MLPK, MLayerKindStr) > 0 then
        begin
            Result := I;
            break;
        end;    
    end;
end;
                   
// unused fn
function GetMechLayerPairKind(LKS : WideString) : TMechanicalLayerPairKind;
// RootKind : basename of layer kind (Assembly Top, Coating, Courtyard etc)
var
    WPos : integer;
    I    : integer;
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
