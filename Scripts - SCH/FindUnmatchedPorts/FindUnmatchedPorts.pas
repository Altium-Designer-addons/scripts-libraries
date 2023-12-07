{..............................................................................}
{ Summary   This script is used to find any floating ports within schematic    }
{           sheets. It will search all schematic sheets within the project,    }
{           and report which sheets have an unconnected port. Floating ports   }
{           are reported in the Messages panel.                                }
{                                                                              }
{           Ports are considered floating on a given schematic sheet if they   }
{           are not connected to either a component pin or sheet entry.        }
{                                                                              }
{           Script will also report invalid combinations of port directions    }
{           such as multiple output ports in the same net, bidirectional ports }
{           connected to input ports, etc. Nets with unmatched ports will be   }
{           added to Messages panel.                                           }
{                                                                              }
{ Created by:    Corey Beyer & Ryan Rutledge                                   }
{..............................................................................}

{..............................................................................}
const
    cScriptName = 'FindUnmatchedPorts';
    cScriptVersion = '1.11';
    IMG_Error = 4;
    IMG_Port = 17;
    IMG_YellowSquare = 72;
    IMG_OrangeSquare = 73;
    IMG_RedSquare = 74;
    sLineBreak2 = sLineBreak + sLineBreak;

var
    Workspace : IWorkSpace;
    MessagesManager : IMessagesManager;
    CurrentDocument : IDocument;

    ComponentList : TInterfaceList;
    GlobalFloatingPortList : TInterfaceList;
    FloatingPortList : TInterfaceList;
    PortList : TInterfaceList;
    SheetSymbolList : TInterfaceList;
    UnmatchedPortNetList : TStringList;
    MixedPortNameNetlist : TStringList;


procedure   _Start; forward;
procedure   About; forward;
procedure   AddMessage_FloatingPort(Port : INetItem); forward;
procedure   AddMessage_MixedPortNames(MixedPortNameList : TInterfaceList); forward;
procedure   AddMessage_UnmatchedPorts(UnmatchedPortList : TInterfaceList); forward;
function    Configure(Parameters : String) : String; forward;
procedure   FillComponentList(Const Dummy : Integer = 0); forward;
procedure   FillFloatingPortList(Const Dummy : Integer = 0); forward;
procedure   FillMixedPortNameNetList(const dummy : Integer = 0); forward;
procedure   FillPortList(Document : IDocument); forward;
procedure   FillSheetSymbolList(Const Dummy : Integer = 0); forward;
procedure   FillUnmatchedPortNetList(const dummy : Integer = 0); forward;
procedure   FindFloatingPorts(ShowMessages : Boolean); forward;
procedure   FindUnmatchedPorts(ShowMessages : Boolean); forward;
procedure   Generate(Parameters : String); forward;
function    GetCompNets(Comp : IComponent) : TStringList; forward;
function    GetUnmatchedReason(UnmatchedPortList : TInterfaceList) : String; forward;
function    GetSheetSymbolNets(SheetSymbol : ISheetSymbol) : TStringList; forward;
procedure   GlobalInit(dummy : Integer = 0); forward;
function    PortDirectionString(Port : INetItem) : String; forward;
function    PredictOutputFileNames(Parameters : String) : String; forward;
procedure   RemoveFloatingPortsFromUnmatchedList(dummy : Integer = 0); forward;
procedure   Start; forward;


procedure   _Start;
begin
    Start;
end;


procedure   About;
var
    MsgText : string;
begin
    MsgText := '"' + cScriptName + '" script version ' + cScriptVersion + sLineBreak +
        sLineBreak +
        'Use "_Start" or "Start" to find unmatched or floating ports within project.' + sLineBreak +
        sLineBreak +
        'Updated versions and documentation may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries' + sLineBreak +
        '/tree/master/Scripts - SCH/FindUnmatchedPorts';

    ShowInfo(MsgText, 'About');
end;


procedure   AddMessage_FloatingPort(Port : INetItem);
var
    MessageText : String;
    MessageCallBackProcess : String;
    MessageCallBackParameters : String;
    MessageDetails : IDXPMessageItemDetails;
begin
    MessageText := Port.DM_LongDescriptorString+' floating on sheet '+Port.DM_OwnerDocumentName+' at '+Port.DM_LocationString;
    MessageCallBackProcess := 'WorkspaceManager:View';
    MessageCallBackParameters := 'CrossProbeNotifySCH='+Port.DM_FullCrossProbeString+'|SchHandle='+Port.DM_SchHandle+'|Kind=ERCObject|TargetFilename='+Port.DM_OwnerDocumentName+'|FullDocumentPath='+Port.DM_OwnerDocumentFullPath;

    MessageDetails := GetWorkSpace.DM_MessagesManager.CreateMesssageItemDetails;
    MessageDetails.RootItem.AddSubItem(MessageText, IMG_Error, '', '');
    MessageDetails.RootItem.Item[0].AddSubItem(Port.DM_LongDescriptorString+' floating on sheet '+Port.DM_OwnerDocumentName+' at '+Port.DM_LocationString, IMG_Port, MessageCallBackProcess, MessageCallBackParameters);

    MessagesManager.BeginUpdate;
    MessagesManager.AddMessage2({MessageClass                 } '[Fatal Error]',
                               {MessageText                  } MessageText,
                               {MessageSource                } cScriptName + ' Script',
                               {MessageDocument              } Port.DM_OwnerDocumentName,
                               {MessageCallBackProcess       } MessageCallBackProcess,
                               {MessageCallBackParameters    } MessageCallBackParameters,
                               {ImageIndex                   } IMG_RedSquare,
                               {ReplaceLastMessageIfSameClass} False,
                               {MessageCallBackProcess2      } '',
                               {MessageCallBackParameters2   } '',
                               {Details                      } MessageDetails);

    MessagesManager.EndUpdate;
end;


procedure   AddMessage_MixedPortNames(MixedPortNameList : TInterfaceList);
var
    Port : INetItem;
    PortIdx : Integer;
    //Net : INet;
    MessageText : String;
    MessageCallBackProcess : String;
    MessageCallBackParameters : String;
    MessageDetails : IDXPMessageItemDetails;
begin
    if MixedPortNameList.Count < 1 then exit;
    Port := MixedPortNameList[0];
    MessageText := 'Net '+Port.DM_FlattenedNetName+' has ports with different names';

    MessageDetails := GetWorkSpace.DM_MessagesManager.CreateMesssageItemDetails;
    MessageDetails.RootItem.AddSubItem('Ports in same net with different names', IMG_YellowSquare, '', '');
    for PortIdx := 0 to MixedPortNameList.Count - 1 do
    begin
        Port := MixedPortNameList[PortIdx];
        MessageCallBackProcess := 'WorkspaceManager:View';
        MessageCallBackParameters := 'CrossProbeNotifySCH='+Port.DM_FullCrossProbeString+'|SchHandle='+Port.DM_SchHandle+'|Kind=ERCObject|TargetFilename='+Port.DM_OwnerDocumentName+'|FullDocumentPath='+Port.DM_OwnerDocumentFullPath;
        MessageDetails.RootItem.Item[0].AddSubItem(PortDirectionString(Port)+' '+Port.DM_LongDescriptorString+' on sheet '+Port.DM_OwnerDocumentName+' at '+Port.DM_LocationString, IMG_Port, MessageCallBackProcess, MessageCallBackParameters);
    end;

    MessagesManager.BeginUpdate;
    MessagesManager.AddMessage2({MessageClass                 } '[Warning]',
                               {MessageText                  } MessageText,
                               {MessageSource                } cScriptName + ' Script',
                               {MessageDocument              } 'Project',
                               {MessageCallBackProcess       } MessageCallBackProcess,
                               {MessageCallBackParameters    } MessageCallBackParameters,
                               {ImageIndex                   } IMG_YellowSquare,
                               {ReplaceLastMessageIfSameClass} False,
                               {MessageCallBackProcess2      } '',
                               {MessageCallBackParameters2   } '',
                               {Details                      } MessageDetails);

    MessagesManager.EndUpdate;
end;


procedure   AddMessage_UnmatchedPorts(UnmatchedPortList : TInterfaceList);
var
    Port : INetItem;
    PortIdx : Integer;
    //Net : INet;
    MessageText : String;
    MessageCallBackProcess : String;
    MessageCallBackParameters : String;
    MessageDetails : IDXPMessageItemDetails;
begin
    if UnmatchedPortList.Count < 1 then exit;
    Port := UnmatchedPortList[0];
    MessageText := 'Net '+Port.DM_FlattenedNetName+' has unmatched/mismatched ports';

    MessageDetails := GetWorkSpace.DM_MessagesManager.CreateMesssageItemDetails;
    MessageDetails.RootItem.AddSubItem(GetUnmatchedReason(UnmatchedPortList), IMG_OrangeSquare, '', '');
    for PortIdx := 0 to UnmatchedPortList.Count - 1 do
    begin
        Port := UnmatchedPortList[PortIdx];
        MessageCallBackProcess := 'WorkspaceManager:View';
        MessageCallBackParameters := 'CrossProbeNotifySCH='+Port.DM_FullCrossProbeString+'|SchHandle='+Port.DM_SchHandle+'|Kind=ERCObject|TargetFilename='+Port.DM_OwnerDocumentName+'|FullDocumentPath='+Port.DM_OwnerDocumentFullPath;
        MessageDetails.RootItem.Item[0].AddSubItem(PortDirectionString(Port)+' '+Port.DM_LongDescriptorString+' on sheet '+Port.DM_OwnerDocumentName+' at '+Port.DM_LocationString, IMG_Port, MessageCallBackProcess, MessageCallBackParameters);
    end;

    MessagesManager.BeginUpdate;
    MessagesManager.AddMessage2({MessageClass                 } '[Error]',
                               {MessageText                  } MessageText,
                               {MessageSource                } cScriptName + ' Script',
                               {MessageDocument              } 'Project',
                               {MessageCallBackProcess       } MessageCallBackProcess,
                               {MessageCallBackParameters    } MessageCallBackParameters,
                               {ImageIndex                   } IMG_OrangeSquare,
                               {ReplaceLastMessageIfSameClass} False,
                               {MessageCallBackProcess2      } '',
                               {MessageCallBackParameters2   } '',
                               {Details                      } MessageDetails);

    MessagesManager.EndUpdate;
end;


function    Configure(Parameters : String) : String;
begin
    Result := '';
end;


procedure   FillComponentList(Const Dummy : Integer = 0);
var
    Comp : Component;
    DocIdx : Integer;
begin
    // Add all components to ComponentList list
    for DocIdx := 0 to CurrentDocument.DM_ComponentCount - 1 do
    begin
        Comp := CurrentDocument.DM_Components(DocIdx);
        ComponentList.Add(Comp);
    end;
end;


procedure   FillFloatingPortList(Const Dummy : Integer = 0);
var
    ComponentNets, SheetSymbolNets : TStringList;

    Port : INetItem;
    PortIdx : Integer;
    PortNet : String;

    ComponentDesignator : String;
    PinNet : String;
    CompIdx : Integer;
    NetIdx : Integer;

    SheetSymbolDesignator : String;
    SheetSymbolNet : String;
    SheetIdx : Integer;

    IsFloating : Boolean;
begin
    // Get each Net from Port list
    for PortIdx := 0 to PortList.Count - 1 do
    begin
        Port := PortList[PortIdx];
        PortNet := Port.DM_FlattenedNetName;
        IsFloating := True;

        // Check Port Net against all Component Pin Nets
        for CompIdx := 0 to ComponentList.Count - 1 do
        begin
            ComponentDesignator := ComponentList[CompIdx].DM_LogicalDesignator;
            ComponentNets := GetCompNets(ComponentList[CompIdx]);

            for NetIdx := 0 to ComponentNets.Count - 1 do
            begin
                PinNet := ComponentNets[NetIdx];
                if (PinNet = PortNet) then
                begin
                    IsFloating := False;
                    break;
                end;
            end;

            if not IsFloating then break;
        end;

        // Check Port Net against all SheetSymbol Pin Nets
        if IsFloating then
        begin
            for SheetIdx := 0 to SheetSymbolList.Count - 1 do
            begin
                SheetSymbolDesignator := SheetSymbolList[SheetIdx].DM_LogicalDesignator;
                SheetSymbolNets := GetSheetSymbolNets(SheetSymbolList[SheetIdx]);

                for NetIdx := 0 to SheetSymbolNets.Count - 1 do
                begin
                    SheetSymbolNet := SheetSymbolNets[NetIdx];
                    if (SheetSymbolNet = PortNet) then
                    begin
                        IsFloating := False;
                        break;
                    end;
                end;

                if not IsFloating then break;
            end;
        end;

        // If no match was found, the Port is floating
        if IsFloating then
        begin
            GlobalFloatingPortList.Add(Port); // used for global record of floating ports
            FloatingPortList.Add(Port);
        end;
    end;
end;


procedure   FillMixedPortNameNetList(const dummy : Integer = 0);
var
    Port : INetItem;
    PortIdx : Integer;
    PortNet : String;
    NetsWithPorts : TStringList;
    PortNames : TStringList;
    NetIdx : Integer;
    NetName : String;
    IsValidCombo : Boolean;
begin
    if PortList.Count = 0 then exit;

    NetsWithPorts := CreateObject(TStringList);
    NetsWithPorts.Sorted := True;

    PortNames := CreateObject(TStringList);
    PortNames.Sorted := True; // needed for duplicate detection
    PortNames.Duplicates := dupIgnore; // for unique port names per net

    // compile all nets that have ports
    for PortIdx := 0 to PortList.Count - 1 do
    begin
        Port := PortList[PortIdx];
        PortNet := Port.DM_FlattenedNetName;
        NetIdx := NetsWithPorts.Indexof(PortNet);
        if NetIdx = -1 then NetsWithPorts.Add(PortNet);
    end;

    // for each net with ports, find unique port names
    for NetIdx := 0 to NetsWithPorts.Count - 1 do
    begin
        NetName := NetsWithPorts[NetIdx];
        IsValidCombo := False;
        PortNames.Clear;

        // for each port in this net, tally port directions
        for PortIdx := 0 to PortList.Count - 1 do
        begin
            Port := PortList[PortIdx];
            PortNet := Port.DM_FlattenedNetName;
            if NetName = PortNet then
            begin
                PortNames.Add(Port.DM_PortName)
            end;
        end;

        // if net has more than one unique port name, add to list
        if PortNames.Count > 1 then MixedPortNameNetlist.Add(NetName);
    end;
end;


procedure   FillPortList(Document : IDocument);
var
    Port : INetItem;
    DocIdx : Integer;
begin
    // Get all ports on schematic sheet
    for DocIdx := 0 to Document.DM_PortCount - 1 do
    begin
        Port := Document.DM_Ports(DocIdx);
        if (Port.DM_FlattenedNetName <> '') then PortList.Add(Port);
    end;
end;


procedure   FillSheetSymbolList(Const Dummy : Integer = 0);
var
    SheetSymbol : ISheetSymbol;
    SheetIdx : Integer;
begin
    // For all schematic sheet symbols
    for SheetIdx := 0 to CurrentDocument.DM_SheetSymbolCount - 1 do
    begin
        SheetSymbol := CurrentDocument.DM_SheetSymbols(SheetIdx);
        SheetSymbolList.Add(SheetSymbol);
    end;
end;


procedure   FillUnmatchedPortNetList(const dummy : Integer = 0);
var
    Port : INetItem;
    PortIdx : Integer;
    PortNet : String;
    NetsWithPorts : TStringList;
    NetIdx : Integer;
    NetName : String;
    NumInput, NumOutput, NumBidirectional, NumUnspecified : Integer;
    IsValidCombo : Boolean;
begin
    if PortList.Count = 0 then exit;

    NetsWithPorts := CreateObject(TStringList);
    NetsWithPorts.Sorted := True;

    // compile all nets that have ports
    for PortIdx := 0 to PortList.Count - 1 do
    begin
        Port := PortList[PortIdx];
        PortNet := Port.DM_FlattenedNetName;
        NetIdx := NetsWithPorts.Indexof(PortNet);
        if NetIdx = -1 then NetsWithPorts.Add(PortNet);
    end;

    // for each net with ports, tally port directions and validate
    for NetIdx := 0 to NetsWithPorts.Count - 1 do
    begin
        NetName := NetsWithPorts[NetIdx];
        NumInput := 0;
        NumOutput := 0;
        NumBidirectional := 0;
        NumUnspecified := 0;
        IsValidCombo := False;

        // for each port in this net, tally port directions
        for PortIdx := 0 to PortList.Count - 1 do
        begin
            Port := PortList[PortIdx];
            PortNet := Port.DM_FlattenedNetName;
            if NetName = PortNet then
            begin
                case Port.DM_Electrical of
                     eElectricInput     : Inc(NumInput);
                     eElectricIO        : Inc(NumBidirectional);
                     eElectricOutput    : Inc(NumOutput);
                     eElectricPassive   : Inc(NumUnspecified);
                end;
            end;
        end;

        // valid combinations
        if (NumOutput = 1) and (NumInput > 0) and (NumBidirectional = 0) and (NumUnspecified = 0) then IsValidCombo := True; // ONE output port with one or more input ports
        if (NumOutput = 0) and (NumInput = 0) and (NumBidirectional > 1) and (NumUnspecified = 0) then IsValidCombo := True; // TWO or more bidirectional ports
        if (NumOutput = 0) and (NumInput = 0) and (NumBidirectional = 0) and (NumUnspecified > 1) then IsValidCombo := True; // TWO or more unspecified ports

        // if net has invalid combination of ports, add it to the list of trouble nets
        if not IsValidCombo then UnmatchedPortNetList.Add(NetName);
    end;
end;


procedure   FindFloatingPorts(ShowMessages : Boolean);
var
    NumFloatingPorts : Integer;
    DocIdx : Integer;
    Port : INetItem;
    PortIdx : Integer;
begin
    NumFloatingPorts := 0;
    GlobalFloatingPortList.Clear;

    // Find all floating ports for all schematic sheets in project
    for DocIdx := 0 to Workspace.DM_FocusedProject.DM_PhysicalDocumentCount - 1 do
    begin
        CurrentDocument := Workspace.DM_FocusedProject.DM_PhysicalDocuments[DocIdx];

        FillPortList(CurrentDocument);
        FillComponentList;
        FillSheetSymbolList;
        FillFloatingPortList;

        NumFloatingPorts := NumFloatingPorts + FloatingPortList.Count;

        if FloatingPortList.Count > 0 then
        begin
            // add messages
            for PortIdx := 0 to FloatingPortList.Count - 1 do
            begin
                Port := FloatingPortList[PortIdx];
                AddMessage_FloatingPort(Port);
            end;
        end;

        // clear lists before processing next schematic
        PortList.Clear;
        ComponentList.Clear;
        SheetSymbolList.Clear;
        FloatingPortList.Clear;
    end;

    // summarize results
    if (NumFloatingPorts > 0) then
    begin
        if (ShowMessages) then ShowWarning(IntToStr(NumFloatingPorts) + ' Floating Port(s)' + sLineBreak2 + 'See Messages Panel for Details');
        Workspace.DM_ShowMessageView;
    end
    else
    begin
        if (ShowMessages) then ShowInfo('No floating ports detected');
    end;
end;


procedure   FindUnmatchedPorts(ShowMessages : Boolean);
var
    DocIdx : Integer;
    Port : INetItem;
    PortIdx : Integer;
    NetName : String;
    NetIdx : Integer;
    LocalPortList : TInterfaceList;
begin
    LocalPortList := CreateObject(TInterfaceList);

    PortList.Clear;
    // compile list of all ports across all schematic sheets in project
    for DocIdx := 0 to Workspace.DM_FocusedProject.DM_PhysicalDocumentCount - 1 do
    begin
        CurrentDocument := Workspace.DM_FocusedProject.DM_PhysicalDocuments[DocIdx];
        FillPortList(CurrentDocument);
    end;

    UnmatchedPortNetList.Clear;
    FillUnmatchedPortNetList;
    RemoveFloatingPortsFromUnmatchedList; // only does anything if FindFloatingPorts already found floating ports
    // for each problem net in UnmatchedPortNetList, add its ports to a message
    for NetIdx := 0 to UnmatchedPortNetList.Count - 1 do
    begin
        NetName := UnmatchedPortNetList[NetIdx];
        LocalPortList.Clear;
        // iterate through all ports to find those matching this net
        for PortIdx := 0 to PortList.Count - 1 do
        begin
            Port := PortList[PortIdx];
            if Port.DM_FlattenedNetName = NetName then LocalPortList.Add(Port);
        end;

        AddMessage_UnmatchedPorts(LocalPortList);
    end;

    MixedPortNameNetlist.Clear;
    FillMixedPortNameNetList;
    // for each problem net in MixedPortNameNetlist, add its ports to a message
    for NetIdx := 0 to MixedPortNameNetlist.Count - 1 do
    begin
        NetName := MixedPortNameNetlist[NetIdx];
        LocalPortList.Clear;
        // iterate through all ports to find those matching this net
        for PortIdx := 0 to PortList.Count - 1 do
        begin
            Port := PortList[PortIdx];
            if Port.DM_FlattenedNetName = NetName then LocalPortList.Add(Port);
        end;

        AddMessage_MixedPortNames(LocalPortList);
    end;

    // summarize results
    if (UnmatchedPortNetList.Count = 0) and (MixedPortNameNetlist.Count = 0) then
    begin
        if (ShowMessages) then ShowInfo('No nets with port problems detected');
        exit;
    end;

    if (UnmatchedPortNetList.Count > 0) then
    begin
        if (ShowMessages) then ShowWarning(IntToStr(UnmatchedPortNetList.Count) + ' Net(s) with potential port direction problems' + sLineBreak2 + 'See Messages Panel for Details');
        Workspace.DM_ShowMessageView;
    end;

    if (MixedPortNameNetlist.Count > 0) then
    begin
        if (ShowMessages) then ShowWarning(IntToStr(MixedPortNameNetlist.Count) + ' Net(s) with mixed port names' + sLineBreak2 + 'See Messages Panel for Details');
        Workspace.DM_ShowMessageView;
    end;
end;


procedure   Generate(Parameters : String);
var
    ShowMessages : Boolean;
begin
    GlobalInit;
    ShowMessages := False;
    FindFloatingPorts(ShowMessages);
end;


function    GetCompNets(Comp : IComponent) : TStringList;
var
    NetList : TStringList;
    MultiPart : IPart;
    Pin : IPin;
    PartIdx, PinIdx : Integer;
begin
    NetList := CreateObject(TStringList);
    Result := NetList;
    if Comp = nil then exit;

    // For Single Part Components, get net info for just the one
    if (Comp.DM_SubPartCount = 1) then
    begin
        for PinIdx := 0 to Comp.DM_PinCount - 1 do
        begin
            Pin := Comp.DM_Pins(PinIdx);
            NetList.Add(Pin.DM_FlattenedNetName);
        end;
    end
    // For Multipart Components, get net info for each part
    else if (Comp.DM_SubPartCount > 1) then
    begin
        for PartIdx := 0 to Comp.DM_SubPartCount - 1 do
        begin
            MultiPart := Comp.DM_SubParts(PartIdx);
            for PinIdx := 0 to MultiPart.DM_PinCount - 1 do
            begin
                Pin := MultiPart.DM_Pins(PinIdx);
                if (Pin.DM_FlattenedNetName <> '?') then NetList.Add(Pin.DM_FlattenedNetName);
            end;
        end;
    end;

    Result := NetList;
end;


function    GetUnmatchedReason(UnmatchedPortList : TInterfaceList) : String;
var
    NumInput, NumOutput, NumBidirectional, NumUnspecified : Integer;
    Port : INetItem;
    PortIdx : Integer;
begin
    Result := '';
    if UnmatchedPortList = nil then exit;

    if UnmatchedPortList.Count = 1 then
    begin
        Result := 'One '+PortDirectionString(UnmatchedPortList[0])+' port with no matches in net';
        exit;
    end;

    NumInput := 0;
    NumOutput := 0;
    NumBidirectional := 0;
    NumUnspecified := 0;

    // for each port in this list, tally port directions
    for PortIdx := 0 to UnmatchedPortList.Count - 1 do
    begin
        Port := UnmatchedPortList[PortIdx];
        case Port.DM_Electrical of
             eElectricInput     : Inc(NumInput);
             eElectricIO        : Inc(NumBidirectional);
             eElectricOutput    : Inc(NumOutput);
             eElectricPassive   : Inc(NumUnspecified);
        end;
    end;

    if (NumOutput = 1) and (NumInput > 0) and (NumBidirectional = 0) and (NumUnspecified = 0)       then Result := Format('1 output port with %d input ports is valid.', [NumInput])
    else if (NumOutput = 0) and (NumInput = 0) and (NumBidirectional > 1) and (NumUnspecified = 0)  then Result := Format('%d bidirectional ports is valid.', [NumInput])
    else if (NumOutput = 0) and (NumInput = 0) and (NumBidirectional = 0) and (NumUnspecified > 1)  then Result := Format('%d unspecified direction ports is valid.', [NumInput])
    else if (NumBidirectional > 1) and ((NumOutput + NumInput + NumUnspecified) > 0)                then Result := Format('%d bidirectional ports with %d port(s) of other types is invalid.', [NumBidirectional, NumOutput + NumInput + NumUnspecified])
    else if (NumOutput = 1) and ((NumBidirectional + NumUnspecified) > 0)                           then Result := Format('1 output port with %d bidirectional and/or unspecified port(s) is invalid.', [NumBidirectional + NumUnspecified])
    else if (NumInput > 0) and (NumOutput > 1) and ((NumBidirectional + NumUnspecified) = 0)        then Result := Format('%d input port(s) with %d output ports is invalid.', [NumInput, NumOutput])
    else if (NumInput > 0) and ((NumBidirectional + NumUnspecified) > 0)                            then Result := Format('%d input port(s) with %d bidirectional and/or unspecified port(s) is invalid.', [NumInput, NumBidirectional + NumUnspecified])
    else if (NumInput > 1) and (NumOutput = 0)                                                      then Result := Format('%d input ports with no output port.', [NumInput])
    else if (NumOutput > 1) and ((NumInput + NumBidirectional + NumUnspecified) = 0)                then Result := Format('%d output ports are invalid.', [NumOutput])
    else if (NumUnspecified > 0)                                                                    then Result := 'Matching unsupported for Unspecified ports'
    else Result := Format('Invalid port(s) combination: %d Input, %d Output, %d Bidirectional, %d Unspecified.', [NumInput, NumOutput, NumBidirectional, NumUnspecified]);
end;

function    GetSheetSymbolNets(SheetSymbol : ISheetSymbol) : TStringList;
var
    NetList : TStringList;
    EntryIdx: Integer;
    SheetSymbolEntry : INetItem;
begin
    NetList := CreateObject(TStringList);
    Result := NetList;
    if SheetSymbol = nil then exit;

    // Get all nets on symbol
    for EntryIdx := 0 to SheetSymbol.DM_SheetEntryCount - 1 do
    begin
        SheetSymbolEntry := SheetSymbol.DM_SheetEntries(EntryIdx);
        NetList.Add(SheetSymbolEntry.DM_FlattenedNetName);
    end;

    Result := NetList;
end;


procedure   GlobalInit(dummy : Integer = 0);
begin
    if (SchServer = nil) then exit;

    Workspace := GetWorkSpace;
    if (Workspace = nil) then exit;

    MessagesManager := Workspace.DM_MessagesManager;
    if (MessagesManager = nil) then exit;
    MessagesManager.ClearMessages;

    ComponentList := CreateObject(TInterfaceList);
    GlobalFloatingPortList := CreateObject(TInterfaceList);
    FloatingPortList := CreateObject(TInterfaceList);
    PortList := CreateObject(TInterfaceList);
    SheetSymbolList := CreateObject(TInterfaceList);
    UnmatchedPortNetList := CreateObject(TStringList);
    MixedPortNameNetlist := CreateObject(TStringList);
end;


function    PortDirectionString(Port : INetItem) : String;
var
    PortType : TPinElectrical;
begin
    // TPinElectrical = (eElectricInput, eElectricIO, eElectricOutput, eElectricOpenCollector, eElectricPassive, eElectricHiZ, eElectricOpenEmitter, eElectricPower);
    // Use Port.DM_Electrical for integer
    // Use Port.DM_ElectricalString for string

    Result := '';
    if Port = nil then exit;

    PortType := Port.DM_Electrical;
    case PortType of
        eElectricInput      : Result := 'Input';
        eElectricIO         : Result := 'Bidirectional';
        eElectricOutput     : Result := 'Output';
        eElectricPassive    : Result := 'Unspecified';
        else Result := 'Invalid Port Type';
    end;
end;


function    PredictOutputFileNames(Parameters : String) : String;
var
    OutputFileNames : TStringList;
begin
    OutputFileNames := CreateObject(TStringList);
    OutputFileNames.Delimiter := '|';
    OutputFileNames.StrictDelimiter := True;
    OutputFileNames.Add('');
    Result := OutputFileNames.DelimitedText;
end;


procedure   RemoveFloatingPortsFromUnmatchedList(dummy : Integer = 0);
var
    NetIdx : Integer;
    NetName : String;
    Port    : INetItem;
    PortIdx : Integer;
    PortNet : String;
begin
    if (UnmatchedPortNetList.Count = 0) or (GlobalFloatingPortList.Count = 0) then exit;

    for NetIdx := UnmatchedPortNetList.Count - 1 downto 0 do
    begin
        NetName := UnmatchedPortNetList[NetIdx];
        for PortIdx := 0 to GlobalFloatingPortList.Count - 1 do
        begin
            Port := GlobalFloatingPortList[PortIdx];
            PortNet := Port.DM_FlattenedNetName;
            // if port is floating, then net name will be unique so we can use it as key
            if NetName = PortNet then
            begin
                UnmatchedPortNetList.Delete(NetIdx);
                break;
            end;
        end;
    end;
end;

procedure   Start;
var
    ShowMessages : Boolean;
begin
    BeginHourGlass;

    GlobalInit;

    ShowMessages := True;
    FindFloatingPorts(ShowMessages);
    FindUnmatchedPorts(ShowMessages);

    EndHourGlass;
end;

