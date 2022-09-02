{..............................................................................}
{ Summary   This script is used to find any floating ports within schematic    }
{           sheets. It will search all schematic sheets within the project,    }
{           and report which sheets have an unconnected port. Floating ports   }
{           are reported in the Messages tab.                                  }
{                                                                              }
{           Ports are considered floating on a given schematic sheet if they   }
{           are not connected to either a component pin or sheet entry.        }
{                                                                              }
{           Changelog:                                                         }
{           - v1.0 - Initial Release                                           }
{           - v1.1 - Hid private functions; Added to output message            }
{                                                                              }
{ Created by:    Corey Beyer                                                   }
{..............................................................................}

{..............................................................................}
const
    ScriptName = 'FindFloatingPorts.pas';
    IMG_Error = 4;
    IMG_Port = 17;
    IMG_YellowSquare = 72;
    NewLine = #13#10;

var
    Workspace : IWorkSpace;
    MessageManager : IMessagesManager;
    CurrentDocument : IDocument;

    PortInfo : TList;
    SheetSymbolInfo : TStringList;
    SheetSymbolNets : TStringList;
    ComponentInfo : TStringList;
    ComponentNets : TStringList;
    FloatingPortsInfo : TStringList;
    FloatingPorts : TList;



function AddUnconnectedPortsToMessageManager(Const Dummy : Integer);
var
    SchSheetName : String;
    MessageText : String;
    MessageCallBackProcess : String;
    MessageCallBackParameters : String;
    MessageDetails : IDXPMessageItemDetails;
    Port : IPort;
    Document : IDocument;
    i, j : Integer;

begin
    // For each schematic sheet with a floating port
    for i := 0 to FloatingPortsInfo.Count - 1 do
    begin
        SchSheetName := FloatingPortsInfo.Strings[i];
        FloatingPorts := FloatingPortsInfo.Objects[i];

        // Get schematic sheet handle
        for j := 0 to Workspace.DM_FocusedProject.DM_PhysicalDocumentCount - 1 do
        begin
            if (Workspace.DM_FocusedProject.DM_PhysicalDocuments[j].DM_FileName = SchSheetName) then
            begin
                Document := Workspace.DM_FocusedProject.DM_PhysicalDocuments[j];
                break;
            end;
        end;

        // Add a warning message for each floating port
        for j := 0 to FloatingPorts.Count - 1 do
        begin
            Port := FloatingPorts[j];

            MessageText := 'Port '+Port.DM_ID+' floating on sheet at '+Port.DM_LocationString;
            MessageCallBackProcess := 'WorkspaceManager:View';
            MessageCallBackParameters := 'CrossProbeNotifySCH=ObjectKind=Port|PrimaryId='+Port.DM_ID+'|SchHandle='+Port.DM_SchHandle+'|Kind=ERCObject|TargetFilename='+Document.DM_FileName+'|FullDocumentPath='+Document.DM_FullPath;

            MessageDetails := GetWorkSpace.DM_MessagesManager.CreateMesssageItemDetails;
            MessageDetails.RootItem.AddSubItem(MessageText, IMG_Error, '', '');
            MessageDetails.RootItem.Item[0].AddSubItem('Port '+Port.DM_ID, IMG_Port, MessageCallBackProcess, MessageCallBackParameters);

            MessageManager.BeginUpdate;
            MessageManager.AddMessage2({MessageClass                 } '[Warning]',
                                       {MessageText                  } MessageText,
                                       {MessageSource                } ScriptName,
                                       {MessageDocument              } Document.DM_FileName,
                                       {MessageCallBackProcess       } MessageCallBackProcess,
                                       {MessageCallBackParameters    } MessageCallBackParameters,
                                       {ImageIndex                   } IMG_YellowSquare,
                                       {ReplaceLastMessageIfSameClass} False,
                                       {MessageCallBackProcess2      } '',
                                       {MessageCallBackParameters2   } '',
                                       {Details                      } MessageDetails);
            MessageManager.EndUpdate;
        end;
    end;

    Workspace.DM_ShowMessageView;
end;



function GetFloatingPorts(Const Dummy : Integer) : Integer;
var
    Port : IPort;
    PortName : String;
    PortNet : String;
    SplitIndex : Integer;

    ComponentDesignator : String;
    PinNet : String;

    SheetSymbolDesignator : String;
    SheetSymbolNet : String;

    NumFloatingPorts : Integer;
    IsFloating : Boolean;
    i, j, k : Integer;

begin
    NumFloatingPorts := 0;
    FloatingPorts := TList.Create;

    // Get each Net from Port list
    for i := 0 to PortInfo.Count - 1 do
    begin
        Port := PortInfo[i];
        PortNet := Port.DM_FlattenedNetName;
        IsFloating := True;

        // Check Port Net against all Component Pin Nets
        for j := 0 to ComponentInfo.Count - 1 do
        begin
            ComponentDesignator := ComponentInfo.Strings[j];
            ComponentNets := ComponentInfo.Objects[j];

            for k := 0 to ComponentNets.Count - 1 do
            begin
                PinNet := ComponentNets[k];
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
            for j := 0 to SheetSymbolInfo.Count - 1 do
            begin
                SheetSymbolDesignator := SheetSymbolInfo.Strings[j];
                SheetSymbolNets := SheetSymbolInfo.Objects[j];

                for k := 0 to SheetSymbolNets.Count - 1 do
                begin
                    SheetSymbolNet := SheetSymbolNets[k];
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
            FloatingPorts.Add(Port);
            Inc(NumFloatingPorts);
        end;
    end;

    if (FloatingPorts.Count > 0) then
        FloatingPortsInfo.AddObject(CurrentDocument.DM_FileName, FloatingPorts);

    Result := NumFloatingPorts;
end;



function GetSheetSymbolNets(Const Dummy : Integer);
var
    SheetSymbol : ISheetSymbol;
    SheetSymbolEntry : INetItem;
    i, j : Integer;

begin
    // For all schematic sheet symbols
    for i := 0 to CurrentDocument.DM_SheetSymbolCount - 1 do
    begin
        SheetSymbol := CurrentDocument.DM_SheetSymbols(i);

        // Get all nets on symbol
        SheetSymbolNets := TStringList.Create;
        for j := 0 to SheetSymbol.DM_SheetEntryCount - 1 do
        begin
            SheetSymbolEntry := SheetSymbol.DM_SheetEntries(j);
            SheetSymbolNets.Add(SheetSymbolEntry.DM_FlattenedNetName);
        end;

        SheetSymbolInfo.AddObject(SheetSymbol.DM_LogicalDesignator, SheetSymbolNets);
    end;
end;



function GetComponentNets(Const Dummy : Integer);
var
    Comp : Component;
    MultiPart : IPart;
    Pin : IPin;
    i, j, k : Integer;

begin
    // Get Net List for Each Component on schematic sheet
    for i := 0 to CurrentDocument.DM_ComponentCount - 1 do
    begin
        Comp := CurrentDocument.DM_Components(i);

        // For Single Part Components, get net info for just the one
        if (Comp.DM_SubPartCount = 1) then
        begin
            ComponentNets := TStringList.Create;
            for k := 0 to Comp.DM_PinCount - 1 do
            begin
                Pin := Comp.DM_Pins(k);
                ComponentNets.Add(Pin.DM_FlattenedNetName);
            end;
            ComponentInfo.AddObject(Comp.DM_FullLogicalDesignator, ComponentNets);
        end
        // For Multipart Components, get net info for each part
        else if (Comp.DM_SubPartCount > 1) then
        begin
            for j := 0 to Comp.DM_SubPartCount - 1 do
            begin
                ComponentNets := TStringList.Create;
                MultiPart := Comp.DM_SubParts(j);
                for k := 0 to MultiPart.DM_PinCount - 1 do
                begin
                    Pin := MultiPart.DM_Pins(k);
                    if (Pin.DM_FlattenedNetName <> '?') then
                        ComponentNets.Add(Pin.DM_FlattenedNetName);
                end;
                ComponentInfo.AddObject(MultiPart.DM_FullLogicalDesignator, ComponentNets);
            end;
        end;
    end;
end;



function GetPorts(Const Dummy : Integer);
var
    Port : IPort;
    PortName : String;
    PortNet : String;
    i : Integer;

begin
    // Get all ports on schematic sheet
    for i := 0 to CurrentDocument.DM_PortCount - 1 do
    begin
        Port := CurrentDocument.DM_Ports(i);
        if (Port.DM_FlattenedNetName <> '') then
            PortInfo.Add(Port);
    end;
end;



procedure Start;
var
    NumFloatingPorts : Integer;
    i : Integer;

begin
    BeginHourGlass;

    if (SchServer = nil) then exit;

    Workspace := GetWorkSpace;
    if (Workspace = nil) then exit;

    MessageManager := Workspace.DM_MessagesManager;
    if (MessageManager = nil) then exit;

    NumFloatingPorts := 0;

    PortInfo := TList.Create;
    SheetSymbolInfo := TStringList.Create;
    ComponentInfo := TStringList.Create;
    FloatingPortsInfo := TStringList.Create;

    // Find all floating ports for all schematic sheets in project
    for i := 0 to Workspace.DM_FocusedProject.DM_PhysicalDocumentCount - 1 do
    begin
        CurrentDocument := Workspace.DM_FocusedProject.DM_PhysicalDocuments[i];

        GetPorts(0);
        GetComponentNets(0);
        GetSheetSymbolNets(0);
        NumFloatingPorts := NumFloatingPorts + GetFloatingPorts(0);

        PortInfo.Clear;
        ComponentInfo.Clear;
    end;

    // Report all floating ports to message manager
    MessageManager.ClearMessages;
    if (NumFloatingPorts > 0) then
    begin
        AddUnconnectedPortsToMessageManager(0);
        ShowWarning(IntToStr(NumFloatingPorts) + ' Floating Port(s)' + NewLine + NewLine + 'See Messages Panel for Details');
    end
    else
    begin
        ShowInfo('0 Floating Port(s)');
    end;

    PortInfo.Free;
    SheetSymbolInfo.Free;
    SheetSymbolNets.Free;
    ComponentInfo.Free;
    ComponentNets.Free;
    FloatingPortsInfo.Free;
    FloatingPorts.Free;

    EndHourGlass;
end;
