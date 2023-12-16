{ Created by:  Ryan Rutledge                                                                                                   }
{ DISCLAIMER: This script is provided "AS IS" in the hopes that it will be useful, but comes with no guarantees or warranties. }
{ Use of this script is conditional on accepting it as-is, and the user is responsible for any issues that may arise from      }
{ its use, including failure to detect a critical problem that results in scrap boards. Please thoroughly verify its fitness   }
{ for your particular use case.                                                                                                }
{ For documentation see README.md                                                                                              }

const
    cScriptTitle            = 'ReturnViaCheck'; // modified from AssemblyTextPrep script
    cConfigFileName         = 'ReturnViaCheckConfig.ini';
    cScriptVersion          = '0.50';
    CustomRule1_Name        = 'ScriptRule_ReturnViaCheck';
    CustomRule1_Kind        = eRule_HoleToHoleClearance;
    //CustomRule1_Kind        = eRule_RoutingViaStyle; // eRule_RoutingViaStyle has really ugly description for this
    cDEBUGLEVEL             = 0;
    DEBUGEXPANSION          = -1; // leave at -1 to disable
    //status bar commands
    cStatusBar_Panel1               =  1;
    cStatusBar_Panel2               =  2;
    cStatusBar_Panel3               =  3;
    cStatusBar_Panel4               =  4;
    cStatusBar_SetDefault           = -1;
    cStatusBar_WindowCaption        = -2;
    cStatusBar_Push                 = -3;
    cStatusBar_Pop                  = -4;
    cStatusBar_ProgressBarStart     = -5;
    cStatusBar_ProgressBarStop      = -6;
    cStatusBar_ProgressBarStep      = -7;
    cStatusBar_ComplexOpBegin       = -8;
    cStatusBar_ComplexOpEnd         = -9;
    cStatusBar_UndeterminedOpBegin  = -10;
    cStatusBar_UndeterminedOpEnd    = -11;

    cImageIndexWarning = 163;
    cImageIndexOrange = 74;

var
    bIgnoreCBChange         : Boolean;
    bUseCustomViolations    : Boolean;
    bProcessing             : Boolean;
    bMetricUnits            : Boolean;
    IsViaSelectable         : Boolean;
    Board                   : IPCB_Board;
    MessagesManager         : IMessagesManager;
    iDebugLevel             : Integer;
    IsAtLeastAD19           : Boolean;
    MLS                     : IPCB_MasterLayerStack;
    VIADISTANCEMAX          : TCoord;
    FailedViaIndex          : Integer;
    FailedViaList           : TInterfaceList;
    ReturnNetList           : TInterfaceList;
    SignalNetList           : TInterfaceList;
    DrillPairsList          : TInterfaceList;
    ReturnViaDrillPairsList : TInterfaceList;
    CustomRulesList         : TInterfaceList;
    ViolationList           : TInterfaceList;
    gvCurrentPercentCount   : Integer;
    gvTotalPercentCount     : Integer;
    gvOldPercent            : Integer;
    gvMarquee               : Boolean;


procedure   _GUI; forward;
procedure   About; forward;
procedure   Main_CheckAll(dummy : Integer = 0); forward;
procedure   Main_Recheck(dummy : Integer = 0); forward;
procedure   AddMessage(MessageClass, MessageText: String); forward;
procedure   AddMessageAdvanced(MessagesManager : IMessagesManager, messageClass : WideString, messageText : WideString, documentName : WideString, imageIndex : Integer, callBackProcess : WideString = '', callBackParameters : WideString = '', messageDetails : IDXPMessageItemDetails = nil); forward;
procedure   AddMessageCallback_Via(Via : IPCB_Via); forward;
procedure   ChangeTextUnits(Units : TUnit); forward;
procedure   ClientClearFilter(dummy : Boolean = False); forward;
procedure   ClientDeSelectAll(dummy : Boolean = False); forward;
procedure   ClientZoomRedraw(dummy : Boolean = False); forward;
procedure   ClientResetAllErrorMarkers(dummy : Boolean = False); forward;
procedure   ClientZoomSelected(dummy : Boolean = False); forward;
function    ConfigFile_GetPath(dummy : String = '') : String; forward;
procedure   ConfigFile_Read(AFileName : String); forward;
procedure   ConfigFile_Write(AFileName : String); forward;
function    CoordToStr(Coords : TCoord) : String; forward;
function    CoordToX(Coords : TCoord) : String; forward;
function    CoordToY(Coords : TCoord) : String; forward;
function    DebugLevelStr(dummy : String = '') : String; forward;
procedure   DebugMessage(const ShowLevel : Integer; const msg : WideString; const Caption : String = 'Confirm or Cancel Debug'); forward;
function    DocumentIsPCB(dummy : Boolean = False) : Boolean; forward;
function    FillDrillPairsList(const DrillPairObj : IPCB_DrillLayerPair; var DrillPairsList : TInterfaceList; const Clear : Boolean = False) : Boolean; forward;
procedure   FillDrillPairsListBox(ListBox : TObject); forward;
procedure   FillNetClassListBox(ListBox : TObject); forward;
procedure   FillNetComboBox(ComboBox : TObject); forward;
function    FillNetListFromClass(const NetClass : IPCB_OBjectClass; var NetList : TInterfaceList; const Clear : Boolean = False) : Boolean; forward;
function    FillNetListFromSingleNet(const Net : IPCB_Net; var NetList : TInterfaceList; const Clear : Boolean = False) : Boolean; forward;
function    FolderIsReadOnly(const AFolderPath : String) : Boolean; forward;
function    GetDelimitedListBoxSelections(ListBox : TListBox; MyDelim : String = ';;') : String; forward;
function    GetDrillPairs(const dummy : Integer = 0) : TInterfaceList; forward;
function    GetIteratorCount(Iter : IPCB_BoardIterator) : Integer; forward;
function    GetNearbyVias(SignalVia : IPCB_Via) : TInterfaceList; forward;
function    GetViaDrillPair(Via : IPCB_Via) : IPCB_DrillLayerPair; forward;
procedure   GUI_BeginProcess(dummy : Boolean = False); forward;
procedure   GUI_EndProcess(dummy : Boolean = False); forward;
function    GUI_ForLoopStart(StatusMsg : String; LoopCount : Integer) : Integer; forward;
procedure   GUI_LoopEnd(dummy : Boolean = False); forward;
procedure   GUI_LoopProgress(StatusMsg : String = ''); forward;
function    HasReturnVia(SignalVia : IPCB_Via) : Boolean; forward;
procedure   IgnoreArea(dummy : Integer = 0); forward;
function    InAllowedDrillStack(Via : IPCB_Via) : Boolean; forward;
procedure   InitialCheckGUI(var status : Integer); forward;
function    IsReturnVia(SignalVia : IPCB_Via; OtherVia : IPCB_Via) : Boolean; forward;
function    IsSelectableCheck(var bCanSelectVia : Boolean); forward;
function    IsSelectableCheckStop(bShowWarning : Boolean = True) : Boolean; forward;
function    IsStringANum(Text : string) : Boolean; forward;
function    ParseDelimitedString(const DelimitedText : String; const MyDelim : String = ';;') : TStringList; forward;
procedure   RefreshFailedVias(dummy : Boolean = False); forward;
function    Rules_AddCustomViaRule(const RuleKind : TRuleKind; const RuleName : WideString; DrillPairObj : IPCB_DrillLayerPair; const Comment : WideString) : IPCB_Rule; forward;
procedure   Rules_ClearDRC_Via(Via : IPCB_Via); forward;
procedure   Rules_CustomPostProcess(ViolationCount : Integer = 0); forward;
procedure   Rules_CustomPreProcess(dummy : Integer = 0); forward;
function    Rules_GetRulesByKind(const RuleKind : TRuleKind = -1) : TInterfaceList; forward;
function    Rules_GetRuleByName(const RulesList : TInterfaceList; const RuleName : WideString) : IPCB_Rule; forward;
function    Rules_GetRuleForVia(const RulesList : TInterfaceList; const Via : IPCB_Via) : IPCB_Rule; forward;
function    Rules_MakeViolation_Via(var Via : IPCB_Via) : IPCB_Violation; forward;
function    Rules_RuleKindToString (ARuleKind : TRuleKind) : String; forward;
function    SelectComboBoxItem(ComboBox : TComboBox; ItemString : String) : Boolean; forward;
function    SelectListBoxItems(ListBox : TListBox; ListItems : TStringList) : Boolean; forward;
procedure   SetButtonEnableStates(EnableState : Boolean); forward;
function    SetDRCAndAddToHighlight(var Prim : IPCB_Primitive); forward;
procedure   SetInitialDrillPairSelections(dummy : Boolean = False); forward;
procedure   SetInitialNetSelections(dummy : Boolean = False); forward;
procedure   SetNetModeSignalStates(RadioGroup : TObject); forward;
procedure   SetNetPickEnableStates(EnableState : Boolean); forward;
function    StrFromObjectId(ObjectId: TObjectId) : String; forward;
procedure   TReturnViaCheckForm.ButtonCancelClick(Sender : TObject); forward;
procedure   TReturnViaCheckForm.ButtonCheckAllClick(Sender : TObject); forward;
procedure   TReturnViaCheckForm.ButtonIgnoreAreaClick(Sender : TObject); forward;
procedure   TReturnViaCheckForm.ButtonIgnoreClick(Sender : TObject); forward;
procedure   TReturnViaCheckForm.ButtonNextClick(Sender : TObject); forward;
procedure   TReturnViaCheckForm.ButtonPreviousClick(Sender : TObject); forward;
procedure   TReturnViaCheckForm.ButtonRecheckClick(Sender : TObject); forward;
procedure   TReturnViaCheckForm.ButtonZoomClick(Sender : TObject); forward;
procedure   TReturnViaCheckForm.InputValueChange(Sender : TObject); forward;
procedure   TReturnViaCheckForm.LabelVersionClick(Sender : TObject); forward;
procedure   TReturnViaCheckForm.MMmilButtonClick(Sender : TObject); forward;
procedure   TReturnViaCheckForm.ReturnViaCheckFormClose(Sender: TObject; var Action: TCloseAction); forward;
procedure   TReturnViaCheckForm.ReturnViaCheckFormCreate(Sender: TObject); forward;
procedure   TReturnViaCheckForm.ReturnViaCheckFormShow(Sender : TObject); forward;
procedure   TReturnViaCheckForm.rgSignalModeClick(Sender: TObject); forward;
procedure   TReturnViaCheckForm.rgReturnModeClick(Sender: TObject); forward;
procedure   TReturnViaCheckForm.UserKeyPress(Sender : TObject; var Key : Char); forward;
procedure   UpdateConstants(dummy : Boolean = False); forward;
procedure   UpdateStatus(const StatusString : String = ''); forward;
procedure   UpdateDrillPairsListFromSelections(dummy : Boolean = False); forward;
procedure   UpdateNavButtonStates(Reset : Boolean = False); forward;
procedure   UpdateNetListsFromSelections(dummy : Boolean = False); forward;
procedure   MyStatusBar_SetState(Index : Integer; const S : WideString); forward;
function    MyStatusBar_GetState(Index : Integer) : Widestring; forward;
procedure   MyStatusBar_SetStateDefault(dummy : Boolean = False); forward;
procedure   MyStatusBar_PushStatus(dummy : Boolean = False); forward;
procedure   MyStatusBar_PopStatus(dummy : Boolean = False); forward;
function    MyPercentActive(dummy : Boolean = False) : Boolean; forward;
function    MyMarqueeActive(dummy : Boolean = False) : Boolean; forward;
function    MyPercent_GetTotal(dummy : Boolean = False) : Integer; forward;
procedure   MyPercent_Init(const InitialString : String ; TotalCount : Integer); forward;
procedure   MyPercent_Finish(dummy : Boolean = False); forward;
procedure   MyPercent_UpdateByNumber(AmountToIncrement : Integer, StatusMsg : String = ''); forward;
procedure   MyPercent_Update(StatusMsg : String = ''); forward;
procedure   MyPercent_BeginUndeterminedOperation(const InitialString : String); forward;
procedure   MyPercent_EndUndeterminedOperation(dummy : Boolean = False); forward;
procedure   MyPercent_BeginComplexOperation(const InitialString : String); forward;
procedure   MyPercent_EndComplexOperation(dummy : Boolean = False); forward;


{ Main GUI }
procedure   _GUI;
var
    status          : Integer;
begin
    BeginHourGlass;
    InitialCheckGUI(status);
    if status = 1 then exit;

    //ReturnViaCheckForm.ShowModal; // Show the GUI
    ReturnViaCheckForm.FormStyle := fsStayOnTop;
    ReturnViaCheckForm.Show;
    EndHourGlass;
end;


{ About information }
procedure   About;
var
    MsgText : string;
begin
    MsgText := '"' + cScriptTitle + '" script version ' + cScriptVersion + sLineBreak +
        sLineBreak +
        'Use "_GUI" to check for the existence of signal return vias.' + sLineBreak +
        sLineBreak +
        'Updated versions and documentation may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries' + sLineBreak +
        '/tree/master/Scripts - PCB/ReturnViaCheck' + sLineBreak +
        sLineBreak +
        'Settings save location:' + sLineBreak +
        ConfigFile_GetPath;

    ShowInfo(MsgText, 'About');
end; { About }


procedure   Main_CheckAll(dummy : Integer = 0);
var
    i, TotalCount   : Integer;
    ProcessedCount, ProgressInt : Integer;
    ViaIter         : IPCB_BoardIterator;
    CurrentVia      : IPCB_Via;
    ReturnNetIndex  : Integer;
begin
    bUseCustomViolations := CheckBoxRuleViolations.Checked;

    if ButtonCheckAll.Caption = 'Restart' then
    begin
        SetNetPickEnableStates(True);
        ClientDeSelectAll;
        ClientClearFilter;

        for i := 0 to FailedViaList.Count - 1 do
        begin
            Rules_ClearDRC_Via(FailedViaList[i]);
        end;

        FailedViaList.Clear;
        FailedViaIndex := -1;
        UpdateStatus;
        UpdateNavButtonStates;
        exit;
    end;

    GUI_BeginProcess;
    PCBServer.PreProcess;
    if Assigned(CustomRulesList) and (CustomRulesList.Count > 0) then Rules_CustomPostProcess; // don't know how to properly clear the custom DRC errors, so just destroy and rebuild
    Rules_CustomPreProcess; // set up custom rules for using Rules & Violations to navigate failing vias
    try

        ProcessedCount := 0;
        ProgressInt := 0;

        SetNetPickEnableStates(False);

        ClientDeselectAll;

        // Populate SignalNetList and ReturnNetList based on GUI selections

        UpdateNetListsFromSelections;

        UpdateDrillPairsListFromSelections;

        FailedViaIndex := -1;
        FailedViaList.Clear;
        MessagesManager.ClearMessages;

        ViaIter := Board.BoardIterator_Create;
        ViaIter.AddFilter_ObjectSet(MkSet(eViaObject));
        ViaIter.AddFilter_LayerSet(AllLayers);
        ViaIter.AddFilter_Method(eProcessAll);

        TotalCount := GetIteratorCount(ViaIter);
        MyPercent_Init('Processing ' + IntToStr(TotalCount) + ' vias', TotalCount);

        CurrentVia := ViaIter.FirstPCBObject;
        while CurrentVia <> nil do
        begin
            if SignalNetList.IndexOf(CurrentVia.Net) >= 0 then
            begin
                if not HasReturnVia(CurrentVia) then
                begin
                    FailedViaList.Add(CurrentVia);
                    Rules_MakeViolation_Via(CurrentVia);
                end;
            end;

            Inc(ProcessedCount);
            MyPercent_Update(Format('%d of %d vias processed', [ProcessedCount, TotalCount]));
            // throttle Messages panel updates for performance
            if ((ProcessedCount / TotalCount) * 100) >= (ProgressInt + 1) then
            begin
                Inc(ProgressInt);
                AddMessage('ReturnViaCheck Status', Format('%d of %d vias processed (%d%%)', [ProcessedCount, TotalCount, ProgressInt]));
            end;

            CurrentVia := ViaIter.NextPCBObject;
        end;
        Board.BoardIterator_Destroy(ViaIter);

        MyPercent_Finish;

        // select and zoom to all failed vias and add messages to panel
        RefreshFailedVias;

        //ClientZoomSelected;

        //UpdateStatus;
        //UpdateNavButtonStates;

        Board.ViewManager_FullUpdate;

        if FailedViaList.Count > 0 then
        begin
            ShowWarning(IntToStr(FailedViaList.Count) + ' violating vias found');
        end
        else
        begin
            SetNetPickEnableStates(True);
            ShowInfo('No violating vias found');
        end;

    finally
        Rules_CustomPostProcess(FailedViaList.Count); // in case FailedViaList is now empty, need to clean up
        PCBServer.PostProcess;
        GUI_EndProcess;
    end;

    ConfigFile_Write(ConfigFile_GetPath);
end;

procedure   Main_Recheck(dummy : Integer = 0);
var
    i, TotalCount   : Integer;
    ProcessedCount, ProgressInt : Integer;
    CurrentVia      : IPCB_Via;
begin
    GUI_BeginProcess;
    PCBServer.PreProcess;
    try
        ProcessedCount := 0;
        ProgressInt := 0;

        TotalCount := FailedViaList.Count;
        MyPercent_Init('Rechecking ' + IntToStr(TotalCount) + ' vias', TotalCount);

        MessagesManager.ClearMessages;
        for i := FailedViaList.Count - 1 downto 0 do
        begin
            Inc(ProcessedCount);
            MyPercent_Update(Format('%d of %d vias processed', [ProcessedCount, TotalCount]));
            if TotalCount > 100 then
            begin
                // throttle Messages panel updates for performance
                if ((ProcessedCount / TotalCount) * 100) >= (ProgressInt + 1) then
                begin
                    Inc(ProgressInt);
                    AddMessage('ReturnViaCheck Status', Format('%d of %d vias processed (%d%%)', [ProcessedCount, TotalCount, ProgressInt]));
                end;
            end
            else
            begin
                AddMessage('ReturnViaCheck Status', Format('%d of %d vias processed (%d%%)', [ProcessedCount, TotalCount, Round((ProcessedCount / TotalCount) * 100)]));
            end;

            CurrentVia := FailedViaList[i];
            if not HasReturnVia(CurrentVia) then
            begin
                //AddMessageCallback_Via(CurrentVia);
                continue;
            end;

            Rules_ClearDRC_Via(CurrentVia);

            FailedViaIndex := -1;
            FailedViaList.Delete(i);
            //CurrentVia.Selected := False;
            //CurrentVia.GraphicallyInvalidate;
        end;

        MyPercent_Finish;

        RefreshFailedVias;

        //if FailedViaIndex >= FailedViaList.Count then FailedViaIndex := FailedViaList.Count - 1;

        //if FailedViaIndex >= 0 then
        //begin
            //ClientDeSelectAll;
            //FailedViaList[FailedViaIndex].Selected := True;
            //ClientZoomSelected;
        //end;

        //UpdateStatus;
        //UpdateNavButtonStates;

        if FailedViaList.Count > 0 then ShowWarning(IntToStr(FailedViaList.Count) + ' violating vias remain');

    finally
        Rules_CustomPostProcess(FailedViaList.Count);
        PCBServer.PostProcess;
        GUI_EndProcess;
    end;
end;


procedure   AddMessage(MessageClass, MessageText: String);
begin
    // https://www.altium.com/ru/documentation/altium-nexus/wsm-api-types-and-constants/#Image%20Index%20Table
    // [!!!] 66 index for debug info
    GetWorkspace.DM_MessagesManager.BeginUpdate();
    GetWorkspace.DM_MessagesManager.AddMessage(MessageClass, MessageText, 'ReturnViaCheck Script', GetWorkspace.DM_FocusedDocument.DM_FileName, '', '', 75, MessageClass = 'ReturnViaCheck Status');
    GetWorkspace.DM_MessagesManager.EndUpdate();
    GetWorkspace.DM_MessagesManager.UpdateWindow();
end;

procedure   AddMessageAdvanced(MessagesManager : IMessagesManager, messageClass : WideString, messageText : WideString, documentName : WideString, imageIndex : Integer, callBackProcess : WideString = '', callBackParameters : WideString = '', messageDetails : IDXPMessageItemDetails = nil);
begin
    MessagesManager.BeginUpdate;
    try
        MessagesManager.AddMessage2({MessageClass                 } messageClass,
                                   {MessageText                  } messageText,
                                   {MessageSource                } cScriptTitle,
                                   {MessageDocument              } documentName,
                                   {MessageCallBackProcess       } callBackProcess,
                                   {MessageCallBackParameters    } callBackParameters,
                                   {ImageIndex                   } imageIndex,
                                   {ReplaceLastMessageIfSameClass} False,
                                   {MessageCallBackProcess2      } '',
                                   {MessageCallBackParameters2   } '',
                                   {Details                      } messageDetails);
    finally
        MessagesManager.EndUpdate;
    end;
end;

procedure   AddMessageCallback_Via(Via : IPCB_Via);
var
    messageClass : WideString;
    messageText : WideString;
    documentName : WideString;
    documentFullPath : WideString;
    callBackProcess : WideString;
    callBackParameters : WideString;
    imageIndex : Integer;
    X1, Y1, X2, Y2 : Integer;
    DX, DY: Integer;
    OpenDocString : WideString;
    BoardRefString : WideString;
    ZoomOnRectString : WideString;
begin
    if Via = nil then exit;

    X1 := Via.BoundingRectangle.Left;
    Y1 := Via.BoundingRectangle.Bottom;
    X2 := Via.BoundingRectangle.Right;
    Y2 := Via.BoundingRectangle.Top;
    DX := X2 - X1;
    DY := Y2 - Y1;

    X1 := X1 - (4 * DX);
    Y1 := Y1 - (3 * DY);
    X2 := X2 + (4 * DX);
    Y2 := Y2 + (3 * DY);

    OpenDocString := '';
    //OpenDocString := Format(' Client.ShowDocument(Client.OpenDocument(''PCB'', ''%s'')); ', [Board.FileName]); // not very performant and only serves to switch to the document if it's inactive
    BoardRefString := Format(' PCB:=PCBServer.GetPCBBoardByPath(''%s''); ', [Board.FileName]);
    ZoomOnRectString := Format(' PCB.GraphicalView_ZoomOnRect(%d, %d, %d, %d); ', [X1, Y1, X2, Y2]);
    callBackProcess := 'ScriptingSystem:RunScriptText';
    callBackParameters := 'Text=var PCB:IPCB_Board; Begin ' + OpenDocString + BoardRefString + 'if PCB=nil then exit;' + ZoomOnRectString + ' end;';

    DebugMessage(3, callBackParameters);

    messageClass := '[WARNING]';
    messageText :=Format('%s net %s has no return via.', [Via.Net.Name, Via.Descriptor]);
    documentName := ExtractFileName(Board.FileName);
    documentFullPath := Board.FileName;
    imageIndex := cImageIndexOrange;

    AddMessageAdvanced(MessagesManager, messageClass, messageText, documentName, imageIndex, callBackProcess, callBackParameters);
end;


procedure   ChangeTextUnits(Units : TUnit);
var
    i           : Integer;
    ControlList : TObjectList;
    EditControl : TObject;
    TempString  : String;
    EditString  : String;
begin
    ControlList := CreateObject(TObjectList);
    ControlList.OwnsObjects := False; // required to not throw invalid pointer errors when list is freed

    ControlList.Add(EditDistanceMax);

    for i := 0 to ControlList.Count - 1 do
    begin
        EditControl := ControlList[i];
        if EditControl = nil then continue;

        TempString := EditControl.Text;
        if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

        if Units = eMetric then
        begin
            if IsStringANum(EditControl.Text) then EditControl.Text := CoordToMMs(MilsToCoord(StrToFloat(TempString)));
        end
        else
        begin
            if IsStringANum(EditControl.Text) then EditControl.Text := CoordToMils(MMsToCoord(StrToFloat(TempString)));
        end;
    end;

end;


procedure   ClientClearFilter(dummy : Boolean = False);
begin
    Client.SendMessage('PCB:RunQuery', 'Clear=True' , 255, Client.CurrentView);
end;

procedure   ClientDeSelectAll(dummy : Boolean = False);
begin
    Client.SendMessage('PCB:DeSelect', 'Scope=All' , 255, Client.CurrentView);
end;

procedure   ClientZoomRedraw(dummy : Boolean = False);
begin
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
end;

procedure   ClientResetAllErrorMarkers(dummy : Boolean = False);
begin
    Client.SendMessage('PCB:ResetAllErrorMarkers', '', 255, Client.CurrentView);
end;

procedure   ClientZoomSelected(dummy : Boolean = False);
begin
    Client.SendMessage('PCB:Zoom', 'Action=Selected' , 255, Client.CurrentView);
end;


function    ConfigFile_GetPath(dummy : String = '') : String;
begin
    Result := ExtractFilePath(GetRunningScriptProjectName) + cConfigFileName;
    if (not FileExists(Result)) or bForbidLocalSettings then Result := IncludeTrailingPathDelimiter(SpecialFolder_AltiumApplicationData) + cConfigFileName;
end;

procedure   ConfigFile_Read(AFileName : String);
var
    IniFile             : TIniFile;
    ConfigDebugCaption  : String;
    SettingsDebugFile   : String;
    SettingsDebugList   : TStringList;
    TempString          : String;
    TempList            : TStringList;
begin
    IniFile := TIniFile.Create(AFileName);
    try
        ReturnViaCheckForm.Top := IniFile.ReadInteger('Window Position', 'Top', ReturnViaCheckForm.Top);
        ReturnViaCheckForm.Left := IniFile.ReadInteger('Window Position', 'Left', ReturnViaCheckForm.Left);

        EditDistanceMax.Text := IniFile.ReadString('Limits', 'Return Via Max Distance', EditDistanceMax.Text);

        MMmilButton.Caption := IniFile.ReadString('Config', 'Units', MMmilButton.Caption);

        CheckBoxRuleViolations.Checked := IniFile.ReadBool('Config', 'Custom Violations', CheckBoxRuleViolations.Checked);

        rgSignalMode.ItemIndex          := IniFile.ReadInteger('Last Used', 'Signal Net Filter Mode', rgSignalMode.ItemIndex);
        SetNetModeSignalStates(rgSignalMode);

        rgReturnMode.ItemIndex          := IniFile.ReadInteger('Last Used', 'Return Net Filter Mode', rgReturnMode.ItemIndex);
        SetNetModeSignalStates(rgReturnMode);

        SelectComboBoxItem(ComboBoxSignalNet, IniFile.ReadString('Last Used', 'Signal Net', ''));
        SelectComboBoxItem(ComboBoxReturnNet, IniFile.ReadString('Last Used', 'Return Net', ''));

        TempString := IniFile.ReadString('Last Used', 'Signal Net Classes', GetDelimitedListBoxSelections(ListBoxSignalNets));
        TempList := ParseDelimitedString(TempString);
        if TempList.Count > 0 then SelectListBoxItems(ListBoxSignalNets, TempList);

        TempString := IniFile.ReadString('Last Used', 'Return Net Classes', GetDelimitedListBoxSelections(ListBoxReturnNets));
        TempList := ParseDelimitedString(TempString);
        if TempList.Count > 0 then SelectListBoxItems(ListBoxReturnNets, TempList);

        TempString := IniFile.ReadString('Last Used', 'Drill Pairs', GetDelimitedListBoxSelections(ListBoxDrillPairs));
        TempList := ParseDelimitedString(TempString);
        if TempList.Count > 0 then SelectListBoxItems(ListBoxDrillPairs, TempList);

        if iDebugLevel > 0 then
        begin
            SettingsDebugList := CreateObject(TStringList);

            if FileExists(AFileName) then
            begin
                SettingsDebugList.LoadFromFile(AFileName);
                ConfigDebugCaption := 'Confirm Settings read from file';
                SettingsDebugList.Insert(0, AFileName);
                DebugMessage(1, SettingsDebugList.Text, ConfigDebugCaption);
            end
            else DebugMessage(1, 'No settings file located. Defaults used.');

            SettingsDebugFile := ChangeFileExt(ConfigFile_GetPath,'.ini') + '_debug.ini';
            ConfigFile_Write(SettingsDebugFile);
            SettingsDebugList.LoadFromFile(SettingsDebugFile);
            DeleteFile(SettingsDebugFile);

            ConfigDebugCaption := 'Confirm Settings used in operation';
            DebugMessage(1, SettingsDebugList.Text, ConfigDebugCaption);
        end;
    finally
        IniFile.Free;
    end;
end;

procedure   ConfigFile_Write(AFileName : String);
var
    IniFile     : TIniFile;
    idx         : Integer;
    TempString  : String;
begin
    IniFile := TIniFile.Create(AFileName);
    try
        IniFile.WriteInteger('Window Position', 'Top', ReturnViaCheckForm.Top);
        IniFile.WriteInteger('Window Position', 'Left', ReturnViaCheckForm.Left);

        IniFile.WriteString('Limits', 'Return Via Max Distance', EditDistanceMax.Text);

        IniFile.WriteString('Config', 'Units', MMmilButton.Caption);
        IniFile.WriteBool('Config', 'Custom Violations', CheckBoxRuleViolations.Checked);

        idx := ComboBoxSignalNet.ItemIndex;
        if idx >= 0 then TempString := ComboBoxSignalNet.Items[idx] else TempString := '';
        IniFile.WriteString('Last Used', 'Signal Net', TempString);

        idx := ComboBoxReturnNet.ItemIndex;
        if idx >= 0 then TempString := ComboBoxReturnNet.Items[idx] else TempString := '';
        IniFile.WriteString('Last Used', 'Return Net', TempString);

        IniFile.WriteString('Last Used', 'Signal Net Classes', GetDelimitedListBoxSelections(ListBoxSignalNets));
        IniFile.WriteString('Last Used', 'Return Net Classes', GetDelimitedListBoxSelections(ListBoxReturnNets));
        IniFile.WriteString('Last Used', 'Drill Pairs', GetDelimitedListBoxSelections(ListBoxDrillPairs));

        IniFile.WriteInteger('Last Used', 'Signal Net Filter Mode', rgSignalMode.ItemIndex);
        IniFile.WriteInteger('Last Used', 'Return Net Filter Mode', rgReturnMode.ItemIndex);
    finally
        IniFile.Free;
    end;
end;


function    CoordToStr(Coords : TCoord) : String;
const
    MAXINT = 2147483647;
    MININT = -2147483647;
begin
    if Coords < MININT then Coords := MININT
    else if Coords > MAXINT then Coords := MAXINT;

    result := CoordUnitToString(Coords, Board.DisplayUnit xor 1);
end;

function    CoordToX(Coords : TCoord) : String;
begin
    result := CoordUnitToString(Coords - Board.XOrigin, Board.DisplayUnit xor 1);
end;

function    CoordToY(Coords : TCoord) : String;
begin
    result := CoordUnitToString(Coords - Board.YOrigin, Board.DisplayUnit xor 1);
end;


function    DebugLevelStr(dummy : String = '') : String;
begin
    Result := '-------------------------  Debug Level: ' + IntToStr(iDebugLevel) + '  -------------------------' + sLineBreak;
end;


procedure   DebugMessage(const ShowLevel : Integer; const msg : WideString; const Caption : String = 'Confirm or Cancel Debug');
begin
    // iDebugLevel must be an Integer global variable initialized before first call to this procedure
    if iDebugLevel >= ShowLevel then
    begin
        msg := DebugLevelStr + msg;
        // if user clicks on Cancel, downgrade the debug level by 1 until it reaches 0
        if ConfirmOKCancelWithCaption(Caption, msg) = False then
            iDebugLevel := Max(iDebugLevel - 1, 0);
    end;
end;


function    DocumentIsPCB(dummy : Boolean = False) : Boolean;
begin
    // set AD build flag
    if not Assigned(IsAtLeastAD19) then if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;
    if not Assigned(iDebugLevel) then iDebugLevel := cDEBUGLEVEL;
    if not Assigned(bProcessing) then bProcessing := False;

    // Checks if current document is a PCB kind if not, show error and return false.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then
    begin
        ShowError('This script must be run from a PCB document.');
        Result := False;
    end
    else Result := True;
end;


function    FillDrillPairsList(const DrillPairObj : IPCB_DrillLayerPair; var DrillPairsList : TInterfaceList; const Clear : Boolean = False) : Boolean;
begin
    Result := False;
    if (DrillPairObj = nil) or (DrillPairsList = nil) then exit;

    if Clear then DrillPairsList.Clear;

    DrillPairsList.Add(DrillPairObj);

    Result := (DrillPairsList.Count > 0);
end;

procedure   FillDrillPairsListBox(ListBox : TObject);
var
    i               : Integer;
    DrillPairObj    : IPCB_DrillLayerPair;
begin
    GetDrillPairs;

    for i := 0 to DrillPairsList.Count - 1 do
    begin
        DrillPairObj := DrillPairsList[i];
        ListBox.Items.AddObject(DrillPairObj.Description, DrillPairObj);
    end;

    if ListBox.Items.Count = 0 then ListBox.Items.Add('!!! NO DRILL PAIRS DEFINED !!!');
end;

procedure   FillNetClassListBox(ListBox : TObject);
var
    Iterator : IPCB_BoardIterator;
    c        : IPCB_ObjectClass;
begin
    Iterator := Board.BoardIterator_Create;

    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eClassObject));
    c := Iterator.FirstPCBObject;
    while c <> nil do
    begin
        if (c.MemberKind = eClassMemberKind_Net) and not (c.Name = 'All Nets') then ListBox.Items.AddObject(c.Name, c);

        c := Iterator.NextPCBObject;
    end;
end;

procedure   FillNetComboBox(ComboBox : TObject);
var
    Iterator : IPCB_BoardIterator;
    Net      : IPCB_Net;
begin
    if ComboBox = nil then exit;
    Iterator := Board.BoardIterator_Create;

    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eNetObject));
    Net := Iterator.FirstPCBObject;
    while Net <> nil do
    begin
        ComboBox.Items.AddObject(Net.Name, Net);
        Net := Iterator.NextPCBObject;
    end;

    if ComboBox.Items.Count > 0 then ComboBox.ItemIndex := 0;
end;

function    FillNetListFromClass(const NetClass : IPCB_OBjectClass; var NetList : TInterfaceList; const Clear : Boolean = False) : Boolean;
var
    Iterator        : IPCB_BoardIterator;
    Net             : IPCB_Net;
begin
    Result := False;
    if (NetClass = nil) or (NetList = nil) then exit;

    if Clear then NetList.Clear;

    Iterator := Board.BoardIterator_Create;
    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eNetObject));
    Net := Iterator.FirstPCBObject;
    while Net <> nil do
    begin
        if NetClass.IsMember(Net) then NetList.Add(Net);
        Net := Iterator.NextPCBObject;
    end;
    Result := (NetList.Count > 0);
end;

function    FillNetListFromSingleNet(const Net : IPCB_Net; var NetList : TInterfaceList; const Clear : Boolean = False) : Boolean;
begin
    Result := False;
    if (Net = nil) or (NetList = nil) then exit;

    if Clear then NetList.Clear;

    NetList.Add(Net);

    Result := (NetList.Count > 0);
end;


function    FolderIsReadOnly(const AFolderPath : String) : Boolean;
var
    TempFile: String;
    FileHandle: Integer;
begin
    Result := True;
    TempFile := IncludeTrailingPathDelimiter(AFolderPath) + 'temp.tmp';
    FileHandle := FileCreate(TempFile);
    if FileHandle > 0 then
    begin
        FileClose(FileHandle);
        DeleteFile(TempFile);
        Result := False;
    end;
end;


function    GetDelimitedListBoxSelections(ListBox : TListBox; MyDelim : String = ';;') : String;
var
    i               : Integer;
    SelectedItems   : String;
begin
    SelectedItems := '';

    for i := 0 to ListBox.Items.Count - 1 do
    begin
        if ListBox.Selected[i] then
        begin
            if SelectedItems <> '' then SelectedItems := SelectedItems + MyDelim;

            SelectedItems := SelectedItems + ListBox.Items[i];
        end;
    end;

    Result := SelectedItems;
end;


function    GetDrillPairs(const dummy : Integer = 0) : TInterfaceList;
var
    i               : Integer;
    DrillPairObj    : IPCB_DrillLayerPair;
begin
    if not Assigned(DrillPairsList) then DrillPairsList := CreateObject(TInterfaceList) else DrillPairsList.Clear;
    for i := 0 to Board.DrillLayerPairsCount - 1 do
    begin
        DrillPairObj := Board.LayerPair(i);
        DrillPairsList.Add(DrillPairObj);
    end;

    Result := DrillPairsList;
end;


function    GetIteratorCount(Iter : IPCB_BoardIterator) : Integer;
var
    count : Integer;
    obj   : IPCB_ObjectClass;
begin
    count := 0;

    obj := Iter.FirstPCBObject;
    while obj <> nil do
    begin
        Inc(count);
        obj := Iter.NextPCBObject;
    end;
    result := count;
end;


function    GetNearbyVias(SignalVia : IPCB_Via) : TInterfaceList;
var
    NearbyVia               : IPCB_Via;
    SIter                   : IPCB_SpatialIterator;
    dX, dY, distance        : TCoord;
    SIterLeft, SIterRight   : TCoord;
    SIterTop, SIterBot      : TCoord;
begin
    Result := CreateObject(TInterfaceList);
    if (SignalVia = nil) or (SignalVia.ObjectId <> eViaObject) then exit;

    // Define the spatial iterator search area
    SIterLeft := SignalVia.x - VIADISTANCEMAX;
    SIterBot := SignalVia.y - VIADISTANCEMAX;
    SIterRight := SignalVia.x + VIADISTANCEMAX;
    SIterTop := SignalVia.y + VIADISTANCEMAX;

    // Create and configure spatial iterator
    SIter := Board.SpatialIterator_Create;
    SIter.AddFilter_ObjectSet(MkSet(eViaObject));
    SIter.AddFilter_Area(SIterLeft, SIterBot, SIterRight, SIterTop);

    // Loop through objects in the spatial iterator
    NearbyVia := SIter.FirstPCBObject;
    while (NearbyVia <> nil) do
    begin
        if (NearbyVia.ObjectId = eViaObject) and (SignalVia.I_ObjectAddress <> NearbyVia.I_ObjectAddress) then Result.Add(NearbyVia);

        NearbyVia := SIter.NextPCBObject;
    end;

    // Destroy the spatial iterator
    Board.SpatialIterator_Destroy(SIter);
end;


function    GetViaDrillPair(Via : IPCB_Via) : IPCB_DrillLayerPair;
var
    idx : Integer;
    DrillPairObj : IPCB_DrillLayerPair;
begin
    Result := nil;
    if not Assigned(DrillPairsList) then GetDrillPairs;

    for idx := 0 to DrillPairsList.Count - 1 do
    begin
        DrillPairObj := DrillPairsList[idx];
        if (Via.StartLayer = DrillPairObj.StartLayer) and (Via.StopLayer = DrillPairObj.StopLayer) then
        begin
            Result := DrillPairObj;
            break;
        end;
    end;
end;


procedure   GUI_BeginProcess(dummy : Boolean = False);
begin
    if bProcessing = False then
    begin
        SetButtonEnableStates(False);
        PCBServer.PreProcess; // start undo
        bProcessing := True;
        BeginHourGlass;
    end;
end;

procedure   GUI_EndProcess(dummy : Boolean = False);
begin
    if bProcessing = True then
    begin
        EndHourGlass;
        bProcessing := False;
        PCBServer.PostProcess; // end undo
        SetButtonEnableStates(True);
    end;
end;

function    GUI_ForLoopStart(StatusMsg : String; LoopCount : Integer) : Integer;
begin
    Result := LoopCount;
    MyPercent_Init(StatusMsg, LoopCount);
    GUI_BeginProcess;
end;

procedure   GUI_LoopEnd(dummy : Boolean = False);
begin
    GUI_EndProcess;
    MyPercent_Finish;
end;

procedure   GUI_LoopProgress(StatusMsg : String = '');
begin
    MyPercent_Update(StatusMsg);
end;


function    HasReturnVia(SignalVia : IPCB_Via) : Boolean;
var
    i               : Integer;
    ViaList         : TInterfaceList;
begin
    Result := True;
    ViaList := CreateObject(TInterfaceList);
    ViaList := GetNearbyVias(SignalVia);

    for i := 0 to ViaList.Count - 1 do
    begin
        if IsReturnVia(SignalVia, ViaList[i]) then exit;
    end;
    Result := False;
end;


procedure   IgnoreArea(dummy : Integer = 0);
var
    X1, X2, Y1, Y2          : TCoord;
    ListVia, AreaVia        : IPCB_Via;
    ListIdx                 : Integer;
    SIter                   : IPCB_SpatialIterator;
    SIterLeft, SIterRight   : TCoord;
    SIterTop, SIterBot      : TCoord;
begin
    // The ChooseRectangleByCorners fn is an interactive function where you are prompted to choose two points on a PCB document.
    if not (Board.ChooseRectangleByCorners( 'Choose first corner (Touching Area)', 'Choose second corner (Touching Area)', X1,Y1,X2,Y2)) then exit;

    // find all vias touching this area using iterator
    SIterLeft := Min(X1, X2);
    SIterRight := Max(X1, X2);
    SIterTop := Max(Y1, Y2);
    SIterBot := Min(Y1, Y2);

    SIter := Board.SpatialIterator_Create;
    SIter.AddFilter_ObjectSet(MkSet(eViaObject));
    SIter.AddFilter_Area(SIterLeft, SIterBot, SIterRight, SIterTop);

    // Loop through vias in the spatial iterator area
    AreaVia := SIter.FirstPCBObject;
    while (AreaVia <> nil) do
    begin
        // check failed via list for this via and remove
        for ListIdx := FailedViaList.Count - 1 downto 0 do
        begin
            ListVia := FailedViaList[ListIdx];
            if AreaVia.I_ObjectAddress = ListVia.I_ObjectAddress then
            begin
                ListVia.Selected := False;
                Rules_ClearDRC_Via(ListVia);
                FailedViaList.Delete(ListIdx);
                break;
            end;
        end;
        AreaVia := SIter.NextPCBObject;
    end;
    Board.SpatialIterator_Destroy(SIter);

    // update failure status and buttons
    FailedViaIndex := Min(FailedViaIndex, FailedViaList.Count - 1);
    UpdateStatus;
    UpdateNavButtonStates;
end;


function    InAllowedDrillStack(Via : IPCB_Via) : Boolean;
var
    i       : Integer;
begin
    Result := False;
    if (Via = nil) or (not Assigned(ReturnViaDrillPairsList)) then exit;

    for i := 0 to ReturnViaDrillPairsList.Count - 1 do
    begin
        if (Via.StartLayer = ReturnViaDrillPairsList[i].StartLayer) and (Via.StopLayer = ReturnViaDrillPairsList[i].StopLayer) then
        begin
            Result := True;
            exit;
        end;
    end;
end;


procedure   InitialCheckGUI(var status : Integer);
var
    i               : Integer;
begin
    status := 0; // clear result status

    if not DocumentIsPCB then
    begin
        status := 1;
        exit;
    end;

    iDebugLevel := cDEBUGLEVEL;

    IsSelectableCheck(IsViaSelectable);

    MLS := Board.MasterLayerStack;

    MessagesManager := GetWorkSpace.DM_MessagesManager;

end;


function    IsReturnVia(SignalVia : IPCB_Via; OtherVia : IPCB_Via) : Boolean;
var
    i                   : Integer;
    distance            : TCoord;
    dX, dY              : TCoord;
    IsOtherStackAllowed : Boolean;
    IsOtherInReturnNet  : Boolean;
begin
    Result := False;
    if (SignalVia = nil) or (OtherVia = nil) then exit;
    if (SignalVia.ObjectId <> eViaObject) or (OtherVia.ObjectId <> eViaObject) then exit;
    if SignalVia.I_ObjectAddress = OtherVia.I_ObjectAddress then exit;

    //IsOtherStackAllowed := ((OtherVia.LowLayer = eBottomLayer) and (OtherVia.HighLayer = eTopLayer)) or ((OtherVia.HighLayer = eBottomLayer) and (OtherVia.LowLayer = eTopLayer));
    IsOtherStackAllowed := InAllowedDrillStack(OtherVia);
    if not IsOtherStackAllowed then exit;

    IsOtherInReturnNet := False;
    for i := 0 to ReturnNetList.Count - 1 do
    begin
        if OtherVia.Net = ReturnNetList[i] then
        begin
            IsOtherInReturnNet := True;
            break;
        end;
    end;
    if not IsOtherInReturnNet then exit;

    dX := abs(SignalVia.x - OtherVia.x) div 1000;
    dY := abs(SignalVia.y - OtherVia.y) div 1000;

    distance := Sqrt((dX * dX) + (dY * dY)) * 1000;
    if distance <= VIADISTANCEMAX then Result := True;
    DebugMessage(3, Format('Distance between %s and %s is %s', [SignalVia.Descriptor, OtherVia.Descriptor, CoordToStr(distance)]));
end;


function    IsSelectableCheck(var bCanSelectVia : Boolean);
var
    checkVia    : Boolean;
    Iter        : IPCB_BoardIterator;
    Obj         : IPCB_ObjectClass;
    tempBool    : Boolean;
begin
    checkVia := True;

    if not Assigned(bCanSelectVia) then bCanSelectVia := False;

    Iter := Board.BoardIterator_Create;
    Iter.AddFilter_ObjectSet(MkSet(eViaObject));
    Iter.AddFilter_LayerSet(AllLayers);
    Iter.AddFilter_Method(eProcessAll);

    Obj := Iter.FirstPCBObject;
    while (Obj <> nil) do
    begin
        if (Obj.ObjectId = eViaObject) and (checkVia) then
        begin
            tempBool := Obj.Selected;
            Obj.Selected := True;
            if Obj.Selected = True then bCanSelectVia := True else bCanSelectVia := False;
            Obj.Selected := tempBool;
            checkVia := False;
        end;

        if not checkVia then break;

        Obj := Iter.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(Iter);

end;


function    IsSelectableCheckStop(bShowWarning : Boolean = True) : Boolean;
begin
    IsSelectableCheck(IsViaSelectable);

    if bShowWarning then
    begin
        if not IsViaSelectable then ShowWarning('Vias are not selectable. Make sure "Vias" are enabled in selection filter.');
    end;

    Result := not IsViaSelectable;
end;


function    IsStringANum(Text : string) : Boolean;
var
    i        : Integer;
    dotCount : Integer;
    hyphenCount : Integer;
    ChSet    : TSet;
begin
    Result := True;

    // Test for number, hyphen, dot, or comma
    ChSet := SetUnion(MkSet(Ord('-'), Ord('.'), Ord(',')), MkSetRange(Ord('0'), Ord('9')));
    for i := 1 to Length(Text) do
        if not InSet(Ord(Text[i]), ChSet) then Result := False;

    // test if there is a hyphen that isn't leading
    ChSet := MkSet(Ord('-'));
    for i := 2 to Length(Text) do
        if InSet(Ord(Text[i]), ChSet) then Result := False;

    // Test for more than one hyphen
    hyphenCount := 0;
    ChSet    := MkSet(Ord('-'));
    for i    := 1 to Length(Text) do
        if InSet(Ord(Text[i]), ChSet) then inc(hyphenCount);

    // Test for more than one dot or comma
    dotCount := 0;
    ChSet    := MkSet(Ord('.'), Ord(','));
    for i    := 1 to Length(Text) do
        if InSet(Ord(Text[i]), ChSet) then inc(dotCount);

    if (dotCount > 1) or (hyphenCount > 1) then Result := False;
end; { IsStringANum }


function    ParseDelimitedString(const DelimitedText : String; const MyDelim : String = ';;') : TStringList;
var
    DelimPos, MAXINT            : Integer;
    CurrentItem, SubText        : String;
    TempList                    : TStringList;
begin
    MAXINT := 2147483647;
    TempList := CreateObject(TStringList);
    TempList.Clear;
    Result := TempList;

    if DelimitedText = '' then exit;

    SubText := DelimitedText;

    repeat
        DelimPos := Pos(MyDelim, SubText);
        if DelimPos > 0 then
        begin
            CurrentItem := Copy(SubText, 1, DelimPos - 1);
            TempList.Add(CurrentItem);
            SubText := Copy(SubText, DelimPos + Length(MyDelim), MAXINT);
        end;
    until DelimPos = 0;

    // Add the last item if there's any text left
    if Length(SubText) > 0 then TempList.Add(SubText);

    Result := TempList;
end;


procedure   RefreshFailedVias(dummy : Boolean = False);
var
    i           : Integer;
    CurrentVia  : IPCB_Via;
begin
    GetWorkspace.DM_ShowMessageView;
    ClientDeselectAll;

    if FailedViaList.Count > 0 then
    begin
        Board.BeginModify;

        for i := 0 to FailedViaList.Count - 1 do
        begin
            CurrentVia := FailedViaList[i];
            CurrentVia.Selected := True;
            //Board.AddObjectToHighlightObjectList(CurrentVia);
            SetDRCAndAddToHighlight(CurrentVia);
            //CurrentVia.GraphicallyInvalidate;
            AddMessageCallback_Via(CurrentVia);
        end;

        //ClientZoomSelected;

        // set of THighlightMethod = (eHighlight_Filter, eHighlight_Zoom, eHighlight_Select, eHighlight_Graph, eHighlight_Dim, eHighlight_Thicken, eHighlight_ZoomCursor, eHighlight_ForceSmooth)
        if FailedViaList.Count > 0 then Board.SetState_Navigate_HighlightObjectList(MkSet(eHighlight_Dim, eHighlight_Zoom, eHighlight_Select), True);

        Board.EndModify;
    end;

    Board.ViewManager_FullUpdate;

    UpdateStatus;
    UpdateNavButtonStates;
end;


function    Rules_AddCustomViaRule(const RuleKind : TRuleKind; const RuleName : WideString; DrillPairObj : IPCB_DrillLayerPair; const Comment : WideString) : IPCB_Rule;
begin
    Result := PCBServer.PCBRuleFactory(RuleKind);
    Result.Name := RuleName + '_' + DrillPairObj.Description;
    // scope TBD
    Result.Scope1Expression := 'IsVia';
    Result.Scope2Expression := 'IsVia';
    //Result.Scope1Expression := 'IsVia and (DrillPair = ''' + DrillPairObj.Description + ''')';
    //Result.Scope2Expression := 'IsVia';

    Result.Comment          := Comment;

    // DRC criteria depends on RuleKind
    // not sure what applies to eRule_RoutingViaStyle, but I'm assuming that
    // unless I define something, the default is to fail, which works out just fine
    //Result.Minimum          := MilsToCoord(10); // example for eRule_MinimumAnnularRing

    Result.Gap := VIADISTANCEMAX; // Gap applies to eRule_HoleToHoleClearance

    Result.DRCEnabled       := true;
    Board.AddPCBObject(Result);
end;

procedure   Rules_ClearDRC_Via(Via : IPCB_Via);
var
    idx : Integer;
    Violation : IPCB_Violation;
    ViolationPrim : IPCB_Primitive;
begin
    DebugMessage(2, 'Rules_ClearDRC_Via() called on ' + Via.Descriptor);

    if Via = nil then exit;

    if not bUseCustomViolations then
    begin
        if Via.DRCError then
        begin
            Via.BeginModify;
            Via.DRCError := False;
            Via.EndModify;
            Via.GraphicallyInvalidate;
            exit;
        end;
    end;

    for idx := ViolationList.Count - 1 downto 0 do
    begin
        Violation := ViolationList[idx];
        if Violation = nil then continue;
        ViolationPrim := Violation.Primitive1;
        if (ViolationPrim <> nil) and (ViolationPrim.I_ObjectAddress = Via.I_ObjectAddress) then
        begin
            if Violation.DRCError then
            begin
                Violation.BeginModify;
                Violation.DRCError := False;
                Violation.Enabled; // TBD
                Violation.Used; // TBD
                Violation.EndModify;
            end;

            if Via.DRCError then
            begin
                Via.BeginModify;
                Via.DRCError := False;
                Via.EndModify;
                Via.GraphicallyInvalidate;
            end;

            Board.BeginModify;
            Board.RemovePCBObject(Violation);
            Board.EndModify;

            PCBServer.DestroyPCBObject(Violation);

            ViolationList.Delete(idx);
        end;
    end;
end;

procedure   Rules_CustomPostProcess(ViolationCount : Integer = 0);
var
    idx : Integer;
    Rule : IPCB_Rule;
    RuleCount : Integer;
begin
    DebugMessage(2, 'Rules_CustomPostProcess(' + IntToStr(ViolationCount) + ') called');

    if not bUseCustomViolations then ViolationCount := 0;

    // "Cleanup" means to turn off DRC flags for custom rules and also get rid of them if there are no violations
    if Assigned(CustomRulesList) then RuleCount := CustomRulesList.Count else RuleCount := 0;
    for idx := 0 to (RuleCount - 1) do
    begin
        Rule := CustomRulesList[idx];
        if Rule <> nil then
        begin
            Rule.DRCEnabled := False; // turn off DRCEnabled

            if ViolationCount = 0 then // remove custom rules from the board if there were no violations
            begin
                Board.RemovePCBObject(Rule);
                PCBServer.DestroyPCBObject(Rule);
            end;
        end;
    end;

    if ViolationCount = 0 then // if doing a cleanup, also refresh view
    begin
        ClientResetAllErrorMarkers;
        Board.ViewManager_FullUpdate;
        ClientZoomRedraw;
    end;
end;

procedure   Rules_CustomPreProcess(dummy : Integer = 0);
var
    idx : Integer;
    DrillPairObj : IPCB_DrillLayerPair;
    Rule : IPCB_Rule;
begin
    DebugMessage(2, 'Rules_CustomPreProcess called');

    if not bUseCustomViolations then exit;

    if not Assigned(DrillPairsList) then GetDrillPairs;

    if not Assigned(CustomRulesList) then CustomRulesList := Rules_GetRulesByKind(CustomRule1_Kind);

    if not Assigned(ViolationList) then ViolationList := CreateObject(TInterfaceList);

    for idx := 0 to (DrillPairsList.Count - 1) do
    begin
        DrillPairObj := DrillPairsList[idx];

        Rule := Rules_GetRuleByName(CustomRulesList, CustomRule1_Name + '_' + DrillPairObj.Description);

        if Rule = nil then
        begin
            Rule := Rules_AddCustomViaRule(CustomRule1_Kind, CustomRule1_Name, DrillPairObj, 'Signal via without nearby return via');
            if Rule <> nil then CustomRulesList.Add(Rule); // allows not having to rebuild CustomRulesList with Rules_GetRulesByKind() before using Rules_GetRuleByName() to find the new rule
        end
        else
        begin
            Rule.Gap := VIADISTANCEMAX; // Gap applies to eRule_HoleToHoleClearance
        end;

        if Rule <> nil then Rule.DRCEnabled := True;
    end;
end;

function    Rules_GetRulesByKind(const RuleKind : TRuleKind = -1) : TInterfaceList;
var
    Iterator : IPCB_BoardIterator;
    Rule     : IPCB_Rule;
begin
    Result := CreateObject(TInterfaceList);

    Iterator := Board.BoardIterator_Create;
    try
        Iterator.AddFilter_ObjectSet(MkSet(eRuleObject));
        Iterator.AddFilter_LayerSet(AllLayers);
        Iterator.AddFilter_Method(eProcessAll);
        Rule := Iterator.FirstPCBObject;
        while (Rule <> Nil) Do
        begin
            if (RuleKind = -1) or (Rule.RuleKind = RuleKind) then
            begin
                if Pos(CustomRule1_Name, Rule.Name) > 0 then Result.Add(Rule); // only include rules that have the magic name in them
            end;

            Rule := Iterator.NextPCBObject;
        end;

    finally
        Board.BoardIterator_Destroy(Iterator);
    end;
end;

function    Rules_GetRuleByName(const RulesList : TInterfaceList; const RuleName : WideString) : IPCB_Rule;
var
    Rule : IPCB_Rule;
    R    : integer;
begin
    Result := nil;
    for R := 0 to (RulesList.Count - 1) do
    begin
        Rule := RulesList.Items(R);
        if Rule.Name = RuleName then
        begin
            Result := Rule;
            break;
        end;
    end;
end;

function    Rules_GetRuleForVia(const RulesList : TInterfaceList; const Via : IPCB_Via) : IPCB_Rule;
var
    idx : Integer;
begin
    Result := Rules_GetRuleByName(RulesList, CustomRule1_Name + '_' + GetViaDrillPair(Via).Description);
end;

function    Rules_MakeViolation_Via(var Via : IPCB_Via) : IPCB_Violation;
var
    Violation : IPCB_Violation;
    Rule : IPCB_Rule;
begin
    DebugMessage(2, 'Rules_MakeViolation_Via() called on ' + Via.Descriptor);

    if not bUseCustomViolations then
    begin
        Result := nil;
        //SetDRCAndAddToHighlight(Via); // called in RefreshFailedVias
        exit;
    end;

    //if Rule.IsUnary then Violation := nil;
    Violation := nil;

    Rule := Rules_GetRuleForVia(CustomRulesList, Via);
    if Rule = nil then exit;

    //if Rule.CheckUnaryScope(Via) then Violation := Rule.ActualCheck(Via, nil); // for unary rule types
    if Rule.CheckUnaryScope(Via) then Violation := Rule.ActualCheck(Via, Via); // for binary rule types

    // if we got a violation to be created, configure it and add to the PCB
    if Violation <> nil then
    begin
        ViolationList.Add(Violation);

        //Violation.Name; // READ ONLY!! :(
        //Violation.Description; // READ ONLY!! :(
        //Violation.Layer := Layer;

        Board.BeginModify;
        //Board.AddObjectToHighlightObjectList(Via); // EXPERIMENTAL
        Board.AddPCBObject(Violation);
        //Board.DispatchMessage(Board.I_ObjectAddress, c_BroadCast, PCBM_BoardRegisteration, Violation.I_ObjectAddress);
        Board.EndModify;

        //Via.BeginModify;
        //Via.DRCError := True;
        //Via.EndModify;
        //Via.GraphicallyInvalidate;
        //SetDRCAndAddToHighlight(Via); // called in RefreshFailedVias
    end;

    Result := Violation;
end;

function    Rules_RuleKindToString (ARuleKind : TRuleKind) : String;
begin
    Result := '';

    case ARuleKind Of
        eRule_Clearance                : Result := 'Clearance';
        eRule_ParallelSegment          : Result := 'ParallelSegment';
        eRule_MaxMinWidth              : Result := 'Width';
        eRule_MaxMinLength             : Result := 'Length';
        eRule_MatchedLengths           : Result := 'MatchedLengths';
        eRule_DaisyChainStubLength     : Result := 'StubLength';
        eRule_PowerPlaneConnectStyle   : Result := 'PlaneConnect';
        eRule_RoutingTopology          : Result := 'RoutingTopology';
        eRule_RoutingPriority          : Result := 'RoutingPriority';
        eRule_RoutingLayers            : Result := 'RoutingLayers';
        eRule_RoutingCornerStyle       : Result := 'RoutingCorners';
        eRule_RoutingViaStyle          : Result := 'RoutingVias';
        eRule_PowerPlaneClearance      : Result := 'PlaneClearance';
        eRule_SolderMaskExpansion      : Result := 'SolderMaskExpansion';
        eRule_PasteMaskExpansion       : Result := 'PasteMaskExpansion';
        eRule_ShortCircuit             : Result := 'ShortCircuit';
        eRule_BrokenNets               : Result := 'UnRoutedNet';
        eRule_ViasUnderSMD             : Result := 'ViasUnderSMD';
        eRule_MaximumViaCount          : Result := 'MaximumViaCount';
        eRule_MinimumAnnularRing       : Result := 'MinimumAnnularRing';
        eRule_PolygonConnectStyle      : Result := 'PolygonConnect';
        eRule_AcuteAngle               : Result := 'AcuteAngle';
        eRule_ConfinementConstraint    : Result := 'RoomDefinition';
        eRule_SMDToCorner              : Result := 'SMDToCorner';
        eRule_ComponentClearance       : Result := 'ComponentClearance';
        eRule_ComponentRotations       : Result := 'ComponentOrientations';
        eRule_PermittedLayers          : Result := 'PermittedLayers';
        eRule_NetsToIgnore             : Result := 'NetsToIgnore';
        eRule_SignalStimulus           : Result := 'SignalStimulus';
        eRule_Overshoot_FallingEdge    : Result := 'OvershootFalling';
        eRule_Overshoot_RisingEdge     : Result := 'OvershootRising';
        eRule_Undershoot_FallingEdge   : Result := 'UndershootFalling';
        eRule_Undershoot_RisingEdge    : Result := 'UndershootRising';
        eRule_MaxMinImpedance          : Result := 'MaxMinImpedance';
        eRule_SignalTopValue           : Result := 'SignalTopValue';
        eRule_SignalBaseValue          : Result := 'SignalBaseValue';
        eRule_FlightTime_RisingEdge    : Result := 'FlightTimeRising';
        eRule_FlightTime_FallingEdge   : Result := 'FlightTimeFalling';
        eRule_LayerStack               : Result := 'LayerStack';
        eRule_MaxSlope_RisingEdge      : Result := 'SlopeRising';
        eRule_MaxSlope_FallingEdge     : Result := 'SlopeFalling';
        eRule_SupplyNets               : Result := 'SupplyNets';
        eRule_MaxMinHoleSize           : Result := 'HoleSize';
        eRule_TestPointStyle           : Result := 'Testpoint';
        eRule_TestPointUsage           : Result := 'TestPointUsage';
        eRule_UnconnectedPin           : Result := 'UnConnectedPin';
        eRule_SMDToPlane               : Result := 'SMDToPlane';
        eRule_SMDNeckDown              : Result := 'SMDNeckDown';
        eRule_LayerPair                : Result := 'LayerPairs';
        eRule_FanoutControl            : Result := 'FanoutControl';
        eRule_MaxMinHeight             : Result := 'Height';
        eRule_DifferentialPairsRouting : Result := 'DiffPairsRouting';
        eRule_HoleToHoleClearance      : Result := 'HoleToHoleClearance';
        eRule_MinimumSolderMaskSliver  : Result := 'MinimumSolderMaskSliver';
        eRule_SilkToSolderMaskClearance: Result := 'SilkToSolderMaskClearance';
        eRule_SilkToSilkClearance      : Result := 'SilkToSilkClearance';
        eRule_NetAntennae              : Result := 'NetAntennae';
        eRule_AssyTestPointStyle       : Result := 'AssyTestPointStyle';
        eRule_AssyTestPointUsage       : Result := 'AssyTestPointUsage';
        eRule_SilkToBoardRegion        : Result := 'SilkToBoardRegion';
        eRule_SMDPADEntry              : Result := 'SMDPADEntry';
        eRule_None                     : Result := 'None';
        eRule_ModifiedPolygon          : Result := 'ModifiedPolygon';
        eRule_BoardOutlineClearance    : Result := 'BoardOutlineClearance';
        eRule_BackDrilling             : Result := 'BackDrilling';
    end;
end;


function    SelectComboBoxItem(ComboBox : TComboBox; ItemString : String) : Boolean;
var
    idx : Integer;
begin
    Result := True;

    if ItemString = '' then exit; // if blank, don't modify

    idx := ComboBox.Items.IndexOf(ItemString);
    if idx <> -1 then ComboBox.ItemIndex := idx else Result := False;
end;


function    SelectListBoxItems(ListBox : TListBox; ListItems : TStringList) : Boolean;
var
    i, idx              : Integer;
    InitialSelections   : TStringList;
begin
    Result := True;

    if ListItems.Count = 0 then exit;

    InitialSelections := CreateObject(TStringList);

    // Remember initial selection state
    for i := 0 to ListBox.Items.Count - 1 do
        if ListBox.Selected[i] then InitialSelections.Add('1') else InitialSelections.Add('0');

    ListBox.Items.BeginUpdate;
    try
        ListBox.ClearSelection;

        for i := 0 to ListItems.Count - 1 do
        begin
            idx := ListBox.Items.IndexOf(ListItems[i]);
            if idx <> -1 then ListBox.Selected[idx] := True
            else
            begin
                // Something didn't match so rollback to initial selection state and abort
                for idx := 0 to ListBox.Items.Count - 1 do ListBox.Selected[idx] := (InitialSelections[idx] = '1');

                Result := False;
                Break;
            end;
        end;
    finally
        ListBox.Items.EndUpdate;
    end;
end;


procedure   SetButtonEnableStates(EnableState : Boolean);
begin
    // Reserved for future use
    //ButtonCheckAll.Enabled                  := EnableState;
end;


function    SetDRCAndAddToHighlight(var Prim : IPCB_Primitive);
begin
    DebugMessage(2, 'SetDRCAndAddToHighlight() called on ' + Prim.Descriptor);

    //if not Prim.DRCError then
    begin
        Prim.BeginModify;
        Prim.DRCError := True; // WHY ISN'T THIS SETTING VIOLATION OVERLAY BY ITSELF???
        Prim.GraphicallyInvalidate;
        Prim.EndModify;
        //Board.DispatchMessage(Board.I_ObjectAddress, c_BroadCast, PCBM_ViewUpdate, Prim.I_ObjectAddress); // experimental
    end;

    //Board.BeginModify;
    Board.AddObjectToHighlightObjectList(Prim);
    //Board.EndModify;
end;


procedure   SetInitialDrillPairSelections(dummy : Boolean = False);
var
    i, SelectIndex  : Integer;
    DrillPairObj    : IPCB_DrillLayerPair;
begin
    SelectIndex := -1;

    // Search for top-bottom drill pair
    for i := 0 to ListBoxDrillPairs.Items.Count - 1 do
    begin
        DrillPairObj := ListBoxDrillPairs.Items.Objects[i];
        if ((DrillPairObj.LowLayer = eBottomLayer) and (DrillPairObj.HighLayer = eTopLayer)) or ((DrillPairObj.LowLayer = eTopLayer) and (DrillPairObj.HighLayer = eBottomLayer)) then
        begin
            SelectIndex := i;
            Break;
        end;
    end;

    if (SelectIndex = -1) then ShowWarning('Unable to find full-stack drill pair') else ListBoxDrillPairs.Selected[SelectIndex] := True;
end;


procedure   SetInitialNetSelections(dummy : Boolean = False);
var
    i, SelectIndex  : Integer;
begin
    SelectIndex := -1;
    if rgReturnMode.ItemIndex = 0 then
    begin
        // Search for exact match 'GND'
        for i := 0 to ComboBoxReturnNet.Items.Count - 1 do
        begin
            if SameText(ComboBoxReturnNet.Items[i], 'GND') then
            begin
                SelectIndex := i;
                Break;
            end;
        end;

        // If 'GND' not found, select the first item (if exists)
        if (SelectIndex = -1) and (ComboBoxReturnNet.Items.Count > 0) then SelectIndex := 0;

        ComboBoxReturnNet.ItemIndex := SelectIndex;
    end;
end;


procedure   SetNetModeSignalStates(RadioGroup : TObject);
begin
    if RadioGroup = rgSignalMode then
    begin
        // signal net cases: 0 = single net; 1 = net class(es); 2 = all nets
        case RadioGroup.ItemIndex of
            0: begin
                ComboBoxSignalNet.Enabled := True;
                ListBoxSignalNets.Enabled := False;
            end;
            1: begin
                ComboBoxSignalNet.Enabled := False;
                ListBoxSignalNets.Enabled := True;
            end;
            2: begin
                ComboBoxSignalNet.Enabled := False;
                ListBoxSignalNets.Enabled := False;
            end;
        end;
    end
    else
    begin
        // return net cases: 0 = single net; 1 = net class(es)
        case RadioGroup.ItemIndex of
            0: begin
                ComboBoxReturnNet.Enabled := True;
                ListBoxReturnNets.Enabled := False;
            end;
            1: begin
                ComboBoxReturnNet.Enabled := False;
                ListBoxReturnNets.Enabled := True;
            end;
        end;
    end;
end;


procedure   SetNetPickEnableStates(EnableState : Boolean);
begin
    // Configure controls to select nets and change distance

    if EnableState then ButtonCheckAll.Caption := 'Check All' else ButtonCheckAll.Caption := 'Restart';

    ButtonIgnore.Enabled                    := not EnableState;
    ButtonIgnoreArea.Enabled                := not EnableState;
    ButtonNext.Enabled                      := not EnableState;
    ButtonPrevious.Enabled                  := not EnableState;
    ButtonRecheck.Enabled                   := not EnableState;
    ButtonZoom.Enabled                      := not EnableState;

    rgSignalMode.Enabled                    := EnableState;
    rgReturnMode.Enabled                    := EnableState;
    ListBoxDrillPairs.Enabled               := EnableState;

    if EnableState then
    begin
        SetNetModeSignalStates(rgSignalMode);
        SetNetModeSignalStates(rgReturnMode);
    end
    else
    begin
        ComboBoxSignalNet.Enabled := False;
        ListBoxSignalNets.Enabled := False;
        ComboBoxReturnNet.Enabled := False;
        ListBoxReturnNets.Enabled := False;
    end;

    // only allow changing config before brand new check
    MMmilButton.Enabled                     := EnableState;
    EditDistanceMax.Enabled                 := EnableState;
    CheckBoxRuleViolations.Enabled          := EnableState;
end;


function    StrFromObjectId(ObjectId: TObjectId) : String;
begin
    case ObjectId of
        0:  Result := 'eNoObject';
        1:  Result := 'eArcObject';
        2:  Result := 'ePadObject';
        3:  Result := 'eViaObject';
        4:  Result := 'eTrackObject';
        5:  Result := 'eTextObject';
        6:  Result := 'eFillObject';
        7:  Result := 'eConnectionObject';
        8:  Result := 'eNetObject';
        9:  Result := 'eComponentObject';
        10: Result := 'ePolyObject';
        11: Result := 'eRegionObject';
        12: Result := 'eComponentBodyObject';
        13: Result := 'eDimensionObject';
        14: Result := 'eCoordinateObject';
        15: Result := 'eClassObject';
        16: Result := 'eRuleObject';
        17: Result := 'eFromToObject';
        18: Result := 'eDifferentialPairObject';
        19: Result := 'eViolationObject';
        20: Result := 'eEmbeddedObject';
        21: Result := 'eEmbeddedBoardObject';
        22: Result := 'eSplitPlaneObject';
        23: Result := 'eTraceObject';
        24: Result := 'eSpareViaObject';
        25: Result := 'eBoardObject';
        26: Result := 'eBoardOutlineObject';
    else
        Result := 'Invalid object ID';
    end;
end;


procedure   TReturnViaCheckForm.ButtonCancelClick(Sender : TObject);
begin
    ReturnViaCheckForm.Close;
end; { TReturnViaCheckForm.ButtonCancelClick }

procedure   TReturnViaCheckForm.ButtonCheckAllClick(Sender : TObject);
begin
    Main_CheckAll;
end; { TReturnViaCheckForm.ButtonCheckAllClick }

procedure   TReturnViaCheckForm.ButtonIgnoreAreaClick(Sender : TObject);
begin
    IgnoreArea;
end;

procedure   TReturnViaCheckForm.ButtonIgnoreClick(Sender : TObject);
begin
    if FailedViaIndex >= 0 then
    begin
        Rules_ClearDRC_Via(FailedViaList[FailedViaIndex]);
        FailedViaList.Delete(FailedViaIndex);
        if FailedViaIndex >= FailedViaList.Count then Dec(FailedViaIndex);
    end;

    if FailedViaIndex >= 0 then
    begin
        ClientDeSelectAll;
        FailedViaList[FailedViaIndex].Selected := True;
        ClientZoomSelected;
    end;

    UpdateStatus;
    UpdateNavButtonStates;
end;

procedure   TReturnViaCheckForm.ButtonNextClick(Sender : TObject);
begin
    if FailedViaIndex < (FailedViaList.Count - 1) then Inc(FailedViaIndex);

    if FailedViaIndex >= 0 then
    begin
        ClientDeSelectAll;
        FailedViaList[FailedViaIndex].Selected := True;
        ClientZoomSelected;
    end;

    UpdateStatus;
    UpdateNavButtonStates;
end;

procedure   TReturnViaCheckForm.ButtonPreviousClick(Sender : TObject);
begin
    if FailedViaIndex > 0 then Dec(FailedViaIndex);

    if FailedViaIndex >= 0 then
    begin
        ClientDeSelectAll;
        FailedViaList[FailedViaIndex].Selected := True;
        ClientZoomSelected;
    end;

    UpdateStatus;
    UpdateNavButtonStates;
end;

procedure   TReturnViaCheckForm.ButtonRecheckClick(Sender : TObject);
begin
    Main_Recheck;
end;

procedure   TReturnViaCheckForm.ButtonZoomClick(Sender : TObject);
begin
    if FailedViaIndex >= 0 then
    begin
        ClientDeSelectAll;
        FailedViaList[FailedViaIndex].Selected := True;
        ClientZoomSelected;
    end;
end;

procedure   TReturnViaCheckForm.InputValueChange(Sender : TObject);
begin

    if IsStringANum(Sender.Text) then
    begin
        Sender.Font.Style := 0;
        Sender.Font.Color := clWindowText;
        SetButtonEnableStates(True);
        UpdateConstants;
    end
    else
    begin
        Sender.Font.Style := MkSet(fsBold, fsItalic, fsUnderline);
        Sender.Font.Color := clRed;
        SetButtonEnableStates(False);
    end;
end; { TReturnViaCheckForm.InputValueChange }

procedure   TReturnViaCheckForm.LabelVersionClick(Sender : TObject);
begin
    About;
end;

procedure   TReturnViaCheckForm.MMmilButtonClick(Sender : TObject);
begin
    if MMmilButton.Caption = 'mil' then
    begin
        MMmilButton.Caption := 'mm';
        ChangeTextUnits(eMetric);
    end
    else
    begin
        MMmilButton.Caption := 'mil';
        ChangeTextUnits(eImperial);
    end;

end; { TReturnViaCheckForm.MMmilButtonClick }

procedure   TReturnViaCheckForm.ReturnViaCheckFormClose(Sender: TObject; var Action: TCloseAction);
var
    i : Integer;
begin
    if Assigned(FailedViaList) and (ButtonCheckAll.Caption = 'Restart') then
    begin
        ClientDeSelectAll;
        ClientClearFilter;

        for i := 0 to FailedViaList.Count - 1 do
        begin
            Rules_ClearDRC_Via(FailedViaList[i]);
        end;

        FailedViaList.Clear;
    end;

    if Assigned(CustomRulesList) and (CustomRulesList.Count > 0) then // and ConfirmNoYes('Clean up temporary DRC rules and violations?') then
    begin
        PCBServer.PreProcess;

        Rules_CustomPostProcess;

        PCBServer.PostProcess;

        ShowInfo('Custom script design rules and DRC violations have been cleaned up');
    end;
end;

procedure   TReturnViaCheckForm.ReturnViaCheckFormCreate(Sender: TObject);
begin
    iDebugLevel := cDEBUGLEVEL;
    FailedViaIndex := -1;

    // set AD build flag
    if (GetBuildNumberPart(Client.GetProductVersion, 0) >= 19) then IsAtLeastAD19 := True else IsAtLeastAD19 := False;
end;

procedure   TReturnViaCheckForm.ReturnViaCheckFormShow(Sender : TObject);
begin
    LabelVersion.Caption := 'About v' + cScriptVersion;

    LabelStatus.Caption := 'Select desired distance and nets to check';

    Application.HintHidePause := 12000; // extend hint show time

    //rgCenterStrategy.Hint := 'Designator centering strategy. Affect resize fitting.' + sLineBreak +
        //'Automatic: larger of component body outline or component pads outline' + sLineBreak +
        //'Center of Bounds: center of component bounding rectangle.' + sLineBreak +
        //'Component Body: centroid of component body outline.' + sLineBreak +
        //'Centroid of Pads: center of rectangle that encloses component-layer pads.' + sLineBreak +
        //'Footprint Origin: center on component origin. Resize using center of pads.';

    FailedViaList := CreateObject(TInterfaceList);
    ReturnNetList := CreateObject(TInterfaceList);
    SignalNetList := CreateObject(TInterfaceList);
    DrillPairsList := CreateObject(TInterfaceList);
    ReturnViaDrillPairsList := CreateObject(TInterfaceList);

    FillNetComboBox(ComboBoxSignalNet);
    FillNetClassListBox(ListBoxSignalNets);

    ComboBoxReturnNet.Items.Assign(ComboBoxSignalNet.Items);
    ListBoxReturnNets.Items.Assign(ListBoxSignalNets.Items);

    FillDrillPairsListBox(ListBoxDrillPairs);

    SetInitialNetSelections;
    SetInitialDrillPairSelections;

    ConfigFile_Read(ConfigFile_GetPath);

    UpdateConstants;

    SetNetPickEnableStates(True);
end; { TReturnViaCheckForm.ReturnViaCheckFormShow }

procedure   TReturnViaCheckForm.rgSignalModeClick(Sender: TObject);
begin
    SetNetModeSignalStates(Sender);
end;

procedure   TReturnViaCheckForm.rgReturnModeClick(Sender: TObject);
begin
    SetNetModeSignalStates(Sender);
end;

{ programmatically, OnKeyPress fires before OnChange event and "catches" the key press }
procedure   TReturnViaCheckForm.UserKeyPress(Sender : TObject; var Key : Char);
begin
    if (Ord(Key) = 13) then
    begin
        Key := #0; // eat the enter keypress to avoid beep
        ReturnViaCheckForm.ActiveControl := ButtonCheckAll; // jump to Check All button
    end;
end; { UserKeyPress }


procedure   UpdateConstants(dummy : Boolean = False);
begin
    case MMmilButton.Caption of
        'mil':
        begin
            StringToCoordUnit(EditDistanceMax.Text, VIADISTANCEMAX, eImperial);
            bMetricUnits    := False;
        end;
        'mm':
        begin
            StringToCoordUnit(EditDistanceMax.Text, VIADISTANCEMAX, eMetric);
            bMetricUnits    := True;
        end;
        else
        begin
            // invalid
        end;
    end;

    VIADISTANCEMAX := MAX(10000, VIADISTANCEMAX); // protect from invalid input
end;


procedure   UpdateStatus(const StatusString : String = '');
begin
    if StatusString <> '' then
    begin
        LabelStatus.Caption := StatusString;
        exit;
    end;

    if FailedViaList.Count = 0 then
    begin
        LabelStatus.Caption := '0 failed vias detected';
        SetNetPickEnableStates(True);
        ClientDeSelectAll;
        exit;
    end;

    if FailedViaIndex = -1 then
    begin
        LabelStatus.Caption := Format('%d vias failed check. Step through below.', [FailedViaList.Count]);
        exit;
    end;

    LabelStatus.Caption := Format('Viewing via %d of %d (%s)', [FailedViaIndex + 1, FailedViaList.Count, FailedViaList[FailedViaIndex].Net.Name]);
end;


procedure   UpdateDrillPairsListFromSelections(dummy : Boolean = False);
var
    i               : Integer;
    ReturnNetIndex  : Integer;
begin
    ReturnViaDrillPairsList.Clear;

    for i := 0 to ListBoxDrillPairs.Items.Count - 1 do
    begin
        if ListBoxDrillPairs.Selected[i] then FillDrillPairsList(ListBoxDrillPairs.Items.Objects[i], ReturnViaDrillPairsList);
    end;

    DebugMessage(1, Format('%d drill pairs in ReturnViaDrillPairsList', [ReturnViaDrillPairsList.Count]));
end;


procedure   UpdateNavButtonStates(Reset : Boolean = False);
begin
    if Reset or (FailedViaList.Count = 0) then
    begin
        ButtonPrevious.Enabled      := False;
        ButtonNext.Enabled          := False;
        ButtonIgnore.Enabled        := False;
        ButtonIgnoreArea.Enabled    := False;
        ButtonZoom.Enabled          := False;
        exit;
    end;

    if ButtonNext.Enabled and (FailedViaIndex = -1) then ButtonNext.Caption := 'First' else ButtonNext.Caption := 'Next';
    ButtonPrevious.Enabled      := FailedViaIndex > 0;
    ButtonNext.Enabled          := (FailedViaIndex < FailedViaList.Count - 1) and (FailedViaList.Count > 0) ;
    ButtonIgnore.Enabled        := (FailedViaIndex >= 0) and (FailedViaList.Count > 0);
    ButtonIgnoreArea.Enabled    := (FailedViaList.Count > 0);
    ButtonZoom.Enabled          := (FailedViaIndex >= 0) and (FailedViaList.Count > 0);
end;


procedure   UpdateNetListsFromSelections(dummy : Boolean = False);
var
    i               : Integer;
    ReturnNetIndex  : Integer;
begin
    ReturnNetList.Clear;
    // case 0 = single net; 1 = net class(es)
    case rgReturnMode.ItemIndex of
        0: if ComboBoxReturnNet.ItemIndex <> -1 then FillNetListFromSingleNet(ComboBoxReturnNet.Items.Objects[ComboBoxReturnNet.ItemIndex], ReturnNetList);
        1: begin
            for i := 0 to ListBoxReturnNets.Items.Count - 1 do
            begin
                if ListBoxReturnNets.Selected[i] then FillNetListFromClass(ListBoxReturnNets.Items.Objects[i], ReturnNetList);
            end;
        end;
    end;
    DebugMessage(1, Format('%d nets in ReturnNetList', [ReturnNetList.Count]));

    SignalNetList.Clear;
    // case 0 = single net; 1 = net class(es); 2 = All Nets
    case rgSignalMode.ItemIndex of
        0: if ComboBoxSignalNet.ItemIndex <> -1 then FillNetListFromSingleNet(ComboBoxSignalNet.Items.Objects[ComboBoxSignalNet.ItemIndex], SignalNetList);
        1: begin
            for i := 0 to ListBoxSignalNets.Items.Count - 1 do
            begin
                if ListBoxSignalNets.Selected[i] then FillNetListFromClass(ListBoxSignalNets.Items.Objects[i], SignalNetList);
            end;
        end;
        2: begin
            // Add all nets from ComboBoxSignalNet to SignalNetList
            for i := 0 to ComboBoxSignalNet.Items.Count - 1 do FillNetListFromSingleNet(ComboBoxSignalNet.Items.Objects[i], SignalNetList);
        end;
    end;

    // Remove any nets that are in ReturnNetList from SignalNetList
    for i := 0 to ReturnNetList.Count - 1 do
    begin
        ReturnNetIndex := SignalNetList.IndexOf(ReturnNetList[i]);
        if ReturnNetIndex >= 0 then SignalNetList.Delete(ReturnNetIndex);
    end;

    DebugMessage(1, Format('%d nets in SignalNetList', [SignalNetList.Count]));
end;


procedure   MyStatusBar_SetState(Index : Integer; const S : WideString);
begin
    Client.GUIManager.StatusBarManager.SetState(Index, S);
end;

function    MyStatusBar_GetState(Index : Integer) : Widestring;
begin
    Result := Client.GUIManager.StatusBarManager.GetState(Index);
end;

procedure   MyStatusBar_SetStateDefault(dummy : Boolean = False);
begin
    Client.GUIManager.StatusBarManager.SetState(cStatusBar_SetDefault,'');
end;

procedure   MyStatusBar_PushStatus(dummy : Boolean = False);
begin
    Client.GUIManager.StatusBarManager.SetState(cStatusBar_Push,'');
end;

procedure   MyStatusBar_PopStatus(dummy : Boolean = False);
begin
    Client.GUIManager.StatusBarManager.SetState(cStatusBar_Pop,'');
end;

function    MyPercentActive(dummy : Boolean = False) : Boolean;
begin
    Result := gvMarquee = False;
end;

function    MyMarqueeActive(dummy : Boolean = False) : Boolean;
begin
    Result := gvMarquee;
end;

function    MyPercent_GetTotal(dummy : Boolean = False) : Integer;
begin
    Result := -1;
    if MyPercentActive then
    begin
        Result := gvTotalPercentCount;
    end;
end;

procedure   MyPercent_Init(const InitialString : String ; TotalCount : Integer);
begin
    MyStatusBar_PushStatus;
    MyStatusBar_SetState(cStatusBar_ProgressBarStart, InitialString);
    gvTotalPercentCount     := TotalCount;
    gvCurrentPercentCount   := 0;
    gvOldPercent            := 0;
    gvMarquee               := False;
end;

procedure   MyPercent_Finish(dummy : Boolean = False);
begin
    //if MyPercentActive then
    //begin
        //MyStatusBar_SetState(cStatusBar_ProgressBarStop,'');
        //MyStatusBar_PopStatus;
    //end;

    MyStatusBar_SetState(cStatusBar_ProgressBarStop,'');
    MyStatusBar_PopStatus;
    MyStatusBar_SetState(cStatusBar_SetDefault,'');
end;

procedure   MyPercent_UpdateByNumber(AmountToIncrement : Integer, StatusMsg : String = '');
begin
    if MyPercentActive then
    begin
        gvCurrentPercentCount := gvCurrentPercentCount + AmountToIncrement - 1;
        MyPercent_Update(StatusMsg);
    end;
end;

procedure   MyPercent_Update(StatusMsg : String = '');
Var
    ThisPercent  : Integer;
    i            : Integer;
begin
    if MyPercentActive then
    begin
        Inc(gvCurrentPercentCount);

        if gvTotalPercentCount = 0 then ThisPercent := 100
        else
            ThisPercent := Round( (100.0 * gvCurrentPercentCount) /
                                  (1.0   * gvTotalPercentCount)  );
        if ThisPercent > 100 then
            ThisPercent := 100;
        for i := gvOldPercent to ThisPercent - 1 do
        begin
            if StatusMsg <> '' then MyStatusBar_SetState(cStatusBar_Panel2, Format('%s (%d%%)', [StatusMsg, ThisPercent]));
            MyStatusBar_SetState(cStatusBar_ProgressBarStep,'');
        end;
        gvOldPercent := ThisPercent;
    end;
end;

procedure   MyPercent_BeginUndeterminedOperation(const InitialString : String);
begin
    MyStatusBar_PushStatus;
    MyStatusBar_SetState(cStatusBar_UndeterminedOpBegin, InitialString);

    gvMarquee := True;
end;

procedure   MyPercent_EndUndeterminedOperation(dummy : Boolean = False);
begin
    if MyMarqueeActive then
    begin
        MyStatusBar_SetState(cStatusBar_UndeterminedOpEnd, '');
        MyStatusBar_PopStatus;
    end;
end;

procedure   MyPercent_BeginComplexOperation(const InitialString : String);
begin
    { status bar is not involved, so no need to push/pop percent stack}
    MyStatusBar_SetState(cStatusBar_ComplexOpBegin, InitialString);
end;

procedure   MyPercent_EndComplexOperation(dummy : Boolean = False);
begin
    MyStatusBar_SetState(cStatusBar_ComplexOpEnd, '');
end;

