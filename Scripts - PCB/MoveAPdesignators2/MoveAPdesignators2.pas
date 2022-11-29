{ ****************************************************************************** }
{ * See README.md for release info
{ ****************************************************************************** }

var
    Board                          : IPCB_Board;
    UnHideDesignators, AbortScript : Boolean;
    PresetFilePath                 : string;
    PresetList                     : TStringList;


const
    UsePresets    = True;
    NumPresets    = 12; // not just for presets, also used to save previous state
    ScriptVersion = '2.01';


{ function to populate a TStringList with preset values }
procedure BuildPresetList(var TempPresetList : TStringList);
begin
    TempPresetList.Clear;
    TempPresetList.Add(EditDistance.Text);
    TempPresetList.Add(tPreset1.Text);
    TempPresetList.Add(tPreset2.Text);
    TempPresetList.Add(tPreset3.Text);
    TempPresetList.Add(tPreset4.Text);
    TempPresetList.Add(tPreset5.Text);
    TempPresetList.Add(tPreset6.Text);
    TempPresetList.Add(tPreset7.Text);
    TempPresetList.Add(tPreset8.Text);
    TempPresetList.Add(MMmilButton.Caption);
    TempPresetList.Add(SelectedCheckBox.Checked);
    TempPresetList.Add(UnHideDesignatorsCheckBox.Checked);
end; { BuildPresetList }


// function to load preset list from file
procedure LoadPresetListFromFile(const dummy : Integer);
begin
    // default file name is MyMoveDesignatorsPresets.txt
    PresetFilePath := ClientAPI_SpecialFolder_AltiumApplicationData + '\MyMoveDesignatorsPresets.txt';
    PresetList     := TStringList.Create;
    if FileExists(PresetFilePath) then
    begin
        // ShowMessage('Loading presets from ' + PresetFilePath);
        PresetList.LoadFromFile(PresetFilePath); // load presets from file if it exists

        case PresetList.Count of
            // add cases here to handle backward compatibility of preset files
            NumPresets :
                begin
                    // do nothing
                end
            else // if PresetList.Count < NumPresets then PresetList file exists but count is short, just regenerate preset file from defaults
                begin
                    // ShowMessage(PresetFilePath + ' exists but is not the correct length. Defaults will be used.');
                    BuildPresetList(PresetList);
                    PresetList.SaveToFile(PresetFilePath);
                end;
        end;

        // set text boxes to match preset list (redundant if list was regenerated above)
        tPreset1.Text                     := PresetList[1];
        tPreset2.Text                     := PresetList[2];
        tPreset3.Text                     := PresetList[3];
        tPreset4.Text                     := PresetList[4];
        tPreset5.Text                     := PresetList[5];
        tPreset6.Text                     := PresetList[6];
        tPreset7.Text                     := PresetList[7];
        tPreset8.Text                     := PresetList[8];
        MMmilButton.Caption               := PresetList[9];
        SelectedCheckBox.Checked          := PresetList[10];
        UnHideDesignatorsCheckBox.Checked := PresetList[11];
        EditDistance.Text                 := PresetList[0]; // Main input field needs to be set last because setting each preset updates it
    end
    else
    begin // if preset file didn't exist at all, create from defaults
        // ShowMessage(PresetFilePath + ' does not exist.');
        BuildPresetList(PresetList);
        PresetList.SaveToFile(PresetFilePath);
    end;
end; { LoadPresetListFromFile }


{ function to transform TTextAutoposition based on rotation }
function TransformAutoPos(Designator : IPCB_Text, tc_AutoPos : TTextAutoposition) : TTextAutoposition;
var
    rotation : Integer;
    mirror   : Boolean;

begin
    rotation := Round(Designator.GetState_Rotation);
    mirror := Designator.GetState_Mirror;
    if rotation > 315 then rotation := 0;

    case rotation of
        0..45 :
            begin
                case tc_AutoPos of
                    eAutoPos_TopLeft        : if mirror then Result := eAutoPos_TopRight else Result := eAutoPos_TopLeft;
                    eAutoPos_CenterLeft     : if mirror then Result := eAutoPos_CenterRight else Result := eAutoPos_CenterLeft;
                    eAutoPos_BottomLeft     : if mirror then Result := eAutoPos_BottomRight else Result := eAutoPos_BottomLeft;
                    eAutoPos_TopCenter      : Result := eAutoPos_TopCenter;
                    eAutoPos_BottomCenter   : Result := eAutoPos_BottomCenter;
                    eAutoPos_TopRight       : if mirror then Result := eAutoPos_TopLeft else Result := eAutoPos_TopRight;
                    eAutoPos_CenterRight    : if mirror then Result := eAutoPos_CenterLeft else Result := eAutoPos_CenterRight;
                    eAutoPos_BottomRight    : if mirror then Result := eAutoPos_BottomLeft else Result := eAutoPos_BottomRight;
                    else                      Result := tc_AutoPos;
                end; { case tc_AutoPos }
            end; { 0..45 }
        46..135 :
            begin
                case tc_AutoPos of
                    eAutoPos_TopLeft        : if mirror then Result := eAutoPos_TopLeft else Result := eAutoPos_TopRight;
                    eAutoPos_CenterLeft     : Result := eAutoPos_TopCenter;
                    eAutoPos_BottomLeft     : if mirror then Result := eAutoPos_TopRight else Result := eAutoPos_TopLeft;
                    eAutoPos_TopCenter      : if mirror then Result := eAutoPos_CenterLeft else Result := eAutoPos_CenterRight;
                    eAutoPos_BottomCenter   : if mirror then Result := eAutoPos_CenterRight else Result := eAutoPos_CenterLeft;
                    eAutoPos_TopRight       : if mirror then Result := eAutoPos_BottomLeft else Result := eAutoPos_BottomRight;
                    eAutoPos_CenterRight    : Result := eAutoPos_BottomCenter;
                    eAutoPos_BottomRight    : if mirror then Result := eAutoPos_BottomRight else Result := eAutoPos_BottomLeft;
                    else                      Result := tc_AutoPos;
                end; { case tc_AutoPos }
            end; { 46..135 }
        136..225 :
            begin
                case tc_AutoPos of
                    eAutoPos_TopLeft        : if mirror then Result := eAutoPos_BottomLeft else Result := eAutoPos_BottomRight;
                    eAutoPos_CenterLeft     : if mirror then Result := eAutoPos_CenterLeft else Result := eAutoPos_CenterRight;
                    eAutoPos_BottomLeft     : if mirror then Result := eAutoPos_TopLeft else Result := eAutoPos_TopRight;
                    eAutoPos_TopCenter      : Result := eAutoPos_BottomCenter;
                    eAutoPos_BottomCenter   : Result := eAutoPos_TopCenter;
                    eAutoPos_TopRight       : if mirror then Result := eAutoPos_BottomRight else Result := eAutoPos_BottomLeft;
                    eAutoPos_CenterRight    : if mirror then Result := eAutoPos_CenterRight else Result := eAutoPos_CenterLeft;
                    eAutoPos_BottomRight    : if mirror then Result := eAutoPos_TopRight else Result := eAutoPos_TopLeft;
                    else                      Result := tc_AutoPos;
                end; { case tc_AutoPos }
            end; { 136..225 }
        226..315 :
            begin
                case tc_AutoPos of
                    eAutoPos_TopLeft        : if mirror then Result := eAutoPos_BottomRight else Result := eAutoPos_BottomLeft;
                    eAutoPos_CenterLeft     : Result := eAutoPos_BottomCenter;
                    eAutoPos_BottomLeft     : if mirror then Result := eAutoPos_BottomLeft else Result := eAutoPos_BottomRight;
                    eAutoPos_TopCenter      : if mirror then Result := eAutoPos_CenterRight else Result := eAutoPos_CenterLeft;
                    eAutoPos_BottomCenter   : if mirror then Result := eAutoPos_CenterLeft else Result := eAutoPos_CenterRight;
                    eAutoPos_TopRight       : if mirror then Result := eAutoPos_TopRight else Result := eAutoPos_TopLeft;
                    eAutoPos_CenterRight    : Result := eAutoPos_TopCenter;
                    eAutoPos_BottomRight    : if mirror then Result := eAutoPos_TopLeft else Result := eAutoPos_TopRight;
                    else                      Result := tc_AutoPos;
                end; { case tc_AutoPos }
            end; { 226..315 }
        else Result := tc_AutoPos;
    end; { case rotation }

end; { TransformAutopos }


// Main procedure
procedure TweakDesignators;
var
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;
    Designator              : IPCB_Text;
    PCBSystemOptions        : IPCB_SystemOptions;
    DRCSetting              : Boolean;
    tc_AutoPos              : TTextAutoposition; // Current AP setting
    DesignatorXmove         : Integer;           // Distance to move
    TempPresetList          : TStringList;

begin
    // set version label
    LabelVersion.Caption := 'v' + ScriptVersion;

    // Verify that the document is a PcbDoc
    // if PCBServer.GetCurrentPCBBoard = Nil Then  Begin
    // Exit;
    // End;

    // Checks if current document is a PCB kind if not, Exit.
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = Nil then Exit;

    // Disables Online DRC during designator movement to improve speed
    PCBSystemOptions := PCBServer.SystemOptions;

    if PCBSystemOptions = Nil then Exit;

    DRCSetting                   := PCBSystemOptions.DoOnlineDRC;
    PCBSystemOptions.DoOnlineDRC := False;
    try
        AbortScript := False;
        TweakDesForm.ShowModal; // Show the settings dialogue (and resume script here after closed?)
        if AbortScript then
        begin
            PresetList.Free;
            Exit;
        end;

        // Notify the pcbserver that we will make changes (Start undo)
        PCBServer.PreProcess;
        ComponentIteratorHandle := Board.BoardIterator_Create;
        ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
        ComponentIteratorHandle.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
        ComponentIteratorHandle.AddFilter_Method(eProcessAll);

        if TweakDesForm.UnHideDesignatorsCheckBox.Checked then UnHideDesignators := True
        else UnHideDesignators := False;

        Component := ComponentIteratorHandle.FirstPCBObject;

        if TweakDesForm.SelectedCheckBox.Checked then
            while (Component <> Nil) do
                if not Component.Selected then Component := ComponentIteratorHandle.NextPCBObject
                else break; // Find the first selected comp if select only checked

        // Set the move distance to DB units converted from mils or mm
        if TweakDesForm.MMmilButton.Caption = 'mm' then DesignatorXmove := MMsToCoord(TweakDesForm.EditDistance.Text)
        else DesignatorXmove := MilsToCoord(TweakDesForm.EditDistance.Text);

        while (Component <> Nil) do
        begin
            Component.BeginModify;

            // Show hidden designators?
            if UnHideDesignators = True then Component.NameOn := True;

            tc_AutoPos := Component.GetState_NameAutoPos; // Get current AP state

            // Set AP to manual
            Component.SetState_NameAutoPos(eAutoPos_Manual);

            // Get the designator handle
            Designator := Component.Name;

            // notify that the pcb object is going to be modified
            // PCBServer.SendMessageToRobots(Designator.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);
            Component.Name.BeginModify;

            case Designator.GetState_Mirror of
                True : // If designator is mirrored, we will assume that X-axis movement should be reversed
                    // Move designator depending on current AP setting
                    case tc_AutoPos of
                        eAutoPos_Manual :;
                        eAutoPos_TopLeft :
                            begin
                                Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation - DesignatorXmove);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_BottomLeft);
                            end;
                        eAutoPos_CenterLeft :
                            begin
                                Designator.MoveToXY(Designator.Xlocation - DesignatorXmove, Designator.Ylocation);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_CenterLeft);
                            end;
                        eAutoPos_BottomLeft :
                            begin
                                Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation + DesignatorXmove);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_TopLeft);
                            end;
                        eAutoPos_TopCenter :
                            begin
                                Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation - DesignatorXmove);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_BottomCenter);
                            end;
                        eAutoPos_BottomCenter :
                            begin
                                Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation + DesignatorXmove);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_TopCenter);
                            end;
                        eAutoPos_TopRight :
                            begin
                                Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation - DesignatorXmove);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_BottomRight);
                            end;
                        eAutoPos_CenterRight :
                            begin
                                Designator.MoveToXY(Designator.Xlocation + DesignatorXmove, Designator.Ylocation);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_CenterRight);
                            end;
                        eAutoPos_BottomRight :
                            begin
                                Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation + DesignatorXmove);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_TopRight);
                            end;
                        eAutoPos_CenterCenter : Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_CenterCenter);
                    end; { case tc_AutoPos }
                False :
                    // Move designator depending on current AP setting
                    case tc_AutoPos of
                        eAutoPos_Manual :;
                        eAutoPos_TopLeft :
                            begin
                                Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation - DesignatorXmove);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_BottomRight);
                            end;
                        eAutoPos_CenterLeft :
                            begin
                                Designator.MoveToXY(Designator.Xlocation + DesignatorXmove, Designator.Ylocation);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_CenterRight);
                            end;
                        eAutoPos_BottomLeft :
                            begin
                                Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation + DesignatorXmove);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_TopRight);
                            end;
                        eAutoPos_TopCenter :
                            begin
                                Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation - DesignatorXmove);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_BottomCenter);
                            end;
                        eAutoPos_BottomCenter :
                            begin
                                Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation + DesignatorXmove);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_TopCenter);
                            end;
                        eAutoPos_TopRight :
                            begin
                                Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation - DesignatorXmove);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_BottomLeft);
                            end;
                        eAutoPos_CenterRight :
                            begin
                                Designator.MoveToXY(Designator.Xlocation - DesignatorXmove, Designator.Ylocation);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_CenterLeft);
                            end;
                        eAutoPos_BottomRight :
                            begin
                                Designator.MoveToXY(Designator.Xlocation, Designator.Ylocation + DesignatorXmove);
                                Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_TopLeft);
                            end;
                        eAutoPos_CenterCenter : Designator.TTFInvertedTextJustify := TransformAutoPos(Designator, eAutoPos_CenterCenter);
                    end; { case tc_AutoPos }
            end; { case Designator.GetState_Mirror }

            // notify that the pcb object is modified
            // PCBServer.SendMessageToRobots(Designator.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);
            Component.Name.EndModify;
            Designator.GraphicallyInvalidate;
            Board.DispatchMessage(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Designator.I_ObjectAddress);

            Component.EndModify;

            // Get the next component handle
            Component := ComponentIteratorHandle.NextPCBObject;
            if TweakDesForm.SelectedCheckBox.Checked then
                while (Component <> Nil) do
                    if not Component.Selected then Component := ComponentIteratorHandle.NextPCBObject
                    else break; // Find the next selected comp if select only checked


        end; { end while }

        // Notify the pcbserver that all changes have been made (Stop undo)
        PCBServer.PostProcess;

        // Refresh the screen (not needed with .GraphicallyInvalidate)
        // Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

        // Destroy the component handle
        Board.BoardIterator_Destroy(ComponentIteratorHandle);

        if UsePresets then
        begin
            // build list of currect preset values
            TempPresetList := TStringList.Create;
            BuildPresetList(TempPresetList);
            if TempPresetList.Equals(PresetList) then
            begin
                // presets match saved list so do nothing
            end
            else
            begin
                // save new list to MyMoveDesignatorsPresets.txt
                TempPresetList.SaveToFile(PresetFilePath);
            end;

            // cleanup
            TempPresetList.Free;
            PresetList.Free;
        end;

    finally
        // Restore DRC setting
        PCBSystemOptions.DoOnlineDRC := DRCSetting;
    end;
end; { TweakDesignators }


function IsStringANum(Text : string) : Boolean;
var
    i        : Integer;
    dotCount : Integer;
    ChSet    : TSet;
begin
    Result := True;

    // Test for number, dot or comma
    ChSet := SetUnion(MkSet(Ord('.'), Ord(',')), MkSetRange(Ord('0'), Ord('9')));
    for i := 1 to Length(Text) do
        if not InSet(Ord(Text[i]), ChSet) then Result := False;

    // Test for more than one dot or comma
    dotCount := 0;
    ChSet    := MkSet(Ord('.'), Ord(','));
    for i    := 1 to Length(Text) do
        if InSet(Ord(Text[i]), ChSet) then inc(dotCount);

    if dotCount > 1 then Result := False;
end; { IsStringANum }


procedure ValidateOnChange(Sender : TObject);
var
    textbox : TEdit;
begin
    textbox := Sender;
    // ShowMessage(textbox.Text);
    if IsStringANum(textbox.Text) then
    begin
        if Sender <> EditDistance then EditDistance.Text := textbox.Text;
        ButtonOK.Enabled                                 := True;
    end
    else ButtonOK.Enabled := False;

end; { ValidateOnChange }


procedure UserKeyPress(Sender : TObject; var Key : Char); // programmatically, OnKeyPress fires before OnChange event and "catches" the key press
begin
    if (ButtonOK.Enabled) and (Ord(Key) = 13) then
    begin
        Key := #0; // catch and discard key press to avoid beep
        if ButtonOK.Enabled then TweakDesForm.Close;
    end;
end; { UserKeyPress }


procedure PresetButtonClicked(Sender : TObject);
begin
    // ShowMessage('PresetButtonClicked');
    if Sender = ButtonPreset1 then EditDistance.Text      := tPreset1.Text
    else if Sender = ButtonPreset2 then EditDistance.Text := tPreset2.Text
    else if Sender = ButtonPreset3 then EditDistance.Text := tPreset3.Text
    else if Sender = ButtonPreset4 then EditDistance.Text := tPreset4.Text
    else if Sender = ButtonPreset5 then EditDistance.Text := tPreset5.Text
    else if Sender = ButtonPreset6 then EditDistance.Text := tPreset6.Text
    else if Sender = ButtonPreset7 then EditDistance.Text := tPreset7.Text
    else if Sender = ButtonPreset8 then EditDistance.Text := tPreset8.Text;
    TweakDesForm.Close;
end; { PresetButtonClicked }


procedure TTweakDesForm.ButtonOKClick(Sender : TObject);
begin
    TweakDesForm.Close;
end; { TTweakDesForm.ButtonOKClick }


procedure TTweakDesForm.MMmilButtonClick(Sender : TObject);
var
    TempString : string;
begin
    TempString := EditDistance.Text;
    if (LastDelimiter(',.', TempString) <> 0) then TempString[LastDelimiter(',.', TempString)] := DecimalSeparator;

    if MMmilButton.Caption = 'mil' then
    begin
        MMmilButton.Caption := 'mm';
        EditDistance.Text   := CoordToMMs(MilsToCoord(StrToFloat(TempString)));
    end
    else
    begin
        MMmilButton.Caption := 'mil';
        EditDistance.Text   := CoordToMils(MMsToCoord(StrToFloat(TempString)));
    end;
    EditDistance.SetFocus;
    EditDistance.Update;
end; { TTweakDesForm.MMmilButtonClick }


procedure TTweakDesForm.EditDistanceChange(Sender : TObject);
begin

    if IsStringANum(EditDistance.Text) then
    begin
        EditDistance.Font.Color := clWindowText;
        ButtonOK.Enabled        := True;
    end
    else
    begin
        ButtonOK.Enabled        := False;
        EditDistance.Font.Color := clRed;
    end;
end; { TTweakDesForm.EditDistanceChange }


procedure TTweakDesForm.ButtonCancelClick(Sender : TObject);
begin
    AbortScript := True;
    TweakDesForm.Close;
end; { TTweakDesForm.ButtonCancelClick }


procedure About;
begin
    ShowMessage('Move Auto-Positioned Designators v' + ScriptVersion + sLineBreak +
            'Updated versions may be found here:' + sLineBreak +
            'https://github.com/Altium-Designer-addons/scripts-libraries');
end; { About }


procedure TTweakDesForm.TweakDesFormShow(Sender : TObject);
begin
    // read presets from file
    LoadPresetListFromFile(0);
end; { TTweakDesForm.TweakDesFormShow }
