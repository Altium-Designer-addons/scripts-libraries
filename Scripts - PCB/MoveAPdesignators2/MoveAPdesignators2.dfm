object TweakDesForm: TTweakDesForm
  Left = 313
  Top = 139
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Move Auto-Positioned Designators'
  ClientHeight = 283
  ClientWidth = 325
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = TweakDesFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelVersion: TLabel
    Left = 21
    Top = 167
    Width = 54
    Height = 13
    Caption = 'version X.X'
  end
  object SelectedCheckBox: TCheckBox
    Left = 35
    Top = 66
    Width = 117
    Height = 17
    Caption = '&Selected Parts Only'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object ButtonOK: TButton
    Left = 14
    Top = 120
    Width = 75
    Height = 32
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = ButtonOKClick
  end
  object EditDistance: TEdit
    Left = 24
    Top = 30
    Width = 64
    Height = 21
    Hint = 'Negative values move away'
    Alignment = taCenter
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Text = '0.5'
    OnChange = EditDistanceChange
  end
  object MMmilButton: TButton
    Left = 102
    Top = 30
    Width = 42
    Height = 20
    Caption = 'mm'
    TabOrder = 3
    OnClick = MMmilButtonClick
  end
  object UnHideDesignatorsCheckBox: TCheckBox
    Left = 35
    Top = 90
    Width = 117
    Height = 17
    Caption = 'UnHide &Designators'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object ButtonCancel: TButton
    Left = 96
    Top = 120
    Width = 75
    Height = 32
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = ButtonCancelClick
  end
  object ButtonPreset1: TButton
    Left = 248
    Top = 17
    Width = 64
    Height = 25
    Caption = 'Preset &1'
    TabOrder = 6
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset1: TEdit
    Left = 176
    Top = 19
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 7
    Text = '4'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset2: TButton
    Left = 248
    Top = 49
    Width = 64
    Height = 25
    Caption = 'Preset &2'
    TabOrder = 8
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset2: TEdit
    Left = 176
    Top = 51
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 9
    Text = '5'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset3: TButton
    Left = 248
    Top = 81
    Width = 64
    Height = 25
    Caption = 'Preset &3'
    TabOrder = 10
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset3: TEdit
    Left = 176
    Top = 83
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 11
    Text = '6'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset4: TButton
    Left = 248
    Top = 113
    Width = 64
    Height = 25
    Caption = 'Preset &4'
    TabOrder = 12
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset4: TEdit
    Left = 176
    Top = 115
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 13
    Text = '8'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset5: TButton
    Left = 248
    Top = 145
    Width = 64
    Height = 25
    Caption = 'Preset &5'
    TabOrder = 14
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset5: TEdit
    Left = 176
    Top = 147
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 15
    Text = '10'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset6: TButton
    Left = 248
    Top = 177
    Width = 64
    Height = 25
    Caption = 'Preset &6'
    TabOrder = 16
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset6: TEdit
    Left = 176
    Top = 179
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 17
    Text = '16'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset7: TButton
    Left = 248
    Top = 209
    Width = 64
    Height = 25
    Caption = 'Preset &7'
    TabOrder = 18
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset7: TEdit
    Left = 176
    Top = 211
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 19
    Text = '20'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset8: TButton
    Left = 248
    Top = 241
    Width = 64
    Height = 25
    Caption = 'Preset &8'
    TabOrder = 20
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset8: TEdit
    Left = 176
    Top = 243
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 21
    Text = '30'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
end
