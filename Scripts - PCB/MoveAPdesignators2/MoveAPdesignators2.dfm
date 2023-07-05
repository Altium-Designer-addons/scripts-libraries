object TweakDesForm: TTweakDesForm
  Left = 313
  Top = 139
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Move Auto-Positioned Designators'
  ClientHeight = 315
  ClientWidth = 346
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
    Left = 8
    Top = 10
    Width = 88
    Height = 16
    AutoSize = False
    Caption = 'version X.X'
    OnClick = LabelVersionClick
  end
  object Bevel1: TBevel
    Left = 104
    Top = 176
    Width = 88
    Height = 80
  end
  object Bevel2: TBevel
    Left = 8
    Top = 176
    Width = 88
    Height = 80
  end
  object Label1: TLabel
    Left = 29
    Top = 63
    Width = 32
    Height = 13
    Caption = 'Units:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 104
    Top = 164
    Width = 88
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'MAX OFFSET'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 164
    Width = 88
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'FIXED OFFSET'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 200
    Top = 36
    Width = 136
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'FIXED PRESETS'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object SelectedCheckBox: TCheckBox
    Left = 19
    Top = 90
    Width = 117
    Height = 17
    Caption = '&Selected Parts Only'
    Checked = True
    State = cbChecked
    TabOrder = 11
  end
  object ButtonOK: TButton
    Left = 16
    Top = 216
    Width = 72
    Height = 32
    Caption = 'OK'
    TabOrder = 14
    OnClick = ButtonOKClick
  end
  object EditDistance: TEdit
    Left = 16
    Top = 184
    Width = 72
    Height = 21
    Hint = 'Negative values move away'
    Alignment = taCenter
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Text = '0.5'
    OnChange = EditDistanceChange
    OnKeyPress = UserKeyPress
  end
  object MMmilButton: TButton
    Left = 70
    Top = 60
    Width = 42
    Height = 20
    Caption = 'mm'
    TabOrder = 10
    OnClick = MMmilButtonClick
  end
  object UnHideDesignatorsCheckBox: TCheckBox
    Left = 19
    Top = 114
    Width = 117
    Height = 17
    Caption = 'UnHide &Designators'
    Checked = True
    State = cbChecked
    TabOrder = 12
  end
  object ButtonCancel: TButton
    Left = 8
    Top = 272
    Width = 184
    Height = 32
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 16
    OnClick = ButtonCancelClick
  end
  object ButtonPreset1: TButton
    Left = 272
    Top = 54
    Width = 64
    Height = 25
    Caption = 'Preset &1'
    TabOrder = 17
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset1: TEdit
    Left = 200
    Top = 56
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 2
    Text = '4'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset2: TButton
    Left = 272
    Top = 86
    Width = 64
    Height = 25
    Caption = 'Preset &2'
    TabOrder = 18
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset2: TEdit
    Left = 200
    Top = 88
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 3
    Text = '5'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset3: TButton
    Left = 272
    Top = 118
    Width = 64
    Height = 25
    Caption = 'Preset &3'
    TabOrder = 19
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset3: TEdit
    Left = 200
    Top = 120
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 4
    Text = '6'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset4: TButton
    Left = 272
    Top = 150
    Width = 64
    Height = 25
    Caption = 'Preset &4'
    TabOrder = 20
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset4: TEdit
    Left = 200
    Top = 152
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 5
    Text = '8'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset5: TButton
    Left = 272
    Top = 182
    Width = 64
    Height = 25
    Caption = 'Preset &5'
    TabOrder = 21
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset5: TEdit
    Left = 200
    Top = 184
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 6
    Text = '10'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset6: TButton
    Left = 272
    Top = 214
    Width = 64
    Height = 25
    Caption = 'Preset &6'
    TabOrder = 22
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset6: TEdit
    Left = 200
    Top = 216
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 7
    Text = '16'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset7: TButton
    Left = 272
    Top = 246
    Width = 64
    Height = 25
    Caption = 'Preset &7'
    TabOrder = 23
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset7: TEdit
    Left = 200
    Top = 248
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 8
    Text = '20'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset8: TButton
    Left = 272
    Top = 278
    Width = 64
    Height = 25
    Caption = 'Preset &8'
    TabOrder = 24
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset8: TEdit
    Left = 200
    Top = 280
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 9
    Text = '30'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonAuto: TButton
    Left = 112
    Top = 216
    Width = 72
    Height = 32
    Caption = 'AUTO'
    TabOrder = 15
    OnClick = ButtonAutoClick
  end
  object EditMaxDistance: TEdit
    Left = 112
    Top = 184
    Width = 72
    Height = 21
    Hint = 'Negative values move away'
    Alignment = taCenter
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Text = '3'
    OnChange = EditMaxDistanceChange
    OnKeyPress = UserKeyPress
  end
  object LazyAutoMoveCheckBox: TCheckBox
    Left = 19
    Top = 138
    Width = 165
    Height = 17
    Caption = 'La&zy Offset (AUTO only)'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 13
  end
  object ButtonInteractiveStart: TButton
    Left = 152
    Top = 8
    Width = 184
    Height = 24
    Caption = 'Interactive Placement Tool'
    TabOrder = 25
    OnClick = ButtonInteractiveStartClick
  end
end
