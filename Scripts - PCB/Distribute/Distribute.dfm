object FormDistribute: TFormDistribute
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Distribute'
  ClientHeight = 277
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormDistributeShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 89
    Width = 192
    Height = 136
  end
  object Bevel2: TBevel
    Left = 8
    Top = 16
    Width = 192
    Height = 72
  end
  object RadioDirections: TRadioGroup
    Left = 112
    Top = 144
    Width = 56
    Height = 72
    Hint = 'Distribution direction. Choose one for more info.'
    Caption = 'Direction'
    DoubleBuffered = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      '&FWD'
      '&CEN'
      '&REV')
    ParentDoubleBuffered = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = RadioDirectionsClick
  end
  object RadioButtonClearance: TRadioButton
    Left = 16
    Top = 32
    Width = 184
    Height = 17
    Caption = 'Distribute by Clearance (Alt+&Q)'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = RadioButtonClearanceClick
  end
  object RadioButtonCenters: TRadioButton
    Left = 16
    Top = 56
    Width = 184
    Height = 17
    Caption = 'Distribute by Centerlines (Alt+&W)'
    TabOrder = 1
    OnClick = RadioButtonCentersClick
  end
  object RadioButtonClearanceVal: TRadioButton
    Left = 16
    Top = 96
    Width = 184
    Height = 17
    Caption = 'Distribute Clearances by: (Alt+&E)'
    TabOrder = 2
    OnClick = RadioButtonClearanceValClick
  end
  object RadioButtonCentersVal: TRadioButton
    Left = 16
    Top = 120
    Width = 184
    Height = 17
    Caption = 'Distribute Centers by: (Alt+&A)'
    TabOrder = 3
    OnClick = RadioButtonCentersValClick
  end
  object EditDistance: TEdit
    Left = 24
    Top = 149
    Width = 40
    Height = 21
    Enabled = False
    TabOrder = 4
    Text = '6'
    OnChange = EditDistanceChange
  end
  object ButtonUnits: TButton
    Left = 72
    Top = 149
    Width = 32
    Height = 20
    Caption = 'mil'
    Enabled = False
    TabOrder = 5
    OnClick = ButtonUnitsClick
  end
  object ButtonOK: TButton
    Left = 16
    Top = 241
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 7
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 104
    Top = 241
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 8
    OnClick = ButtonCancelClick
  end
  object ButtonPreset1: TButton
    Left = 280
    Top = 17
    Width = 64
    Height = 25
    Caption = 'Preset &1'
    TabOrder = 9
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset1: TEdit
    Left = 208
    Top = 19
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 10
    Text = '4'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset2: TButton
    Left = 280
    Top = 49
    Width = 64
    Height = 25
    Caption = 'Preset &2'
    TabOrder = 11
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset2: TEdit
    Left = 208
    Top = 51
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 12
    Text = '5'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset3: TButton
    Left = 280
    Top = 81
    Width = 64
    Height = 25
    Caption = 'Preset &3'
    TabOrder = 13
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset3: TEdit
    Left = 208
    Top = 83
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 14
    Text = '6'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset4: TButton
    Left = 280
    Top = 113
    Width = 64
    Height = 25
    Caption = 'Preset &4'
    TabOrder = 15
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset4: TEdit
    Left = 208
    Top = 115
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 16
    Text = '8'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset5: TButton
    Left = 280
    Top = 145
    Width = 64
    Height = 25
    Caption = 'Preset &5'
    TabOrder = 17
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset5: TEdit
    Left = 208
    Top = 147
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 18
    Text = '10'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset6: TButton
    Left = 280
    Top = 177
    Width = 64
    Height = 25
    Caption = 'Preset &6'
    TabOrder = 19
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset6: TEdit
    Left = 208
    Top = 179
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 20
    Text = '16'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset7: TButton
    Left = 280
    Top = 209
    Width = 64
    Height = 25
    Caption = 'Preset &7'
    TabOrder = 21
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset7: TEdit
    Left = 208
    Top = 211
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 22
    Text = '20'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset8: TButton
    Left = 280
    Top = 241
    Width = 64
    Height = 25
    Caption = 'Preset &8'
    TabOrder = 23
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset8: TEdit
    Left = 208
    Top = 243
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 24
    Text = '30'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
end
