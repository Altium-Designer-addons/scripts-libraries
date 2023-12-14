object FormDistribute: TFormDistribute
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Distribute'
  ClientHeight = 298
  ClientWidth = 400
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
    Top = 88
    Width = 240
    Height = 144
  end
  object Bevel2: TBevel
    Left = 8
    Top = 8
    Width = 240
    Height = 80
  end
  object LabelVersion: TLabel
    Left = 13
    Top = 215
    Width = 54
    Height = 13
    Caption = 'version X.X'
  end
  object RadioDirections: TRadioGroup
    Left = 120
    Top = 145
    Width = 96
    Height = 80
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
      '&CEN/PAD/VIA'
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
    Top = 16
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
    Top = 64
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
    Left = 32
    Top = 150
    Width = 40
    Height = 21
    Enabled = False
    TabOrder = 4
    Text = '6'
    OnChange = EditDistanceChange
  end
  object ButtonUnits: TButton
    Left = 80
    Top = 150
    Width = 32
    Height = 20
    Caption = 'mil'
    Enabled = False
    TabOrder = 5
    OnClick = ButtonUnitsClick
  end
  object ButtonOK: TButton
    Left = 16
    Top = 265
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 7
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 104
    Top = 265
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 8
    OnClick = ButtonCancelClick
  end
  object ButtonPreset1: TButton
    Left = 328
    Top = 9
    Width = 64
    Height = 25
    Caption = 'Preset &1'
    TabOrder = 9
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset1: TEdit
    Left = 256
    Top = 11
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
    Left = 328
    Top = 41
    Width = 64
    Height = 25
    Caption = 'Preset &2'
    TabOrder = 11
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset2: TEdit
    Left = 256
    Top = 43
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
    Left = 328
    Top = 73
    Width = 64
    Height = 25
    Caption = 'Preset &3'
    TabOrder = 13
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset3: TEdit
    Left = 256
    Top = 75
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
    Left = 328
    Top = 105
    Width = 64
    Height = 25
    Caption = 'Preset &4'
    TabOrder = 15
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset4: TEdit
    Left = 256
    Top = 107
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
    Left = 328
    Top = 137
    Width = 64
    Height = 25
    Caption = 'Preset &5'
    TabOrder = 17
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset5: TEdit
    Left = 256
    Top = 139
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
    Left = 328
    Top = 169
    Width = 64
    Height = 25
    Caption = 'Preset &6'
    TabOrder = 19
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset6: TEdit
    Left = 256
    Top = 171
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
    Left = 328
    Top = 201
    Width = 64
    Height = 25
    Caption = 'Preset &7'
    TabOrder = 21
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset7: TEdit
    Left = 256
    Top = 203
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
    Left = 328
    Top = 233
    Width = 64
    Height = 25
    Caption = 'Preset &8'
    TabOrder = 23
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset8: TEdit
    Left = 256
    Top = 235
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
  object CheckBoxTrimEnds: TCheckBox
    Left = 16
    Top = 240
    Width = 184
    Height = 17
    Caption = '&Trim Unconnected Track Ends'
    TabOrder = 25
    OnClick = CheckBoxTrimEndsClick
  end
  object CheckBoxPadViaClearance: TCheckBox
    Left = 31
    Top = 38
    Width = 145
    Height = 17
    TabStop = False
    Caption = 'Pad/&Via Clearance (Alt+V)'
    Enabled = False
    TabOrder = 26
    Visible = False
    OnClick = CheckBoxPadViaClearanceClick
  end
  object EditPadViaClearance: TEdit
    Left = 176
    Top = 36
    Width = 40
    Height = 21
    TabStop = False
    Enabled = False
    TabOrder = 27
    Text = '6'
    Visible = False
    OnChange = EditPadViaClearanceChange
  end
  object ButtonPadViaUnits: TButton
    Left = 218
    Top = 36
    Width = 27
    Height = 20
    Caption = 'mil'
    Enabled = False
    TabOrder = 28
    TabStop = False
    Visible = False
    OnClick = ButtonPadViaUnitsClick
  end
end
