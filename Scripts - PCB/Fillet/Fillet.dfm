object FormFillet: TFormFillet
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Fillet'
  ClientHeight = 421
  ClientWidth = 194
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormFilletClose
  OnShow = FormFilletShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 30
    Top = 21
    Width = 65
    Height = 13
    Caption = 'Radius Value:'
  end
  object Label2: TLabel
    Left = 30
    Top = 69
    Width = 49
    Height = 13
    Caption = 'Arc Mode:'
  end
  object LabelVersion: TLabel
    Left = 3
    Top = 2
    Width = 96
    Height = 16
    AutoSize = False
    Caption = 'version X.X'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = LabelVersionClick
  end
  object ButtonOK: TButton
    Left = 24
    Top = 109
    Width = 64
    Height = 25
    Caption = 'O&K'
    TabOrder = 11
    TabStop = False
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 104
    Top = 109
    Width = 64
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 10
    TabStop = False
    OnClick = ButtonCancelClick
  end
  object tRadius: TEdit
    Left = 101
    Top = 17
    Width = 67
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 0
    Text = '32.25'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object RadioPanel: TPanel
    Left = 33
    Top = 43
    Width = 128
    Height = 24
    BevelOuter = bvNone
    Ctl3D = True
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 9
    object RadioUnitsMils: TRadioButton
      Left = 0
      Top = 0
      Width = 40
      Height = 16
      Caption = 'mils'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object RadioUnitsMM: TRadioButton
      Left = 40
      Top = 0
      Width = 40
      Height = 16
      Caption = 'mm'
      TabOrder = 0
    end
    object RadioUnitsRatio: TRadioButton
      Left = 80
      Top = 0
      Width = 40
      Height = 16
      Hint = 
        'Ratio of max radius given connected tracks, or of existing arcs ' +
        'if relative mode'
      Caption = '%'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = RadioUnitsRatioClick
    end
  end
  object Button1: TButton
    Left = 104
    Top = 163
    Width = 64
    Height = 25
    Caption = 'Preset &1'
    TabOrder = 12
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset1: TEdit
    Left = 24
    Top = 165
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 1
    Text = '25.2'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object Button2: TButton
    Left = 104
    Top = 195
    Width = 64
    Height = 25
    Caption = 'Preset &2'
    TabOrder = 13
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset2: TEdit
    Left = 24
    Top = 197
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 2
    Text = '39.1'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object Button3: TButton
    Left = 104
    Top = 227
    Width = 64
    Height = 25
    Caption = 'Preset &3'
    TabOrder = 14
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset3: TEdit
    Left = 24
    Top = 229
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 3
    Text = '32.4'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object Button4: TButton
    Left = 104
    Top = 259
    Width = 64
    Height = 25
    Caption = 'Preset &4'
    TabOrder = 15
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset4: TEdit
    Left = 24
    Top = 261
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 4
    Text = '46.3'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object Button5: TButton
    Left = 104
    Top = 291
    Width = 64
    Height = 25
    Caption = 'Preset &5'
    TabOrder = 16
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset5: TEdit
    Left = 24
    Top = 293
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 5
    Text = '41.05'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object Button6: TButton
    Left = 104
    Top = 323
    Width = 64
    Height = 25
    Caption = 'Preset &6'
    TabOrder = 17
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset6: TEdit
    Left = 24
    Top = 325
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
  object Button7: TButton
    Left = 104
    Top = 355
    Width = 64
    Height = 25
    Caption = 'Preset &7'
    TabOrder = 18
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset7: TEdit
    Left = 24
    Top = 357
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 7
    Text = '20'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object Button8: TButton
    Left = 104
    Top = 387
    Width = 64
    Height = 25
    Caption = 'Preset &8'
    TabOrder = 19
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset8: TEdit
    Left = 24
    Top = 389
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    TabOrder = 8
    Text = '30'
    TextHint = 'Enter value for fixed radius'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ArcsPanel: TPanel
    Left = 33
    Top = 85
    Width = 128
    Height = 24
    BevelOuter = bvNone
    Ctl3D = True
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 20
    object RadioArcsAbsolute: TRadioButton
      Left = 0
      Top = 0
      Width = 65
      Height = 16
      Hint = 
        'New radii will be drawn between tracks connected directly or tho' +
        'ugh an existing fillet'
      Caption = 'Absolute'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioUnitsRatioClick
    end
    object RadioArcsRelative: TRadioButton
      Left = 65
      Top = 0
      Width = 65
      Height = 16
      Hint = 
        'Modifies existing arc radius by desired amount. % mode is % of c' +
        'urrent radius'
      Caption = 'Relative'
      TabOrder = 1
      TabStop = True
      OnClick = RadioUnitsRatioClick
    end
  end
  object CheckBoxRounding: TCheckBox
    Left = 24
    Top = 139
    Width = 80
    Height = 17
    Hint = 'Round final radius to nearest 1 mil or 0.025 mm'
    BiDiMode = bdLeftToRight
    Caption = 'Round Final'
    ParentBiDiMode = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 21
  end
  object CheckBoxGlossCoincident: TCheckBox
    Left = 104
    Top = 139
    Width = 72
    Height = 17
    Hint = 
      'When arcs are aligned along their half-angles, attempts to make ' +
      'them coincident. Reverses radius order for ratio >= 45%'
    BiDiMode = bdLeftToRight
    Caption = 'Gloss Arcs'
    ParentBiDiMode = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 22
  end
end
