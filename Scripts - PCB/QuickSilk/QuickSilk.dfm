object QuickSilkForm: TQuickSilkForm
  Left = 313
  Top = 139
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'QuickSilk Silkscreen Helper Script'
  ClientHeight = 547
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = QuickSilkFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelVersion: TLabel
    Left = 8
    Top = 4
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
  object Bevel1: TBevel
    Left = 104
    Top = 408
    Width = 88
    Height = 80
  end
  object Bevel2: TBevel
    Left = 8
    Top = 408
    Width = 88
    Height = 80
  end
  object Label1: TLabel
    Left = 5
    Top = 45
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
    Top = 396
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
    Top = 396
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
    Top = 268
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
  object Label5: TLabel
    Left = 8
    Top = 244
    Width = 328
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'Move Auto-Positioned Designators'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel3: TBevel
    Left = 1
    Top = 264
    Width = 343
    Height = 280
  end
  object Bevel4: TBevel
    Left = 1
    Top = 64
    Width = 343
    Height = 168
  end
  object Label6: TLabel
    Left = 96
    Top = 44
    Width = 152
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'Configuration'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label7: TLabel
    Left = 272
    Top = 68
    Width = 64
    Height = 13
    Hint = 
      'For performance reasons, clearance may be up to 1 mil larger tha' +
      'n these values'
    Alignment = taCenter
    AutoSize = False
    Caption = 'Clearance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object LabelClearanceText: TLabel
    Left = 152
    Top = 92
    Width = 112
    Height = 14
    Hint = 'Clearance to other text objects'
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Other Silkscreen Text'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object LabelClearanceBody: TLabel
    Left = 160
    Top = 116
    Width = 104
    Height = 14
    Hint = 
      'Clearance to component bodies (bounding rectangle, not detailed ' +
      'contours)'
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Component Bodies'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object LabelClearancePad: TLabel
    Left = 152
    Top = 140
    Width = 112
    Height = 14
    Hint = 'Clearance to pads'
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Pads'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object LabelClearanceCutout: TLabel
    Left = 152
    Top = 164
    Width = 112
    Height = 14
    Hint = 'Clearance to cutout regions'
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Cutout Regions'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object LabelClearanceDefault: TLabel
    Left = 152
    Top = 188
    Width = 112
    Height = 14
    Hint = 'Clearance to other objects (arcs, tracks, fills, regions)'
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Other Objects'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label8: TLabel
    Left = 16
    Top = 68
    Width = 64
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object SelectedCheckBox: TCheckBox
    Left = 19
    Top = 322
    Width = 117
    Height = 17
    Caption = '&Selected Parts Only'
    Checked = True
    State = cbChecked
    TabOrder = 11
  end
  object ButtonOK: TButton
    Left = 16
    Top = 448
    Width = 72
    Height = 32
    Hint = 'Save settings and apply fixed offset'
    Caption = 'OK'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 14
    OnClick = ButtonOKClick
  end
  object EditDistance: TEdit
    Left = 16
    Top = 416
    Width = 72
    Height = 21
    Hint = 'Negative values move away'
    Alignment = taCenter
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Text = '20'
    TextHint = 'fixed offset'
    OnChange = EditDistanceChange
    OnKeyPress = UserKeyPress
  end
  object MMmilButton: TButton
    Left = 46
    Top = 40
    Width = 42
    Height = 24
    Hint = 'Click to change between mil and mm units'
    Caption = 'mil'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    OnClick = MMmilButtonClick
  end
  object UnHideDesignatorsCheckBox: TCheckBox
    Left = 19
    Top = 346
    Width = 117
    Height = 17
    Caption = 'UnHide &Designators'
    Checked = True
    State = cbChecked
    TabOrder = 12
  end
  object ButtonCancel: TButton
    Left = 8
    Top = 504
    Width = 184
    Height = 32
    Hint = 'Click to close without saving settings'
    Cancel = True
    Caption = 'Cancel'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 16
    OnClick = ButtonCancelClick
  end
  object ButtonPreset1: TButton
    Left = 272
    Top = 286
    Width = 64
    Height = 25
    Caption = 'Preset &1'
    TabOrder = 17
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset1: TEdit
    Left = 200
    Top = 288
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Text = '4'
    TextHint = 'Enter value for fixed offset'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset2: TButton
    Left = 272
    Top = 318
    Width = 64
    Height = 25
    Caption = 'Preset &2'
    TabOrder = 18
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset2: TEdit
    Left = 200
    Top = 320
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Text = '5'
    TextHint = 'Enter value for fixed offset'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset3: TButton
    Left = 272
    Top = 350
    Width = 64
    Height = 25
    Caption = 'Preset &3'
    TabOrder = 19
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset3: TEdit
    Left = 200
    Top = 352
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    Text = '6'
    TextHint = 'Enter value for fixed offset'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset4: TButton
    Left = 272
    Top = 382
    Width = 64
    Height = 25
    Caption = 'Preset &4'
    TabOrder = 20
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset4: TEdit
    Left = 200
    Top = 384
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    Text = '8'
    TextHint = 'Enter value for fixed offset'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset5: TButton
    Left = 272
    Top = 414
    Width = 64
    Height = 25
    Caption = 'Preset &5'
    TabOrder = 21
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset5: TEdit
    Left = 200
    Top = 416
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    Text = '10'
    TextHint = 'Enter value for fixed offset'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset6: TButton
    Left = 272
    Top = 446
    Width = 64
    Height = 25
    Caption = 'Preset &6'
    TabOrder = 22
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset6: TEdit
    Left = 200
    Top = 448
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    Text = '16'
    TextHint = 'Enter value for fixed offset'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset7: TButton
    Left = 272
    Top = 478
    Width = 64
    Height = 25
    Caption = 'Preset &7'
    TabOrder = 23
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset7: TEdit
    Left = 200
    Top = 480
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    Text = '20'
    TextHint = 'Enter value for fixed offset'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset8: TButton
    Left = 272
    Top = 510
    Width = 64
    Height = 25
    Caption = 'Preset &8'
    TabOrder = 24
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset8: TEdit
    Left = 200
    Top = 512
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    Text = '30'
    TextHint = 'Enter value for fixed offset'
    OnChange = ValidateOnChange
    OnKeyPress = UserKeyPress
  end
  object ButtonAuto: TButton
    Left = 112
    Top = 448
    Width = 72
    Height = 32
    Hint = 'Save settings and apply automatic offset'
    Caption = 'AUTO'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 15
    OnClick = ButtonAutoClick
  end
  object EditMaxDistance: TEdit
    Left = 112
    Top = 416
    Width = 72
    Height = 21
    Hint = 'Attempt move WITHIN max offset'
    Alignment = taCenter
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Text = '120'
    TextHint = 'max offset'
    OnChange = EditMaxDistanceChange
    OnKeyPress = UserKeyPress
  end
  object LazyAutoMoveCheckBox: TCheckBox
    Left = 19
    Top = 370
    Width = 165
    Height = 17
    Caption = 'La&zy Offset (AUTO only)'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 13
  end
  object ButtonInteractiveStart: TButton
    Left = 152
    Top = 0
    Width = 184
    Height = 24
    Hint = 
      'Will prevent SpaceNavigator from panning if launched from this b' +
      'utton'
    Caption = 'Interactive Placement Tool'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 25
    OnClick = ButtonInteractiveStartClick
  end
  object CheckBoxPersistent: TCheckBox
    Left = 8
    Top = 88
    Width = 97
    Height = 17
    Hint = 
      'ENABLED: right-click after silkscreen placement to exit location' +
      ' picking; DISABLED: automatically ready for next component after' +
      ' placement of silkscreen.'
    Caption = 'Persistent Mode'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 26
  end
  object CheckBoxAnyAngle: TCheckBox
    Left = 8
    Top = 112
    Width = 120
    Height = 17
    Hint = 'ENABLED: angles that aren'#39't 0/90 degrees are supported'
    Caption = 'Any-Angle Placement'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 27
  end
  object RadioGroupParentOnly: TRadioGroup
    Left = 8
    Top = 136
    Width = 120
    Height = 88
    Hint = 
      'Default behavior is to avoid only objects in parent component; P' +
      'ress CTRL to temporarily avoid all objects'
    Caption = 'Clearance Mode'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      'Use CTRL Key'
      'Parent Only'
      'All Objects')
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 28
  end
  object tClearanceText: TEdit
    Left = 272
    Top = 88
    Width = 64
    Height = 24
    Align = alCustom
    Alignment = taCenter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    OEMConvert = True
    ParentFont = False
    TabOrder = 29
    Text = '5'
    TextHint = 'Text object clearance'
    OnChange = EditClearanceChange
  end
  object tClearanceBody: TEdit
    Left = 272
    Top = 112
    Width = 64
    Height = 24
    Align = alCustom
    Alignment = taCenter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    OEMConvert = True
    ParentFont = False
    TabOrder = 30
    Text = '8'
    TextHint = 'Component Body Clearance'
    OnChange = EditClearanceChange
  end
  object tClearancePad: TEdit
    Left = 272
    Top = 136
    Width = 64
    Height = 24
    Align = alCustom
    Alignment = taCenter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    OEMConvert = True
    ParentFont = False
    TabOrder = 31
    Text = '8'
    TextHint = 'Pad object clearance'
    OnChange = EditClearanceChange
  end
  object tClearanceCutout: TEdit
    Left = 272
    Top = 160
    Width = 64
    Height = 24
    Align = alCustom
    Alignment = taCenter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    OEMConvert = True
    ParentFont = False
    TabOrder = 32
    Text = '0'
    TextHint = 'Silkscreen cutout region clearance'
    OnChange = EditClearanceChange
  end
  object tClearanceDefault: TEdit
    Left = 272
    Top = 184
    Width = 64
    Height = 24
    Align = alCustom
    Alignment = taCenter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    OEMConvert = True
    ParentFont = False
    TabOrder = 33
    Text = '6'
    TextHint = 'Default clearance'
    OnChange = EditClearanceChange
  end
  object ButtonSaveConfig: TButton
    Left = 272
    Top = 40
    Width = 64
    Height = 24
    Hint = 'Click to save all current values'
    Caption = 'SAVE'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 34
    OnClick = ButtonSaveConfigClick
  end
end
