object QuickSilkForm: TQuickSilkForm
  Left = 313
  Top = 139
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'QuickSilk Silkscreen Helper Script'
  ClientHeight = 571
  ClientWidth = 362
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
    Left = 16
    Top = 12
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
    Left = 112
    Top = 424
    Width = 88
    Height = 80
  end
  object Bevel2: TBevel
    Left = 16
    Top = 424
    Width = 88
    Height = 80
  end
  object LabelUnits: TLabel
    Left = 13
    Top = 53
    Width = 32
    Height = 13
    Caption = 'Units:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = MMmilButtonClick
  end
  object Label2: TLabel
    Left = 112
    Top = 412
    Width = 88
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'MAX MOVE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 16
    Top = 412
    Width = 88
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'FIXED MOVE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 208
    Top = 284
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
  object LabelMain: TLabel
    Left = 16
    Top = 256
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
    Left = 8
    Top = 280
    Width = 344
    Height = 280
  end
  object Bevel4: TBevel
    Left = 8
    Top = 72
    Width = 344
    Height = 176
  end
  object Label6: TLabel
    Left = 104
    Top = 48
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
    Left = 280
    Top = 76
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
    Left = 169
    Top = 100
    Width = 103
    Height = 13
    Hint = 'Clearance to other text objects. Click to select violations.'
    Alignment = taRightJustify
    Caption = 'Other Silkscreen Text'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = RuleCheckClick
    OnMouseEnter = LabelClearanceMouseEnter
    OnMouseLeave = LabelClearanceMouseLeave
  end
  object LabelClearanceBody: TLabel
    Left = 183
    Top = 124
    Width = 89
    Height = 13
    Hint = 
      'Clearance to component bodies (bounding rectangle, not detailed ' +
      'contours). Click to select violations.'
    Alignment = taRightJustify
    Caption = 'Component Bodies'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = RuleCheckClick
    OnMouseEnter = LabelClearanceMouseEnter
    OnMouseLeave = LabelClearanceMouseLeave
  end
  object LabelClearancePad: TLabel
    Left = 249
    Top = 148
    Width = 23
    Height = 13
    Hint = 'Clearance to pads. Click to select violations.'
    Alignment = taRightJustify
    Caption = 'Pads'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = RuleCheckClick
    OnMouseEnter = LabelClearanceMouseEnter
    OnMouseLeave = LabelClearanceMouseLeave
  end
  object LabelClearanceCutout: TLabel
    Left = 198
    Top = 172
    Width = 74
    Height = 13
    Hint = 'Clearance to cutout regions. Click to select violations.'
    Alignment = taRightJustify
    Caption = 'Cutout Regions'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = RuleCheckClick
    OnMouseEnter = LabelClearanceMouseEnter
    OnMouseLeave = LabelClearanceMouseLeave
  end
  object LabelClearanceDefault: TLabel
    Left = 204
    Top = 220
    Width = 68
    Height = 13
    Hint = 
      'Clearance to other objects (arcs, tracks, fills, regions). Click' +
      ' to select violations.'
    Alignment = taRightJustify
    Caption = 'Other Objects'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = RuleCheckClick
    OnMouseEnter = LabelClearanceMouseEnter
    OnMouseLeave = LabelClearanceMouseLeave
  end
  object Label8: TLabel
    Left = 24
    Top = 76
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
  object Label9: TLabel
    Left = 15
    Top = 285
    Width = 169
    Height = 13
    Caption = 'NOTE: does not support Any-Angle'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object LabelClearanceAll: TLabel
    Left = 165
    Top = 76
    Width = 107
    Height = 13
    Hint = 'Click to select all violating text'
    Alignment = taRightJustify
    Caption = ' Select All Violating'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = RuleCheckClick
    OnMouseEnter = LabelClearanceMouseEnter
    OnMouseLeave = LabelClearanceMouseLeave
  end
  object LabelClearanceVia: TLabel
    Left = 253
    Top = 196
    Width = 19
    Height = 13
    Hint = 'Clearance to vias. Click to select violations.'
    Alignment = taRightJustify
    Caption = 'Vias'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = RuleCheckClick
    OnMouseEnter = LabelClearanceMouseEnter
    OnMouseLeave = LabelClearanceMouseLeave
  end
  object ButtonOK: TButton
    Left = 24
    Top = 464
    Width = 72
    Height = 32
    Hint = 'Save settings and apply fixed offset'
    Caption = 'OK'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = ButtonOKClick
  end
  object EditDistance: TEdit
    Left = 24
    Top = 432
    Width = 72
    Height = 21
    Hint = 'Negative values move away'
    Alignment = taCenter
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Text = '20'
    TextHint = 'fixed offset'
    OnChange = InputValueChange
    OnKeyPress = UserKeyPress
  end
  object MMmilButton: TButton
    Left = 54
    Top = 48
    Width = 42
    Height = 24
    Hint = 'Click to change between mil and mm units'
    Caption = 'mil'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 13
    OnClick = MMmilButtonClick
  end
  object UnHideDesignatorsCheckBox: TCheckBox
    Left = 24
    Top = 302
    Width = 117
    Height = 17
    Caption = 'Un&Hide Designators'
    TabOrder = 14
  end
  object ButtonCancel: TButton
    Left = 16
    Top = 520
    Width = 184
    Height = 32
    Hint = 'Click to close without saving settings'
    Cancel = True
    Caption = 'Cancel'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = ButtonCancelClick
  end
  object ButtonPreset1: TButton
    Left = 280
    Top = 302
    Width = 64
    Height = 25
    Caption = 'Preset &1'
    TabOrder = 32
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset1: TEdit
    Left = 208
    Top = 304
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    Text = '4'
    TextHint = 'Enter value for fixed offset'
    OnChange = PresetValueChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset2: TButton
    Left = 280
    Top = 334
    Width = 64
    Height = 25
    Caption = 'Preset &2'
    TabOrder = 33
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset2: TEdit
    Left = 208
    Top = 336
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    Text = '5'
    TextHint = 'Enter value for fixed offset'
    OnChange = PresetValueChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset3: TButton
    Left = 280
    Top = 366
    Width = 64
    Height = 25
    Caption = 'Preset &3'
    TabOrder = 34
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset3: TEdit
    Left = 208
    Top = 368
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    Text = '6'
    TextHint = 'Enter value for fixed offset'
    OnChange = PresetValueChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset4: TButton
    Left = 280
    Top = 398
    Width = 64
    Height = 25
    Caption = 'Preset &4'
    TabOrder = 35
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset4: TEdit
    Left = 208
    Top = 400
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    Text = '8'
    TextHint = 'Enter value for fixed offset'
    OnChange = PresetValueChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset5: TButton
    Left = 280
    Top = 430
    Width = 64
    Height = 25
    Caption = 'Preset &5'
    TabOrder = 36
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset5: TEdit
    Left = 208
    Top = 432
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    Text = '10'
    TextHint = 'Enter value for fixed offset'
    OnChange = PresetValueChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset6: TButton
    Left = 280
    Top = 462
    Width = 64
    Height = 25
    Caption = 'Preset &6'
    TabOrder = 37
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset6: TEdit
    Left = 208
    Top = 464
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    Text = '16'
    TextHint = 'Enter value for fixed offset'
    OnChange = PresetValueChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset7: TButton
    Left = 280
    Top = 494
    Width = 64
    Height = 25
    Caption = 'Preset &7'
    TabOrder = 38
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset7: TEdit
    Left = 208
    Top = 496
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 11
    Text = '20'
    TextHint = 'Enter value for fixed offset'
    OnChange = PresetValueChange
    OnKeyPress = UserKeyPress
  end
  object ButtonPreset8: TButton
    Left = 280
    Top = 526
    Width = 64
    Height = 25
    Caption = 'Preset &8'
    TabOrder = 39
    TabStop = False
    OnClick = PresetButtonClicked
  end
  object tPreset8: TEdit
    Left = 208
    Top = 528
    Width = 64
    Height = 21
    Hint = 'Presets only apply to fixed offset'
    Align = alCustom
    Alignment = taCenter
    OEMConvert = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 12
    Text = '30'
    TextHint = 'Enter value for fixed offset'
    OnChange = PresetValueChange
    OnKeyPress = UserKeyPress
  end
  object ButtonAuto: TButton
    Left = 120
    Top = 464
    Width = 72
    Height = 32
    Hint = 'Save settings and apply automatic offset'
    Caption = 'AUTO'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = ButtonAutoClick
  end
  object EditMaxDistance: TEdit
    Left = 120
    Top = 432
    Width = 72
    Height = 21
    Hint = 'Attempt move WITHIN max offset'
    Alignment = taCenter
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Text = '9999'
    TextHint = 'max offset'
    Visible = False
    OnChange = InputValueChange
    OnKeyPress = UserKeyPress
  end
  object LazyAutoMoveCheckBox: TCheckBox
    Left = 24
    Top = 318
    Width = 165
    Height = 17
    Caption = 'La&zy Move (AUTO only)'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 15
  end
  object ButtonInteractiveStart: TButton
    Left = 160
    Top = 8
    Width = 184
    Height = 24
    Hint = 
      'Will prevent SpaceNavigator from panning if launched from this b' +
      'utton'
    Caption = 'Interactive Placement &Tool'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 31
    OnClick = ButtonInteractiveStartClick
  end
  object CheckBoxPersistent: TCheckBox
    Left = 24
    Top = 96
    Width = 97
    Height = 17
    Hint = 
      'ENABLED: right-click after silkscreen placement to exit location' +
      ' picking; DISABLED: automatically ready for next component after' +
      ' placement of silkscreen.'
    Caption = '&Persistent Mode'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 25
    OnClick = ConfigClick
  end
  object CheckBoxAnyAngle: TCheckBox
    Left = 24
    Top = 112
    Width = 120
    Height = 17
    Hint = 'ENABLED: angles that aren'#39't 0/90 degrees are supported'
    Caption = '&Any-Angle Placement'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 26
    OnClick = ConfigClick
  end
  object RadioGroupParentOnly: TRadioGroup
    Left = 16
    Top = 152
    Width = 104
    Height = 80
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
      'Use &CTRL Key'
      'Parent &Only'
      'All O&bjects')
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 28
    OnClick = ConfigClick
  end
  object tClearanceText: TEdit
    Left = 280
    Top = 96
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
    TabOrder = 18
    Text = '5'
    TextHint = 'Text object clearance'
    OnChange = InputValueChange
    OnKeyPress = ClearanceEnterKey
  end
  object tClearanceBody: TEdit
    Left = 280
    Top = 120
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
    TabOrder = 19
    Text = '8'
    TextHint = 'Component Body Clearance'
    OnChange = InputValueChange
    OnKeyPress = ClearanceEnterKey
  end
  object tClearancePad: TEdit
    Left = 280
    Top = 144
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
    TabOrder = 20
    Text = '8'
    TextHint = 'Pad object clearance'
    OnChange = InputValueChange
    OnKeyPress = ClearanceEnterKey
  end
  object tClearanceCutout: TEdit
    Left = 280
    Top = 168
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
    TabOrder = 21
    Text = '0'
    TextHint = 'Silkscreen cutout region clearance'
    OnChange = InputValueChange
    OnKeyPress = ClearanceEnterKey
  end
  object tClearanceDefault: TEdit
    Left = 280
    Top = 216
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
    TabOrder = 23
    Text = '6'
    TextHint = 'Default clearance'
    OnChange = InputValueChange
    OnKeyPress = ClearanceEnterKey
  end
  object ButtonSaveConfig: TButton
    Left = 280
    Top = 48
    Width = 64
    Height = 24
    Hint = 'Click to save all current values'
    Caption = '&SAVE'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 30
    OnClick = ButtonSaveConfigClick
  end
  object CheckBoxExtraOffsets: TCheckBox
    Left = 24
    Top = 128
    Width = 120
    Height = 17
    Hint = 
      'ENABLED: (time consuming) when avoiding all objects, will try ap' +
      'proaching parent from every offset'
    Caption = 'Try &Extra Offsets'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 27
    OnClick = ConfigClick
  end
  object CheckBoxAutoParentOnly: TCheckBox
    Left = 24
    Top = 334
    Width = 165
    Height = 17
    Hint = 'ENABLED: AutoMove only avoids parent component'#39's objects'
    Caption = 'Pa&rent Only (AUTO only)'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 16
  end
  object CheckBoxLocalSettings: TCheckBox
    Left = 16
    Top = 26
    Width = 128
    Height = 17
    Hint = 'ENABLED: Save configuration settings in script folder'
    Caption = 'Local Setti&ngs'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 29
    OnClick = CheckBoxLocalSettingsClick
  end
  object RadioGroupSelectionScope: TRadioGroup
    Left = 16
    Top = 353
    Width = 184
    Height = 56
    Caption = 'Selection Scope'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      'A&LL Components'
      'Selecte&d Components')
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 17
    OnClick = ConfigClick
  end
  object tClearanceVia: TEdit
    Left = 280
    Top = 192
    Width = 64
    Height = 24
    Hint = 
      'Negative values allowed, automatically coerced to -half diameter' +
      ' at most.'
    Align = alCustom
    Alignment = taCenter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    OEMConvert = True
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 22
    Text = '3'
    OnChange = InputValueChange
    OnKeyPress = ClearanceEnterKey
  end
  object ComboBoxViaRule: TComboBox
    Left = 128
    Top = 192
    Width = 120
    Height = 24
    AutoComplete = False
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial Narrow'
    Font.Style = []
    ItemIndex = 1
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 24
    Text = 'Pad (tented OK)'
    OnSelect = ConfigClick
    Items.Strings = (
      'Waived'
      'Pad (tented OK)'
      'From Pad'
      'Hole (tented OK)'
      'From Hole')
  end
end
