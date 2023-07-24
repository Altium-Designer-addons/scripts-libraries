object AssemblyTextPrepForm: TAssemblyTextPrepForm
  Left = 313
  Top = 139
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'AssemblyTextPrep Helper Script'
  ClientHeight = 555
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = AssemblyTextPrepFormCreate
  OnMouseEnter = AssemblyTextPrepFormMouseEnter
  OnShow = AssemblyTextPrepFormShow
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
    Left = 0
    Top = 392
    Width = 344
    Height = 160
  end
  object LabelLimits: TLabel
    Left = 144
    Top = 4
    Width = 192
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Designator Size Limits'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object LabelUnits: TLabel
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
    OnClick = MMmilButtonClick
  end
  object Label1: TLabel
    Left = 144
    Top = 24
    Width = 64
    Height = 17
    Hint = 'Absolute minimum designator height'
    Alignment = taCenter
    AutoSize = False
    Caption = 'Min.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label2: TLabel
    Left = 208
    Top = 24
    Width = 64
    Height = 17
    Hint =
      'Nominal designator height. Will start tapering off to AbsMax val' +
      'ue after this.'
    Alignment = taCenter
    AutoSize = False
    Caption = 'Nom.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label3: TLabel
    Left = 272
    Top = 24
    Width = 64
    Height = 17
    Hint = 'Absolute maximum designator height'
    Alignment = taCenter
    AutoSize = False
    Caption = 'AbsMax'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label4: TLabel
    Left = 72
    Top = 376
    Width = 96
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Quick Shortcuts'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Bevel2: TBevel
    Left = 0
    Top = 128
    Width = 344
    Height = 224
  end
  object Label5: TLabel
    Left = 64
    Top = 112
    Width = 192
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Mass Processing'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label6: TLabel
    Left = 176
    Top = 376
    Width = 134
    Height = 13
    Hint = 'Underlined Quick Shortcuts operate on selections only'
    Caption = '(Underlined = Applies to All)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object LabelAssyLayerPair: TLabel
    Left = 8
    Top = 88
    Width = 94
    Height = 16
    Caption = 'layer1 <----> layer2'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial Narrow'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Transparent = False
  end
  object Label7: TLabel
    Left = 10
    Top = 72
    Width = 75
    Height = 14
    Hint =
      'Layers found to have existing .Designator special strings on the' +
      'm'
    Caption = 'Assy Layers:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Bevel3: TBevel
    Left = 112
    Top = 488
    Width = 24
    Height = 0
  end
  object Label8: TLabel
    Left = 212
    Top = 212
    Width = 64
    Height = 17
    Hint =
      'Aspect ratio threshold above which Best Fit mode will rotate tex' +
      't orthogonal to part.'
    Alignment = taRightJustify
    AutoSize = True
    Caption = 'AR Threshold'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object ButtonProcess: TButton
    Left = 120
    Top = 312
    Width = 104
    Height = 32
    Caption = 'Process'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = ButtonProcessClick
  end
  object rgSelectionScope: TRadioGroup
    Left = 176
    Top = 248
    Width = 160
    Height = 56
    Caption = 'Selection Scope'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 1
    Items.Strings = (
      'A&LL Components'
      'Selecte&d Only')
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object CheckBoxResize: TCheckBox
    Left = 16
    Top = 258
    Width = 136
    Height = 17
    Hint = 'ENABLED: resize text to fit within pad boundaries'
    TabStop = False
    Caption = 'Resize Text'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 9
    OnClick = ConfigClick
  end
  object CheckBoxNormalize: TCheckBox
    Left = 16
    Top = 282
    Width = 136
    Height = 17
    Hint =
      'ENABLED: normalize modified text after placement to be right-rea' +
      'ding'
    TabStop = False
    Caption = 'Normalize Text'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 10
    OnClick = ConfigClick
  end
  object rgOrientation: TRadioGroup
    Left = 176
    Top = 132
    Width = 160
    Height = 76
    Hint = 'Designator orientation strategy. See AR Threshold for Best Fit.'
    Caption = 'Designator Orientation'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      'Best Fit'
      'Match Component'
      'Orthogonal to Component')
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 12
    OnClick = ConfigClick
  end
  object EditSizeMin: TEdit
    Left = 144
    Top = 40
    Width = 64
    Height = 24
    Hint = 'Absolute minimum text height'
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
    TabOrder = 4
    Text = '10'
    TextHint = 'Text object clearance'
    OnChange = InputValueChange
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
    TabOrder = 3
    TabStop = False
    OnClick = MMmilButtonClick
  end
  object CheckBoxLocalSettings: TCheckBox
    Left = 8
    Top = 18
    Width = 96
    Height = 17
    Hint = 'ENABLED: Save configuration settings in script folder'
    TabStop = False
    Caption = 'Local Setti&ngs'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 13
    OnClick = CheckBoxLocalSettingsClick
  end
  object rgCenterStrategy: TRadioGroup
    Left = 8
    Top = 132
    Width = 160
    Height = 114
    Hint = 'Designator centering strategy'
    Caption = 'Centering Strategy'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      'Automatic'
      'Center of Bounds'
      'Component Body'
      'Centroid of Pads'
      'Footprint Origin')
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 11
    OnClick = ConfigClick
  end
  object EditSizeNom: TEdit
    Left = 208
    Top = 40
    Width = 64
    Height = 24
    Hint = 'Nominal text height'
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
    TabOrder = 5
    Text = '50'
    TextHint = 'nominal'
    OnChange = InputValueChange
    OnKeyPress = UserKeyPress
  end
  object EditSizeMax: TEdit
    Left = 272
    Top = 40
    Width = 64
    Height = 24
    Hint = 'Absolute maximum text height'
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
    TabOrder = 6
    Text = '120'
    TextHint = 'Text object clearance'
    OnChange = InputValueChange
    OnKeyPress = UserKeyPress
  end
  object ButtonResetOrigin: TButton
    Left = 8
    Top = 400
    Width = 104
    Height = 32
    Hint =
      'Reset designator to footprint origin, matching footprint rotatio' +
      'n'
    Caption = 'Reset To Origin'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 14
    TabStop = False
    OnClick = ButtonResetOriginClick
  end
  object ButtonAutoAdjust: TButton
    Left = 120
    Top = 400
    Width = 104
    Height = 32
    Hint =
      'Center designator in bounds, orient and resize for best fit, and' +
      ' normalize reading direction'
    Caption = 'Auto Adjust'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 15
    TabStop = False
    OnClick = ButtonAutoAdjustClick
  end
  object ButtonNormalizeDesignator: TButton
    Left = 232
    Top = 400
    Width = 104
    Height = 32
    Hint = 'Normalize designator text to be right-reading'
    Caption = 'Normalize Assy Text'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 16
    TabStop = False
    OnClick = ButtonNormalizeDesignatorClick
  end
  object ButtonResetCenter: TButton
    Left = 8
    Top = 440
    Width = 104
    Height = 32
    Hint =
      'Reset designator to center in bounds, matching footprint rotatio' +
      'n'
    Caption = 'Reset To Center'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 17
    TabStop = False
    OnClick = ButtonResetCenterClick
  end
  object ButtonResizeNoMove: TButton
    Left = 120
    Top = 440
    Width = 104
    Height = 32
    Hint =
      'Resize designator without changing position or rotation, avoidin' +
      'g other objects on layer'
    Caption = 'Resize w/o Move'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 18
    TabStop = False
    OnClick = ButtonResizeNoMoveClick
  end
  object ButtonNormalizeAnyText: TButton
    Left = 232
    Top = 440
    Width = 104
    Height = 32
    Hint =
      'Normalize any text to be right-reading while translating justifi' +
      'cation'
    Caption = 'Normalize Any Text'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 19
    TabStop = False
    OnClick = ButtonNormalizeAnyTextClick
  end
  object ButtonZoomSelected: TButton
    Left = 8
    Top = 472
    Width = 104
    Height = 32
    Hint = 'Zoom PCB window to selected items'
    Caption = 'Zoom Selected'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 20
    TabStop = False
    OnClick = ButtonZoomSelectedClick
  end
  object ButtonSelectMissing: TButton
    Left = 120
    Top = 472
    Width = 88
    Height = 32
    Hint = 'Select components that are missing an assembly designator'
    Caption = 'Select Missing'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 21
    TabStop = False
    OnClick = ButtonSelectMissingClick
  end
  object ButtonAddDesignators: TButton
    Left = 232
    Top = 472
    Width = 104
    Height = 32
    Hint =
      'Add designators to selected components if needed, using existing' +
      ' components to determine assembly layer'
    Caption = 'Add Designators'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 22
    TabStop = False
    OnClick = ButtonAddDesignatorsClick
  end
  object ButtonSaveConfig: TButton
    Left = 8
    Top = 312
    Width = 72
    Height = 32
    Hint = 'Click to save all current values'
    Caption = '&SAVE'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    OnClick = ButtonSaveConfigClick
  end
  object ButtonCancel: TButton
    Left = 264
    Top = 312
    Width = 72
    Height = 32
    Hint = 'Click to close without saving settings'
    Cancel = True
    Caption = 'Cancel'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
  object ButtonSelectComponents: TButton
    Left = 8
    Top = 512
    Width = 104
    Height = 32
    Hint = 'Select components of any selected .Designator special strings'
    Caption = 'Select Components'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 23
    TabStop = False
    OnClick = ButtonSelectComponentsClick
  end
  object ButtonSelectBoth: TButton
    Left = 136
    Top = 512
    Width = 72
    Height = 32
    Hint =
      'Select components and .Designator specials strings for any selec' +
      'tion of either'
    Caption = 'Select Both'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 24
    TabStop = False
    OnClick = ButtonSelectBothClick
  end
  object ButtonSelectDesignators: TButton
    Left = 232
    Top = 512
    Width = 104
    Height = 32
    Hint = 'Select .Designator special strings of any selected components'
    Caption = 'Select Designators'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 25
    TabStop = False
    OnClick = ButtonSelectDesignatorsClick
  end
  object Panel1: TPanel
    Left = 112
    Top = 527
    Width = 24
    Height = 3
    BevelOuter = bvNone
    Color = clBtnText
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 26
  end
  object Panel2: TPanel
    Left = 208
    Top = 487
    Width = 24
    Height = 3
    BevelOuter = bvNone
    Color = clBtnText
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 27
  end
  object Panel3: TPanel
    Left = 208
    Top = 527
    Width = 24
    Height = 3
    BevelOuter = bvNone
    Color = clBtnText
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 28
  end
  object EditAspectRatioTol: TEdit
    Left = 280
    Top = 208
    Width = 56
    Height = 24
    Hint =
      'Bounds height:width ratio exceeding this will rotate orthogonal ' +
      'to footprint'
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
    TabOrder = 7
    Text = '1.1'
    TextHint = 'Text object clearance'
    OnChange = InputValueChange
    OnKeyPress = UserKeyPress
  end
end
