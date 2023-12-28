object ReturnViaCheckForm: TReturnViaCheckForm
  Left = 313
  Top = 139
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'ReturnViaCheck Helper Script'
  ClientHeight = 683
  ClientWidth = 634
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = ReturnViaCheckFormClose
  OnCreate = ReturnViaCheckFormCreate
  OnMouseEnter = ReturnViaCheckFormMouseEnter
  OnShow = ReturnViaCheckFormShow
  DesignSize = (
    634
    683)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 0
    Top = 240
    Width = 344
    Height = 312
  end
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
  object LabelUnits: TLabel
    Left = 13
    Top = 29
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
    Left = 112
    Top = 8
    Width = 64
    Height = 17
    Hint = 'Absolute minimum designator height'
    Alignment = taCenter
    AutoSize = False
    Caption = 'Max Distance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Bevel1: TBevel
    Left = 0
    Top = 56
    Width = 344
    Height = 184
  end
  object Bevel3: TBevel
    Left = 112
    Top = 488
    Width = 24
    Height = 0
  end
  object Label2: TLabel
    Left = 8
    Top = 61
    Width = 328
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '< -- Signal Net Filter -- >'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object Label3: TLabel
    Left = 8
    Top = 245
    Width = 328
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '< -- Return Net Filter -- >'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object LabelStatus: TLabel
    Left = 8
    Top = 565
    Width = 328
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'status'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object Label4: TLabel
    Left = 8
    Top = 429
    Width = 328
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '< -- Drill Pairs Allowed For Return -- >'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object LabelStackupMode: TLabel
    Left = 400
    Top = 9
    Width = 184
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Only applies to "Use Stackup"'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object LabelHelp: TLabel
    Left = 576
    Top = 6
    Width = 24
    Height = 18
    Alignment = taCenter
    AutoSize = False
    Caption = '?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    OnClick = LabelHelpClick
    OnMouseEnter = LabelHelpMouseEnter
    OnMouseLeave = LabelHelpMouseLeave
  end
  object ButtonCheckAll: TButton
    Left = 8
    Top = 640
    Width = 104
    Height = 32
    Hint = 'Check all signal vias for return via within specified distance'
    Caption = 'Check All'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = ButtonCheckAllClick
  end
  object rgReturnMode: TRadioGroup
    Left = 8
    Top = 266
    Width = 96
    Height = 56
    Hint = 'Net mode selection mode for "Return" net(s)'
    Caption = 'Return Net Mode'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      'Single Net'
      'Net Class(es)')
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = rgModeClick
  end
  object rgSignalMode: TRadioGroup
    Left = 8
    Top = 82
    Width = 96
    Height = 76
    Hint = 'Net mode selection mode for "Signal" net(s)'
    Caption = 'Signal Net Mode'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 2
    Items.Strings = (
      'Single Net'
      'Net Class(es)'
      'All Nets')
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = rgModeClick
  end
  object EditDistanceMax: TEdit
    Left = 112
    Top = 24
    Width = 64
    Height = 24
    Hint = 'Max distance between via centers'
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
    Text = '50'
    TextHint = 'Max via pitch'
    OnChange = InputValueChange
    OnKeyPress = UserKeyPress
  end
  object MMmilButton: TButton
    Left = 54
    Top = 24
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
  object ButtonZoom: TButton
    Left = 216
    Top = 584
    Width = 56
    Height = 32
    Hint = 'Zoom and select current failed via'
    Caption = 'Select'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    TabStop = False
    OnClick = ButtonZoomClick
  end
  object ButtonNext: TButton
    Left = 280
    Top = 584
    Width = 56
    Height = 32
    Hint = 'Go to next failed via'
    Caption = 'Next'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    TabStop = False
    OnClick = ButtonNextClick
  end
  object ButtonPrevious: TButton
    Left = 152
    Top = 584
    Width = 56
    Height = 32
    Hint = 'Go to previous failed via'
    Caption = 'Previous'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    TabStop = False
    OnClick = ButtonPreviousClick
  end
  object ButtonCancel: TButton
    Left = 264
    Top = 640
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
  object ButtonRecheck: TButton
    Left = 120
    Top = 640
    Width = 104
    Height = 32
    Hint =
      'Recheck detected failed vias and remove any that now have return' +
      ' vias'
    Caption = 'Recheck Fails'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    TabStop = False
    OnClick = ButtonRecheckClick
  end
  object ComboBoxSignalNet: TComboBox
    Left = 112
    Top = 80
    Width = 224
    Height = 21
    TabOrder = 10
  end
  object ComboBoxReturnNet: TComboBox
    Left = 112
    Top = 264
    Width = 224
    Height = 21
    TabOrder = 11
  end
  object ListBoxSignalNets: TListBox
    Left = 112
    Top = 104
    Width = 224
    Height = 128
    Hint = '"Signal" via net classes (allows multi-select)'
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 12
  end
  object ListBoxReturnNets: TListBox
    Left = 112
    Top = 288
    Width = 224
    Height = 128
    Hint = '"Return" via net classes (allows multi-select)'
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 13
  end
  object ButtonIgnore: TButton
    Left = 88
    Top = 584
    Width = 56
    Height = 32
    Hint = 'Ignore current via'
    Caption = 'Ignore'
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
    OnClick = ButtonIgnoreClick
  end
  object ListBoxDrillPairs: TListBox
    Left = 112
    Top = 448
    Width = 224
    Height = 96
    Hint = '"Return" via drill pairs (allows multi-select)'
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 15
  end
  object ButtonIgnoreArea: TButton
    Left = 8
    Top = 584
    Width = 72
    Height = 32
    Hint = 'Ignore all vias touching rectangle'
    Caption = 'Ignore Area'
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
    OnClick = ButtonIgnoreAreaClick
  end
  object CheckBoxRuleViolations: TCheckBox
    Left = 232
    Top = 28
    Width = 105
    Height = 17
    Hint =
      'WIP. Creates custom Hole to Hole clearance violations for naviga' +
      'tion'
    BiDiMode = bdRightToLeft
    Caption = 'Custom Violations'
    ParentBiDiMode = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 17
  end
  object ScrollBoxStackup: TScrollBox
    Left = 352
    Top = 24
    Width = 272
    Height = 648
    Anchors = [akTop, akRight]
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 18
    OnMouseEnter = StackupScrollBoxEnterLeave
    OnMouseLeave = StackupScrollBoxEnterLeave
    object PaintBoxStackup: TPaintBox
      Left = 48
      Top = 0
      Width = 200
      Height = 616
      OnMouseEnter = StackupScrollBoxEnterLeave
      OnPaint = DrawStackup
    end
  end
  object ButtonSaveStackup: TButton
    Left = 352
    Top = 0
    Width = 48
    Height = 24
    Hint = 'Save current reference layers without requiring a new check.'
    Caption = 'Save'
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
    Visible = False
    OnClick = ButtonSaveStackupClick
    OnMouseEnter = StackupScrollBoxEnterLeave
  end
  object rgViaCheckMode: TRadioGroup
    Left = 8
    Top = 450
    Width = 96
    Height = 56
    Hint =
      'Use Stackup : automatically evaluate reference layer connections' +
      ' using stackup REF tags'
    Caption = 'Via Check Mode'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      'Use Stackup'
      'Drill Pairs')
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 20
    OnClick = rgModeClick
  end
end
