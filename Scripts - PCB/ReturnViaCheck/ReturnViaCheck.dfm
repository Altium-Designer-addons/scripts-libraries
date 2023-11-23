object ReturnViaCheckForm: TReturnViaCheckForm
  Left = 313
  Top = 139
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'ReturnViaCheck Helper Script'
  ClientHeight = 603
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = ReturnViaCheckFormCreate
  OnShow = ReturnViaCheckFormShow
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
  object LabelUnits: TLabel
    Left = 5
    Top = 37
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
    Left = 104
    Top = 16
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
    Top = 64
    Width = 344
    Height = 416
  end
  object Bevel3: TBevel
    Left = 112
    Top = 488
    Width = 24
    Height = 0
  end
  object Label2: TLabel
    Left = 8
    Top = 69
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
    Top = 277
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
    Top = 493
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
  object ButtonCheckAll: TButton
    Left = 8
    Top = 560
    Width = 104
    Height = 32
    Caption = 'Check All'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = ButtonCheckAllClick
  end
  object rgReturnMode: TRadioGroup
    Left = 8
    Top = 298
    Width = 96
    Height = 56
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
    OnClick = rgReturnModeClick
  end
  object rgSignalMode: TRadioGroup
    Left = 8
    Top = 90
    Width = 96
    Height = 76
    Hint = 'Designator orientation strategy. See AR Threshold for Best Fit.'
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
    OnClick = rgSignalModeClick
  end
  object EditDistanceMax: TEdit
    Left = 104
    Top = 32
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
    Top = 32
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
    Left = 184
    Top = 512
    Width = 72
    Height = 32
    Hint = 'Normalize designator text to be right-reading'
    Caption = 'Zoom'
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
    Left = 264
    Top = 512
    Width = 72
    Height = 32
    Hint = 
      'Normalize any text to be right-reading while translating justifi' +
      'cation'
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
    Left = 104
    Top = 512
    Width = 72
    Height = 32
    Hint = 'Zoom PCB window to selected items'
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
    Top = 560
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
    Top = 560
    Width = 104
    Height = 32
    Hint = 
      'Check selected text and deselect text that does not interfere wi' +
      'th anything'
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
    Top = 88
    Width = 224
    Height = 21
    TabOrder = 10
  end
  object ComboBoxReturnNet: TComboBox
    Left = 112
    Top = 296
    Width = 224
    Height = 21
    TabOrder = 11
  end
  object ListBoxSignalNets: TListBox
    Left = 112
    Top = 112
    Width = 224
    Height = 152
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 12
  end
  object ListBoxReturnNets: TListBox
    Left = 112
    Top = 320
    Width = 224
    Height = 152
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 13
  end
  object ButtonIgnore: TButton
    Left = 8
    Top = 512
    Width = 72
    Height = 32
    Hint = 'Zoom PCB window to selected items'
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
end
