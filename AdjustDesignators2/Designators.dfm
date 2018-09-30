object FormAdjustDesignators: TFormAdjustDesignators
  Left = 72
  Top = 192
  Caption = 'Designators'
  ClientHeight = 508
  ClientWidth = 309
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblTime: TLabel
    Left = 35
    Top = 480
    Width = 69
    Height = 13
    Caption = 'Elapsed Time :'
  end
  object Shape1: TShape
    Left = 89
    Top = 232
    Width = 101
    Height = 0
  end
  object Shape2: TShape
    Left = 88
    Top = 229
    Width = 112
    Height = 8
  end
  object btnApply: TButton
    Left = 224
    Top = 475
    Width = 56
    Height = 25
    Caption = 'Apply'
    TabOrder = 0
    OnClick = btnApplyClick
  end
  object GroupBox3: TGroupBox
    Left = 160
    Top = 289
    Width = 128
    Height = 136
    Caption = 'Comments (Drawings)'
    TabOrder = 1
    object cbxRotateCmts: TCheckBox
      Left = 16
      Top = 92
      Width = 79
      Height = 17
      Caption = 'Rotate XY'
      TabOrder = 3
    end
    object cbxCenterCmts: TCheckBox
      Left = 16
      Top = 112
      Width = 56
      Height = 17
      Caption = 'Center'
      TabOrder = 4
    end
    object cbxSizeCmts: TCheckBox
      Left = 16
      Top = 70
      Width = 80
      Height = 17
      Caption = 'Adjust Size'
      TabOrder = 2
    end
    object cbxHideCmts: TCheckBox
      Left = 16
      Top = 24
      Width = 96
      Height = 16
      Caption = 'Hide Selected'
      TabOrder = 0
    end
    object cbxUnHideCmts: TCheckBox
      Left = 16
      Top = 44
      Width = 96
      Height = 16
      Caption = 'UnHide Selected'
      TabOrder = 1
    end
    object cbxAllCmts: TCheckBox
      Left = 87
      Top = 114
      Width = 32
      Height = 16
      Caption = 'All'
      TabOrder = 5
      OnClick = cbxAllCmtsClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 289
    Width = 128
    Height = 136
    Caption = 'Designators (Silkscreen)'
    TabOrder = 2
    object cbxCenterRefs: TCheckBox
      Left = 16
      Top = 113
      Width = 56
      Height = 17
      Caption = 'Center'
      TabOrder = 4
    end
    object cbxRotateRefs: TCheckBox
      Left = 16
      Top = 93
      Width = 80
      Height = 17
      Caption = 'Rotate XY'
      TabOrder = 3
    end
    object cbxSizeRefs: TCheckBox
      Left = 16
      Top = 71
      Width = 80
      Height = 17
      Caption = 'Adjust Size'
      TabOrder = 2
    end
    object cbxHideRefs: TCheckBox
      Left = 16
      Top = 24
      Width = 96
      Height = 16
      Caption = 'Hide Selected'
      TabOrder = 0
    end
    object cbxUnHideRefs: TCheckBox
      Left = 16
      Top = 45
      Width = 96
      Height = 16
      Caption = 'UnHide Selected'
      TabOrder = 1
    end
    object cbxAllRefs: TCheckBox
      Left = 90
      Top = 114
      Width = 32
      Height = 16
      Caption = 'All'
      TabOrder = 5
      OnClick = cbxAllRefsClick
    end
  end
  object GroupBox4: TGroupBox
    Left = 24
    Top = 15
    Width = 264
    Height = 121
    Caption = 'Designators and Comments'
    TabOrder = 3
    object Label4: TLabel
      Left = 15
      Top = 49
      Width = 82
      Height = 13
      Caption = 'Maximum Height:'
    end
    object Label3: TLabel
      Left = 16
      Top = 24
      Width = 81
      Height = 13
      Caption = 'Minimum Height: '
    end
    object EditMinHeight: TEdit
      Left = 112
      Top = 22
      Width = 32
      Height = 21
      NumbersOnly = True
      TabOrder = 0
      Text = '10'
    end
    object EditMaxHeight: TEdit
      Left = 113
      Top = 47
      Width = 31
      Height = 21
      NumbersOnly = True
      TabOrder = 1
      Text = '50'
    end
    object RadioButtonMM: TRadioButton
      Left = 156
      Top = 46
      Width = 48
      Height = 17
      Caption = 'mm'
      TabOrder = 2
      OnClick = RadioButtonMMClick
    end
    object RadioButtonMil: TRadioButton
      Left = 156
      Top = 28
      Width = 48
      Height = 17
      Caption = 'mil'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = RadioButtonMilClick
    end
    object cbxRotateSelectedStrings: TCheckBox
      Left = 16
      Top = 93
      Width = 200
      Height = 17
      Caption = 'Rotate Strings to Match Components'
      TabOrder = 4
    end
    object cbxUseStrokeFonts: TCheckBox
      Left = 16
      Top = 72
      Width = 232
      Height = 16
      Caption = 'Use Stroke Fonts (UnCheck for True Type)'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
  end
  object GroupBox2: TGroupBox
    Left = 24
    Top = 153
    Width = 264
    Height = 120
    Caption = 'Mechanical Layers (Drawings)'
    TabOrder = 4
    object Label1: TLabel
      Left = 17
      Top = 84
      Width = 46
      Height = 13
      Caption = 'Bot Side: '
    end
    object Label2: TLabel
      Left = 66
      Top = 28
      Width = 57
      Height = 13
      Caption = 'Comments: '
    end
    object Label5: TLabel
      Left = 130
      Top = 28
      Width = 60
      Height = 13
      Caption = 'Test Points: '
    end
    object Label6: TLabel
      Left = 15
      Top = 52
      Width = 45
      Height = 13
      Caption = 'Top Side:'
    end
    object EditCommentsTop: TEdit
      Left = 81
      Top = 51
      Width = 32
      Height = 21
      NumbersOnly = True
      TabOrder = 0
      Text = '2'
      OnChange = EditCommentsTopChange
    end
    object EditCommentsBot: TEdit
      Left = 81
      Top = 88
      Width = 32
      Height = 21
      NumbersOnly = True
      TabOrder = 1
      Text = '3'
      OnChange = EditCommentsBotChange
    end
    object EditTestPointsBot: TEdit
      Left = 130
      Top = 88
      Width = 32
      Height = 21
      NumbersOnly = True
      TabOrder = 3
      Text = '3'
    end
    object EditTestPointsTop: TEdit
      Left = 130
      Top = 51
      Width = 32
      Height = 21
      NumbersOnly = True
      TabOrder = 2
      Text = '2'
    end
  end
  object btnSelectAll: TButton
    Left = 146
    Top = 440
    Width = 64
    Height = 25
    Caption = 'Select All'
    TabOrder = 5
    OnClick = btnSelectAllClick
  end
  object btnSave: TButton
    Left = 85
    Top = 440
    Width = 48
    Height = 25
    Caption = 'Save'
    TabOrder = 6
    OnClick = btnSaveClick
  end
  object btnUp: TButton
    Left = 217
    Top = 440
    Width = 23
    Height = 25
    Caption = 'Up'
    TabOrder = 7
    OnClick = btnUpClick
  end
  object btnDown: TButton
    Left = 248
    Top = 440
    Width = 40
    Height = 25
    Caption = 'Down'
    TabOrder = 8
    OnClick = btnDownClick
  end
  object btnLoad: TButton
    Left = 32
    Top = 440
    Width = 48
    Height = 25
    Caption = 'Load'
    TabOrder = 9
    OnClick = btnLoadClick
  end
  object btnSelectTop: TButton
    Left = 206
    Top = 203
    Width = 64
    Height = 24
    Caption = 'Select Top'
    TabOrder = 10
    OnClick = btnSelectTopClick
  end
  object btnSelectBot: TButton
    Left = 206
    Top = 240
    Width = 64
    Height = 24
    Caption = 'Select Bot'
    TabOrder = 11
    OnClick = btnSelectBotClick
  end
end
