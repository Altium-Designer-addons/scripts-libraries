object frmAddWireStubs: TfrmAddWireStubs
  Left = 0
  Top = 0
  Caption = 'Add Wire Stubs'
  ClientHeight = 381
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 264
    Height = 120
    Caption = 'Settings'
    TabOrder = 0
    object chkOnlySelectedComps: TCheckBox
      Left = 24
      Top = 32
      Width = 232
      Height = 16
      Caption = 'Add stubs only on selected Components'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkAddNetlabels: TCheckBox
      Left = 24
      Top = 56
      Width = 232
      Height = 16
      Caption = 'Add Netlabels'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkDesignatorToLabel: TCheckBox
      Left = 24
      Top = 80
      Width = 232
      Height = 16
      Caption = 'Use Designator as Labelname'
      TabOrder = 2
    end
  end
  object btnAddStubs: TButton
    Left = 304
    Top = 296
    Width = 104
    Height = 32
    Caption = 'Add Stubs'
    TabOrder = 1
    OnClick = btnAddStubsClick
  end
  object Cancel: TButton
    Left = 304
    Top = 336
    Width = 104
    Height = 32
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelClick
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 144
    Width = 264
    Height = 224
    Caption = 'Length'
    TabOrder = 3
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 58
      Height = 13
      Caption = 'Stub Length'
    end
    object Label2: TLabel
      Left = 16
      Top = 112
      Width = 52
      Height = 13
      Caption = 'Offset Top'
    end
    object Label3: TLabel
      Left = 16
      Top = 136
      Width = 68
      Height = 13
      Caption = 'Offset Bottom'
    end
    object Label4: TLabel
      Left = 16
      Top = 160
      Width = 59
      Height = 13
      Caption = 'Offset Right'
    end
    object Label5: TLabel
      Left = 16
      Top = 184
      Width = 53
      Height = 13
      Caption = 'Offset Left'
    end
    object Label6: TLabel
      Left = 16
      Top = 64
      Width = 232
      Height = 13
      Caption = 'All lengths are measured in multiples of grid size.'
    end
    object Label7: TLabel
      Left = 16
      Top = 80
      Width = 185
      Height = 13
      Caption = 'Offsets are measured from end of pin.'
    end
    object txtStub: TEdit
      Left = 216
      Top = 24
      Width = 24
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 0
      Text = '7'
    end
    object txtOffsetTop: TEdit
      Left = 216
      Top = 112
      Width = 24
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 1
      Text = '1'
    end
    object txtOffsetBot: TEdit
      Left = 216
      Top = 136
      Width = 24
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 2
      Text = '1'
    end
    object txtOffsetRight: TEdit
      Left = 216
      Top = 160
      Width = 24
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 3
      Text = '3'
    end
    object txtOffsetLeft: TEdit
      Left = 216
      Top = 184
      Width = 24
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 4
      Text = '6'
    end
  end
  object GroupBox3: TGroupBox
    Left = 288
    Top = 8
    Width = 136
    Height = 272
    Caption = 'Ignore Patterns'
    TabOrder = 4
    object txtIgnore: TEdit
      Left = 16
      Top = 24
      Width = 104
      Height = 21
      TabOrder = 0
      Text = 'GND'
      OnKeyPress = txtIgnoreKeyPress
    end
    object lstIgnore: TListBox
      Left = 16
      Top = 104
      Width = 104
      Height = 120
      ItemHeight = 13
      TabOrder = 1
      OnKeyDown = lstIgnoreKeyDown
    end
    object btnAdd: TButton
      Left = 16
      Top = 72
      Width = 104
      Height = 24
      Caption = 'Add'
      TabOrder = 2
      OnClick = btnAddClick
    end
    object btnRemove: TButton
      Left = 16
      Top = 232
      Width = 104
      Height = 24
      Caption = 'Remove'
      TabOrder = 3
      OnClick = btnRemoveClick
    end
    object chkCaseSensitive: TCheckBox
      Left = 16
      Top = 48
      Width = 104
      Height = 17
      Caption = 'Case Sensitive'
      TabOrder = 4
    end
  end
end
