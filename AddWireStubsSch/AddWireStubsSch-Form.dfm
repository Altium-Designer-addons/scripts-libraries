object frmAddWireStubs: TfrmAddWireStubs
  Left = 0
  Top = 0
  Caption = 'Add Wire Stubs'
  ClientHeight = 353
  ClientWidth = 279
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
    Left = 8
    Top = 312
    Width = 104
    Height = 32
    Caption = 'Add Stubs'
    TabOrder = 1
    OnClick = btnAddStubsClick
  end
  object Cancel: TButton
    Left = 168
    Top = 312
    Width = 104
    Height = 32
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelClick
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 144
    Width = 264
    Height = 152
    Caption = 'Length (in multiples of grid size)'
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
      Top = 48
      Width = 52
      Height = 13
      Caption = 'Offset Top'
    end
    object Label3: TLabel
      Left = 16
      Top = 72
      Width = 68
      Height = 13
      Caption = 'Offset Bottom'
    end
    object Label4: TLabel
      Left = 16
      Top = 96
      Width = 59
      Height = 13
      Caption = 'Offset Right'
    end
    object Label5: TLabel
      Left = 16
      Top = 120
      Width = 53
      Height = 13
      Caption = 'Offset Left'
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
      Top = 48
      Width = 24
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 1
      Text = '1'
    end
    object txtOffsetBot: TEdit
      Left = 216
      Top = 72
      Width = 24
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 2
      Text = '1'
    end
    object txtOffsetRight: TEdit
      Left = 216
      Top = 96
      Width = 24
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 3
      Text = '3'
    end
    object txtOffsetLeft: TEdit
      Left = 216
      Top = 120
      Width = 24
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 4
      Text = '3'
    end
  end
end
