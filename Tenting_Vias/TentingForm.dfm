object frmTenting: TfrmTenting
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Tenting!!'
  ClientHeight = 283
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = frmTentingCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 152
    Top = 72
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Color = clBtnFace
    ParentColor = False
  end
  object grbTenting: TGroupBox
    Left = 16
    Top = 176
    Width = 128
    Height = 88
    Caption = 'Force complete tenting'
    TabOrder = 0
    object chkTop: TCheckBox
      Left = 16
      Top = 24
      Width = 104
      Height = 24
      Caption = 'on &Top'
      TabOrder = 0
    end
    object chkBottom: TCheckBox
      Left = 16
      Top = 56
      Width = 104
      Height = 24
      Caption = 'on &Bottom'
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 152
    Top = 184
    Width = 80
    Height = 24
    Caption = 'Make Tenting'
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 152
    Top = 240
    Width = 80
    Height = 24
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 8
    Width = 216
    Height = 88
    Caption = 'Current Tenting'
    TabOrder = 3
    object lblTenting: TLabel
      Left = 16
      Top = 24
      Width = 79
      Height = 13
      Caption = 'Number of Vias: '
    end
    object lblViaCount: TLabel
      Left = 144
      Top = 24
      Width = 48
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblTopTenting: TLabel
      Left = 144
      Top = 48
      Width = 48
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
    end
    object Label1: TLabel
      Left = 16
      Top = 48
      Width = 76
      Height = 13
      Caption = 'Tenting on Top:'
    end
    object Label2: TLabel
      Left = 16
      Top = 64
      Width = 92
      Height = 13
      Caption = 'Tenting on Bottom:'
    end
    object lblBotTenting: TLabel
      Left = 144
      Top = 67
      Width = 48
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Color = clBtnFace
      ParentColor = False
    end
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 104
    Width = 216
    Height = 56
    Caption = 'Selected  Vias'
    TabOrder = 4
    object lblSelected: TLabel
      Left = 145
      Top = 27
      Width = 48
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Color = clBtnFace
      ParentColor = False
    end
    object chkSelected: TCheckBox
      Left = 16
      Top = 24
      Width = 104
      Height = 24
      Caption = 'only selected Vias'
      TabOrder = 0
    end
  end
end
