object frmTenting: TfrmTenting
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Tenting!!'
  ClientHeight = 305
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = frmTentingCreate
  PixelsPerInch = 101
  TextHeight = 14
  object Label3: TLabel
    Left = 164
    Top = 78
    Width = 51
    Height = 14
    Alignment = taRightJustify
    AutoSize = False
    Color = clBtnFace
    ParentColor = False
  end
  object grbTenting: TGroupBox
    Left = 17
    Top = 190
    Width = 151
    Height = 94
    Caption = 'Force complete tenting'
    TabOrder = 0
    object chkTop: TCheckBox
      Left = 17
      Top = 26
      Width = 112
      Height = 26
      Caption = 'on &Top'
      TabOrder = 0
    end
    object chkBottom: TCheckBox
      Left = 17
      Top = 60
      Width = 112
      Height = 26
      Caption = 'on &Bottom'
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 180
    Top = 198
    Width = 86
    Height = 26
    Caption = 'Make Tenting'
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 180
    Top = 258
    Width = 86
    Height = 26
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object GroupBox1: TGroupBox
    Left = 17
    Top = 9
    Width = 250
    Height = 94
    Caption = 'Current Tenting'
    TabOrder = 3
    object lblTenting: TLabel
      Left = 17
      Top = 26
      Width = 102
      Height = 14
      Caption = 'Number of Vias: '
    end
    object lblViaCount: TLabel
      Left = 155
      Top = 26
      Width = 52
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblTopTenting: TLabel
      Left = 155
      Top = 52
      Width = 52
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
    end
    object Label1: TLabel
      Left = 17
      Top = 52
      Width = 102
      Height = 14
      Caption = 'Tenting on Top:'
    end
    object Label2: TLabel
      Left = 17
      Top = 69
      Width = 118
      Height = 14
      Caption = 'Tenting on Bottom:'
    end
    object lblBotTenting: TLabel
      Left = 155
      Top = 72
      Width = 52
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Color = clBtnFace
      ParentColor = False
    end
  end
  object GroupBox2: TGroupBox
    Left = 17
    Top = 112
    Width = 250
    Height = 60
    Caption = 'Selected  Vias'
    TabOrder = 4
    object lblSelected: TLabel
      Left = 156
      Top = 29
      Width = 52
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Color = clBtnFace
      ParentColor = False
    end
    object chkSelected: TCheckBox
      Left = 17
      Top = 26
      Width = 126
      Height = 26
      Caption = 'only selected Vias'
      TabOrder = 0
    end
  end
end
