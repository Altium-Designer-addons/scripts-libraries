object FormViaBarrelRelief: TFormViaBarrelRelief
  Left = 173
  Top = 176
  Caption = 'Via Soldermask Barrel Relief'
  ClientHeight = 226
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
  object lblRelief: TLabel
    Left = 72
    Top = 27
    Width = 115
    Height = 13
    Caption = 'Soldermask Relief in mils'
  end
  object lblThreshold: TLabel
    Left = 72
    Top = 59
    Width = 124
    Height = 13
    Caption = 'Hole Size Threshold in mils'
  end
  object lblResults: TLabel
    Left = 48
    Top = 163
    Width = 3
    Height = 13
  end
  object Label1: TLabel
    Left = 35
    Top = 160
    Width = 3
    Height = 13
  end
  object txtRelief: TEdit
    Left = 24
    Top = 24
    Width = 40
    Height = 21
    Alignment = taCenter
    TabOrder = 0
    Text = '5'
    TextHint = 'Typical Value is 5 mils'
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 96
    Width = 232
    Height = 56
    Caption = 'Selected  Vias'
    TabOrder = 1
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
      Width = 160
      Height = 24
      Caption = 'Update Only Selected Vias'
      TabOrder = 0
    end
  end
  object btnCancel: TButton
    Left = 184
    Top = 184
    Width = 63
    Height = 24
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnRelief: TButton
    Left = 104
    Top = 184
    Width = 72
    Height = 24
    Caption = 'Relief Vias'
    TabOrder = 3
    OnClick = btnReliefClick
  end
  object txtThreshold: TEdit
    Left = 24
    Top = 56
    Width = 40
    Height = 21
    Alignment = taCenter
    TabOrder = 4
    Text = '13'
    TextHint = 'Hole Sizes Larger than The Threshold Size will be Relieved'
  end
  object btnTent: TButton
    Left = 24
    Top = 184
    Width = 72
    Height = 24
    Caption = 'Tent Vias'
    TabOrder = 5
    OnClick = btnTentClick
  end
end
