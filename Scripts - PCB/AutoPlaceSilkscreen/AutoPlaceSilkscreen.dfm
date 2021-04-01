object Form_PlaceSilk: TForm_PlaceSilk
  Left = 0
  Top = 0
  Caption = 'Silkscreen Auto Placer'
  ClientHeight = 406
  ClientWidth = 463
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = Form_PlaceSilkActivate
  PixelsPerInch = 96
  TextHeight = 13
  object lblCmpOutLayer: TLabel
    Left = 16
    Top = 188
    Width = 126
    Height = 13
    Caption = 'Component Outline Layer:'
  end
  object RG_Filter: TRadioGroup
    Left = 16
    Top = 16
    Width = 149
    Height = 72
    Caption = 'Filter Options'
    ItemIndex = 0
    Items.Strings = (
      'Place Entire Board'
      'Place Selected')
    TabOrder = 0
  end
  object RG_Failures: TRadioGroup
    Left = 16
    Top = 95
    Width = 185
    Height = 73
    Caption = 'Failed Placement Options'
    ItemIndex = 0
    Items.Strings = (
      'Center Over Components'
      'Place Off Board (Bottom Left)')
    TabOrder = 2
  end
  object GB_AllowUnder: TGroupBox
    Left = 224
    Top = 16
    Width = 216
    Height = 336
    Caption = 'Allow Silk Under Specified Components'
    TabOrder = 1
    object MEM_AllowUnder: TMemo
      Left = 11
      Top = 27
      Width = 185
      Height = 301
      Lines.Strings = (
        'MEM_AllowUnder')
      TabOrder = 0
      OnEnter = MEM_AllowUnderEnter
    end
  end
  object BTN_Run: TButton
    Left = 367
    Top = 367
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 3
    OnClick = BTN_RunClick
  end
  object ProgressBar1: TProgressBar
    Left = 12
    Top = 369
    Width = 340
    Height = 22
    TabOrder = 4
  end
  object cbCmpOutlineLayer: TComboBox
    Left = 15
    Top = 206
    Width = 193
    Height = 21
    TabOrder = 5
    Text = 'cbCmpOutlineLayer'
    OnChange = cbCmpOutlineLayerChange
  end
  object chkAvoidVias: TCheckBox
    Left = 15
    Top = 242
    Width = 97
    Height = 17
    Caption = 'Avoid VIAs'
    TabOrder = 6
  end
end
