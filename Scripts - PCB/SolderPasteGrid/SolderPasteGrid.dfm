object TextForm: TTextForm
  Left = 98
  Top = 113
  Caption = 'Paste Mask Grid Creator'
  ClientHeight = 202
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 9
    Top = 168
    Width = 344
    Height = 2
  end
  object lblMinGridSize: TLabel
    Left = 88
    Top = 24
    Width = 112
    Height = 13
    Caption = 'Minimum Grid Size (mils)'
  end
  object Label2: TLabel
    Left = 88
    Top = 56
    Width = 90
    Height = 13
    Caption = 'Minimum Gap (mils)'
  end
  object lblMinCover: TLabel
    Left = 88
    Top = 88
    Width = 166
    Height = 13
    Caption = 'Minimum Paste Mask Coverage (%)'
  end
  object bRun: TButton
    Left = 272
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 0
    OnClick = bRunClick
  end
  object txtMinGridSize: TEdit
    Left = 16
    Top = 16
    Width = 56
    Height = 21
    NumbersOnly = True
    TabOrder = 1
    Text = '40'
  end
  object txtMinGap: TEdit
    Left = 16
    Top = 48
    Width = 56
    Height = 21
    NumbersOnly = True
    TabOrder = 2
    Text = '7'
  end
  object txtMinCover: TEdit
    Left = 16
    Top = 80
    Width = 56
    Height = 21
    NumbersOnly = True
    TabOrder = 3
    Text = '50'
  end
end
