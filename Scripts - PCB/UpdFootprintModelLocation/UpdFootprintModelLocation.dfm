object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Footprint New Location'
  ClientHeight = 123
  ClientWidth = 698
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 64
    Width = 244
    Height = 13
    Caption = 'Example: D:\work\Altium\Spirit Level Library.PcbLib'
  end
  object Label2: TLabel
    Left = 16
    Top = 16
    Width = 103
    Height = 13
    Caption = 'Footprint Lib Location'
  end
  object tLink: TEdit
    Left = 16
    Top = 40
    Width = 656
    Height = 21
    TabOrder = 0
  end
  object bGo: TButton
    Left = 16
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Go'
    TabOrder = 1
    OnClick = bGoClick
  end
end
