object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Fillet'
  ClientHeight = 120
  ClientWidth = 271
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
    Top = 8
    Width = 107
    Height = 13
    Caption = 'Relative Raduis Value:'
  end
  object Label2: TLabel
    Left = 16
    Top = 32
    Width = 16
    Height = 13
    Caption = 'Min'
  end
  object Label3: TLabel
    Left = 232
    Top = 32
    Width = 20
    Height = 13
    Caption = 'Max'
  end
  object LabelValue: TLabel
    Left = 136
    Top = 8
    Width = 12
    Height = 13
    Caption = '50'
  end
  object LabelRadius: TLabel
    Left = 200
    Top = 8
    Width = 57
    Height = 13
    Caption = 'LabelRadius'
    Visible = False
  end
  object ButtonOK: TButton
    Left = 96
    Top = 88
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 184
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object ScrollBarPerc: TScrollBar
    Left = 16
    Top = 56
    Width = 240
    Height = 17
    PageSize = 1
    Position = 50
    TabOrder = 2
    OnChange = ScrollBarPercChange
  end
end
