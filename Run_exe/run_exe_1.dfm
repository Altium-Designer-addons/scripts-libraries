object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Run Application'
  ClientHeight = 167
  ClientWidth = 221
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
    Top = 16
    Width = 145
    Height = 13
    Caption = 'Please select programm to run'
  end
  object Button1: TButton
    Left = 32
    Top = 40
    Width = 96
    Height = 25
    Caption = 'PCB Toolkit'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 32
    Top = 80
    Width = 96
    Height = 24
    Caption = 'TraceSimNew'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 32
    Top = 120
    Width = 96
    Height = 25
    Caption = 'Tx-Line'
    TabOrder = 2
    OnClick = Button3Click
  end
end
