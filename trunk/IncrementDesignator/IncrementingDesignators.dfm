object Form1: TForm1
  Left = -1680
  Top = 0
  Caption = 'Increment'
  ClientHeight = 103
  ClientWidth = 193
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = Form1Create
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 35
    Width = 98
    Height = 13
    Caption = 'Next Index Number:'
  end
  object Button1: TButton
    Left = 16
    Top = 64
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 104
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Edit_Nr: TEdit
    Left = 128
    Top = 32
    Width = 48
    Height = 21
    TabOrder = 0
    Text = '1'
  end
  object CheckBoxPinName: TCheckBox
    Left = 24
    Top = 8
    Width = 96
    Height = 17
    Caption = 'Move Pin Name'
    Checked = True
    State = cbChecked
    TabOrder = 3
    Visible = False
  end
end
