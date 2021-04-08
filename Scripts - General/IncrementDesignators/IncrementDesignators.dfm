object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Increment'
  ClientHeight = 153
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
    Top = 11
    Width = 98
    Height = 13
    Caption = 'Next Index Number:'
  end
  object Label2: TLabel
    Left = 24
    Top = 40
    Width = 93
    Height = 13
    Caption = 'Increment Number:'
  end
  object Button1: TButton
    Left = 16
    Top = 120
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 104
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Edit_Nr: TEdit
    Left = 128
    Top = 8
    Width = 48
    Height = 21
    TabOrder = 0
    Text = '1'
  end
  object CheckBoxPinName: TCheckBox
    Left = 24
    Top = 88
    Width = 152
    Height = 17
    Caption = 'Move Pin Name'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 3
    Visible = False
    OnClick = CheckBoxPinNameClick
  end
  object CheckBoxSwap: TCheckBox
    Left = 24
    Top = 64
    Width = 104
    Height = 17
    Caption = 'Swap Designators'
    TabOrder = 2
    OnClick = CheckBoxSwapClick
  end
  object Edit_inc: TEdit
    Left = 128
    Top = 37
    Width = 49
    Height = 21
    TabOrder = 1
    Text = '1'
  end
end
