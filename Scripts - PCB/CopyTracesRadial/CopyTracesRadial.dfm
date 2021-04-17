object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 336
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object centreXLabel: TLabel
    Left = 16
    Top = 16
    Width = 42
    Height = 13
    Caption = 'Centre X'
  end
  object centreYLabel: TLabel
    Left = 160
    Top = 16
    Width = 42
    Height = 13
    Caption = 'Centre Y'
  end
  object startsuffixLabel: TLabel
    Left = 16
    Top = 64
    Width = 55
    Height = 13
    Caption = 'Start Suffix'
  end
  object endsuffixLabel: TLabel
    Left = 160
    Top = 64
    Width = 49
    Height = 13
    Caption = 'End Suffix'
  end
  object anglestepLabel: TLabel
    Left = 16
    Top = 112
    Width = 52
    Height = 13
    Caption = 'Angle Step'
  end
  object CentreX_TEdit: TEdit
    Left = 16
    Top = 32
    Width = 112
    Height = 21
    TabOrder = 0
    OnClick = bExitClick
  end
  object CentreY_TEdit: TEdit
    Left = 160
    Top = 32
    Width = 112
    Height = 21
    TabOrder = 1
  end
  object Angle_Step_TEdit: TEdit
    Left = 16
    Top = 128
    Width = 112
    Height = 21
    TabOrder = 4
  end
  object bDraw: TButton
    Left = 16
    Top = 200
    Width = 112
    Height = 24
    Caption = 'Submit'
    TabOrder = 7
    OnClick = bEnterClick
  end
  object End_N_TEdit: TEdit
    Left = 160
    Top = 80
    Width = 112
    Height = 21
    TabOrder = 3
  end
  object Start_N_TEdit: TEdit
    Left = 16
    Top = 80
    Width = 112
    Height = 21
    TabOrder = 2
  end
  object autoincrementBox: TCheckBox
    Left = 168
    Top = 120
    Width = 136
    Height = 40
    Caption = 'Auto increment net name'
    TabOrder = 5
  end
  object FlipLayerBox: TCheckBox
    Left = 168
    Top = 160
    Width = 96
    Height = 16
    Caption = 'Flip Layer'
    TabOrder = 6
  end
  object exitButton: TButton
    Left = 160
    Top = 200
    Width = 112
    Height = 24
    Caption = 'Exit'
    TabOrder = 8
    OnClick = bExitClick
  end
end
