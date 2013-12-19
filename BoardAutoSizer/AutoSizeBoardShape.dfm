object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Resize Board Outline'
  ClientHeight = 224
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = Form1Activate
  PixelsPerInch = 96
  TextHeight = 13
  object Options: TGroupBox
    Left = 9
    Top = 10
    Width = 407
    Height = 174
    Caption = 'Options'
    Color = clBtnFace
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    TabStop = True
    object Label1: TLabel
      Left = 50
      Top = 68
      Width = 131
      Height = 13
      Caption = '2. Select Mechanical Layer:'
    end
    object Label2: TLabel
      Left = 50
      Top = 104
      Width = 86
      Height = 13
      Caption = '2. Set Line Width:'
    end
    object Label3: TLabel
      Left = 50
      Top = 140
      Width = 74
      Height = 13
      Caption = '3. Set Pullback:'
    end
    object Label_width: TLabel
      Left = 300
      Top = 104
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object Label_pullback: TLabel
      Left = 300
      Top = 144
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object Label4: TLabel
      Left = 50
      Top = 32
      Width = 165
      Height = 13
      Caption = '1. Place Embedded Board Array(s)'
    end
    object ComboBox1: TComboBox
      Left = 230
      Top = 64
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object Edit1: TEdit
      Left = 230
      Top = 99
      Width = 63
      Height = 21
      TabOrder = 1
      Text = '7'
    end
    object ComboBox2: TComboBox
      Left = 231
      Top = 80
      Width = 145
      Height = 21
      TabOrder = 2
      Text = 'ComboBox2_hidden'
      Visible = False
    end
    object Edit2: TEdit
      Left = 230
      Top = 139
      Width = 63
      Height = 21
      TabOrder = 3
      Text = '7'
    end
    object MetricBox: TCheckBox
      Left = 350
      Top = 122
      Width = 97
      Height = 17
      Caption = 'Metric'
      TabOrder = 4
      OnClick = MetricBoxClick
    end
    object PlaceEmbButton: TButton
      Left = 230
      Top = 26
      Width = 75
      Height = 25
      Caption = 'Place'
      TabOrder = 5
      OnClick = PlaceEmbButtonClick
    end
  end
  object RunButton: TButton
    Left = 239
    Top = 189
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 1
    OnClick = RunButtonClick
  end
  object CancelButton: TButton
    Left = 330
    Top = 189
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
end
