object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Add Text to Board'
  ClientHeight = 292
  ClientWidth = 349
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = Form1Create
  OnKeyPress = Form1KeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object XPLabel1: TXPLabel
    Left = 24
    Top = 32
    Width = 51
    Height = 13
    Caption = 'Enter Text'
  end
  object XPLabel2: TXPLabel
    Left = 24
    Top = 80
    Width = 46
    Height = 13
    Caption = 'X Position'
  end
  object XPLabel3: TXPLabel
    Left = 128
    Top = 80
    Width = 46
    Height = 13
    Caption = 'Y Position'
  end
  object XPLabel4: TXPLabel
    Left = 24
    Top = 128
    Width = 56
    Height = 13
    Caption = 'Text Height'
  end
  object XPLabel5: TXPLabel
    Left = 128
    Top = 128
    Width = 62
    Height = 13
    Caption = 'Stroke Width'
  end
  object XPLabel6: TXPLabel
    Left = 232
    Top = 80
    Width = 24
    Height = 13
    Caption = 'Units'
  end
  object XPLabel7: TXPLabel
    Left = 232
    Top = 128
    Width = 24
    Height = 13
    Caption = 'Units'
  end
  object Label1: TLabel
    Left = 176
    Top = 176
    Width = 27
    Height = 13
    Caption = 'Layer'
  end
  object Label2: TLabel
    Left = 24
    Top = 176
    Width = 41
    Height = 13
    Caption = 'Rotation'
  end
  object BoardText: TXPEdit
    Left = 24
    Top = 48
    Width = 288
    Height = 21
    TabOrder = 0
    Text = ''
  end
  object XPos: TXPEdit
    Left = 24
    Top = 96
    Width = 96
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object YPos: TXPEdit
    Left = 128
    Top = 96
    Width = 96
    Height = 21
    TabOrder = 2
    Text = '0'
  end
  object Submit: TXPButton
    Left = 24
    Top = 248
    Width = 72
    Height = 24
    Caption = 'Create Text'
    ParentColor = False
    TabOrder = 9
    TabStop = False
    OnClick = SubmitClick
  end
  object TextHeight: TXPEdit
    Left = 24
    Top = 144
    Width = 96
    Height = 21
    TabOrder = 4
    Text = '1'
  end
  object StrokeWidth: TXPEdit
    Left = 128
    Top = 144
    Width = 96
    Height = 21
    TabOrder = 5
    Text = '0.2'
  end
  object CoordUnits: TComboBox
    Left = 232
    Top = 96
    Width = 81
    Height = 21
    TabOrder = 3
  end
  object FontUnits: TComboBox
    Left = 232
    Top = 144
    Width = 80
    Height = 21
    TabOrder = 6
  end
  object LayerSelect: TComboBox
    Left = 176
    Top = 192
    Width = 136
    Height = 21
    TabOrder = 8
  end
  object Rotation: TEdit
    Left = 24
    Top = 192
    Width = 136
    Height = 21
    TabOrder = 7
    Text = '0'
  end
  object CloseReady: TCheckBox
    Left = 24
    Top = 224
    Width = 104
    Height = 17
    Caption = 'Close on Creation'
    Checked = True
    State = cbChecked
    TabOrder = 10
  end
end
