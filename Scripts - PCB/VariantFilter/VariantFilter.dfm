object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'VariantFilter'
  ClientHeight = 297
  ClientWidth = 373
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object RadioButton1: TRadioButton
    Left = 16
    Top = 48
    Width = 280
    Height = 17
    Caption = 'Variant 1'
    TabOrder = 0
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 16
    Top = 72
    Width = 280
    Height = 17
    Caption = 'Variant 2'
    TabOrder = 1
    OnClick = RadioButton2Click
  end
  object RadioButton3: TRadioButton
    Left = 16
    Top = 96
    Width = 280
    Height = 17
    Caption = 'Variant 3'
    TabOrder = 2
    OnClick = RadioButton3Click
  end
  object RadioButton4: TRadioButton
    Left = 16
    Top = 120
    Width = 280
    Height = 17
    Caption = 'Variant 4'
    TabOrder = 3
    OnClick = RadioButton4Click
  end
  object RadioButton5: TRadioButton
    Left = 16
    Top = 144
    Width = 280
    Height = 17
    Caption = 'Variant 5'
    TabOrder = 4
    OnClick = RadioButton5Click
  end
  object RadioButton6: TRadioButton
    Left = 16
    Top = 168
    Width = 280
    Height = 17
    Caption = 'Variant 6'
    TabOrder = 5
    OnClick = RadioButton6Click
  end
  object RadioButton7: TRadioButton
    Left = 16
    Top = 192
    Width = 280
    Height = 17
    Caption = 'Variant 7'
    TabOrder = 6
    OnClick = RadioButton7Click
  end
  object RadioButton8: TRadioButton
    Left = 16
    Top = 216
    Width = 280
    Height = 17
    Caption = 'Variant 8'
    TabOrder = 7
    OnClick = RadioButton8Click
  end
  object RadioButton9: TRadioButton
    Left = 16
    Top = 240
    Width = 280
    Height = 17
    Caption = 'Variant 9'
    TabOrder = 8
    OnClick = RadioButton9Click
  end
  object RadioButton10: TRadioButton
    Left = 16
    Top = 264
    Width = 280
    Height = 17
    Caption = 'Variant 10'
    TabOrder = 9
    OnClick = RadioButton10Click
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 16
    Width = 64
    Height = 17
    Caption = 'All Fitted'
    Checked = True
    State = cbChecked
    TabOrder = 10
    OnMouseUp = CheckBox1MouseUp
  end
  object CheckBox2: TCheckBox
    Left = 88
    Top = 16
    Width = 88
    Height = 17
    Caption = 'Fitted Original'
    TabOrder = 11
    OnMouseUp = CheckBox2MouseUp
  end
  object CheckBox3: TCheckBox
    Left = 184
    Top = 16
    Width = 96
    Height = 17
    Caption = 'Fitted Alternate'
    TabOrder = 12
    OnMouseUp = CheckBox3MouseUp
  end
  object CheckBox4: TCheckBox
    Left = 296
    Top = 16
    Width = 72
    Height = 17
    Caption = 'Not Fitted'
    TabOrder = 13
    OnMouseUp = CheckBox4MouseUp
  end
end
