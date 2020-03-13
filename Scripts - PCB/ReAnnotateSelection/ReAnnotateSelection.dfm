object FormReAnnotate: TFormReAnnotate
  Left = 0
  Top = 0
  Caption = 'ReAnnotate on PCB'
  ClientHeight = 355
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 16
    Top = 16
    Width = 264
    Height = 112
    Caption = 'Annotation Order:'
    TabOrder = 0
    object RadioButton1: TRadioButton
      Left = 16
      Top = 16
      Width = 152
      Height = 17
      Caption = 'Left->Right, Up->Down'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 16
      Top = 40
      Width = 144
      Height = 17
      Caption = 'Right->Left, Up->Down'
      TabOrder = 1
    end
    object RadioButton3: TRadioButton
      Left = 16
      Top = 64
      Width = 136
      Height = 17
      Caption = 'Left->Right, Down->Up'
      TabOrder = 2
    end
    object RadioButton4: TRadioButton
      Left = 16
      Top = 88
      Width = 136
      Height = 17
      Caption = 'Right->Left, Down->Up'
      TabOrder = 3
    end
  end
  object ButtonOK: TButton
    Left = 120
    Top = 320
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 208
    Top = 320
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 208
    Width = 264
    Height = 88
    Caption = 'Layer Order:'
    TabOrder = 3
    object CheckBoxMirror: TCheckBox
      Left = 16
      Top = 63
      Width = 168
      Height = 17
      Caption = 'Mirror left/right on bottom side'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object RadioButtonTop: TRadioButton
      Left = 16
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Top First'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object RadioButtonBottom: TRadioButton
      Left = 16
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Bottom First'
      TabOrder = 2
    end
  end
  object GroupBox3: TGroupBox
    Left = 16
    Top = 136
    Width = 264
    Height = 64
    Caption = 'Initial Direction'
    TabOrder = 4
    object RadioButtonHorizontal: TRadioButton
      Left = 16
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Horizontal'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButtonVertical: TRadioButton
      Left = 16
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Vertical'
      TabOrder = 1
    end
  end
end
