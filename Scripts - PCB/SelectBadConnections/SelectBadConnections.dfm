object FormSelectBadConnections: TFormSelectBadConnections
  Left = 0
  Top = 0
  Caption = 'Select Bad Connections'
  ClientHeight = 97
  ClientWidth = 251
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
    Left = 8
    Top = 16
    Width = 51
    Height = 13
    Caption = 'Tolerance:'
  end
  object EditTolerance: TEdit
    Left = 64
    Top = 13
    Width = 64
    Height = 21
    TabOrder = 0
    Text = '0.001'
    OnChange = EditToleranceChange
  end
  object RadioButtonMM: TRadioButton
    Left = 184
    Top = 14
    Width = 40
    Height = 17
    Caption = 'mm'
    TabOrder = 1
  end
  object RadioButtonMil: TRadioButton
    Left = 136
    Top = 14
    Width = 32
    Height = 17
    Caption = 'mil'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object ButtonOK: TButton
    Left = 80
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 168
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = ButtonCancelClick
  end
end
