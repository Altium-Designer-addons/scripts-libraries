object FormMultiPCBProject: TFormMultiPCBProject
  Left = 0
  Top = 0
  Caption = 'Update PCB Document'
  ClientHeight = 117
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object RadioButtonAll: TRadioButton
    Left = 16
    Top = 40
    Width = 152
    Height = 17
    Caption = 'Update All PCB Documents'
    TabOrder = 0
    OnClick = RadioButtonAllClick
  end
  object RadioButtonSingle: TRadioButton
    Left = 16
    Top = 16
    Width = 96
    Height = 17
    Caption = 'Update only:'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = RadioButtonSingleClick
  end
  object ButtonUpdate: TButton
    Left = 184
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Update'
    TabOrder = 2
    OnClick = ButtonUpdateClick
  end
  object ButtonCancel: TButton
    Left = 264
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
  object ComboBoxDocuments: TComboBox
    Left = 120
    Top = 15
    Width = 216
    Height = 21
    TabOrder = 4
  end
end
