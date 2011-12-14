object APDesForm: TAPDesForm
  Left = 294
  Top = 136
  Caption = ' Auto-Position Designators'
  ClientHeight = 81
  ClientWidth = 279
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object SelectedCheckBox: TCheckBox
    Left = 27
    Top = 18
    Width = 117
    Height = 17
    Caption = 'Selected Parts Only'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object OKButton: TButton
    Left = 174
    Top = 6
    Width = 75
    Height = 34
    Caption = 'OK'
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object UnHideDesignatorsCheckBox: TCheckBox
    Left = 27
    Top = 50
    Width = 117
    Height = 17
    Caption = 'UnHide Designators'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 176
    Top = 45
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = CancelButtonClick
  end
end
