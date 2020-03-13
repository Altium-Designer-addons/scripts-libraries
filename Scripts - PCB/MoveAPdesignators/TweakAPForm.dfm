object TweakDesForm: TTweakDesForm
  Left = 313
  Top = 139
  Caption = 'Tweak Auto-Positioned Designators'
  ClientHeight = 99
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
    Left = 35
    Top = 42
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
  object AdjustAmtEdit: TEdit
    Left = 37
    Top = 14
    Width = 48
    Height = 21
    Alignment = taCenter
    TabOrder = 2
    Text = '0.5'
  end
  object MMmilButton: TButton
    Left = 102
    Top = 14
    Width = 42
    Height = 20
    Caption = 'MM'
    TabOrder = 3
    OnClick = MMmilButtonClick
  end
  object UnHideDesignatorsCheckBox: TCheckBox
    Left = 35
    Top = 66
    Width = 117
    Height = 17
    Caption = 'UnHide Designators'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object CancelButton: TButton
    Left = 176
    Top = 61
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = CancelButtonClick
  end
end
