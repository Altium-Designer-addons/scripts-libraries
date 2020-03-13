object MoveToLayer: TMoveToLayer
  Left = 0
  Top = 0
  Caption = 'Move to Layer'
  ClientHeight = 100
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = MoveToLayerShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelText: TLabel
    Left = 16
    Top = 24
    Width = 119
    Height = 13
    Caption = 'Destination Signal Layer:'
  end
  object ComboBoxLayer: TComboBox
    Left = 144
    Top = 21
    Width = 184
    Height = 21
    AutoDropDown = True
    TabOrder = 0
  end
  object ButtonOK: TButton
    Left = 160
    Top = 64
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 248
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
end
