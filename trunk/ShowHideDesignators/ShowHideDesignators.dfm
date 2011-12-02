object FormShowHideDesignators: TFormShowHideDesignators
  Left = 16
  Top = 15
  Caption = 'Show/Hide Designators'
  ClientHeight = 128
  ClientWidth = 258
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonShow: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Show All'
    TabOrder = 0
    OnClick = ButtonShowClick
  end
  object ButtonHide: TButton
    Left = 16
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Hide All'
    TabOrder = 1
    OnClick = ButtonHideClick
  end
  object ButtonClose: TButton
    Left = 160
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = ButtonCloseClick
  end
end
