object FormFilletWithRadius: TFormFilletWithRadius
  Left = 0
  Top = 0
  Caption = 'Fillet With Radius'
  ClientHeight = 90
  ClientWidth = 212
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
    Left = 16
    Top = 16
    Width = 36
    Height = 13
    Caption = 'Radius:'
  end
  object ButtonOK: TButton
    Left = 32
    Top = 48
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = ButtonOKClick
  end
  object ButtonUnits: TButton
    Left = 160
    Top = 10
    Width = 35
    Height = 24
    Caption = 'mil'
    TabOrder = 3
    OnClick = ButtonUnitsClick
  end
  object ButtonCancel: TButton
    Left = 120
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
  object EditRadius: TEdit
    Left = 64
    Top = 13
    Width = 88
    Height = 21
    TabOrder = 0
    OnChange = EditRadiusChange
  end
end
