object RotStepForm: TRotStepForm
  Left = 78
  Top = 114
  BorderIcons = [biSystemMenu]
  Caption = 'Rotation Increment'
  ClientHeight = 73
  ClientWidth = 161
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
    Top = 48
    Width = 45
    Height = 18
    Caption = '(n.nn)'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
  end
  object RotStepInput: TEdit
    Left = 8
    Top = 8
    Width = 64
    Height = 31
    Alignment = taCenter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    Text = '1.23'
    OnKeyUp = RotStepInputKeyUp
  end
  object OKButton: TButton
    Left = 88
    Top = 8
    Width = 56
    Height = 24
    Caption = 'OK'
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 88
    Top = 40
    Width = 56
    Height = 24
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
end
