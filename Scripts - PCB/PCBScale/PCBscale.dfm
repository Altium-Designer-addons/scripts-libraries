object FormPCBscale: TFormPCBscale
  Left = 0
  Top = 0
  Caption = 'Scale selected PCB objects'
  ClientHeight = 92
  ClientWidth = 290
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
    Width = 63
    Height = 13
    Caption = 'Scale Factor:'
  end
  object EditRatio: TEdit
    Left = 85
    Top = 13
    Width = 88
    Height = 21
    TabOrder = 0
    Text = '1'
    OnChange = EditRatioChange
  end
  object ButtonOK: TButton
    Left = 120
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 208
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
end
