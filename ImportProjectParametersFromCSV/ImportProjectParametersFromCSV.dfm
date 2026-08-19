object ImportProjectParamsForm: TImportProjectParamsForm
  Left = 640
  Top = 320
  Caption = 'Import Project Parameters from CSV'
  ClientHeight = 125
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelFile: TLabel
    Left = 16
    Top = 24
    Width = 132
    Height = 13
    Caption = 'CSV file path:'
  end
  object EditFile: TEdit
    Left = 16
    Top = 44
    Width = 392
    Height = 21
    TabOrder = 0
  end
  object ButtonBrowse: TButton
    Left = 416
    Top = 42
    Width = 81
    Height = 25
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = ButtonBrowseClick
  end
  object ButtonOK: TButton
    Left = 328
    Top = 88
    Width = 81
    Height = 25
    Caption = 'Import'
    TabOrder = 2
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 416
    Top = 88
    Width = 81
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
end
