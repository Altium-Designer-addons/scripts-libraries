object ImportPinsForm: TImportPinsForm
  Left = 117
  Top = 90
  BorderStyle = bsDialog
  Caption = 'Pins Importer'
  ClientHeight = 102
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelImport: TLabel
    Left = 8
    Top = 8
    Width = 121
    Height = 13
    Caption = 'CSV Format File to import:'
  end
  object Edit: TEdit
    Left = 8
    Top = 24
    Width = 337
    Height = 21
    TabOrder = 0
  end
  object ButtonBrowse: TButton
    Left = 352
    Top = 22
    Width = 65
    Height = 26
    Caption = 'Browse ...'
    TabOrder = 1
    OnClick = ButtonBrowseClick
  end
  object ButtonRun: TButton
    Left = 8
    Top = 72
    Width = 408
    Height = 25
    Caption = 'Run'
    TabOrder = 2
    OnClick = ButtonRunClick
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'Comma Separated Values (*.csv)|*.CSV|Text files (*.txt)|*.TXT|Al' +
      'l Files (*.*)|*.*'
    Title = 'Open'
    Left = 8
    Top = 56
  end
end
