object FormLengthTuning: TFormLengthTuning
  Left = 0
  Top = 0
  Caption = 'Length Tuning Helper:'
  ClientHeight = 243
  ClientWidth = 256
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
    Top = 144
    Width = 49
    Height = 13
    Caption = 'Net Class:'
  end
  object LabelFileName: TLabel
    Left = 96
    Top = 70
    Width = 77
    Height = 13
    Caption = 'No File Choosen'
  end
  object LabelComponent: TLabel
    Left = 16
    Top = 24
    Width = 58
    Height = 13
    Caption = 'IC Package:'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 207
    Height = 13
    Caption = 'Choose File With Length Info inside this IC:'
  end
  object Label3: TLabel
    Left = 16
    Top = 120
    Width = 219
    Height = 13
    Caption = 'Choose Net Class that needs to be equalized:'
  end
  object ComboBoxNetClass: TComboBox
    Left = 80
    Top = 142
    Width = 157
    Height = 21
    Style = csDropDownList
    Sorted = True
    TabOrder = 0
  end
  object CheckBoxViaLength: TCheckBox
    Left = 16
    Top = 168
    Width = 128
    Height = 17
    Caption = 'Include Length of Vias'
    TabOrder = 1
  end
  object ButtonOK: TButton
    Left = 64
    Top = 200
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 152
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
  object ButtonLoadFile: TButton
    Left = 16
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Open File'
    Enabled = False
    TabOrder = 4
    OnClick = ButtonLoadFileClick
  end
  object ComboBoxIC: TComboBox
    Left = 88
    Top = 22
    Width = 144
    Height = 21
    Style = csDropDownList
    Sorted = True
    TabOrder = 5
    OnChange = ComboBoxICChange
  end
  object OpenFileDialog: TOpenDialog
    DefaultExt = 'csv'
    Filter = 'Pkg File|*.pkg|CSV File | *.csv'
    Left = 200
    Top = 88
  end
end
