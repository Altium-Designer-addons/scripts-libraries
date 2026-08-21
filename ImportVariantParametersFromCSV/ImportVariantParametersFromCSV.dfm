object ImportVariantParamsForm: TImportVariantParamsForm
  Left = 640
  Top = 320
  Caption = 'Import Variant Parameters from CSV'
  ClientHeight = 240
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormVariantShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelVariant: TLabel
    Left = 16
    Top = 24
    Width = 92
    Height = 13
    Caption = 'Select variant:'
  end
  object ComboBoxVariants: TComboBox
    Left = 16
    Top = 44
    Width = 296
    Height = 21
    Style = csDropDownList
    TabOrder = 0
  end
  object ButtonNewVariant: TButton
    Left = 328
    Top = 42
    Width = 169
    Height = 25
    Caption = 'New Variant (next number)'
    TabOrder = 1
    OnClick = ButtonNewVariantClick
  end
  object CheckBoxCopyParams: TCheckBox
    Left = 16
    Top = 76
    Width = 300
    Height = 17
    Caption = 'Copy parameters from previous variant'
    Checked = True
    TabOrder = 2
  end
  object LabelFile: TLabel
    Left = 16
    Top = 112
    Width = 132
    Height = 13
    Caption = 'CSV file path:'
  end
  object EditFile: TEdit
    Left = 16
    Top = 132
    Width = 392
    Height = 21
    TabOrder = 3
  end
  object ButtonBrowse: TButton
    Left = 416
    Top = 130
    Width = 81
    Height = 25
    Caption = 'Browse...'
    TabOrder = 4
    OnClick = ButtonBrowseClick
  end
  object ButtonOK: TButton
    Left = 328
    Top = 188
    Width = 81
    Height = 25
    Caption = 'Import'
    TabOrder = 5
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 416
    Top = 188
    Width = 81
    Height = 25
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = ButtonCancelClick
  end
end
