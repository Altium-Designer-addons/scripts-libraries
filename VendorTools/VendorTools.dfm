object VendorToolsForm: TVendorToolsForm
  Left = 0
  Top = 0
  Caption = 'Vendor Tools'
  ClientHeight = 227
  ClientWidth = 289
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
    Top = 24
    Width = 155
    Height = 13
    Caption = 'Component Physical Designator:'
  end
  object ComboBoxDesignator: TComboBox
    Left = 184
    Top = 21
    Width = 96
    Height = 21
    TabOrder = 0
  end
  object GroupBoxVendors: TGroupBox
    Left = 16
    Top = 56
    Width = 80
    Height = 128
    Caption = 'Vendor'
    TabOrder = 1
    object RadioButtonActel: TRadioButton
      Left = 8
      Top = 24
      Width = 56
      Height = 17
      Caption = 'Actel'
      Enabled = False
      TabOrder = 0
    end
    object RadioButtonAltera: TRadioButton
      Left = 8
      Top = 44
      Width = 56
      Height = 17
      Caption = 'Altera'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object RadioButtonLattice: TRadioButton
      Left = 8
      Top = 64
      Width = 56
      Height = 17
      Caption = 'Lattice'
      TabOrder = 2
    end
    object RadioButtonXilinx: TRadioButton
      Left = 8
      Top = 84
      Width = 56
      Height = 17
      Caption = 'Xilinx'
      TabOrder = 3
    end
    object RadioButtonCustom: TRadioButton
      Left = 8
      Top = 104
      Width = 113
      Height = 17
      Caption = 'Custom'
      TabOrder = 4
    end
  end
  object GroupBoxOptions: TGroupBox
    Left = 104
    Top = 56
    Width = 176
    Height = 104
    Caption = 'Options'
    TabOrder = 2
    object Label2: TLabel
      Left = 16
      Top = 59
      Width = 62
      Height = 13
      Caption = 'Wire Length:'
    end
    object Label3: TLabel
      Left = 16
      Top = 80
      Width = 120
      Height = 13
      Caption = '(multiplied by visible grid)'
    end
    object RadioButtonImport: TRadioButton
      Left = 16
      Top = 24
      Width = 56
      Height = 17
      Caption = 'Import'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButtonImportClick
    end
    object RadioButtonExport: TRadioButton
      Left = 80
      Top = 24
      Width = 56
      Height = 17
      Caption = 'Export'
      TabOrder = 1
      OnClick = RadioButtonExportClick
    end
    object EditLength: TEdit
      Left = 104
      Top = 56
      Width = 41
      Height = 21
      TabOrder = 2
      Text = '5'
    end
  end
  object ButtonCancel: TButton
    Left = 192
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
  object ButtonOK: TButton
    Left = 104
    Top = 192
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = ButtonOKClick
  end
  object OpenDialog: TOpenDialog
    Left = 16
    Top = 192
  end
  object SaveDialog: TSaveDialog
    Left = 48
    Top = 192
  end
end
