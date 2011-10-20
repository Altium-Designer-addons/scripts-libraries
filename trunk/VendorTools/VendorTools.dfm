object VendorToolsForm: TVendorToolsForm
  Left = 0
  Top = 0
  Caption = 'Vendor Tools'
  ClientHeight = 218
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
    Width = 88
    Height = 21
    TabOrder = 0
  end
  object GroupBoxVendors: TGroupBox
    Left = 16
    Top = 56
    Width = 80
    Height = 112
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
      Enabled = False
      TabOrder = 2
    end
    object RadioButtonXilinx: TRadioButton
      Left = 8
      Top = 84
      Width = 56
      Height = 17
      Caption = 'Xilinx'
      Enabled = False
      TabOrder = 3
    end
  end
  object GroupBoxOptions: TGroupBox
    Left = 112
    Top = 56
    Width = 160
    Height = 112
    Caption = 'Options'
    TabOrder = 2
    object Label2: TLabel
      Left = 24
      Top = 59
      Width = 59
      Height = 13
      Caption = 'Wire length:'
    end
    object Label3: TLabel
      Left = 24
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
    end
    object RadioButtonExport: TRadioButton
      Left = 80
      Top = 24
      Width = 56
      Height = 17
      Caption = 'Export'
      TabOrder = 1
    end
    object EditLength: TEdit
      Left = 104
      Top = 56
      Width = 41
      Height = 21
      TabOrder = 2
      Text = '10'
    end
  end
  object ButtonCancel: TButton
    Left = 192
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
  object ButtonOK: TButton
    Left = 104
    Top = 184
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = ButtonOKClick
  end
  object OpenDialog: TOpenDialog
    Left = 16
    Top = 184
  end
  object SaveDialog: TSaveDialog
    Left = 48
    Top = 184
  end
end
