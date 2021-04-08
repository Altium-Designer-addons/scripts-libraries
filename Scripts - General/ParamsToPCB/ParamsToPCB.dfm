object FormParamsToPCB: TFormParamsToPCB
  Left = 21
  Top = 16
  Caption = 'Import Parameters to PCB'
  ClientHeight = 332
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormMechLayerDesignatorsShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxTop: TGroupBox
    Left = 16
    Top = 224
    Width = 288
    Height = 56
    Caption = 'Parameters for Components on Top will be placed on:'
    TabOrder = 4
    object RadioButtonLayer2: TRadioButton
      Left = 159
      Top = 23
      Width = 113
      Height = 17
      Caption = 'Layer 2'
      TabOrder = 0
      TabStop = True
    end
    object RadioButtonLayer1: TRadioButton
      Left = 16
      Top = 23
      Width = 113
      Height = 17
      Caption = 'Layer 1'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
  end
  object GroupBoxLayers: TGroupBox
    Left = 16
    Top = 104
    Width = 288
    Height = 48
    Caption = 'Do you want parameters on single layer or layer pair?'
    TabOrder = 3
    object RadioButtonPair: TRadioButton
      Left = 16
      Top = 23
      Width = 113
      Height = 17
      Caption = 'Layer Pairs'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButtonPairClick
    end
    object RadioButtonSingle: TRadioButton
      Left = 151
      Top = 23
      Width = 113
      Height = 17
      Caption = 'Single Layer'
      TabOrder = 1
      OnClick = RadioButtonSingleClick
    end
  end
  object GroupBoxLayer: TGroupBox
    Left = 16
    Top = 160
    Width = 288
    Height = 56
    Caption = 'Choose Mech Layer Pair:'
    TabOrder = 2
    object ComboBoxLayers: TComboBox
      Left = 8
      Top = 27
      Width = 272
      Height = 21
      TabOrder = 0
      Text = 'ComboBoxLayers'
      OnChange = ComboBoxLayersChange
    end
  end
  object ButtonOK: TButton
    Left = 144
    Top = 296
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 232
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object GroupBoxParameter: TGroupBox
    Left = 16
    Top = 16
    Width = 288
    Height = 80
    Caption = 'Parameter Options'
    TabOrder = 5
    object Label1: TLabel
      Left = 16
      Top = 27
      Width = 89
      Height = 13
      Caption = 'Choose parameter'
    end
    object ComboBoxParameters: TComboBox
      Left = 119
      Top = 24
      Width = 161
      Height = 21
      TabOrder = 0
    end
    object RadioButtonSilk: TRadioButton
      Left = 152
      Top = 56
      Width = 113
      Height = 17
      Caption = 'Place on Silk'
      TabOrder = 1
      OnClick = RadioButtonSilkClick
    end
    object RadioButtonMech: TRadioButton
      Left = 16
      Top = 56
      Width = 113
      Height = 17
      Caption = 'Place on Mech'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = RadioButtonMechClick
    end
  end
end
