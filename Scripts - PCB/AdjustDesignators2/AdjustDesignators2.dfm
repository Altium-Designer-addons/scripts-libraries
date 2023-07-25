object FormAdjustDesignators: TFormAdjustDesignators
  Left = 0
  Top = 0
  Caption = 'Adjust Designators'
  ClientHeight = 464
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnShow = FormAdjustDesignatorsShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxOptions: TGroupBox
    Left = 16
    Top = 16
    Width = 280
    Height = 128
    Caption = 'General Options:'
    TabOrder = 0
    object Label1: TLabel
      Left = 104
      Top = 98
      Width = 91
      Height = 13
      Caption = 'Maximum Height:'
    end
    object Label2: TLabel
      Left = 104
      Top = 74
      Width = 92
      Height = 13
      Caption = 'Minimum Height: '
    end
    object EditMaxHeight: TEdit
      Left = 192
      Top = 95
      Width = 32
      Height = 21
      TabOrder = 0
      Text = '60'
      OnChange = EditMaxHeightChange
    end
    object EditMinHeight: TEdit
      Left = 192
      Top = 71
      Width = 32
      Height = 21
      TabOrder = 1
      Text = '1'
    end
    object CheckBoxUnhide: TCheckBox
      Left = 16
      Top = 24
      Width = 120
      Height = 17
      Caption = 'Unhide Designators'
      TabOrder = 2
    end
    object CheckBoxLock: TCheckBox
      Left = 16
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Lock Strings'
      TabOrder = 3
    end
    object RadioButtonMM: TRadioButton
      Left = 16
      Top = 72
      Width = 48
      Height = 17
      Caption = 'mm'
      TabOrder = 4
    end
    object RadioButton2: TRadioButton
      Left = 16
      Top = 96
      Width = 48
      Height = 17
      Caption = 'mil'
      Checked = True
      TabOrder = 5
      TabStop = True
    end
    object cbxUseStrokeFonts: TCheckBox
      Left = 159
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Use Stroke Fonts'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object cbxUseMultiLine: TCheckBox
      Left = 160
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Use Multi-Line'
      TabOrder = 7
    end
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 296
    Width = 280
    Height = 128
    Caption = 'Designators to include'
    TabOrder = 1
    object CheckBoxOverlay: TCheckBox
      Left = 16
      Top = 24
      Width = 208
      Height = 17
      Caption = 'Modify Designators on Overlay Layers'
      TabOrder = 0
    end
    object CheckBoxMech: TCheckBox
      Left = 16
      Top = 48
      Width = 224
      Height = 17
      Caption = 'Modify Designators on Mechanical Layers'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBoxMechClick
    end
    object RadioButtonSingle: TRadioButton
      Left = 136
      Top = 72
      Width = 80
      Height = 17
      Caption = 'Single Layer'
      TabOrder = 2
      OnClick = RadioButtonsingleClick
    end
    object RadioButtonPair: TRadioButton
      Left = 32
      Top = 72
      Width = 72
      Height = 17
      Caption = 'Layer Pair'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = RadioButtonPairClick
    end
    object ComboBoxDesignators: TComboBox
      Left = 16
      Top = 96
      Width = 248
      Height = 21
      TabOrder = 4
      Text = 'ComboBoxDesignators'
    end
  end
  object ButtonOK: TButton
    Left = 88
    Top = 432
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 176
    Top = 432
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 152
    Width = 280
    Height = 128
    Caption = 'Layer Options:'
    TabOrder = 4
    object CheckBoxOverlayPrimitives: TCheckBox
      Left = 16
      Top = 23
      Width = 152
      Height = 17
      Caption = 'Consider Overlay Primitives'
      TabOrder = 0
    end
    object CheckBoxMechPrimitives: TCheckBox
      Left = 16
      Top = 48
      Width = 176
      Height = 17
      Caption = 'Consider Mech Layer Primitives'
      TabOrder = 1
      OnClick = CheckBoxMechPrimitivesClick
    end
    object RadioButtonLayerPair: TRadioButton
      Left = 32
      Top = 72
      Width = 72
      Height = 17
      Caption = 'Layer Pair'
      Checked = True
      Enabled = False
      TabOrder = 2
      TabStop = True
      OnClick = RadioButtonLayerPairClick
    end
    object RadioButtonLayerSingle: TRadioButton
      Left = 136
      Top = 72
      Width = 80
      Height = 17
      Caption = 'Single Layer'
      Enabled = False
      TabOrder = 3
      OnClick = RadioButtonLayerSingleClick
    end
    object ComboBoxLayers: TComboBox
      Left = 16
      Top = 99
      Width = 248
      Height = 21
      Enabled = False
      TabOrder = 4
      Text = 'ComboBoxLayers'
    end
  end
end
