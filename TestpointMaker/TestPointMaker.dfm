object TestPointMakerForm: TTestPointMakerForm
  Left = 0
  Top = 0
  Caption = 'Testpoint Maker'
  ClientHeight = 265
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOK: TButton
    Left = 136
    Top = 232
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 224
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 16
    Width = 296
    Height = 208
    Caption = 'Options:'
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 97
      Width = 81
      Height = 13
      Caption = 'Select Net Class:'
    end
    object Label2: TLabel
      Left = 147
      Top = 155
      Width = 53
      Height = 13
      Caption = 'Fabrication'
    end
    object Label3: TLabel
      Left = 155
      Top = 179
      Width = 45
      Height = 13
      Caption = 'Assembly'
    end
    object Label4: TLabel
      Left = 212
      Top = 131
      Width = 18
      Height = 13
      Caption = 'Top'
    end
    object Label5: TLabel
      Left = 252
      Top = 131
      Width = 34
      Height = 13
      Caption = 'Bottom'
    end
    object Label6: TLabel
      Left = 16
      Top = 24
      Width = 44
      Height = 13
      Caption = 'Pad Size:'
    end
    object Label7: TLabel
      Left = 16
      Top = 51
      Width = 47
      Height = 13
      Caption = 'Hole Size:'
    end
    object ComboBoxNets: TComboBox
      Left = 104
      Top = 95
      Width = 176
      Height = 21
      TabOrder = 0
      Text = 'All Nets'
    end
    object CheckBoxFabTop: TCheckBox
      Left = 215
      Top = 153
      Width = 17
      Height = 17
      TabOrder = 1
    end
    object CheckBoxAssyTop: TCheckBox
      Left = 215
      Top = 177
      Width = 17
      Height = 17
      TabOrder = 2
    end
    object CheckBoxFabBottom: TCheckBox
      Left = 263
      Top = 153
      Width = 17
      Height = 17
      Enabled = False
      TabOrder = 3
    end
    object CheckBoxAssyBottom: TCheckBox
      Left = 263
      Top = 177
      Width = 17
      Height = 17
      Enabled = False
      TabOrder = 4
    end
    object EditSize: TEdit
      Left = 72
      Top = 21
      Width = 24
      Height = 21
      TabOrder = 5
      Text = '2'
      OnChange = EditSizeChange
    end
    object GroupBoxUnits: TGroupBox
      Left = 216
      Top = 16
      Width = 64
      Height = 56
      Caption = 'Units'
      TabOrder = 6
      object RadioButtonmm: TRadioButton
        Left = 9
        Top = 16
        Width = 32
        Height = 17
        Caption = 'mm'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object RadioButtonmil: TRadioButton
        Left = 9
        Top = 32
        Width = 32
        Height = 17
        Caption = 'mil'
        TabOrder = 1
      end
    end
    object GroupBox2: TGroupBox
      Left = 136
      Top = 16
      Width = 64
      Height = 56
      Caption = 'Pad Type'
      TabOrder = 7
      object RadioButtonSM: TRadioButton
        Left = 9
        Top = 16
        Width = 32
        Height = 17
        Caption = 'SM'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioButtonSMClick
      end
      object RadioButtonTH: TRadioButton
        Left = 9
        Top = 32
        Width = 32
        Height = 17
        Caption = 'TH'
        TabOrder = 1
        OnClick = RadioButtonTHClick
      end
    end
    object EditHoleSize: TEdit
      Left = 72
      Top = 48
      Width = 24
      Height = 21
      Enabled = False
      TabOrder = 8
      Text = '1'
      OnChange = EditHoleSizeChange
    end
    object CheckBoxForce: TCheckBox
      Left = 16
      Top = 128
      Width = 97
      Height = 17
      Caption = 'Force Testpoints'
      TabOrder = 9
    end
  end
end
