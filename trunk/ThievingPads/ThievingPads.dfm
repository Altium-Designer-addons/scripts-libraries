object ThievingPads: TThievingPads
  Left = 32
  Top = 18
  Caption = 'Thieving Pads'
  ClientHeight = 393
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
  object ButtonOK: TButton
    Left = 112
    Top = 360
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 200
    Top = 360
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
  object GroupBoxDistances: TGroupBox
    Left = 8
    Top = 176
    Width = 272
    Height = 96
    Caption = 'Distances:'
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 67
      Width = 84
      Height = 13
      Caption = 'To Board Outline:'
    end
    object Label2: TLabel
      Left = 16
      Top = 19
      Width = 72
      Height = 13
      Caption = 'Between Pads:'
    end
    object Label3: TLabel
      Left = 16
      Top = 43
      Width = 101
      Height = 13
      Caption = 'To Electrical Objects:'
    end
    object EditOutline: TEdit
      Left = 133
      Top = 64
      Width = 41
      Height = 21
      Color = clWhite
      TabOrder = 2
      Text = '0.5'
      OnChange = EditOutlineChange
    end
    object EditBetween: TEdit
      Left = 133
      Top = 16
      Width = 40
      Height = 21
      TabOrder = 0
      Text = '0.8'
      OnChange = EditBetweenChange
    end
    object EditElectrical: TEdit
      Left = 133
      Top = 40
      Width = 40
      Height = 21
      Enabled = False
      TabOrder = 1
      Text = '0.5'
      OnChange = EditElectricalChange
    end
    object CheckBoxOutline: TCheckBox
      Left = 184
      Top = 65
      Width = 80
      Height = 17
      Caption = 'Create Rule'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBoxBetween: TCheckBox
      Left = 184
      Top = 17
      Width = 80
      Height = 17
      Caption = 'Create Rule'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBoxElectrical: TCheckBox
      Left = 184
      Top = 41
      Width = 80
      Height = 17
      Caption = 'Create Rule'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 5
    end
  end
  object GroupBoxOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 272
    Height = 160
    Caption = 'Options:'
    TabOrder = 0
    object Label4: TLabel
      Left = 16
      Top = 110
      Width = 28
      Height = 13
      Caption = 'Units:'
    end
    object Label9: TLabel
      Left = 16
      Top = 85
      Width = 44
      Height = 13
      Caption = 'Pad Size:'
    end
    object Label10: TLabel
      Left = 16
      Top = 130
      Width = 36
      Height = 13
      Caption = 'Layers:'
    end
    object CheckBoxObjectsOutside: TCheckBox
      Left = 16
      Top = 24
      Width = 192
      Height = 17
      Caption = 'Place Thieving Pads Outside Board'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBoxObjectsInside: TCheckBox
      Left = 16
      Top = 48
      Width = 192
      Height = 17
      Caption = 'Place Thieving Pads Inside Board'
      TabOrder = 1
      OnClick = CheckBoxObjectsInsideClick
    end
    object RadioButtonmm: TRadioButton
      Left = 72
      Top = 108
      Width = 32
      Height = 17
      Caption = 'mm'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object RadioButtonmil: TRadioButton
      Left = 120
      Top = 108
      Width = 32
      Height = 17
      Caption = 'mil'
      TabOrder = 3
    end
    object EditSize: TEdit
      Left = 72
      Top = 83
      Width = 32
      Height = 21
      TabOrder = 4
      Text = '2'
      OnChange = EditSizeChange
    end
    object CheckBoxTop: TCheckBox
      Left = 72
      Top = 128
      Width = 40
      Height = 17
      Caption = 'Top'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object CheckBoxMid: TCheckBox
      Left = 120
      Top = 128
      Width = 40
      Height = 17
      Caption = 'Mid'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object CheckBoxBottom: TCheckBox
      Left = 168
      Top = 128
      Width = 56
      Height = 17
      Caption = 'Bottom'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
  end
  object GroupBoxNote: TGroupBox
    Left = 8
    Top = 280
    Width = 272
    Height = 72
    Caption = 'Note:'
    TabOrder = 4
    object Label6: TLabel
      Left = 8
      Top = 16
      Width = 255
      Height = 13
      Caption = 'You need to have Board Shape primitives on Keepout'
    end
    object Label7: TLabel
      Left = 8
      Top = 32
      Width = 258
      Height = 13
      Caption = 'layer. Otherwise distance to Board Outline will not be '
    end
    object Label8: TLabel
      Left = 8
      Top = 48
      Width = 97
      Height = 13
      Caption = 'calculated correctly.'
    end
  end
end
