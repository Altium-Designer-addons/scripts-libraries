object ThievingPads: TThievingPads
  Left = 8
  Top = 8
  Caption = 'Thieving Pads'
  ClientHeight = 650
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 17
  object ButtonOK: TButton
    Left = 146
    Top = 607
    Width = 99
    Height = 32
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 262
    Top = 607
    Width = 98
    Height = 32
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
  object GroupBoxDistances: TGroupBox
    Left = 10
    Top = 262
    Width = 356
    Height = 230
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Rules and Distances:'
    TabOrder = 1
    object Label1: TLabel
      Left = 21
      Top = 88
      Width = 107
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'To Board Outline:'
    end
    object Label2: TLabel
      Left = 21
      Top = 25
      Width = 90
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Between Pads:'
    end
    object Label3: TLabel
      Left = 21
      Top = 56
      Width = 128
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'To Electrical Objects:'
    end
    object EditOutline: TEdit
      Left = 174
      Top = 84
      Width = 54
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Color = clWhite
      TabOrder = 2
      Text = '0.5'
      OnChange = EditOutlineChange
    end
    object EditBetween: TEdit
      Left = 174
      Top = 21
      Width = 52
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 0
      Text = '0.8'
      OnChange = EditBetweenChange
    end
    object EditElectrical: TEdit
      Left = 174
      Top = 52
      Width = 52
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Enabled = False
      TabOrder = 1
      Text = '0.5'
      OnChange = EditElectricalChange
    end
    object CheckBoxOutline: TCheckBox
      Left = 241
      Top = 85
      Width = 104
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Create Rule'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBoxBetween: TCheckBox
      Left = 241
      Top = 22
      Width = 104
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Create Rule'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBoxElectrical: TCheckBox
      Left = 241
      Top = 54
      Width = 104
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Create Rule'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 5
    end
    object CheckBoxRoutingWidthRule: TCheckBox
      Left = 21
      Top = 157
      Width = 241
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Create Routing Width Rule'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object CheckBoxCompClearRule: TCheckBox
      Left = 21
      Top = 126
      Width = 282
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Create Component Clearance Rule'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
    object CheckBoxShortCircuitRule: TCheckBox
      Left = 21
      Top = 188
      Width = 303
      Height = 23
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Create Short Circuit Rule (for board cutout)'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 8
    end
  end
  object GroupBoxOptions: TGroupBox
    Left = 10
    Top = 10
    Width = 356
    Height = 241
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Options:'
    TabOrder = 0
    object Label4: TLabel
      Left = 21
      Top = 186
      Width = 35
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Units:'
    end
    object Label9: TLabel
      Left = 21
      Top = 153
      Width = 55
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Pad Size:'
    end
    object Label10: TLabel
      Left = 21
      Top = 212
      Width = 45
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Layers:'
    end
    object Label12: TLabel
      Left = 21
      Top = 26
      Width = 124
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Place Thieving Pads:'
    end
    object CheckBoxObjectsOutside: TCheckBox
      Left = 52
      Top = 52
      Width = 210
      Height = 23
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Outside Board Shape'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBoxObjectsInside: TCheckBox
      Left = 52
      Top = 84
      Width = 210
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Inside Board Shape'
      TabOrder = 1
      OnClick = CheckBoxObjectsInsideClick
    end
    object RadioButtonmm: TRadioButton
      Left = 94
      Top = 183
      Width = 42
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'mm'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object RadioButtonmil: TRadioButton
      Left = 157
      Top = 183
      Width = 42
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'mil'
      TabOrder = 3
    end
    object EditSize: TEdit
      Left = 94
      Top = 150
      Width = 42
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 4
      Text = '2'
      OnChange = EditSizeChange
    end
    object CheckBoxTop: TCheckBox
      Left = 94
      Top = 209
      Width = 52
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Top'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object CheckBoxMid: TCheckBox
      Left = 157
      Top = 209
      Width = 52
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Mid'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object CheckBoxBottom: TCheckBox
      Left = 220
      Top = 209
      Width = 73
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Bottom'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
    object CheckBoxCutouts: TCheckBox
      Left = 52
      Top = 115
      Width = 210
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'In Board Cutouts'
      TabOrder = 8
      OnClick = CheckBoxCutoutsClick
    end
  end
  object GroupBoxNote: TGroupBox
    Left = 10
    Top = 502
    Width = 356
    Height = 94
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Note:'
    TabOrder = 4
    object Label6: TLabel
      Left = 10
      Top = 21
      Width = 327
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'You need to have Board Shape primitives on Keepout'
    end
    object Label7: TLabel
      Left = 10
      Top = 42
      Width = 322
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'layer. Otherwise distance to Board Outline will not be '
    end
    object Label8: TLabel
      Left = 10
      Top = 63
      Width = 122
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'calculated correctly.'
    end
  end
end
