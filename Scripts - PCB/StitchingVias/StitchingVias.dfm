object StitchingVias: TStitchingVias
  Left = 32
  Top = 18
  Caption = 'Stitching Vias'
  ClientHeight = 479
  ClientWidth = 289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = StitchingViasShow
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOK: TButton
    Left = 120
    Top = 440
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 208
    Top = 440
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
  object GroupBoxDistances: TGroupBox
    Left = 8
    Top = 136
    Width = 280
    Height = 200
    Caption = 'Rules and Distances:'
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
      Width = 68
      Height = 13
      Caption = 'Between Vias:'
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
      TabOrder = 5
    end
    object CheckBoxBetween: TCheckBox
      Left = 184
      Top = 17
      Width = 80
      Height = 17
      Caption = 'Create Rule'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBoxElectrical: TCheckBox
      Left = 184
      Top = 41
      Width = 80
      Height = 17
      Caption = 'Create Rule'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBoxCompClearRule: TCheckBox
      Left = 16
      Top = 119
      Width = 232
      Height = 17
      Caption = 'Create "Component Clearence" Rule'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object CheckBoxPolyConnRule: TCheckBox
      Left = 16
      Top = 167
      Width = 240
      Height = 17
      Caption = 'Create "Polygon Connect Style" Rule'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
    object CheckBoxPlaneConnRule: TCheckBox
      Left = 16
      Top = 143
      Width = 248
      Height = 17
      Caption = 'Create "Power Plane Connect Style" Rule'
      Checked = True
      State = cbChecked
      TabOrder = 8
    end
    object CheckBoxViaStyleRule: TCheckBox
      Left = 16
      Top = 96
      Width = 208
      Height = 17
      Caption = 'Create "Routing Via Style" Rule'
      Checked = True
      State = cbChecked
      TabOrder = 9
    end
  end
  object GroupBoxOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 272
    Height = 120
    Caption = 'Options:'
    TabOrder = 0
    object Label9: TLabel
      Left = 16
      Top = 20
      Width = 72
      Height = 13
      Caption = 'Size / Hole Size'
    end
    object Label5: TLabel
      Left = 16
      Top = 47
      Width = 51
      Height = 13
      Caption = 'Net Name:'
    end
    object Label4: TLabel
      Left = 142
      Top = 20
      Width = 4
      Height = 13
      Caption = '/'
    end
    object EditSize: TEdit
      Left = 104
      Top = 17
      Width = 32
      Height = 21
      TabOrder = 0
      Text = '2'
      OnChange = EditSizeChange
    end
    object EditHoleSize: TEdit
      Left = 152
      Top = 17
      Width = 32
      Height = 21
      TabOrder = 1
      Text = '1'
      OnChange = EditHoleSizeChange
    end
    object ComboBoxNets: TComboBox
      Left = 80
      Top = 44
      Width = 105
      Height = 21
      TabOrder = 2
    end
    object GroupBoxUnits: TGroupBox
      Left = 200
      Top = 8
      Width = 64
      Height = 56
      Caption = 'Units'
      TabOrder = 3
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
    object RadioButtonAllPolygons: TRadioButton
      Left = 32
      Top = 72
      Width = 224
      Height = 17
      Caption = 'Place Vias over all Polygons and Split Planes'
      Checked = True
      TabOrder = 4
      TabStop = True
    end
    object RadioButtonSelectedPolygons: TRadioButton
      Left = 32
      Top = 96
      Width = 224
      Height = 17
      Caption = 'Avoid Selected Polygons and Split Planes'
      TabOrder = 5
    end
  end
  object GroupBoxNote: TGroupBox
    Left = 8
    Top = 344
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
