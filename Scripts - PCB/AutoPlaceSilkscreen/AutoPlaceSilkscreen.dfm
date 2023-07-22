object Form_PlaceSilk: TForm_PlaceSilk
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Silkscreen Auto Placer'
  ClientHeight = 416
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = Form_PlaceSilkClose
  OnCreate = Form_PlaceSilkCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblCmpOutLayer: TLabel
    Left = 16
    Top = 180
    Width = 126
    Height = 13
    Caption = 'Component Outline Layer:'
  end
  object RotationStrategyLbl: TLabel
    Left = 16
    Top = 248
    Width = 90
    Height = 13
    Caption = 'Rotation Strategy:'
  end
  object PositionDeltaLbl: TLabel
    Left = 226
    Top = 328
    Width = 216
    Height = 13
    Caption = 'Position Delta (toward comp before iterating)'
  end
  object Label2: TLabel
    Left = 226
    Top = 180
    Width = 41
    Height = 13
    Caption = 'Position:'
  end
  object HintLbl: TLabel
    Left = 136
    Top = 396
    Width = 183
    Height = 13
    Caption = 'HALT EXECUTION: Ctrl + Pause/Break'
    Visible = False
  end
  object RG_Filter: TRadioGroup
    Left = 16
    Top = 16
    Width = 149
    Height = 72
    Caption = 'Filter Options'
    ItemIndex = 0
    Items.Strings = (
      'Place Entire Board'
      'Place Selected')
    TabOrder = 0
  end
  object RG_Failures: TRadioGroup
    Left = 16
    Top = 95
    Width = 185
    Height = 73
    Caption = 'Failed Placement Options'
    ItemIndex = 0
    Items.Strings = (
      'Center Over Components'
      'Place Off Board (Bottom Left)'
      'Restore Original')
    TabOrder = 2
  end
  object GB_AllowUnder: TGroupBox
    Left = 224
    Top = 16
    Width = 216
    Height = 152
    Caption = 'Allow Silk Under Specified Components'
    TabOrder = 1
    object MEM_AllowUnder: TMemo
      Left = 11
      Top = 27
      Width = 185
      Height = 109
      Lines.Strings = (
        'MEM_AllowUnder')
      TabOrder = 0
      OnEnter = MEM_AllowUnderEnter
    end
  end
  object BTN_Run: TButton
    Left = 367
    Top = 367
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 3
    OnClick = BTN_RunClick
  end
  object ProgressBar1: TProgressBar
    Left = 12
    Top = 369
    Width = 340
    Height = 22
    TabOrder = 4
  end
  object cbCmpOutlineLayer: TComboBox
    Left = 15
    Top = 198
    Width = 193
    Height = 21
    TabOrder = 5
    Text = 'cbCmpOutlineLayer'
    OnChange = cbCmpOutlineLayerChange
  end
  object chkAvoidVias: TCheckBox
    Left = 15
    Top = 226
    Width = 97
    Height = 17
    Caption = 'Avoid VIAs'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object RotationStrategyCb: TComboBox
    Left = 16
    Top = 266
    Width = 193
    Height = 21
    Style = csDropDownList
    ItemIndex = 5
    TabOrder = 7
    Text = 'Along Pins'
    Items.Strings = (
      'Component Rotation'
      'Horizontal Rotation'
      'Vertical Rotation'
      'Along Side'
      'Along Axis'
      'Along Pins'
      'KLC Style')
  end
  object FixedSizeChk: TCheckBox
    Left = 16
    Top = 320
    Width = 64
    Height = 17
    Caption = 'Fixed Size'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
  object FixedSizeEdt: TEdit
    Left = 96
    Top = 320
    Width = 113
    Height = 21
    TabOrder = 9
    Text = '0.8mm'
  end
  object FixedWidthChk: TCheckBox
    Left = 16
    Top = 344
    Width = 80
    Height = 17
    Caption = 'Fixed Width'
    Checked = True
    State = cbChecked
    TabOrder = 10
  end
  object FixedWidthEdt: TEdit
    Left = 96
    Top = 344
    Width = 113
    Height = 21
    TabOrder = 11
    Text = '0.15mm'
  end
  object PositionDeltaEdt: TEdit
    Left = 226
    Top = 344
    Width = 214
    Height = 21
    TabOrder = 12
    Text = '0.42mm'
  end
  object PositionsClb: TCheckListBox
    Left = 226
    Top = 196
    Width = 214
    Height = 120
    ItemHeight = 13
    Items.Strings = (
      'TopCenter'
      'CenterRight'
      'BottomCenter'
      'CenterLeft'
      'TopLeft'
      'TopRight'
      'BottomLeft'
      'BottomRight')
    TabOrder = 13
  end
  object TryAlteredRotationChk: TCheckBox
    Left = 16
    Top = 296
    Width = 128
    Height = 17
    Caption = 'Try Alternate Rotation'
    Checked = True
    State = cbChecked
    TabOrder = 14
  end
end
