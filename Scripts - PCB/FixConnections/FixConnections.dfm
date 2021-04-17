object FixConnectionsForm: TFixConnectionsForm
  Left = 8
  Top = 46
  Caption = 'Fix Connections'
  ClientHeight = 364
  ClientWidth = 211
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
    Left = 25
    Top = 16
    Width = 109
    Height = 13
    Caption = 'Select Net Class to fix:'
  end
  object Label2: TLabel
    Left = 25
    Top = 72
    Width = 51
    Height = 13
    Caption = 'Tolerance:'
  end
  object lblCnt: TLabel
    Left = 24
    Top = 143
    Width = 120
    Height = 13
    Caption = 'Found Bad Connections :'
  end
  object lblTime: TLabel
    Left = 25
    Top = 160
    Width = 69
    Height = 13
    Caption = 'Elapsed Time :'
  end
  object ButtonOK: TButton
    Left = 25
    Top = 104
    Width = 72
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 113
    Top = 104
    Width = 72
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object ComboBoxClasses: TComboBox
    Left = 25
    Top = 40
    Width = 160
    Height = 21
    Style = csDropDownList
    Sorted = True
    TabOrder = 2
  end
  object CheckBoxShortPrims: TCheckBox
    Left = 25
    Top = 232
    Width = 136
    Height = 17
    Caption = 'Remove short primitives'
    TabOrder = 3
  end
  object CheckBoxFixOverlaps: TCheckBox
    Left = 25
    Top = 256
    Width = 144
    Height = 17
    Caption = 'Fix Overlapping primitives'
    TabOrder = 4
    OnClick = CheckBoxFixOverlapsClick
  end
  object CheckBoxSplitTTracks: TCheckBox
    Left = 25
    Top = 280
    Width = 144
    Height = 17
    Caption = 'Split "T" Tracks'
    TabOrder = 5
  end
  object EditTolerance: TEdit
    Left = 97
    Top = 69
    Width = 40
    Height = 21
    TabOrder = 6
    Text = '0.1'
    OnChange = EditToleranceChange
  end
  object ButtonUnits: TButton
    Left = 145
    Top = 69
    Width = 40
    Height = 20
    Caption = 'mil'
    TabOrder = 7
    OnClick = ButtonUnitsClick
  end
  object CheckBoxSelectBadConnections: TCheckBox
    Left = 25
    Top = 184
    Width = 136
    Height = 17
    Caption = 'Select Bad Connections'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = CheckBoxSelectBadConnectionsClick
  end
  object CheckBoxSplitXTracks: TCheckBox
    Left = 25
    Top = 304
    Width = 97
    Height = 17
    Caption = 'Split "X" Tracks'
    Enabled = False
    TabOrder = 9
  end
  object CheckBoxFixBadConnectons: TCheckBox
    Left = 25
    Top = 328
    Width = 120
    Height = 17
    Caption = 'Fix Bad Connectons'
    Enabled = False
    TabOrder = 10
  end
  object CheckBoxShortBadConnections: TCheckBox
    Left = 25
    Top = 208
    Width = 176
    Height = 17
    Caption = 'Remove Short Bad Connections'
    TabOrder = 11
  end
end
