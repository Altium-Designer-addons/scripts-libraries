object FormDistribute: TFormDistribute
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Distribute'
  ClientHeight = 193
  ClientWidth = 194
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormDistributeShow
  PixelsPerInch = 96
  TextHeight = 13
  object RadioButtonClearance: TRadioButton
    Left = 16
    Top = 16
    Width = 168
    Height = 17
    Caption = 'Distribute by Clearance'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = RadioButtonClearanceClick
  end
  object RadioButtonCenters: TRadioButton
    Left = 16
    Top = 40
    Width = 168
    Height = 17
    Caption = 'Distribute by Centerlines'
    TabOrder = 1
    OnClick = RadioButtonCentersClick
  end
  object RadioButtonClearanceVal: TRadioButton
    Left = 16
    Top = 64
    Width = 144
    Height = 17
    Caption = 'Distribute Clearances by:'
    TabOrder = 2
    OnClick = RadioButtonClearanceValClick
  end
  object RadioButtonCentersVal: TRadioButton
    Left = 16
    Top = 88
    Width = 152
    Height = 17
    Caption = 'Distribute Centers by:'
    TabOrder = 3
    OnClick = RadioButtonCentersValClick
  end
  object EditDistance: TEdit
    Left = 88
    Top = 112
    Width = 40
    Height = 21
    Enabled = False
    TabOrder = 4
    Text = '6'
    OnChange = EditDistanceChange
  end
  object ButtonUnits: TButton
    Left = 136
    Top = 112
    Width = 32
    Height = 20
    Caption = 'mil'
    Enabled = False
    TabOrder = 5
    OnClick = ButtonUnitsClick
  end
  object ButtonOK: TButton
    Left = 16
    Top = 152
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 6
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 104
    Top = 152
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 7
    OnClick = ButtonCancelClick
  end
end
