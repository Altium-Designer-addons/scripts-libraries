object MainFrm: TMainFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Round Polygon Corners'
  ClientHeight = 211
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = MainFrmClose
  OnCreate = MainFrmCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ChordLbl: TLabel
    Left = 24
    Top = 164
    Width = 29
    Height = 13
    Caption = 'Chord'
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 16
    Width = 120
    Height = 17
    Caption = 'Remove Small Edges'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object CheckBox2: TCheckBox
    Left = 24
    Top = 40
    Width = 152
    Height = 17
    Caption = 'Remove Redundant Vertices'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object CheckBox3: TCheckBox
    Left = 24
    Top = 64
    Width = 97
    Height = 17
    Caption = 'Unwrap Bevels'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object CheckBox4: TCheckBox
    Left = 24
    Top = 88
    Width = 97
    Height = 17
    Caption = 'Aling To Axis'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object CheckBox5: TCheckBox
    Left = 24
    Top = 112
    Width = 97
    Height = 17
    Caption = 'Round Corners'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object RunBtn: TButton
    Left = 224
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 5
    OnClick = RunBtnClick
  end
  object CheckBox6: TCheckBox
    Left = 24
    Top = 136
    Width = 97
    Height = 17
    Caption = 'Equalize Radius'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object ChordEdt: TEdit
    Left = 56
    Top = 160
    Width = 121
    Height = 21
    TabOrder = 7
    Text = '1.5mm'
  end
end
