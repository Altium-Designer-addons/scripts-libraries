object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Fillet'
  ClientHeight = 100
  ClientWidth = 170
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    170
    100)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 16
    Width = 65
    Height = 13
    Caption = 'Radius Value:'
  end
  object ButtonOK: TButton
    Left = 16
    Top = 64
    Width = 64
    Height = 25
    Caption = 'O&K'
    Default = True
    TabOrder = 3
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 88
    Top = 64
    Width = 64
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
  object tRadius: TEdit
    Left = 104
    Top = 13
    Width = 48
    Height = 21
    Anchors = [akLeft, akBottom]
    OEMConvert = True
    TabOrder = 0
    Text = '31.25'
    TextHint = 'Enter value for fixed radius'
    OnChange = tRadiusChange
    ExplicitTop = 12
  end
  object RadioPanel: TPanel
    Left = 32
    Top = 40
    Width = 120
    Height = 24
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    object RadioUnitsMils: TRadioButton
      Left = 0
      Top = 0
      Width = 40
      Height = 16
      Caption = 'mils'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioUnitsMM: TRadioButton
      Left = 40
      Top = 0
      Width = 40
      Height = 16
      Caption = 'mm'
      TabOrder = 1
      TabStop = True
    end
    object RadioUnitsRatio: TRadioButton
      Left = 80
      Top = 0
      Width = 40
      Height = 16
      Caption = '%'
      TabOrder = 2
      TabStop = True
      OnClick = RadioUnitsRatioClick
    end
  end
end
