object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Fillet'
  ClientHeight = 100
  ClientWidth = 180
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    180
    100)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 16
    Width = 65
    Height = 13
    Caption = 'Radius Value:'
  end
  object ButtonOK: TButton
    Left = 24
    Top = 64
    Width = 64
    Height = 25
    Caption = 'O&K'
    Default = True
    TabOrder = 3
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 96
    Top = 64
    Width = 64
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
  object tRadius: TEdit
    Left = 111
    Top = 14
    Width = 48
    Height = 21
    Alignment = taCenter
    Anchors = []
    OEMConvert = True
    TabOrder = 0
    Text = '32.25'
    TextHint = 'Enter value for fixed radius'
    OnChange = tRadiusChange
  end
  object RadioPanel: TPanel
    Left = 38
    Top = 40
    Width = 128
    Height = 24
    BevelOuter = bvNone
    Ctl3D = True
    ParentBackground = False
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
