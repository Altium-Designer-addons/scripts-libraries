object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Fillet'
  ClientHeight = 371
  ClientWidth = 180
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = Form1Show
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
    TabOrder = 11
    TabStop = False
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 96
    Top = 64
    Width = 64
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 10
    TabStop = False
    OnClick = ButtonCancelClick
  end
  object tRadius: TEdit
    Left = 111
    Top = 14
    Width = 48
    Height = 21
    Align = alCustom
    Alignment = taCenter
    Anchors = [akTop, akRight]
    OEMConvert = True
    TabOrder = 0
    Text = '32.25'
    TextHint = 'Enter value for fixed radius'
    OnChange = tRadiusChange
    OnKeyPress = tRadiusKeyPress
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
    TabOrder = 9
    object RadioUnitsMils: TRadioButton
      Left = 0
      Top = 0
      Width = 40
      Height = 16
      Caption = 'mils'
      Checked = True
      TabOrder = 1
    end
    object RadioUnitsMM: TRadioButton
      Left = 40
      Top = 0
      Width = 40
      Height = 16
      Caption = 'mm'
      TabOrder = 0
    end
    object RadioUnitsRatio: TRadioButton
      Left = 80
      Top = 0
      Width = 40
      Height = 16
      Caption = '%'
      TabOrder = 2
      OnClick = RadioUnitsRatioClick
    end
  end
  object Button1: TButton
    Left = 96
    Top = 105
    Width = 64
    Height = 25
    Caption = 'Preset &1'
    TabOrder = 12
    TabStop = False
    OnClick = Button1Click
  end
  object tPreset1: TEdit
    Left = 24
    Top = 107
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    Anchors = [akTop, akRight]
    OEMConvert = True
    TabOrder = 1
    Text = '25.2'
    TextHint = 'Enter value for fixed radius'
    OnChange = tRadiusChange
    OnKeyPress = tPreset1KeyPress
  end
  object Button2: TButton
    Left = 96
    Top = 137
    Width = 64
    Height = 25
    Caption = 'Preset &2'
    TabOrder = 13
    TabStop = False
    OnClick = Button2Click
  end
  object tPreset2: TEdit
    Left = 24
    Top = 139
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    Anchors = [akTop, akRight]
    OEMConvert = True
    TabOrder = 2
    Text = '39.1'
    TextHint = 'Enter value for fixed radius'
    OnChange = tRadiusChange
    OnKeyPress = tPreset2KeyPress
  end
  object Button3: TButton
    Left = 96
    Top = 169
    Width = 64
    Height = 25
    Caption = 'Preset &3'
    TabOrder = 14
    TabStop = False
    OnClick = Button3Click
  end
  object tPreset3: TEdit
    Left = 24
    Top = 171
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    Anchors = [akTop, akRight]
    OEMConvert = True
    TabOrder = 3
    Text = '32.4'
    TextHint = 'Enter value for fixed radius'
    OnChange = tRadiusChange
    OnKeyPress = tPreset3KeyPress
  end
  object Button4: TButton
    Left = 96
    Top = 201
    Width = 64
    Height = 25
    Caption = 'Preset &4'
    TabOrder = 15
    TabStop = False
    OnClick = Button4Click
  end
  object tPreset4: TEdit
    Left = 24
    Top = 203
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    Anchors = [akTop, akRight]
    OEMConvert = True
    TabOrder = 4
    Text = '46.3'
    TextHint = 'Enter value for fixed radius'
    OnChange = tRadiusChange
    OnKeyPress = tPreset4KeyPress
  end
  object Button5: TButton
    Left = 96
    Top = 233
    Width = 64
    Height = 25
    Caption = 'Preset &5'
    TabOrder = 16
    TabStop = False
    OnClick = Button5Click
  end
  object tPreset5: TEdit
    Left = 24
    Top = 235
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    Anchors = [akTop, akRight]
    OEMConvert = True
    TabOrder = 5
    Text = '41.05'
    TextHint = 'Enter value for fixed radius'
    OnChange = tRadiusChange
    OnKeyPress = tPreset5KeyPress
  end
  object Button6: TButton
    Left = 96
    Top = 265
    Width = 64
    Height = 25
    Caption = 'Preset &6'
    TabOrder = 17
    TabStop = False
    OnClick = Button6Click
  end
  object tPreset6: TEdit
    Left = 24
    Top = 267
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    Anchors = [akTop, akRight]
    OEMConvert = True
    TabOrder = 6
    Text = '10'
    TextHint = 'Enter value for fixed radius'
    OnChange = tRadiusChange
    OnKeyPress = tPreset6KeyPress
  end
  object Button7: TButton
    Left = 96
    Top = 297
    Width = 64
    Height = 25
    Caption = 'Preset &7'
    TabOrder = 18
    TabStop = False
    OnClick = Button7Click
  end
  object tPreset7: TEdit
    Left = 24
    Top = 299
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    Anchors = [akTop, akRight]
    OEMConvert = True
    TabOrder = 7
    Text = '20'
    TextHint = 'Enter value for fixed radius'
    OnChange = tRadiusChange
    OnKeyPress = tPreset7KeyPress
  end
  object Button8: TButton
    Left = 96
    Top = 329
    Width = 64
    Height = 25
    Caption = 'Preset &8'
    TabOrder = 19
    TabStop = False
    OnClick = Button8Click
  end
  object tPreset8: TEdit
    Left = 24
    Top = 331
    Width = 64
    Height = 21
    Align = alCustom
    Alignment = taCenter
    Anchors = [akTop, akRight]
    OEMConvert = True
    TabOrder = 8
    Text = '30'
    TextHint = 'Enter value for fixed radius'
    OnChange = tRadiusChange
    OnKeyPress = tPreset8KeyPress
  end
end
