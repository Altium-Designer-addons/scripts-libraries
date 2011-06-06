object ZoomToComponent: TZoomToComponent
  Left = 0
  Top = 0
  Caption = 'Zoom Component'
  ClientHeight = 105
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = ZoomToComponentCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TextComponentDesignator: TEdit
    Left = 16
    Top = 8
    Width = 264
    Height = 21
    TabOrder = 0
    Text = '*'
  end
  object OKButton: TButton
    Left = 128
    Top = 64
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 208
    Top = 64
    Width = 72
    Height = 24
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    TabStop = False
    OnClick = CancelButtonClick
  end
  object CheckBoxSelect: TCheckBox
    Left = 232
    Top = 40
    Width = 48
    Height = 17
    Caption = 'Select'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object CheckBoxZoom: TCheckBox
    Left = 136
    Top = 40
    Width = 48
    Height = 17
    Caption = 'Zoom'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object CheckBoxMask: TCheckBox
    Left = 184
    Top = 40
    Width = 48
    Height = 17
    Caption = 'Mask'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object Button1: TButton
    Left = 16
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 6
    OnClick = Button1Click
  end
end
