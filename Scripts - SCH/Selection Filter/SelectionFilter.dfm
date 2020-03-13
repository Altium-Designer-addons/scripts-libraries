object RectangleForm: TRectangleForm
  Left = 0
  Top = 0
  Caption = 'Select touching rectangle'
  ClientHeight = 256
  ClientWidth = 265
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = RectangleFormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxObjects: TGroupBox
    Left = 8
    Top = 8
    Width = 248
    Height = 200
    Caption = 'Choose selectable objects'
    TabOrder = 0
    object CheckBoxSheetSymbol: TCheckBox
      Left = 16
      Top = 96
      Width = 97
      Height = 17
      Caption = 'Sheet Symbols'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBoxPort: TCheckBox
      Left = 16
      Top = 72
      Width = 97
      Height = 17
      Caption = 'Ports'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBoxWire: TCheckBox
      Left = 16
      Top = 48
      Width = 224
      Height = 17
      Caption = 'Wire / Bus / Bus entry / Signal Harness'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object CheckBoxPart: TCheckBox
      Left = 16
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Part'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBoxHarnessConnector: TCheckBox
      Left = 16
      Top = 120
      Width = 112
      Height = 17
      Caption = 'Harness connector'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBoxNetLabel: TCheckBox
      Left = 16
      Top = 144
      Width = 97
      Height = 17
      Caption = 'Net Label'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object CheckBoxPowerPort: TCheckBox
      Left = 16
      Top = 168
      Width = 97
      Height = 17
      Caption = 'Power Port'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
  end
  object ButtonOK: TButton
    Left = 96
    Top = 224
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = ButtonOKClick
  end
  object ButtonClose: TButton
    Left = 176
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = ButtonCloseClick
  end
end
