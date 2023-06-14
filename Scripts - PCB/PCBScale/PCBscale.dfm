object FormPCBscale: TFormPCBscale
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Scale selected PCB objects'
  ClientHeight = 220
  ClientWidth = 257
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
    Left = 16
    Top = 48
    Width = 63
    Height = 13
    Caption = 'Scale Factor:'
  end
  object LabelActualBoardSize: TLabel
    Left = 16
    Top = 104
    Width = 102
    Height = 13
    Caption = 'LabelActualBoardSize'
  end
  object Label3: TLabel
    Left = 16
    Top = 128
    Width = 24
    Height = 13
    Caption = 'new:'
  end
  object Label2: TLabel
    Left = 128
    Top = 128
    Width = 6
    Height = 13
    Caption = 'x'
  end
  object Label5: TLabel
    Left = 12
    Top = 152
    Width = 206
    Height = 13
    Caption = 'these values refers to the actual boardsize'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 12
    Top = 168
    Width = 140
    Height = 13
    Caption = 'and not to the selected items'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LabelVersion: TLabel
    Left = 16
    Top = 8
    Width = 128
    Height = 13
    AutoSize = False
    Caption = 'ScriptVer'
    OnClick = LabelVersionClick
  end
  object EditRatio: TEdit
    Left = 85
    Top = 45
    Width = 59
    Height = 21
    TabOrder = 0
    Text = '1'
    OnChange = EditRatioChange
    OnExit = UpdateFields
    OnKeyPress = UserKeyPress
  end
  object ButtonOK: TButton
    Left = 80
    Top = 184
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 6
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 168
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 7
    OnClick = ButtonCancelClick
  end
  object RBmm: TRadioButton
    Left = 16
    Top = 72
    Width = 40
    Height = 17
    Caption = 'mm'
    Checked = True
    TabOrder = 4
    TabStop = True
    OnClick = UpdateFields
  end
  object RBmil: TRadioButton
    Left = 64
    Top = 72
    Width = 40
    Height = 17
    Caption = 'mil'
    TabOrder = 5
    OnClick = UpdateFields
  end
  object ENewWidth: TEdit
    Left = 53
    Top = 125
    Width = 67
    Height = 21
    TabOrder = 2
    Text = '1'
    OnExit = ENewWidthExit
    OnKeyPress = UserKeyPress
  end
  object ENewHeight: TEdit
    Left = 141
    Top = 125
    Width = 67
    Height = 21
    TabOrder = 3
    Text = '1'
    OnExit = ENewHeightExit
    OnKeyPress = UserKeyPress
  end
  object GroupBoxAnchor: TGroupBox
    Left = 152
    Top = 8
    Width = 93
    Height = 88
    Caption = 'Anchor Location'
    TabOrder = 1
    OnClick = GroupBoxAnchorClick
    object RBTopLeft: TRadioButton
      Tag = 1
      Left = 16
      Top = 16
      Width = 16
      Height = 16
      TabOrder = 0
      OnClick = FocusEditRatio
    end
    object RBCenterLeft: TRadioButton
      Tag = 2
      Left = 16
      Top = 40
      Width = 16
      Height = 16
      TabOrder = 1
      OnClick = FocusEditRatio
    end
    object RBBottomLeft: TRadioButton
      Tag = 3
      Left = 16
      Top = 64
      Width = 16
      Height = 16
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = FocusEditRatio
    end
    object RBTopCenter: TRadioButton
      Tag = 4
      Left = 40
      Top = 16
      Width = 16
      Height = 16
      TabOrder = 3
      OnClick = FocusEditRatio
    end
    object RBCenter: TRadioButton
      Tag = 5
      Left = 40
      Top = 40
      Width = 16
      Height = 16
      TabOrder = 4
      OnClick = FocusEditRatio
    end
    object RBBottomCenter: TRadioButton
      Tag = 6
      Left = 40
      Top = 64
      Width = 16
      Height = 16
      TabOrder = 5
      OnClick = FocusEditRatio
    end
    object RBTopRight: TRadioButton
      Tag = 7
      Left = 64
      Top = 16
      Width = 16
      Height = 16
      TabOrder = 6
      OnClick = FocusEditRatio
    end
    object RBCenterRight: TRadioButton
      Tag = 8
      Left = 64
      Top = 40
      Width = 16
      Height = 16
      TabOrder = 7
      OnClick = FocusEditRatio
    end
    object RBBottomRight: TRadioButton
      Tag = 9
      Left = 64
      Top = 64
      Width = 16
      Height = 16
      TabOrder = 8
      OnClick = FocusEditRatio
    end
  end
end
