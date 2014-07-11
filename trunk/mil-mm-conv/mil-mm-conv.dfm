object frmCalc: TfrmCalc
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'metric <-> imperial'
  ClientHeight = 409
  ClientWidth = 185
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = InitCreate
  OnShow = InitShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 5
    Width = 170
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'Please enter a dimension'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 21
    Width = 170
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'with its unit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblUnit: TLabel
    Left = 145
    Top = 64
    Width = 30
    Height = 13
    AutoSize = False
    Caption = '    '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblClipBrd: TLabel
    Left = 4
    Top = 356
    Width = 176
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'Clipboard filled in!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
    Visible = False
  end
  object Label3: TLabel
    Left = 8
    Top = 37
    Width = 170
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = '('#39'in'#39', '#39'mil'#39', '#39'mm'#39' or '#39'um'#39')'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object grpcalc: TGroupBox
    Left = 16
    Top = 164
    Width = 152
    Height = 88
    Caption = 'Imperial'
    TabOrder = 3
    object lblmil: TLabel
      Left = 120
      Top = 29
      Width = 12
      Height = 13
      Caption = 'mil'
    end
    object lblinch: TLabel
      Left = 120
      Top = 61
      Width = 19
      Height = 13
      Caption = 'inch'
    end
    object edmil: TEdit
      Left = 16
      Top = 24
      Width = 88
      Height = 21
      Alignment = taRightJustify
      Color = clMenu
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 0
    end
    object edinch: TEdit
      Left = 16
      Top = 56
      Width = 88
      Height = 21
      Alignment = taRightJustify
      Color = clMenu
      TabOrder = 1
    end
  end
  object btnClose: TButton
    Left = 46
    Top = 378
    Width = 80
    Height = 24
    Cancel = True
    Caption = 'Close'
    TabOrder = 5
    OnClick = btnCloseClick
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 260
    Width = 152
    Height = 88
    Caption = 'Metric'
    TabOrder = 4
    object lblum: TLabel
      Left = 120
      Top = 61
      Width = 14
      Height = 13
      Caption = #181'm'
    end
    object lblmm: TLabel
      Left = 120
      Top = 29
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object edum: TEdit
      Left = 16
      Top = 56
      Width = 88
      Height = 21
      Alignment = taRightJustify
      Color = clMenu
      TabOrder = 1
    end
    object edmm: TEdit
      Left = 16
      Top = 24
      Width = 88
      Height = 21
      Alignment = taRightJustify
      Color = clMenu
      TabOrder = 0
    end
  end
  object edInput: TEdit
    Left = 42
    Top = 60
    Width = 96
    Height = 21
    Alignment = taRightJustify
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
  end
  object btnCalc: TButton
    Left = 8
    Top = 108
    Width = 169
    Height = 40
    Caption = 'Convert'
    Default = True
    TabOrder = 2
    OnClick = btnCalcClick
  end
  object chbClipBrd: TCheckBox
    Left = 8
    Top = 85
    Width = 170
    Height = 17
    Hint = 
      'If you check this box, the result will be immediatly available w' +
      'ith '#39'Copy > Paste'#39' or CTRL + V.'
    HelpType = htKeyword
    Caption = 'Copy the result to the clipboard'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
end
