object frmCalc: TfrmCalc
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'metric <-> imperial'
  ClientHeight = 228
  ClientWidth = 147
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object grpcalc: TGroupBox
    Left = 16
    Top = 8
    Width = 112
    Height = 152
    Caption = 'metric <-> imperial'
    TabOrder = 0
    object lblum: TLabel
      Left = 80
      Top = 56
      Width = 14
      Height = 13
      Caption = 'um'
    end
    object lblmm: TLabel
      Left = 80
      Top = 88
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object lblmil: TLabel
      Left = 80
      Top = 115
      Width = 12
      Height = 13
      Caption = 'mil'
    end
    object edInput: TEdit
      Left = 8
      Top = 16
      Width = 64
      Height = 21
      Alignment = taRightJustify
      TabOrder = 0
    end
    object edmm: TEdit
      Left = 8
      Top = 80
      Width = 64
      Height = 21
      Alignment = taRightJustify
      Color = clMenu
      TabOrder = 1
    end
    object edum: TEdit
      Left = 8
      Top = 48
      Width = 64
      Height = 21
      Alignment = taRightJustify
      Color = clMenu
      TabOrder = 2
    end
    object edmil: TEdit
      Left = 8
      Top = 115
      Width = 64
      Height = 21
      Alignment = taRightJustify
      Color = clMenu
      TabOrder = 3
    end
    object btnCalc: TButton
      Left = 80
      Top = 16
      Width = 24
      Height = 24
      Caption = 'c'
      Default = True
      TabOrder = 4
      OnClick = btnCalcClick
    end
  end
  object btnClose: TButton
    Left = 16
    Top = 176
    Width = 112
    Height = 40
    Cancel = True
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
end
