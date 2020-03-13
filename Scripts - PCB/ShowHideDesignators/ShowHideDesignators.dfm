object FormShowHideDesignators: TFormShowHideDesignators
  Left = 16
  Top = 15
  Margins.Left = 0
  Margins.Top = 0
  Margins.Right = 0
  Margins.Bottom = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Show/Hide Designators & Comments'
  ClientHeight = 96
  ClientWidth = 250
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormShowHideClose
  OnCreate = FormShowHideDesignatorsCreate
  FormKind = fkModal
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    Left = 160
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 0
    OnClick = ButtonCloseClick
  end
  object chkDesignator: TCheckBox
    Left = 16
    Top = 24
    Width = 97
    Height = 17
    Caption = 'Show Designator'
    TabOrder = 1
  end
  object chkComment: TCheckBox
    Left = 16
    Top = 48
    Width = 97
    Height = 17
    Caption = 'Show Comment'
    TabOrder = 2
  end
  object btnApply: TButton
    Left = 160
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 3
    OnClick = ButtonApplyClick
  end
end
