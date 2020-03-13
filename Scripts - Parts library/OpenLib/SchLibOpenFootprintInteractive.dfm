object FrmModelList: TFrmModelList
  Left = 0
  Top = 0
  Caption = 'Select model to show'
  ClientHeight = 531
  ClientWidth = 737
  Color = clBtnFace
  Constraints.MinHeight = 380
  Constraints.MinWidth = 450
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnKeyDown = FrmModelListKeyDown
  OnMouseWheelDown = FrmModelListMouseWheelDown
  OnMouseWheelUp = FrmModelListMouseWheelUp
  OnShow = FrmModelListShow
  DesignSize = (
    737
    531)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 102
    Height = 13
    Caption = 'Available Models &List:'
    OnClick = Label1Click
  end
  object btnOk: TButton
    Left = 16
    Top = 475
    Width = 248
    Height = 28
    Anchors = [akLeft, akBottom]
    Caption = '&Ok'
    Default = True
    TabOrder = 0
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 632
    Top = 475
    Width = 75
    Height = 28
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object StrGrdModels: TStringGrid
    Left = 16
    Top = 32
    Width = 689
    Height = 440
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 7
    DefaultColWidth = 100
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goRowSizing, goColSizing, goRowSelect]
    TabOrder = 2
    OnDblClick = StrGrdModelsDblClick
    OnKeyDown = StrGrdModelsKeyDown
    ColWidths = (
      62
      134
      113
      60
      100
      100
      100)
  end
  object SBStatusBar: TStatusBar
    Left = 0
    Top = 512
    Width = 737
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
end
