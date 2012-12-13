object CheckinSchPcbLibsForm: TCheckinSchPcbLibsForm
  Left = 5
  Top = 4
  Width = 1008
  Height = 827
  VertScrollBar.ButtonSize = 10
  VertScrollBar.Color = clBtnShadow
  VertScrollBar.Margin = 10
  VertScrollBar.ParentColor = False
  VertScrollBar.Range = 100
  VertScrollBar.Smooth = True
  VertScrollBar.Size = 10
  VertScrollBar.ThumbSize = 10
  VertScrollBar.Tracking = True
  Caption = 'foo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object formText01: TLabel
    Left = 14
    Top = 20
    Width = 15
    Height = 13
    Caption = 'foo'
  end
  object formEditBox1: TMemo
    Left = 14
    Top = 48
    Width = 970
    Height = 632
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 13
    Font.Name = 'Lucida Console'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object formButtonOk: TButton
    Left = 350
    Top = 700
    Width = 100
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = formButtonOkClick
  end
  object formButtonCancel: TButton
    Left = 550
    Top = 700
    Width = 100
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = formButtonCancelClick
  end
  object formStatusBar1: TXStatusBar
    Left = 0
    Top = 775
    Width = 1000
    Height = 25
    Enabled = False
    Panels = <>
    ParentShowHint = False
    ShowHint = False
    SimplePanel = True
    SimpleText = 'foo'
    SizeGrip = False
  end
end
