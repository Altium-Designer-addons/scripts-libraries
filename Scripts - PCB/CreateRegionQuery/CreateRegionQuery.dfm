object RegionQueryForm: TRegionQueryForm
  Left = 0
  Top = 0
  Caption = 'Create Region Query'
  ClientHeight = 145
  ClientWidth = 601
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  OnActivate = btnSelectAreaClick
  PixelsPerInch = 96
  TextHeight = 13
  object lblImperial: TLabel
    Left = 8
    Top = 8
    Width = 32
    Height = 24
    AutoSize = False
    Caption = 'Mils'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
  end
  object lblMetric: TLabel
    Left = 8
    Top = 56
    Width = 32
    Height = 24
    AutoSize = False
    Caption = 'MM'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
  end
  object txtImperial: TEdit
    Left = 40
    Top = 8
    Width = 392
    Height = 24
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = 'txtImperial'
  end
  object txtMetric: TEdit
    Left = 40
    Top = 56
    Width = 392
    Height = 24
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = 'txtMetric'
  end
  object btnSelectArea: TButton
    Left = 8
    Top = 104
    Width = 120
    Height = 32
    Caption = 'Select Area'
    TabOrder = 2
    OnClick = btnSelectAreaClick
  end
  object btnCancel: TButton
    Left = 448
    Top = 104
    Width = 144
    Height = 32
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object btnImperialCopy: TButton
    Left = 448
    Top = 8
    Width = 48
    Height = 24
    Caption = 'COPY'
    TabOrder = 4
    OnClick = btnImperialCopyClick
  end
  object btnMetricCopy: TButton
    Left = 448
    Top = 56
    Width = 48
    Height = 24
    Caption = 'COPY'
    TabOrder = 5
    OnClick = btnMetricCopyClick
  end
  object btnImperialCopyClose: TButton
    Left = 504
    Top = 8
    Width = 88
    Height = 24
    Caption = 'COPY && CLOSE'
    TabOrder = 6
    OnClick = btnImperialCopyCloseClick
  end
  object btnMetricCopyClose: TButton
    Left = 504
    Top = 56
    Width = 88
    Height = 24
    Caption = 'COPY && CLOSE'
    TabOrder = 7
    OnClick = btnMetricCopyCloseClick
  end
end
