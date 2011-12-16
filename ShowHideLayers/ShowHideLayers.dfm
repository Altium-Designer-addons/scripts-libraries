object ShowHideLayers: TShowHideLayers
  Left = 37
  Top = 21
  Caption = 'Show/Hide Layers'
  ClientHeight = 701
  ClientWidth = 197
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = ShowHideLayersClose
  OnResize = ShowHideLayersResize
  OnShow = ShowHideLayersShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControlLayers: TTabControl
    Left = 8
    Top = 8
    Width = 184
    Height = 688
    TabOrder = 0
    Tabs.Strings = (
      'Copper'
      'Mech'
      'Other')
    TabIndex = 0
    OnChange = TabControlLayersChange
    object Shape1: TShape
      Left = 8
      Top = 48
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape2: TShape
      Left = 8
      Top = 68
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape3: TShape
      Left = 8
      Top = 88
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape4: TShape
      Left = 8
      Top = 108
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape5: TShape
      Left = 8
      Top = 128
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape6: TShape
      Left = 8
      Top = 148
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape7: TShape
      Left = 8
      Top = 168
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape8: TShape
      Left = 8
      Top = 188
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape9: TShape
      Left = 8
      Top = 208
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape10: TShape
      Left = 8
      Top = 228
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape11: TShape
      Left = 8
      Top = 248
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape12: TShape
      Left = 8
      Top = 268
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape13: TShape
      Left = 8
      Top = 288
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape14: TShape
      Left = 8
      Top = 308
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape15: TShape
      Left = 8
      Top = 328
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape16: TShape
      Left = 8
      Top = 348
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape17: TShape
      Left = 8
      Top = 368
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape18: TShape
      Left = 8
      Top = 388
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape19: TShape
      Left = 8
      Top = 408
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape20: TShape
      Left = 8
      Top = 428
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape21: TShape
      Left = 8
      Top = 448
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape22: TShape
      Left = 8
      Top = 468
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape23: TShape
      Left = 8
      Top = 488
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape24: TShape
      Left = 8
      Top = 508
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape25: TShape
      Left = 8
      Top = 528
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape26: TShape
      Left = 8
      Top = 548
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape27: TShape
      Left = 8
      Top = 568
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape28: TShape
      Left = 8
      Top = 588
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape29: TShape
      Left = 8
      Top = 608
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape30: TShape
      Left = 8
      Top = 628
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape31: TShape
      Left = 8
      Top = 648
      Width = 13
      Height = 13
      Visible = False
    end
    object Shape32: TShape
      Left = 8
      Top = 668
      Width = 13
      Height = 13
      Visible = False
    end
    object CheckBox1: TCheckBox
      Left = 24
      Top = 46
      Width = 150
      Height = 17
      Caption = 'CheckBox1'
      Color = clRed
      Enabled = False
      ParentColor = False
      TabOrder = 0
      Visible = False
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 24
      Top = 66
      Width = 150
      Height = 17
      Caption = 'CheckBox2'
      Enabled = False
      TabOrder = 1
      Visible = False
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 24
      Top = 86
      Width = 150
      Height = 17
      Caption = 'CheckBox3'
      Enabled = False
      TabOrder = 2
      Visible = False
      OnClick = CheckBox3Click
    end
    object CheckBox4: TCheckBox
      Left = 24
      Top = 106
      Width = 150
      Height = 17
      Caption = 'CheckBox4'
      Enabled = False
      TabOrder = 3
      Visible = False
      OnClick = CheckBox4Click
    end
    object CheckBox5: TCheckBox
      Left = 24
      Top = 126
      Width = 150
      Height = 17
      Caption = 'CheckBox5'
      Enabled = False
      TabOrder = 4
      Visible = False
      OnClick = CheckBox5Click
    end
    object CheckBox6: TCheckBox
      Left = 24
      Top = 146
      Width = 150
      Height = 17
      Caption = 'CheckBox6'
      Enabled = False
      TabOrder = 5
      Visible = False
      OnClick = CheckBox6Click
    end
    object CheckBox7: TCheckBox
      Left = 24
      Top = 166
      Width = 150
      Height = 17
      Caption = 'CheckBox7'
      Enabled = False
      TabOrder = 6
      Visible = False
      OnClick = CheckBox7Click
    end
    object CheckBox8: TCheckBox
      Left = 24
      Top = 186
      Width = 150
      Height = 17
      Caption = 'CheckBox8'
      Enabled = False
      TabOrder = 7
      Visible = False
      OnClick = CheckBox8Click
    end
    object CheckBox9: TCheckBox
      Left = 24
      Top = 206
      Width = 150
      Height = 17
      Caption = 'CheckBox9'
      Enabled = False
      TabOrder = 8
      Visible = False
      OnClick = CheckBox9Click
    end
    object CheckBox10: TCheckBox
      Left = 24
      Top = 226
      Width = 150
      Height = 17
      Caption = 'CheckBox10'
      Enabled = False
      TabOrder = 9
      Visible = False
      OnClick = CheckBox10Click
    end
    object CheckBox11: TCheckBox
      Left = 24
      Top = 246
      Width = 150
      Height = 17
      Caption = 'CheckBox11'
      Enabled = False
      TabOrder = 10
      Visible = False
      OnClick = CheckBox11Click
    end
    object CheckBox12: TCheckBox
      Left = 24
      Top = 266
      Width = 150
      Height = 17
      Caption = 'CheckBox12'
      Enabled = False
      TabOrder = 11
      Visible = False
      OnClick = CheckBox12Click
    end
    object CheckBox13: TCheckBox
      Left = 24
      Top = 286
      Width = 150
      Height = 17
      Caption = 'CheckBox13'
      Enabled = False
      TabOrder = 12
      Visible = False
      OnClick = CheckBox13Click
    end
    object CheckBox14: TCheckBox
      Left = 24
      Top = 306
      Width = 150
      Height = 17
      Caption = 'CheckBox14'
      Enabled = False
      TabOrder = 13
      Visible = False
      OnClick = CheckBox14Click
    end
    object CheckBox15: TCheckBox
      Left = 24
      Top = 326
      Width = 150
      Height = 17
      Caption = 'CheckBox15'
      Enabled = False
      TabOrder = 14
      Visible = False
      OnClick = CheckBox15Click
    end
    object CheckBox16: TCheckBox
      Left = 24
      Top = 346
      Width = 150
      Height = 17
      Caption = 'CheckBox16'
      Enabled = False
      TabOrder = 15
      Visible = False
      OnClick = CheckBox16Click
    end
    object CheckBox17: TCheckBox
      Left = 24
      Top = 366
      Width = 150
      Height = 17
      Caption = 'CheckBox17'
      Enabled = False
      TabOrder = 16
      Visible = False
      OnClick = CheckBox17Click
    end
    object CheckBox18: TCheckBox
      Left = 24
      Top = 386
      Width = 150
      Height = 17
      Caption = 'CheckBox18'
      Enabled = False
      TabOrder = 17
      Visible = False
      OnClick = CheckBox18Click
    end
    object CheckBox19: TCheckBox
      Left = 24
      Top = 406
      Width = 150
      Height = 17
      Caption = 'CheckBox19'
      Enabled = False
      TabOrder = 18
      Visible = False
      OnClick = CheckBox19Click
    end
    object CheckBox20: TCheckBox
      Left = 24
      Top = 426
      Width = 150
      Height = 17
      Caption = 'CheckBox20'
      Enabled = False
      TabOrder = 19
      Visible = False
      OnClick = CheckBox20Click
    end
    object CheckBox21: TCheckBox
      Left = 24
      Top = 446
      Width = 150
      Height = 17
      Caption = 'CheckBox21'
      Enabled = False
      TabOrder = 20
      Visible = False
      OnClick = CheckBox21Click
    end
    object CheckBox22: TCheckBox
      Left = 24
      Top = 466
      Width = 150
      Height = 17
      Caption = 'CheckBox22'
      Enabled = False
      TabOrder = 21
      Visible = False
      OnClick = CheckBox22Click
    end
    object CheckBox23: TCheckBox
      Left = 24
      Top = 486
      Width = 150
      Height = 17
      Caption = 'CheckBox23'
      Enabled = False
      TabOrder = 22
      Visible = False
      OnClick = CheckBox23Click
    end
    object CheckBox24: TCheckBox
      Left = 24
      Top = 506
      Width = 150
      Height = 17
      Caption = 'CheckBox24'
      Enabled = False
      TabOrder = 23
      Visible = False
      OnClick = CheckBox24Click
    end
    object CheckBox25: TCheckBox
      Left = 24
      Top = 526
      Width = 150
      Height = 17
      Caption = 'CheckBox25'
      Enabled = False
      TabOrder = 24
      Visible = False
      OnClick = CheckBox25Click
    end
    object CheckBox26: TCheckBox
      Left = 24
      Top = 546
      Width = 150
      Height = 17
      Caption = 'CheckBox26'
      Enabled = False
      TabOrder = 25
      Visible = False
      OnClick = CheckBox26Click
    end
    object CheckBox27: TCheckBox
      Left = 24
      Top = 566
      Width = 150
      Height = 17
      Caption = 'CheckBox27'
      Enabled = False
      TabOrder = 26
      Visible = False
      OnClick = CheckBox27Click
    end
    object CheckBox28: TCheckBox
      Left = 24
      Top = 586
      Width = 150
      Height = 17
      Caption = 'CheckBox28'
      Enabled = False
      TabOrder = 27
      Visible = False
      OnClick = CheckBox28Click
    end
    object CheckBox29: TCheckBox
      Left = 24
      Top = 606
      Width = 150
      Height = 17
      Caption = 'CheckBox29'
      Enabled = False
      TabOrder = 28
      Visible = False
      OnClick = CheckBox29Click
    end
    object CheckBox30: TCheckBox
      Left = 24
      Top = 626
      Width = 150
      Height = 17
      Caption = 'CheckBox30'
      Enabled = False
      TabOrder = 29
      Visible = False
      OnClick = CheckBox30Click
    end
    object CheckBox31: TCheckBox
      Left = 24
      Top = 646
      Width = 150
      Height = 17
      Caption = 'CheckBox31'
      Enabled = False
      TabOrder = 30
      Visible = False
      OnClick = CheckBox31Click
    end
    object CheckBox32: TCheckBox
      Left = 24
      Top = 666
      Width = 150
      Height = 17
      Caption = 'CheckBox32'
      Enabled = False
      TabOrder = 31
      Visible = False
      OnClick = CheckBox32Click
    end
    object CheckBoxAll: TCheckBox
      Left = 8
      Top = 24
      Width = 97
      Height = 17
      Caption = 'All'
      TabOrder = 32
      OnClick = CheckBoxAllClick
    end
  end
end
