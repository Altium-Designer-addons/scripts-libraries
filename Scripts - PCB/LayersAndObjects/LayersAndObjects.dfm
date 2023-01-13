object FormLayersPanel: TFormLayersPanel
  Left = 0
  Top = 0
  Caption = 'Layers & Objects'
  ClientHeight = 747
  ClientWidth = 810
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormLayersPanelActivate
  OnClose = FormLayersPanelClose
  OnDeactivate = FormLayersPanelDeactivate
  OnMouseEnter = FormLayersPanelMouseEnter
  OnMouseLeave = FormLayersPanelMouseLeave
  OnResize = FormLayersPanelResize
  OnShow = FormLayersPanelShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelVersion: TLabel
    Left = 4
    Top = 1
    Width = 54
    Height = 13
    Caption = 'version X.X'
  end
  object GroupBoxCopper: TGroupBox
    Left = 16
    Top = 16
    Width = 245
    Height = 1016
    Color = clBtnFace
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    OnMouseEnter = GroupBoxCopperMouseEnter
    object Image1: TImage
      Left = 1
      Top = 0
      Width = 243
      Height = 22
      Picture.Data = {
        1154436F6D707265737365644269746D617078DAEDD8595293511445E1380B86
        E00832009E78718E2641B06F501A51141544A3604BA3602F4823515490468DC9
        89EB94B5D5570AA9FDD51F48555ED7BE37956247A1D07EA8D8F8DB546CBC0E36
        DF1C2814DAE25F5BEB835F9FFFAE5EAFFF08DFF10D3B611B5BD80C5FB181F5F0
        059FF1096BE1233EA01656F11E2B61194B580CEFB08079BC0D6FF01AAFC24BBC
        C0F3F00C7398C5D3F00433980E5398C4E3F0080FF120DCC73D4C603CDCC51D54
        C36DDCC258B889518C841BB88E6B180E57710543E1322E61305CC400FAD1177A
        7101E7430FCEE16C3883D338154EE2048EE358388A6E748523E844259451C2E1
        E0FEDDBFFB77FFEEDFFDBB7FF7FF3FF6AFE597CB8DA7D27C4A9552A951FC9FE5
        BB7FF7EFF3DFFDBB7FF7EFFEDDBFFB77FFEEDFFDBB7FF7EFFEDDBFFB77FFEEDF
        FDFBF74FF7EFFE7DFEBB7FF7EFFEDDBFFB77FFEEDFFDFBFBBFFB77FF3EFFDDBF
        FB77FFEEDFFDBB7FF7EFFEDDBFFB77FFEEDFFDBB7FF7EFFEDDFF1EEF7FB556DB
        B509AC412650834C6005328145C804A4FF9C80F49F1390FE7302FBBEFF6148FF
        4390FE0721FD4BFCD9BFC49FFD4BFCD9BFC49FFDEF42FCADE75F13D8814C6013
        328175C804E40AC809C8159013902B20272057404E40AE00E93F2720FDE704A4
        FF9C80F49F1390FE7302D27F4E40FACF0948FFE390FEAB90FEC720FD8F40FA97
        C33FFB97C33FFB97C33FFB97C35FE2CFFE25FEEC5FE2CFFE25FEEC5FE2CFFE25
        FEEC5FE2CFFEFF1AFF4F4A554CBB}
      Stretch = True
    end
    object Label1: TLabel
      Left = 8
      Top = 4
      Width = 81
      Height = 13
      Caption = 'Copper Layers'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ImageArrowDownCopper: TImage
      Left = 224
      Top = 2
      Width = 19
      Height = 19
      Enabled = False
      Picture.Data = {
        1154436F6D707265737365644269746D617078DA9D944B4EC330108683C40DD8
        7004B66C380062CB69E03834F1D84E137543DC90B2A7B1522ED04A3D42E10E65
        6CC756B013D3F26B1CE5E1CF331ECF64719924F78F0BBC2ADDE1B8C171D58F8B
        E45ABF7FEEBF0F559665511473ADDC8A538AC6940103A0685A6045080959CE18
        07B2AA974D5539A3E98C66292524C27200A436B2DDEFB6DF5F076378DFAD3F9A
        EA95CED40AA36C0EF0D975863AFE965B0171C8328FCD690F1EA7A5F0760D693A
        64E79CBFD775083E3DDC86F89B10883B169D6EA4F44245D09817BC8EFC052337
        6CE8D451218E33316F8E5D896AE8D49BEF3DE2CC081BD9EF89ECE87E43F6DCFD
        AA545BD6CB7364BF2ACFFA88FF3CDFD1F2304EFF5357527A75756A3D4B39D50B
        B13E6ADB4608079ED1BF4BA4326CDEA9DE37F2FF1E564C8B5A191CD91F17653F
        A2}
      Visible = False
      OnClick = ImageArrowDownCopperClick
    end
    object ImageArrowUpCopper: TImage
      Left = 224
      Top = 2
      Width = 19
      Height = 19
      Picture.Data = {
        1154436F6D707265737365644269746D617078DA9D945152C3201086E38C37F0
        C523F8EA8B07E8F8EA69F43836610969327D3134A6BEB761522FA0331E41BD43
        5D20EC4430B4F59F2593001FCB2E6C96E74932BB5BE253EB06DB15B68BA19D25
        97A6FF61181FABAAAAB22C17468593E01C2DD706390047330227C658C88A3C17
        C0D6CDAAAD6B329ECE799672C622AC00406AA7BA8FF7B7EFAF4F6BF8DE6F376D
        FDC4E77A853FD902E0B5EF2DB5FF2D5A0171C8328F2DF800EEA7A5F16E0B693A
        661742BC344D1C24FC594AC48945A73BA568ABF7B7D768347FFC8973CCCE1F71
        E7960D9DD27C6F1DEB1AF346EC5AD6617E2CE581D6F541D6E261679C8DC41BB2
        A7C6AB53ED582FCF9178759ECD111F3CDF305E72FA9F7BA59477AF8EBDCF4A4D
        D542AC8EBAAE9592C013EA77855486C53B55FB56FEDFC32937E24E1647F6070B
        333FA2}
      OnClick = ImageArrowUpCopperClick
    end
    object ShapeCopper1: TShape
      Left = 16
      Top = 50
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper1MouseDown
    end
    object ShapeCopper2: TShape
      Left = 16
      Top = 70
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper2MouseDown
    end
    object ShapeCopper3: TShape
      Left = 16
      Top = 90
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper3MouseDown
    end
    object ShapeCopper4: TShape
      Left = 16
      Top = 110
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper4MouseDown
    end
    object ShapeCopper5: TShape
      Left = 16
      Top = 130
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper5MouseDown
    end
    object ShapeCopper6: TShape
      Left = 16
      Top = 150
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper6MouseDown
    end
    object ShapeCopper7: TShape
      Left = 16
      Top = 170
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper7MouseDown
    end
    object ShapeCopper8: TShape
      Left = 16
      Top = 190
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper8MouseDown
    end
    object ShapeCopper16: TShape
      Left = 16
      Top = 350
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper16MouseDown
    end
    object ShapeCopper15: TShape
      Left = 16
      Top = 330
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper15MouseDown
    end
    object ShapeCopper14: TShape
      Left = 16
      Top = 310
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper14MouseDown
    end
    object ShapeCopper13: TShape
      Left = 16
      Top = 290
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper13MouseDown
    end
    object ShapeCopper12: TShape
      Left = 16
      Top = 270
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper12MouseDown
    end
    object ShapeCopper11: TShape
      Left = 16
      Top = 250
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper11MouseDown
    end
    object ShapeCopper10: TShape
      Left = 16
      Top = 230
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper10MouseDown
    end
    object ShapeCopper9: TShape
      Left = 16
      Top = 210
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper9MouseDown
    end
    object ShapeCopper24: TShape
      Left = 16
      Top = 510
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper24MouseDown
    end
    object ShapeCopper23: TShape
      Left = 16
      Top = 490
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper23MouseDown
    end
    object ShapeCopper22: TShape
      Left = 16
      Top = 470
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper22MouseDown
    end
    object ShapeCopper21: TShape
      Left = 16
      Top = 450
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper21MouseDown
    end
    object ShapeCopper20: TShape
      Left = 16
      Top = 430
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper20MouseDown
    end
    object ShapeCopper19: TShape
      Left = 16
      Top = 410
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper19MouseDown
    end
    object ShapeCopper18: TShape
      Left = 16
      Top = 390
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper18MouseDown
    end
    object ShapeCopper17: TShape
      Left = 16
      Top = 370
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper17MouseDown
    end
    object ShapeCopper32: TShape
      Left = 16
      Top = 670
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper32MouseDown
    end
    object ShapeCopper31: TShape
      Left = 16
      Top = 650
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper31MouseDown
    end
    object ShapeCopper30: TShape
      Left = 16
      Top = 630
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper30MouseDown
    end
    object ShapeCopper29: TShape
      Left = 16
      Top = 610
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper29MouseDown
    end
    object ShapeCopper28: TShape
      Left = 16
      Top = 590
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper28MouseDown
    end
    object ShapeCopper27: TShape
      Left = 16
      Top = 570
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper27MouseDown
    end
    object ShapeCopper26: TShape
      Left = 16
      Top = 550
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper26MouseDown
    end
    object ShapeCopper25: TShape
      Left = 16
      Top = 530
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper25MouseDown
    end
    object ShapeCopper40: TShape
      Left = 16
      Top = 830
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper40MouseDown
    end
    object ShapeCopper39: TShape
      Left = 16
      Top = 810
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper39MouseDown
    end
    object ShapeCopper38: TShape
      Left = 16
      Top = 790
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper38MouseDown
    end
    object ShapeCopper37: TShape
      Left = 16
      Top = 770
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper37MouseDown
    end
    object ShapeCopper36: TShape
      Left = 16
      Top = 750
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper36MouseDown
    end
    object ShapeCopper35: TShape
      Left = 16
      Top = 730
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper35MouseDown
    end
    object ShapeCopper34: TShape
      Left = 16
      Top = 710
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper34MouseDown
    end
    object ShapeCopper33: TShape
      Left = 16
      Top = 690
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper33MouseDown
    end
    object ShapeCopper48: TShape
      Left = 16
      Top = 990
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper48MouseDown
    end
    object ShapeCopper47: TShape
      Left = 16
      Top = 970
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper47MouseDown
    end
    object ShapeCopper46: TShape
      Left = 16
      Top = 950
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper46MouseDown
    end
    object ShapeCopper45: TShape
      Left = 16
      Top = 930
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper45MouseDown
    end
    object ShapeCopper44: TShape
      Left = 16
      Top = 910
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper44MouseDown
    end
    object ShapeCopper43: TShape
      Left = 16
      Top = 890
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper43MouseDown
    end
    object ShapeCopper42: TShape
      Left = 16
      Top = 870
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper42MouseDown
    end
    object ShapeCopper41: TShape
      Left = 16
      Top = 850
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeCopper41MouseDown
    end
    object CBCopper1: TCheckBox
      Left = 32
      Top = 48
      Width = 136
      Height = 17
      Caption = 'CBCopper1'
      Enabled = False
      TabOrder = 0
      Visible = False
      OnClick = CBCopper1Click
    end
    object CBCopperAll: TCheckBox
      Left = 208
      Top = 26
      Width = 28
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'All'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 1
      OnClick = CBCopperAllClick
    end
    object CBCopperSelected: TCheckBox
      Left = 144
      Top = 26
      Width = 57
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'Visible'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 2
      OnClick = CBCopperSelectedClick
    end
    object CBCopper2: TCheckBox
      Left = 32
      Top = 68
      Width = 136
      Height = 17
      Caption = 'CBCopper2'
      Enabled = False
      TabOrder = 3
      Visible = False
      OnClick = CBCopper2Click
    end
    object CBCopper4: TCheckBox
      Left = 32
      Top = 108
      Width = 136
      Height = 17
      Caption = 'CBCopper4'
      Enabled = False
      TabOrder = 4
      Visible = False
      OnClick = CBCopper4Click
    end
    object CBCopper3: TCheckBox
      Left = 32
      Top = 88
      Width = 136
      Height = 17
      Caption = 'CBCopper3'
      Enabled = False
      TabOrder = 5
      Visible = False
      OnClick = CBCopper3Click
    end
    object CBCopper8: TCheckBox
      Left = 32
      Top = 188
      Width = 136
      Height = 17
      Caption = 'CBCopper8'
      Enabled = False
      TabOrder = 6
      Visible = False
      OnClick = CBCopper8Click
    end
    object CBCopper7: TCheckBox
      Left = 32
      Top = 168
      Width = 136
      Height = 17
      Caption = 'CBCopper7'
      Enabled = False
      TabOrder = 7
      Visible = False
      OnClick = CBCopper7Click
    end
    object CBCopper6: TCheckBox
      Left = 32
      Top = 148
      Width = 136
      Height = 17
      Caption = 'CBCopper6'
      Enabled = False
      TabOrder = 8
      Visible = False
      OnClick = CBCopper6Click
    end
    object CBCopper5: TCheckBox
      Left = 32
      Top = 128
      Width = 136
      Height = 17
      Caption = 'CBCopper5'
      Enabled = False
      TabOrder = 9
      Visible = False
      OnClick = CBCopper5Click
    end
    object CBCopper9: TCheckBox
      Left = 32
      Top = 208
      Width = 136
      Height = 17
      Caption = 'CBCopper9'
      Enabled = False
      TabOrder = 10
      Visible = False
      OnClick = CBCopper9Click
    end
    object CBCopper10: TCheckBox
      Left = 32
      Top = 228
      Width = 136
      Height = 17
      Caption = 'CBCopper10'
      Enabled = False
      TabOrder = 11
      Visible = False
      OnClick = CBCopper10Click
    end
    object CBCopper11: TCheckBox
      Left = 32
      Top = 248
      Width = 136
      Height = 17
      Caption = 'CBCopper11'
      Enabled = False
      TabOrder = 12
      Visible = False
      OnClick = CBCopper11Click
    end
    object CBCopper12: TCheckBox
      Left = 32
      Top = 268
      Width = 136
      Height = 17
      Caption = 'CBCopper12'
      Enabled = False
      TabOrder = 13
      Visible = False
      OnClick = CBCopper12Click
    end
    object CBCopper13: TCheckBox
      Left = 32
      Top = 288
      Width = 136
      Height = 17
      Caption = 'CBCopper13'
      Enabled = False
      TabOrder = 14
      Visible = False
      OnClick = CBCopper13Click
    end
    object CBCopper14: TCheckBox
      Left = 32
      Top = 308
      Width = 136
      Height = 17
      Caption = 'CBCopper14'
      Enabled = False
      TabOrder = 15
      Visible = False
      OnClick = CBCopper14Click
    end
    object CBCopper15: TCheckBox
      Left = 32
      Top = 328
      Width = 136
      Height = 17
      Caption = 'CBCopper15'
      Enabled = False
      TabOrder = 16
      Visible = False
      OnClick = CBCopper15Click
    end
    object CBCopper16: TCheckBox
      Left = 32
      Top = 348
      Width = 136
      Height = 17
      Caption = 'CBCopper16'
      Enabled = False
      TabOrder = 17
      Visible = False
      OnClick = CBCopper16Click
    end
    object CBCopper17: TCheckBox
      Left = 32
      Top = 368
      Width = 136
      Height = 17
      Caption = 'CBCopper17'
      Enabled = False
      TabOrder = 18
      Visible = False
      OnClick = CBCopper17Click
    end
    object CBCopper18: TCheckBox
      Left = 32
      Top = 388
      Width = 136
      Height = 17
      Caption = 'CBCopper18'
      Enabled = False
      TabOrder = 19
      Visible = False
      OnClick = CBCopper18Click
    end
    object CBCopper19: TCheckBox
      Left = 32
      Top = 408
      Width = 136
      Height = 17
      Caption = 'CBCopper19'
      Enabled = False
      TabOrder = 20
      Visible = False
      OnClick = CBCopper19Click
    end
    object CBCopper20: TCheckBox
      Left = 32
      Top = 428
      Width = 136
      Height = 17
      Caption = 'CBCopper20'
      Enabled = False
      TabOrder = 21
      Visible = False
      OnClick = CBCopper20Click
    end
    object CBCopper21: TCheckBox
      Left = 32
      Top = 448
      Width = 136
      Height = 17
      Caption = 'CBCopper21'
      Enabled = False
      TabOrder = 22
      Visible = False
      OnClick = CBCopper21Click
    end
    object CBCopper22: TCheckBox
      Left = 32
      Top = 468
      Width = 136
      Height = 17
      Caption = 'CBCopper22'
      Enabled = False
      TabOrder = 23
      Visible = False
      OnClick = CBCopper22Click
    end
    object CBCopper23: TCheckBox
      Left = 32
      Top = 488
      Width = 136
      Height = 17
      Caption = 'CBCopper23'
      Enabled = False
      TabOrder = 24
      Visible = False
      OnClick = CBCopper23Click
    end
    object CBCopper24: TCheckBox
      Left = 32
      Top = 508
      Width = 136
      Height = 17
      Caption = 'CBCopper24'
      Enabled = False
      TabOrder = 25
      Visible = False
      OnClick = CBCopper24Click
    end
    object CBCopper25: TCheckBox
      Left = 32
      Top = 528
      Width = 136
      Height = 17
      Caption = 'CBCopper25'
      Enabled = False
      TabOrder = 26
      Visible = False
      OnClick = CBCopper25Click
    end
    object CBCopper26: TCheckBox
      Left = 32
      Top = 548
      Width = 136
      Height = 17
      Caption = 'CBCopper26'
      Enabled = False
      TabOrder = 27
      Visible = False
      OnClick = CBCopper26Click
    end
    object CBCopper27: TCheckBox
      Left = 32
      Top = 568
      Width = 136
      Height = 17
      Caption = 'CBCopper27'
      Enabled = False
      TabOrder = 28
      Visible = False
      OnClick = CBCopper27Click
    end
    object CBCopper28: TCheckBox
      Left = 32
      Top = 588
      Width = 136
      Height = 17
      Caption = 'CBCopper28'
      Enabled = False
      TabOrder = 29
      Visible = False
      OnClick = CBCopper28Click
    end
    object CBCopper29: TCheckBox
      Left = 32
      Top = 608
      Width = 136
      Height = 17
      Caption = 'CBCopper29'
      Enabled = False
      TabOrder = 30
      Visible = False
      OnClick = CBCopper29Click
    end
    object CBCopper30: TCheckBox
      Left = 32
      Top = 628
      Width = 136
      Height = 17
      Caption = 'CBCopper30'
      Enabled = False
      TabOrder = 31
      Visible = False
      OnClick = CBCopper30Click
    end
    object CBCopper31: TCheckBox
      Left = 32
      Top = 648
      Width = 136
      Height = 17
      Caption = 'CBCopper31'
      Enabled = False
      TabOrder = 32
      Visible = False
      OnClick = CBCopper31Click
    end
    object CBCopper32: TCheckBox
      Left = 32
      Top = 668
      Width = 136
      Height = 17
      Caption = 'CBCopper32'
      Enabled = False
      TabOrder = 33
      Visible = False
      OnClick = CBCopper32Click
    end
    object CBCopper33: TCheckBox
      Left = 32
      Top = 688
      Width = 136
      Height = 17
      Caption = 'CBCopper33'
      Enabled = False
      TabOrder = 34
      Visible = False
      OnClick = CBCopper33Click
    end
    object CBCopper34: TCheckBox
      Left = 32
      Top = 708
      Width = 136
      Height = 17
      Caption = 'CBCopper34'
      Enabled = False
      TabOrder = 35
      Visible = False
      OnClick = CBCopper34Click
    end
    object CBCopper35: TCheckBox
      Left = 32
      Top = 728
      Width = 136
      Height = 17
      Caption = 'CBCopper35'
      Enabled = False
      TabOrder = 36
      Visible = False
      OnClick = CBCopper35Click
    end
    object CBCopper36: TCheckBox
      Left = 32
      Top = 748
      Width = 136
      Height = 17
      Caption = 'CBCopper36'
      Enabled = False
      TabOrder = 37
      Visible = False
      OnClick = CBCopper36Click
    end
    object CBCopper37: TCheckBox
      Left = 32
      Top = 768
      Width = 136
      Height = 17
      Caption = 'CBCopper37'
      Enabled = False
      TabOrder = 38
      Visible = False
      OnClick = CBCopper37Click
    end
    object CBCopper38: TCheckBox
      Left = 32
      Top = 788
      Width = 136
      Height = 17
      Caption = 'CBCopper38'
      Enabled = False
      TabOrder = 39
      Visible = False
      OnClick = CBCopper38Click
    end
    object CBCopper39: TCheckBox
      Left = 32
      Top = 808
      Width = 136
      Height = 17
      Caption = 'CBCopper39'
      Enabled = False
      TabOrder = 40
      Visible = False
      OnClick = CBCopper39Click
    end
    object CBCopper40: TCheckBox
      Left = 32
      Top = 828
      Width = 136
      Height = 17
      Caption = 'CBCopper40'
      Enabled = False
      TabOrder = 41
      Visible = False
      OnClick = CBCopper40Click
    end
    object CBCopper41: TCheckBox
      Left = 32
      Top = 848
      Width = 136
      Height = 17
      Caption = 'CBCopper41'
      Enabled = False
      TabOrder = 42
      Visible = False
      OnClick = CBCopper41Click
    end
    object CBCopper42: TCheckBox
      Left = 32
      Top = 868
      Width = 136
      Height = 17
      Caption = 'CBCopper42'
      Enabled = False
      TabOrder = 43
      Visible = False
      OnClick = CBCopper42Click
    end
    object CBCopper43: TCheckBox
      Left = 32
      Top = 888
      Width = 136
      Height = 17
      Caption = 'CBCopper43'
      Enabled = False
      TabOrder = 44
      Visible = False
      OnClick = CBCopper43Click
    end
    object CBCopper44: TCheckBox
      Left = 32
      Top = 908
      Width = 136
      Height = 17
      Caption = 'CBCopper44'
      Enabled = False
      TabOrder = 45
      Visible = False
      OnClick = CBCopper44Click
    end
    object CBCopper45: TCheckBox
      Left = 32
      Top = 928
      Width = 136
      Height = 17
      Caption = 'CBCopper45'
      Enabled = False
      TabOrder = 46
      Visible = False
      OnClick = CBCopper45Click
    end
    object CBCopper46: TCheckBox
      Left = 32
      Top = 948
      Width = 136
      Height = 17
      Caption = 'CBCopper46'
      Enabled = False
      TabOrder = 47
      Visible = False
      OnClick = CBCopper46Click
    end
    object CBCopper47: TCheckBox
      Left = 32
      Top = 968
      Width = 136
      Height = 17
      Caption = 'CBCopper47'
      Enabled = False
      TabOrder = 48
      Visible = False
      OnClick = CBCopper47Click
    end
    object CBCopper48: TCheckBox
      Left = 32
      Top = 988
      Width = 136
      Height = 17
      Caption = 'CBCopper48'
      Enabled = False
      TabOrder = 49
      Visible = False
      OnClick = CBCopper48Click
    end
    object CBSignals: TCheckBox
      Left = 155
      Top = 48
      Width = 81
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'Signal Layers'
      Checked = True
      Enabled = False
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 50
      Visible = False
      OnClick = CBSignalsClick
    end
    object CBPlanes: TCheckBox
      Left = 158
      Top = 68
      Width = 78
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'Plane Layers'
      Checked = True
      Enabled = False
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 51
      Visible = False
      OnClick = CBPlanesClick
    end
  end
  object GroupBoxMech: TGroupBox
    Left = 280
    Top = 16
    Width = 245
    Height = 696
    TabOrder = 1
    OnMouseEnter = GroupBoxMechMouseEnter
    object Image2: TImage
      Left = 1
      Top = 0
      Width = 243
      Height = 22
      Picture.Data = {
        1154436F6D707265737365644269746D617078DAEDD8595293511445E1380B86
        E00832009E78718E2641B06F501A51141544A3604BA3602F4823515490468DC9
        89EB94B5D5570AA9FDD51F48555ED7BE37956247A1D07EA8D8F8DB546CBC0E36
        DF1C2814DAE25F5BEB835F9FFFAE5EAFFF08DFF10D3B611B5BD80C5FB181F5F0
        059FF1096BE1233EA01656F11E2B61194B580CEFB08079BC0D6FF01AAFC24BBC
        C0F3F00C7398C5D3F00433980E5398C4E3F0080FF120DCC73D4C603CDCC51D54
        C36DDCC258B889518C841BB88E6B180E57710543E1322E61305CC400FAD1177A
        7101E7430FCEE16C3883D338154EE2048EE358388A6E748523E844259451C2E1
        E0FEDDBFFB77FFEEDFFDBB7FF7FF3FF6AFE597CB8DA7D27C4A9552A951FC9FE5
        BB7FF7EFF3DFFDBB7FF7EFFEDDBFFB77FFEEDFFDBB7FF7EFFEDDBFFB77FFEEDF
        FDFBF74FF7EFFE7DFEBB7FF7EFFEDDBFFB77FFEEDFFDFBFBBFFB77FF3EFFDDBF
        FB77FFEEDFFDBB7FF7EFFEDDBFFB77FFEEDFFDBB7FF7EFFEDDFF1EEF7FB556DB
        B509AC412650834C6005328145C804A4FF9C80F49F1390FE7302FBBEFF6148FF
        4390FE0721FD4BFCD9BFC49FFD4BFCD9BFC49FFDEF42FCADE75F13D8814C6013
        328175C804E40AC809C8159013902B20272057404E40AE00E93F2720FDE704A4
        FF9C80F49F1390FE7302D27F4E40FACF0948FFE390FEAB90FEC720FD8F40FA97
        C33FFB97C33FFB97C33FFB97C35FE2CFFE25FEEC5FE2CFFE25FEEC5FE2CFFE25
        FEEC5FE2CFFEFF1AFF4F4A554CBB}
      Stretch = True
    end
    object Label2: TLabel
      Left = 8
      Top = 4
      Width = 104
      Height = 13
      Caption = 'Mechanical Layers'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ImageArrowDownMech: TImage
      Left = 224
      Top = 2
      Width = 19
      Height = 19
      Enabled = False
      Picture.Data = {
        1154436F6D707265737365644269746D617078DA9D944B4EC330108683C40DD8
        7004B66C380062CB69E03834F1D84E137543DC90B2A7B1522ED04A3D42E10E65
        6CC756B013D3F26B1CE5E1CF331ECF64719924F78F0BBC2ADDE1B8C171D58F8B
        E45ABF7FEEBF0F559665511473ADDC8A538AC6940103A0685A6045080959CE18
        07B2AA974D5539A3E98C66292524C27200A436B2DDEFB6DF5F076378DFAD3F9A
        EA95CED40AA36C0EF0D975863AFE965B0171C8328FCD690F1EA7A5F0760D693A
        64E79CBFD775083E3DDC86F89B10883B169D6EA4F44245D09817BC8EFC052337
        6CE8D451218E33316F8E5D896AE8D49BEF3DE2CC081BD9EF89ECE87E43F6DCFD
        AA545BD6CB7364BF2ACFFA88FF3CDFD1F2304EFF5357527A75756A3D4B39D50B
        B13E6ADB4608079ED1BF4BA4326CDEA9DE37F2FF1E564C8B5A191CD91F17653F
        A2}
      Visible = False
      OnClick = ImageArrowDownMechClick
    end
    object ImageArrowUpMech: TImage
      Left = 224
      Top = 2
      Width = 19
      Height = 19
      Picture.Data = {
        1154436F6D707265737365644269746D617078DA9D945152C3201086E38C37F0
        C523F8EA8B07E8F8EA69F43836610969327D3134A6BEB761522FA0331E41BD43
        5D20EC4430B4F59F2593001FCB2E6C96E74932BB5BE253EB06DB15B68BA19D25
        97A6FF61181FABAAAAB22C17468593E01C2DD706390047330227C658C88A3C17
        C0D6CDAAAD6B329ECE799672C622AC00406AA7BA8FF7B7EFAF4F6BF8DE6F376D
        FDC4E77A853FD902E0B5EF2DB5FF2D5A0171C8328F2DF800EEA7A5F16E0B693A
        661742BC344D1C24FC594AC48945A73BA568ABF7B7D768347FFC8973CCCE1F71
        E7960D9DD27C6F1DEB1AF346EC5AD6617E2CE581D6F541D6E261679C8DC41BB2
        A7C6AB53ED582FCF9178759ECD111F3CDF305E72FA9F7BA59477AF8EBDCF4A4D
        D542AC8EBAAE9592C013EA77855486C53B55FB56FEDFC32937E24E1647F6070B
        333FA2}
      OnClick = ImageArrowUpMechClick
    end
    object ShapeMech1: TShape
      Left = 16
      Top = 50
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech1MouseDown
    end
    object ShapeMech2: TShape
      Left = 16
      Top = 70
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech2MouseDown
    end
    object ShapeMech3: TShape
      Left = 16
      Top = 90
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech3MouseDown
    end
    object ShapeMech4: TShape
      Left = 16
      Top = 110
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech4MouseDown
    end
    object ShapeMech5: TShape
      Left = 16
      Top = 130
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech5MouseDown
    end
    object ShapeMech6: TShape
      Left = 16
      Top = 150
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech6MouseDown
    end
    object ShapeMech7: TShape
      Left = 16
      Top = 170
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech7MouseDown
    end
    object ShapeMech8: TShape
      Left = 16
      Top = 190
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech8MouseDown
    end
    object ShapeMech16: TShape
      Left = 16
      Top = 350
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech16MouseDown
    end
    object ShapeMech15: TShape
      Left = 16
      Top = 330
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech15MouseDown
    end
    object ShapeMech14: TShape
      Left = 16
      Top = 310
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech14MouseDown
    end
    object ShapeMech13: TShape
      Left = 16
      Top = 290
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech13MouseDown
    end
    object ShapeMech12: TShape
      Left = 16
      Top = 270
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech12MouseDown
    end
    object ShapeMech11: TShape
      Left = 16
      Top = 250
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech11MouseDown
    end
    object ShapeMech10: TShape
      Left = 16
      Top = 230
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech10MouseDown
    end
    object ShapeMech9: TShape
      Left = 16
      Top = 210
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech9MouseDown
    end
    object ShapeMech24: TShape
      Left = 16
      Top = 510
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech24MouseDown
    end
    object ShapeMech23: TShape
      Left = 16
      Top = 490
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech23MouseDown
    end
    object ShapeMech22: TShape
      Left = 16
      Top = 470
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech22MouseDown
    end
    object ShapeMech21: TShape
      Left = 16
      Top = 450
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech21MouseDown
    end
    object ShapeMech20: TShape
      Left = 16
      Top = 430
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech20MouseDown
    end
    object ShapeMech19: TShape
      Left = 16
      Top = 410
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech19MouseDown
    end
    object ShapeMech18: TShape
      Left = 16
      Top = 390
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech18MouseDown
    end
    object ShapeMech17: TShape
      Left = 16
      Top = 370
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech17MouseDown
    end
    object ShapeMech32: TShape
      Left = 16
      Top = 670
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech32MouseDown
    end
    object ShapeMech31: TShape
      Left = 16
      Top = 650
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech31MouseDown
    end
    object ShapeMech30: TShape
      Left = 16
      Top = 630
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech30MouseDown
    end
    object ShapeMech29: TShape
      Left = 16
      Top = 610
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech29MouseDown
    end
    object ShapeMech28: TShape
      Left = 16
      Top = 590
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech28MouseDown
    end
    object ShapeMech27: TShape
      Left = 16
      Top = 570
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech27MouseDown
    end
    object ShapeMech26: TShape
      Left = 16
      Top = 550
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech26MouseDown
    end
    object ShapeMech25: TShape
      Left = 16
      Top = 530
      Width = 13
      Height = 13
      Enabled = False
      Visible = False
      OnMouseDown = ShapeMech25MouseDown
    end
    object CBMech1: TCheckBox
      Left = 32
      Top = 48
      Width = 136
      Height = 17
      Caption = 'CBMech1'
      Enabled = False
      TabOrder = 0
      Visible = False
      OnClick = CBMech1Click
    end
    object CBMechAll: TCheckBox
      Left = 208
      Top = 26
      Width = 28
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'All'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 1
      OnClick = CBMechAllClick
    end
    object CBMechSelected: TCheckBox
      Left = 144
      Top = 26
      Width = 57
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'Visible'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 2
      OnClick = CBMechSelectedClick
    end
    object CBMech2: TCheckBox
      Left = 32
      Top = 68
      Width = 136
      Height = 17
      Caption = 'CBMech2'
      Enabled = False
      TabOrder = 3
      Visible = False
      OnClick = CBMech2Click
    end
    object CBMech4: TCheckBox
      Left = 32
      Top = 108
      Width = 136
      Height = 17
      Caption = 'CBMech4'
      Enabled = False
      TabOrder = 4
      Visible = False
      OnClick = CBMech4Click
    end
    object CBMech3: TCheckBox
      Left = 32
      Top = 88
      Width = 136
      Height = 17
      Caption = 'CBMech3'
      Enabled = False
      TabOrder = 5
      Visible = False
      OnClick = CBMech3Click
    end
    object CBMech8: TCheckBox
      Left = 32
      Top = 188
      Width = 136
      Height = 17
      Caption = 'CBMech8'
      Enabled = False
      TabOrder = 6
      Visible = False
      OnClick = CBMech8Click
    end
    object CBMech7: TCheckBox
      Left = 32
      Top = 168
      Width = 136
      Height = 17
      Caption = 'CBMech7'
      Enabled = False
      TabOrder = 7
      Visible = False
      OnClick = CBMech7Click
    end
    object CBMech6: TCheckBox
      Left = 32
      Top = 148
      Width = 136
      Height = 17
      Caption = 'CBMech6'
      Enabled = False
      TabOrder = 8
      Visible = False
      OnClick = CBMech6Click
    end
    object CBMech5: TCheckBox
      Left = 32
      Top = 128
      Width = 136
      Height = 17
      Caption = 'CBMech5'
      Enabled = False
      TabOrder = 9
      Visible = False
      OnClick = CBMech5Click
    end
    object CBMech9: TCheckBox
      Left = 32
      Top = 208
      Width = 136
      Height = 17
      Caption = 'CBMech9'
      Enabled = False
      TabOrder = 10
      Visible = False
      OnClick = CBMech9Click
    end
    object CBMech10: TCheckBox
      Left = 32
      Top = 228
      Width = 136
      Height = 17
      Caption = 'CBMech10'
      Enabled = False
      TabOrder = 11
      Visible = False
      OnClick = CBMech10Click
    end
    object CBMech11: TCheckBox
      Left = 32
      Top = 248
      Width = 136
      Height = 17
      Caption = 'CBMech11'
      Enabled = False
      TabOrder = 12
      Visible = False
      OnClick = CBMech11Click
    end
    object CBMech12: TCheckBox
      Left = 32
      Top = 268
      Width = 136
      Height = 17
      Caption = 'CBMech12'
      Enabled = False
      TabOrder = 13
      Visible = False
      OnClick = CBMech12Click
    end
    object CBMech13: TCheckBox
      Left = 32
      Top = 288
      Width = 136
      Height = 17
      Caption = 'CBMech13'
      Enabled = False
      TabOrder = 14
      Visible = False
      OnClick = CBMech13Click
    end
    object CBMech14: TCheckBox
      Left = 32
      Top = 308
      Width = 136
      Height = 17
      Caption = 'CBMech14'
      Enabled = False
      TabOrder = 15
      Visible = False
      OnClick = CBMech14Click
    end
    object CBMech15: TCheckBox
      Left = 32
      Top = 328
      Width = 136
      Height = 17
      Caption = 'CBMech15'
      Enabled = False
      TabOrder = 16
      Visible = False
      OnClick = CBMech15Click
    end
    object CBMech16: TCheckBox
      Left = 32
      Top = 348
      Width = 136
      Height = 17
      Caption = 'CBMech16'
      Enabled = False
      TabOrder = 17
      Visible = False
      OnClick = CBMech16Click
    end
    object CBMech17: TCheckBox
      Left = 32
      Top = 368
      Width = 136
      Height = 17
      Caption = 'CBMech17'
      Enabled = False
      TabOrder = 18
      Visible = False
      OnClick = CBMech17Click
    end
    object CBMech18: TCheckBox
      Left = 32
      Top = 388
      Width = 136
      Height = 17
      Caption = 'CBMech18'
      Enabled = False
      TabOrder = 19
      Visible = False
      OnClick = CBMech18Click
      OnMouseEnter = GroupBoxObjectsMouseEnter
    end
    object CBMech19: TCheckBox
      Left = 32
      Top = 408
      Width = 136
      Height = 17
      Caption = 'CBMech19'
      Enabled = False
      TabOrder = 20
      Visible = False
      OnClick = CBMech19Click
    end
    object CBMech20: TCheckBox
      Left = 32
      Top = 428
      Width = 136
      Height = 17
      Caption = 'CBMech20'
      Enabled = False
      TabOrder = 21
      Visible = False
      OnClick = CBMech20Click
    end
    object CBMech21: TCheckBox
      Left = 32
      Top = 448
      Width = 136
      Height = 17
      Caption = 'CBMech21'
      Enabled = False
      TabOrder = 22
      Visible = False
      OnClick = CBMech21Click
    end
    object CBMech22: TCheckBox
      Left = 32
      Top = 468
      Width = 136
      Height = 17
      Caption = 'CBMech22'
      Enabled = False
      TabOrder = 23
      Visible = False
      OnClick = CBMech22Click
    end
    object CBMech23: TCheckBox
      Left = 32
      Top = 488
      Width = 136
      Height = 17
      Caption = 'CBMech23'
      Enabled = False
      TabOrder = 24
      Visible = False
      OnClick = CBMech23Click
    end
    object CBMech24: TCheckBox
      Left = 32
      Top = 508
      Width = 136
      Height = 17
      Caption = 'CBMech24'
      Enabled = False
      TabOrder = 25
      Visible = False
      OnClick = CBMech24Click
    end
    object CBMech25: TCheckBox
      Left = 32
      Top = 528
      Width = 136
      Height = 17
      Caption = 'CBMech25'
      Enabled = False
      TabOrder = 26
      Visible = False
      OnClick = CBMech25Click
    end
    object CBMech26: TCheckBox
      Left = 32
      Top = 548
      Width = 136
      Height = 17
      Caption = 'CBMech26'
      Enabled = False
      TabOrder = 27
      Visible = False
      OnClick = CBMech26Click
    end
    object CBMech27: TCheckBox
      Left = 32
      Top = 568
      Width = 136
      Height = 17
      Caption = 'CBMech27'
      Enabled = False
      TabOrder = 28
      Visible = False
      OnClick = CBMech27Click
    end
    object CBMech28: TCheckBox
      Left = 32
      Top = 588
      Width = 136
      Height = 17
      Caption = 'CBMech28'
      Enabled = False
      TabOrder = 29
      Visible = False
      OnClick = CBMech28Click
    end
    object CBMech29: TCheckBox
      Left = 32
      Top = 608
      Width = 136
      Height = 17
      Caption = 'CBMech29'
      Enabled = False
      TabOrder = 30
      Visible = False
      OnClick = CBMech29Click
    end
    object CBMech30: TCheckBox
      Left = 32
      Top = 628
      Width = 136
      Height = 17
      Caption = 'CBMech30'
      Enabled = False
      TabOrder = 31
      Visible = False
      OnClick = CBMech30Click
    end
    object CBMech31: TCheckBox
      Left = 32
      Top = 648
      Width = 136
      Height = 17
      Caption = 'CBMech31'
      Enabled = False
      TabOrder = 32
      Visible = False
      OnClick = CBMech31Click
    end
    object CBMech32: TCheckBox
      Left = 32
      Top = 668
      Width = 136
      Height = 17
      Caption = 'CBMech32'
      Enabled = False
      TabOrder = 33
      Visible = False
      OnClick = CBMech32Click
    end
    object CBUnPaired: TCheckBox
      Left = 176
      Top = 48
      Width = 60
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'Unpaired'
      Checked = True
      Enabled = False
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 34
      Visible = False
      OnClick = CBUnPairedClick
    end
    object CBPaired: TCheckBox
      Left = 188
      Top = 68
      Width = 48
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'Paired'
      Checked = True
      Enabled = False
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 35
      Visible = False
      OnClick = CBPairedClick
    end
  end
  object GroupBoxOther: TGroupBox
    Left = 544
    Top = 16
    Width = 245
    Height = 256
    TabOrder = 2
    OnMouseEnter = GroupBoxOtherMouseEnter
    object Image3: TImage
      Left = 1
      Top = 0
      Width = 243
      Height = 22
      Picture.Data = {
        1154436F6D707265737365644269746D617078DAEDD8595293511445E1380B86
        E00832009E78718E2641B06F501A51141544A3604BA3602F4823515490468DC9
        89EB94B5D5570AA9FDD51F48555ED7BE37956247A1D07EA8D8F8DB546CBC0E36
        DF1C2814DAE25F5BEB835F9FFFAE5EAFFF08DFF10D3B611B5BD80C5FB181F5F0
        059FF1096BE1233EA01656F11E2B61194B580CEFB08079BC0D6FF01AAFC24BBC
        C0F3F00C7398C5D3F00433980E5398C4E3F0080FF120DCC73D4C603CDCC51D54
        C36DDCC258B889518C841BB88E6B180E57710543E1322E61305CC400FAD1177A
        7101E7430FCEE16C3883D338154EE2048EE358388A6E748523E844259451C2E1
        E0FEDDBFFB77FFEEDFFDBB7FF7FF3FF6AFE597CB8DA7D27C4A9552A951FC9FE5
        BB7FF7EFF3DFFDBB7FF7EFFEDDBFFB77FFEEDFFDBB7FF7EFFEDDBFFB77FFEEDF
        FDFBF74FF7EFFE7DFEBB7FF7EFFEDDBFFB77FFEEDFFDFBFBBFFB77FF3EFFDDBF
        FB77FFEEDFFDBB7FF7EFFEDDBFFB77FFEEDFFDBB7FF7EFFEDDFF1EEF7FB556DB
        B509AC412650834C6005328145C804A4FF9C80F49F1390FE7302FBBEFF6148FF
        4390FE0721FD4BFCD9BFC49FFD4BFCD9BFC49FFDEF42FCADE75F13D8814C6013
        328175C804E40AC809C8159013902B20272057404E40AE00E93F2720FDE704A4
        FF9C80F49F1390FE7302D27F4E40FACF0948FFE390FEAB90FEC720FD8F40FA97
        C33FFB97C33FFB97C33FFB97C35FE2CFFE25FEEC5FE2CFFE25FEEC5FE2CFFE25
        FEEC5FE2CFFEFF1AFF4F4A554CBB}
      Stretch = True
    end
    object Label3: TLabel
      Left = 8
      Top = 4
      Width = 73
      Height = 13
      Caption = 'Other Layers'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ImageArrowDownOther: TImage
      Left = 224
      Top = 2
      Width = 19
      Height = 19
      Enabled = False
      Picture.Data = {
        1154436F6D707265737365644269746D617078DA9D944B4EC330108683C40DD8
        7004B66C380062CB69E03834F1D84E137543DC90B2A7B1522ED04A3D42E10E65
        6CC756B013D3F26B1CE5E1CF331ECF64719924F78F0BBC2ADDE1B8C171D58F8B
        E45ABF7FEEBF0F559665511473ADDC8A538AC6940103A0685A6045080959CE18
        07B2AA974D5539A3E98C66292524C27200A436B2DDEFB6DF5F076378DFAD3F9A
        EA95CED40AA36C0EF0D975863AFE965B0171C8328FCD690F1EA7A5F0760D693A
        64E79CBFD775083E3DDC86F89B10883B169D6EA4F44245D09817BC8EFC052337
        6CE8D451218E33316F8E5D896AE8D49BEF3DE2CC081BD9EF89ECE87E43F6DCFD
        AA545BD6CB7364BF2ACFFA88FF3CDFD1F2304EFF5357527A75756A3D4B39D50B
        B13E6ADB4608079ED1BF4BA4326CDEA9DE37F2FF1E564C8B5A191CD91F17653F
        A2}
      Visible = False
      OnClick = ImageArrowDownOtherClick
    end
    object ImageArrowUpOther: TImage
      Left = 224
      Top = 2
      Width = 19
      Height = 19
      Picture.Data = {
        1154436F6D707265737365644269746D617078DA9D945152C3201086E38C37F0
        C523F8EA8B07E8F8EA69F43836610969327D3134A6BEB761522FA0331E41BD43
        5D20EC4430B4F59F2593001FCB2E6C96E74932BB5BE253EB06DB15B68BA19D25
        97A6FF61181FABAAAAB22C17468593E01C2DD706390047330227C658C88A3C17
        C0D6CDAAAD6B329ECE799672C622AC00406AA7BA8FF7B7EFAF4F6BF8DE6F376D
        FDC4E77A853FD902E0B5EF2DB5FF2D5A0171C8328F2DF800EEA7A5F16E0B693A
        661742BC344D1C24FC594AC48945A73BA568ABF7B7D768347FFC8973CCCE1F71
        E7960D9DD27C6F1DEB1AF346EC5AD6617E2CE581D6F541D6E261679C8DC41BB2
        A7C6AB53ED582FCF9178759ECD111F3CDF305E72FA9F7BA59477AF8EBDCF4A4D
        D542AC8EBAAE9592C013EA77855486C53B55FB56FEDFC32937E24E1647F6070B
        333FA2}
      OnClick = ImageArrowUpOtherClick
    end
    object ShapeTopOverlay: TShape
      Left = 16
      Top = 50
      Width = 13
      Height = 13
      OnMouseDown = ShapeTopOverlayMouseDown
    end
    object ShapeBottomOverlay: TShape
      Left = 16
      Top = 70
      Width = 13
      Height = 13
      OnMouseDown = ShapeBottomOverlayMouseDown
    end
    object ShapeTopSolder: TShape
      Left = 16
      Top = 90
      Width = 13
      Height = 13
      OnMouseDown = ShapeTopSolderMouseDown
    end
    object ShapeBottomSolder: TShape
      Left = 16
      Top = 110
      Width = 13
      Height = 13
      OnMouseDown = ShapeBottomSolderMouseDown
    end
    object ShapeTopPaste: TShape
      Left = 16
      Top = 130
      Width = 13
      Height = 13
      OnMouseDown = ShapeTopPasteMouseDown
    end
    object ShapeBottomPaste: TShape
      Left = 16
      Top = 150
      Width = 13
      Height = 13
      OnMouseDown = ShapeBottomPasteMouseDown
    end
    object ShapeDrillGuide: TShape
      Left = 16
      Top = 170
      Width = 13
      Height = 13
      OnMouseDown = ShapeDrillGuideMouseDown
    end
    object ShapeDrillDraw: TShape
      Left = 16
      Top = 190
      Width = 13
      Height = 13
      OnMouseDown = ShapeDrillDrawMouseDown
    end
    object ShapeMultiLayer: TShape
      Left = 16
      Top = 230
      Width = 13
      Height = 13
      OnMouseDown = ShapeMultiLayerMouseDown
    end
    object ShapeKeepOut: TShape
      Left = 16
      Top = 210
      Width = 13
      Height = 13
      OnMouseDown = ShapeKeepOutMouseDown
    end
    object CBTopOverlay: TCheckBox
      Left = 32
      Top = 48
      Width = 75
      Height = 17
      Caption = 'Top Overlay'
      TabOrder = 0
      OnClick = CBTopOverlayClick
    end
    object CBOtherAll: TCheckBox
      Left = 208
      Top = 26
      Width = 28
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'All'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 1
      OnClick = CBOtherAllClick
    end
    object CBOtherSelected: TCheckBox
      Left = 144
      Top = 26
      Width = 57
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'Visible'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 2
      OnClick = CBOtherSelectedClick
    end
    object CBBottomOverlay: TCheckBox
      Left = 32
      Top = 68
      Width = 96
      Height = 17
      Caption = 'Bottom Overlay'
      TabOrder = 3
      OnClick = CBBottomOverlayClick
    end
    object CBBottomSolder: TCheckBox
      Left = 32
      Top = 108
      Width = 110
      Height = 17
      Caption = 'Bottom Solder Mask'
      TabOrder = 4
      OnClick = CBBottomSolderClick
    end
    object CBTopSolder: TCheckBox
      Left = 32
      Top = 88
      Width = 96
      Height = 17
      Caption = 'Top Solder Mask'
      TabOrder = 5
      OnClick = CBTopSolderClick
    end
    object CBDrillDraw: TCheckBox
      Left = 32
      Top = 188
      Width = 88
      Height = 17
      Caption = 'Drill Drawing'
      TabOrder = 6
      OnClick = CBDrillDrawClick
    end
    object CBDrillGuide: TCheckBox
      Left = 32
      Top = 168
      Width = 80
      Height = 17
      Caption = 'Drill Guide'
      TabOrder = 7
      OnClick = CBDrillGuideClick
    end
    object CBBottomPaste: TCheckBox
      Left = 32
      Top = 148
      Width = 88
      Height = 17
      Caption = 'Bottom Paste'
      TabOrder = 8
      OnClick = CBBottomPasteClick
    end
    object CBTopPaste: TCheckBox
      Left = 32
      Top = 128
      Width = 80
      Height = 17
      Caption = 'Top Paste'
      TabOrder = 9
      OnClick = CBTopPasteClick
    end
    object CBKeepOut: TCheckBox
      Left = 32
      Top = 208
      Width = 96
      Height = 17
      Caption = 'Keep Out Layer'
      TabOrder = 10
      OnClick = CBKeepOutClick
    end
    object CBMultiLayer: TCheckBox
      Left = 32
      Top = 228
      Width = 72
      Height = 17
      Caption = 'Multi Layer'
      TabOrder = 11
      OnClick = CBMultiLayerClick
    end
    object CBOverlay: TCheckBox
      Left = 182
      Top = 48
      Width = 54
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'Overlay'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 12
      OnClick = CBOverlayClick
    end
    object CBMask: TCheckBox
      Left = 196
      Top = 68
      Width = 40
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'Mask'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 13
      OnClick = CBMaskClick
    end
    object CBDrill: TCheckBox
      Left = 203
      Top = 88
      Width = 33
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'Drill'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 14
      OnClick = CBDrillClick
    end
    object CBOther: TCheckBox
      Left = 192
      Top = 108
      Width = 44
      Height = 17
      BiDiMode = bdRightToLeft
      Caption = 'Other'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 15
      OnClick = CBOtherClick
    end
  end
  object GroupBoxObjects: TGroupBox
    Left = 544
    Top = 280
    Width = 245
    Height = 196
    TabOrder = 3
    OnMouseEnter = GroupBoxObjectsMouseEnter
    object Image4: TImage
      Left = 1
      Top = 0
      Width = 243
      Height = 22
      Picture.Data = {
        1154436F6D707265737365644269746D617078DAEDD8595293511445E1380B86
        E00832009E78718E2641B06F501A51141544A3604BA3602F4823515490468DC9
        89EB94B5D5570AA9FDD51F48555ED7BE37956247A1D07EA8D8F8DB546CBC0E36
        DF1C2814DAE25F5BEB835F9FFFAE5EAFFF08DFF10D3B611B5BD80C5FB181F5F0
        059FF1096BE1233EA01656F11E2B61194B580CEFB08079BC0D6FF01AAFC24BBC
        C0F3F00C7398C5D3F00433980E5398C4E3F0080FF120DCC73D4C603CDCC51D54
        C36DDCC258B889518C841BB88E6B180E57710543E1322E61305CC400FAD1177A
        7101E7430FCEE16C3883D338154EE2048EE358388A6E748523E844259451C2E1
        E0FEDDBFFB77FFEEDFFDBB7FF7FF3FF6AFE597CB8DA7D27C4A9552A951FC9FE5
        BB7FF7EFF3DFFDBB7FF7EFFEDDBFFB77FFEEDFFDBB7FF7EFFEDDBFFB77FFEEDF
        FDFBF74FF7EFFE7DFEBB7FF7EFFEDDBFFB77FFEEDFFDFBFBBFFB77FF3EFFDDBF
        FB77FFEEDFFDBB7FF7EFFEDDBFFB77FFEEDFFDBB7FF7EFFEDDFF1EEF7FB556DB
        B509AC412650834C6005328145C804A4FF9C80F49F1390FE7302FBBEFF6148FF
        4390FE0721FD4BFCD9BFC49FFD4BFCD9BFC49FFDEF42FCADE75F13D8814C6013
        328175C804E40AC809C8159013902B20272057404E40AE00E93F2720FDE704A4
        FF9C80F49F1390FE7302D27F4E40FACF0948FFE390FEAB90FEC720FD8F40FA97
        C33FFB97C33FFB97C33FFB97C35FE2CFFE25FEEC5FE2CFFE25FEEC5FE2CFFE25
        FEEC5FE2CFFEFF1AFF4F4A554CBB}
      Stretch = True
    end
    object ImageArrowDownObjects: TImage
      Left = 224
      Top = 2
      Width = 19
      Height = 19
      Enabled = False
      Picture.Data = {
        1154436F6D707265737365644269746D617078DA9D944B4EC330108683C40DD8
        7004B66C380062CB69E03834F1D84E137543DC90B2A7B1522ED04A3D42E10E65
        6CC756B013D3F26B1CE5E1CF331ECF64719924F78F0BBC2ADDE1B8C171D58F8B
        E45ABF7FEEBF0F559665511473ADDC8A538AC6940103A0685A6045080959CE18
        07B2AA974D5539A3E98C66292524C27200A436B2DDEFB6DF5F076378DFAD3F9A
        EA95CED40AA36C0EF0D975863AFE965B0171C8328FCD690F1EA7A5F0760D693A
        64E79CBFD775083E3DDC86F89B10883B169D6EA4F44245D09817BC8EFC052337
        6CE8D451218E33316F8E5D896AE8D49BEF3DE2CC081BD9EF89ECE87E43F6DCFD
        AA545BD6CB7364BF2ACFFA88FF3CDFD1F2304EFF5357527A75756A3D4B39D50B
        B13E6ADB4608079ED1BF4BA4326CDEA9DE37F2FF1E564C8B5A191CD91F17653F
        A2}
      Visible = False
      OnClick = ImageArrowDownObjectsClick
    end
    object Label4: TLabel
      Left = 8
      Top = 4
      Width = 43
      Height = 13
      Caption = 'Objects'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ImageArrowUpObjects: TImage
      Left = 224
      Top = 2
      Width = 19
      Height = 19
      Picture.Data = {
        1154436F6D707265737365644269746D617078DA9D945152C3201086E38C37F0
        C523F8EA8B07E8F8EA69F43836610969327D3134A6BEB761522FA0331E41BD43
        5D20EC4430B4F59F2593001FCB2E6C96E74932BB5BE253EB06DB15B68BA19D25
        97A6FF61181FABAAAAB22C17468593E01C2DD706390047330227C658C88A3C17
        C0D6CDAAAD6B329ECE799672C622AC00406AA7BA8FF7B7EFAF4F6BF8DE6F376D
        FDC4E77A853FD902E0B5EF2DB5FF2D5A0171C8328F2DF800EEA7A5F16E0B693A
        661742BC344D1C24FC594AC48945A73BA568ABF7B7D768347FFC8973CCCE1F71
        E7960D9DD27C6F1DEB1AF346EC5AD6617E2CE581D6F541D6E261679C8DC41BB2
        A7C6AB53ED582FCF9178759ECD111F3CDF305E72FA9F7BA59477AF8EBDCF4A4D
        D542AC8EBAAE9592C013EA77855486C53B55FB56FEDFC32937E24E1647F6070B
        333FA2}
      OnClick = ImageArrowUpObjectsClick
    end
    object CBFills: TCheckBox
      Left = 16
      Top = 168
      Width = 40
      Height = 17
      AllowGrayed = True
      Caption = 'Fills'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBFillsClick
    end
    object CBRegions: TCheckBox
      Left = 16
      Top = 148
      Width = 64
      Height = 17
      AllowGrayed = True
      Caption = 'Regions'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 1
      OnClick = CBRegionsClick
    end
    object CBStrings: TCheckBox
      Left = 96
      Top = 48
      Width = 48
      Height = 17
      AllowGrayed = True
      Caption = 'Strings'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CBStringsClick
    end
    object CBPads: TCheckBox
      Left = 16
      Top = 88
      Width = 48
      Height = 17
      AllowGrayed = True
      Caption = 'Pads'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CBPadsClick
    end
    object CBVias: TCheckBox
      Left = 16
      Top = 108
      Width = 40
      Height = 17
      AllowGrayed = True
      Caption = 'Vias'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CBViasClick
    end
    object CBArcs: TCheckBox
      Left = 16
      Top = 68
      Width = 48
      Height = 17
      AllowGrayed = True
      Caption = 'Arcs'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = CBArcsClick
    end
    object CBTracks: TCheckBox
      Left = 16
      Top = 48
      Width = 48
      Height = 17
      AllowGrayed = True
      Caption = 'Tracks'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = CBTracksClick
    end
    object CBComponents: TCheckBox
      Left = 96
      Top = 88
      Width = 80
      Height = 17
      AllowGrayed = True
      Caption = 'Components'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 7
      OnClick = CBComponentsClick
    end
    object CBPolygons: TCheckBox
      Left = 16
      Top = 128
      Width = 64
      Height = 17
      AllowGrayed = True
      Caption = 'Polygons'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = CBPolygonsClick
    end
    object CBDimensions: TCheckBox
      Left = 96
      Top = 128
      Width = 80
      Height = 17
      AllowGrayed = True
      Caption = 'Dimensions'
      Checked = True
      State = cbChecked
      TabOrder = 9
      OnClick = CBDimensionsClick
    end
    object CBConnections: TCheckBox
      Left = 96
      Top = 168
      Width = 97
      Height = 17
      Caption = 'Connection Lines'
      Checked = True
      State = cbChecked
      TabOrder = 10
      OnClick = CBConnectionsClick
    end
    object CBRooms: TCheckBox
      Left = 96
      Top = 108
      Width = 56
      Height = 17
      AllowGrayed = True
      Caption = 'Rooms'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 11
      OnClick = CBRoomsClick
    end
    object CBObjectsAll: TCheckBox
      Left = 208
      Top = 26
      Width = 28
      Height = 17
      AllowGrayed = True
      BiDiMode = bdRightToLeft
      Caption = 'All'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 12
      OnClick = CBObjectsAllClick
    end
    object CBShowObjects: TCheckBox
      Left = 144
      Top = 26
      Width = 57
      Height = 17
      AllowGrayed = True
      BiDiMode = bdRightToLeft
      Caption = 'Visible'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 13
      OnClick = CBShowObjectsClick
    end
    object CB3DModels: TCheckBox
      Left = 96
      Top = 68
      Width = 64
      Height = 17
      AllowGrayed = True
      Caption = '3D Models'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 14
      OnClick = CB3DModelsClick
    end
    object CBCoordinates: TCheckBox
      Left = 96
      Top = 148
      Width = 80
      Height = 17
      AllowGrayed = True
      Caption = 'Coordinates'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 15
      OnClick = CBCoordinatesClick
    end
  end
end
