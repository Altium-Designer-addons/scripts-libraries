object FormFilterObjects: TFormFilterObjects
  Left = 6
  Top = 3
  Width = 319
  Height = 413
  AutoScroll = True
  Caption = 'Filter Objects'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxObjects: TGroupBox
    Left = 8
    Top = 8
    Width = 144
    Height = 360
    Caption = 'Objects'
    DockSite = True
    DragMode = dmAutomatic
    TabOrder = 0
    object CheckBoxFills: TCheckBox
      Left = 16
      Top = 184
      Width = 97
      Height = 17
      Caption = 'Fills'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBoxRegions: TCheckBox
      Left = 16
      Top = 160
      Width = 97
      Height = 17
      Caption = 'Solid Regions'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBoxStrings: TCheckBox
      Left = 16
      Top = 136
      Width = 97
      Height = 17
      Caption = 'Strings'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object CheckBoxPads: TCheckBox
      Left = 16
      Top = 112
      Width = 97
      Height = 17
      Caption = 'Pads'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBoxVias: TCheckBox
      Left = 16
      Top = 88
      Width = 97
      Height = 17
      Caption = 'Vias'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBoxArcs: TCheckBox
      Left = 16
      Top = 64
      Width = 97
      Height = 17
      Caption = 'Arcs'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object CheckBoxTracks: TCheckBox
      Left = 16
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Tracks'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object CheckBoxComponents: TCheckBox
      Left = 16
      Top = 280
      Width = 97
      Height = 17
      Caption = 'Components'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = CheckBoxComponentsClick
    end
    object CheckBoxPolygons: TCheckBox
      Left = 16
      Top = 304
      Width = 97
      Height = 17
      Caption = 'Polygons'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = CheckBoxPolygonsClick
    end
    object CheckBoxDimensions: TCheckBox
      Left = 16
      Top = 328
      Width = 97
      Height = 17
      Caption = 'Dimensions'
      Checked = True
      State = cbChecked
      TabOrder = 9
      OnClick = CheckBoxDimensionsClick
    end
    object CheckBoxConnections: TCheckBox
      Left = 16
      Top = 232
      Width = 97
      Height = 17
      Caption = 'Connection Lines'
      Checked = True
      State = cbChecked
      TabOrder = 10
    end
    object CheckBoxRooms: TCheckBox
      Left = 16
      Top = 208
      Width = 97
      Height = 17
      Caption = 'Rooms'
      Checked = True
      State = cbChecked
      TabOrder = 11
    end
    object CheckBoxAllObjects: TCheckBox
      Left = 8
      Top = 16
      Width = 32
      Height = 17
      BiDiMode = bdLeftToRight
      Caption = 'All'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 12
      OnClick = CheckBoxAllObjectsClick
    end
  end
  object GroupBoxLayers: TGroupBox
    Left = 160
    Top = 8
    Width = 136
    Height = 256
    Caption = 'Layers'
    TabOrder = 1
    object CheckBoxMid: TCheckBox
      Left = 16
      Top = 64
      Width = 96
      Height = 17
      Caption = 'Mid Layers'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBoxPlane: TCheckBox
      Left = 16
      Top = 112
      Width = 97
      Height = 17
      Caption = 'Internal Planes'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBoxMech: TCheckBox
      Left = 16
      Top = 136
      Width = 97
      Height = 17
      Caption = 'Mech Layers'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object CheckBoxOverlay: TCheckBox
      Left = 16
      Top = 160
      Width = 97
      Height = 17
      Caption = 'Overlay Layers'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBoxSolder: TCheckBox
      Left = 16
      Top = 184
      Width = 97
      Height = 17
      Caption = 'Solder Layers'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBoxPaste: TCheckBox
      Left = 16
      Top = 208
      Width = 97
      Height = 17
      Caption = 'Paste Layers'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object CheckBoxDrill: TCheckBox
      Left = 16
      Top = 232
      Width = 97
      Height = 17
      Caption = 'Drill Layers'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object CheckBoxAllLayers: TCheckBox
      Left = 7
      Top = 16
      Width = 33
      Height = 17
      BiDiMode = bdLeftToRight
      Caption = 'All'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 7
      OnClick = CheckBoxAllLayersClick
    end
    object CheckBoxTop: TCheckBox
      Left = 16
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Top Layer'
      Checked = True
      State = cbChecked
      TabOrder = 8
    end
    object CheckBoxBottom: TCheckBox
      Left = 16
      Top = 88
      Width = 97
      Height = 17
      Caption = 'Bottom Layer'
      Checked = True
      State = cbChecked
      TabOrder = 9
    end
  end
  object ButtonOK: TButton
    Left = 216
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Apply'
    Default = True
    TabOrder = 2
    OnClick = ButtonOKClick
  end
  object GroupBoxOptions: TGroupBox
    Left = 160
    Top = 272
    Width = 136
    Height = 64
    Caption = 'Advanced Options'
    TabOrder = 3
    object CheckBoxWithinArea: TCheckBox
      Left = 8
      Top = 16
      Width = 104
      Height = 17
      Caption = 'Within Area'
      TabOrder = 0
      OnClick = CheckBoxWithinAreaClick
    end
    object CheckBoxCurrentLayer: TCheckBox
      Left = 8
      Top = 39
      Width = 120
      Height = 17
      Caption = 'Current Layer Only'
      TabOrder = 1
      OnClick = CheckBoxCurrentLayerClick
    end
  end
end
