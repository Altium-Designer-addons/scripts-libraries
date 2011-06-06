object IBISEditor: TIBISEditor
  Left = 0
  Top = 0
  Caption = 'IBIS Editor'
  ClientHeight = 742
  ClientWidth = 562
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 530
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = IBISEditorCreate
  OnResize = IBISEditorResize
  PixelsPerInch = 96
  TextHeight = 13
  object LabelOpen: TLabel
    Left = 128
    Top = 14
    Width = 85
    Height = 13
    Caption = 'No Model Opened'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LabelComponent: TLabel
    Left = 16
    Top = 40
    Width = 94
    Height = 13
    Caption = 'Choose Component'
    Enabled = False
  end
  object ButtonOpen: TButton
    Left = 8
    Top = 8
    Width = 104
    Height = 25
    Caption = 'Open Base Model'
    TabOrder = 0
    OnClick = ButtonOpenClick
  end
  object ComboBoxComponent: TComboBox
    Left = 120
    Top = 36
    Width = 145
    Height = 21
    Style = csDropDownList
    Enabled = False
    TabOrder = 1
    OnSelect = ComboBoxComponentSelect
  end
  object ButtonSave: TButton
    Left = 456
    Top = 48
    Width = 99
    Height = 24
    Align = alCustom
    Caption = 'Save IBIS File'
    Enabled = False
    TabOrder = 2
    OnClick = ButtonSaveClick
  end
  object WinXPTabControl: TWinXPTabControl
    Left = 0
    Top = 72
    Width = 562
    Height = 670
    Align = alBottom
    Enabled = False
    TabOrder = 3
    OnChange = WinXPTabControlChange
    Tabs.Strings = (
      'Table'
      'Advanced')
    TabIndex = 0
    object StringGridPins: TStringGrid
      Left = 4
      Top = 22
      Width = 554
      Height = 644
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alClient
      ColCount = 4
      DefaultColWidth = 136
      DefaultRowHeight = 21
      Enabled = False
      FixedCols = 0
      RowCount = 30
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goTabs]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = StringGridPinsClick
      ColWidths = (
        58
        103
        125
        246)
    end
    object GroupBoxOverride: TGroupBox
      Left = 8
      Top = 40
      Width = 288
      Height = 152
      Caption = 'Override'
      TabOrder = 2
      Visible = False
      object CheckBoxSubmodel: TCheckBox
        Left = 16
        Top = 48
        Width = 144
        Height = 17
        Caption = '[Add Submodel] Override '
        TabOrder = 0
        OnClick = CheckBoxSubmodelClick
      end
      object RadioGroupPinModels: TRadioGroup
        Left = 16
        Top = 72
        Width = 112
        Height = 64
        Caption = 'Pin Models'
        TabOrder = 1
      end
      object RadioButtonInputs: TRadioButton
        Left = 24
        Top = 88
        Width = 72
        Height = 17
        Caption = 'Input Pins'
        Checked = True
        Enabled = False
        TabOrder = 2
        TabStop = True
      end
      object RadioButtonOutputs: TRadioButton
        Left = 24
        Top = 112
        Width = 96
        Height = 17
        Caption = 'Output Pins'
        Enabled = False
        TabOrder = 3
      end
      object CheckBoxModelSelector: TCheckBox
        Left = 16
        Top = 24
        Width = 144
        Height = 17
        Caption = '[Model Selector] Override'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
    end
    object GroupBoxDescription: TGroupBox
      Left = 8
      Top = 288
      Width = 288
      Height = 80
      Caption = 'Description for pins without model selector'
      TabOrder = 3
      Visible = False
      object LabelSystemPin: TLabel
        Left = 16
        Top = 23
        Width = 56
        Height = 13
        Caption = 'System Pin:'
      end
      object LabelModelPin: TLabel
        Left = 16
        Top = 51
        Width = 49
        Height = 13
        Caption = 'Model Pin:'
      end
      object EditSystemPin: TEdit
        Left = 80
        Top = 20
        Width = 192
        Height = 21
        TabOrder = 0
        Text = 'System Pin'
      end
      object EditModelPin: TEdit
        Left = 80
        Top = 48
        Width = 192
        Height = 21
        TabOrder = 1
        Text = 'No Model Selector for this Pin'
      end
    end
    object ComboBoxModelSelect: TComboBox
      Left = 392
      Top = 56
      Width = 145
      Height = 21
      TabOrder = 4
      TabStop = False
      Visible = False
      OnChange = ComboBoxModelSelectChange
    end
    object RadioGroupAltiumVersion: TRadioGroup
      Left = 8
      Top = 200
      Width = 288
      Height = 80
      Caption = ' Altium Designer Version'
      TabOrder = 5
      Visible = False
    end
    object RadioButtonRelease: TRadioButton
      Left = 16
      Top = 224
      Width = 272
      Height = 17
      Caption = 'R10 - Faster Calculation, No IBIS Version Modification'
      Checked = True
      TabOrder = 6
      TabStop = True
      Visible = False
    end
    object RadioButtonSummer: TRadioButton
      Left = 16
      Top = 248
      Width = 272
      Height = 17
      Caption = 'S09 - Slower Calculation, Modify IBIS Version Number'
      TabOrder = 7
      Visible = False
    end
    object GroupBoxModelSelectorGroup: TGroupBox
      Left = 8
      Top = 376
      Width = 536
      Height = 280
      Caption = 'Override Model Selectors by Group'
      TabOrder = 8
      Visible = False
      object CheckBoxModelSelectorGroups: TCheckBox
        Left = 8
        Top = 24
        Width = 368
        Height = 17
        Caption = 
          'Override Model Selector for all pins that have same model select' +
          'or name'
        TabOrder = 0
        OnClick = CheckBoxModelSelectorGroupsClick
      end
      object StringGridModelSelectors: TStringGrid
        Left = 2
        Top = 55
        Width = 532
        Height = 223
        Align = alBottom
        ColCount = 3
        DefaultRowHeight = 21
        FixedCols = 0
        RowCount = 10
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goTabs]
        TabOrder = 1
        OnClick = StringGridModelSelectorsClick
        ColWidths = (
          139
          106
          280)
      end
      object ComboBoxModelGroup: TComboBox
        Left = 432
        Top = 24
        Width = 97
        Height = 21
        TabOrder = 2
        TabStop = False
        Visible = False
        OnChange = ComboBoxModelGroupChange
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.ibs'
    Title = 'Save IBIS File'
    Left = 528
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.ibs'
    Title = 'Open IBIS File'
    Left = 488
    Top = 8
  end
end
