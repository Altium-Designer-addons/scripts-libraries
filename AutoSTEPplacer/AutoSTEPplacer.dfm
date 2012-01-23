object FormAutoSTEPplacer: TFormAutoSTEPplacer
  Left = 0
  Top = 0
  Caption = 'STEP Loader'
  ClientHeight = 169
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 34
    Height = 13
    Caption = 'Folder:'
  end
  object Label2: TLabel
    Left = 200
    Top = 50
    Width = 40
    Height = 13
    Caption = 'Up Axis:'
  end
  object EditFolderName: TEdit
    Left = 48
    Top = 13
    Width = 240
    Height = 21
    TabOrder = 0
  end
  object CheckBoxSubfolders: TCheckBox
    Left = 16
    Top = 48
    Width = 105
    Height = 17
    BiDiMode = bdLeftToRight
    Caption = 'Include Subfolders'
    Checked = True
    ParentBiDiMode = False
    State = cbChecked
    TabOrder = 2
  end
  object ComboBoxTop: TComboBox
    Left = 248
    Top = 46
    Width = 40
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 1
    Text = 'Y'
    Items.Strings = (
      'Z'
      'Y')
  end
  object ButtonCancel: TButton
    Left = 216
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = ButtonCancelClick
  end
  object ButtonOK: TButton
    Left = 136
    Top = 128
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = ButtonOKClick
  end
  object CheckBoxSmart: TCheckBox
    Left = 16
    Top = 72
    Width = 152
    Height = 17
    Caption = 'Smart Name Detection'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object CheckBoxDelete: TCheckBox
    Left = 16
    Top = 96
    Width = 152
    Height = 17
    Caption = 'Delete Existing Models'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
end
