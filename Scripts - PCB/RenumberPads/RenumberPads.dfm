object RenumberPads: TRenumberPads
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Renumber Pads'
  ClientHeight = 145
  ClientWidth = 298
  Color = clBtnFace
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poScreenCenter
  OnCreate = RenumberPadsCreate
  FormKind = fkModal
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 280
    Height = 128
    Caption = 'Renumber setup'
    TabOrder = 0
    object lblFirstPadIndex: TLabel
      Left = 32
      Top = 29
      Width = 73
      Height = 13
      Caption = 'First Pad Index'
    end
    object lblPadIncrement: TLabel
      Left = 32
      Top = 60
      Width = 101
      Height = 13
      Caption = 'Pad Index Increment'
    end
    object edFirstPadNumber: TEdit
      Left = 160
      Top = 24
      Width = 81
      Height = 21
      Alignment = taRightJustify
      NumbersOnly = True
      TabOrder = 0
      Text = '1'
    end
    object btnOK: TButton
      Left = 40
      Top = 88
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 168
      Top = 88
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 3
      OnClick = btnCancelClick
    end
    object edPadIncrement: TEdit
      Left = 160
      Top = 56
      Width = 81
      Height = 21
      Alignment = taRightJustify
      NumbersOnly = True
      TabOrder = 1
      Text = '1'
    end
  end
end
