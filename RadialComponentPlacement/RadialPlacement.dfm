object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form'
  ClientHeight = 391
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Center_X_Label: TLabel
    Left = 64
    Top = 0
    Width = 45
    Height = 13
    Caption = 'Center_X'
  end
  object Center_Y_Label: TLabel
    Left = 208
    Top = 0
    Width = 42
    Height = 13
    Caption = 'Center X'
  end
  object Start_Angle_Label: TLabel
    Left = 64
    Top = 48
    Width = 54
    Height = 13
    Caption = 'Start Angle'
  end
  object End_Angle_Label: TLabel
    Left = 208
    Top = 48
    Width = 48
    Height = 13
    Caption = 'End Angle'
    Visible = False
  end
  object First_Element_Label: TLabel
    Left = 64
    Top = 144
    Width = 62
    Height = 13
    Caption = 'First Element'
  end
  object Last_Element_Label: TLabel
    Left = 208
    Top = 144
    Width = 61
    Height = 13
    Caption = 'Last Element'
  end
  object Layer_Label: TLabel
    Left = 64
    Top = 192
    Width = 27
    Height = 13
    Caption = 'Layer'
  end
  object Radius_Label: TLabel
    Left = 208
    Top = 192
    Width = 32
    Height = 13
    Caption = 'Radius'
  end
  object Angle_Step_Label: TLabel
    Left = 64
    Top = 248
    Width = 52
    Height = 13
    Caption = 'Angle Step'
  end
  object Angle_Offset_Label: TLabel
    Left = 208
    Top = 248
    Width = 61
    Height = 13
    Caption = 'Angle Offset'
  end
  object First_room_name: TLabel
    Left = 64
    Top = 96
    Width = 81
    Height = 13
    Caption = 'First Room Name'
  end
  object Last_Room_Name: TLabel
    Left = 208
    Top = 96
    Width = 80
    Height = 13
    Caption = 'Last Room Name'
  end
  object bDraw: TButton
    Left = 57
    Top = 300
    Width = 75
    Height = 25
    Caption = 'Draw'
    TabOrder = 11
    OnClick = bDrawClick
  end
  object bClose: TButton
    Left = 200
    Top = 300
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 12
    OnClick = bCloseClick
  end
  object Center_X_TEdit: TEdit
    Left = 56
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object Center_Y_TEdit: TEdit
    Left = 200
    Top = 16
    Width = 112
    Height = 21
    TabOrder = 1
  end
  object Radius_TEdit: TEdit
    Left = 200
    Top = 208
    Width = 112
    Height = 21
    TabOrder = 8
  end
  object Start_Angle_TEdit: TEdit
    Left = 56
    Top = 64
    Width = 120
    Height = 21
    TabOrder = 2
  end
  object End_Angle_TEdit: TEdit
    Left = 200
    Top = 64
    Width = 112
    Height = 21
    TabOrder = 3
    Visible = False
  end
  object First_Element_TEdit: TEdit
    Left = 56
    Top = 160
    Width = 120
    Height = 21
    TabOrder = 6
  end
  object Angle_Offset_TEdit: TEdit
    Left = 200
    Top = 264
    Width = 112
    Height = 21
    TabOrder = 10
  end
  object Angle_Step_TEdit: TEdit
    Left = 56
    Top = 264
    Width = 121
    Height = 21
    TabOrder = 9
  end
  object Last_Element_TEdit: TEdit
    Left = 200
    Top = 160
    Width = 112
    Height = 21
    TabOrder = 7
  end
  object First_Room_Name_TEdit: TEdit
    Left = 56
    Top = 112
    Width = 120
    Height = 21
    TabOrder = 4
  end
  object Last_Room_Name_TEdit: TEdit
    Left = 200
    Top = 112
    Width = 112
    Height = 21
    TabOrder = 5
  end
  object Layer_TEdit: TEdit
    Left = 56
    Top = 208
    Width = 120
    Height = 21
    TabOrder = 13
    TextHint = '"Top Layer" or "Bottom Layer"'
  end
end
