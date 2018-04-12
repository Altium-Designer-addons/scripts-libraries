object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form'
  ClientHeight = 489
  ClientWidth = 514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 17
  object Center_X_Label: TLabel
    Left = 80
    Top = 0
    Width = 57
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Center_X'
  end
  object Center_Y_Label: TLabel
    Left = 260
    Top = 0
    Width = 53
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Center X'
  end
  object Start_Angle_Label: TLabel
    Left = 80
    Top = 60
    Width = 67
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Start Angle'
  end
  object End_Angle_Label: TLabel
    Left = 260
    Top = 60
    Width = 61
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'End Angle'
    Visible = False
  end
  object First_Element_Label: TLabel
    Left = 80
    Top = 180
    Width = 78
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'First Element'
  end
  object Last_Element_Label: TLabel
    Left = 260
    Top = 180
    Width = 78
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Last Element'
  end
  object Layer_Label: TLabel
    Left = 80
    Top = 240
    Width = 34
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Layer'
  end
  object Radius_Label: TLabel
    Left = 260
    Top = 240
    Width = 40
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Radius'
  end
  object Angle_Step_Label: TLabel
    Left = 80
    Top = 310
    Width = 65
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Angle Step'
  end
  object Angle_Offset_Label: TLabel
    Left = 260
    Top = 310
    Width = 73
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Angle Offset'
  end
  object First_room_name: TLabel
    Left = 80
    Top = 120
    Width = 105
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'First Room Name'
  end
  object Last_Room_Name: TLabel
    Left = 260
    Top = 120
    Width = 105
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Last Room Name'
  end
  object bDraw: TButton
    Left = 71
    Top = 375
    Width = 94
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Draw'
    TabOrder = 11
    OnClick = bDrawClick
  end
  object bClose: TButton
    Left = 250
    Top = 375
    Width = 94
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Close'
    TabOrder = 12
    OnClick = bCloseClick
  end
  object Center_X_TEdit: TEdit
    Left = 70
    Top = 20
    Width = 151
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 0
  end
  object Center_Y_TEdit: TEdit
    Left = 250
    Top = 20
    Width = 140
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 1
  end
  object Radius_TEdit: TEdit
    Left = 250
    Top = 260
    Width = 140
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 8
  end
  object Start_Angle_TEdit: TEdit
    Left = 70
    Top = 80
    Width = 150
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 2
  end
  object End_Angle_TEdit: TEdit
    Left = 250
    Top = 80
    Width = 140
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 3
    Visible = False
  end
  object First_Element_TEdit: TEdit
    Left = 70
    Top = 200
    Width = 150
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 6
  end
  object Angle_Offset_TEdit: TEdit
    Left = 250
    Top = 330
    Width = 140
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 10
  end
  object Angle_Step_TEdit: TEdit
    Left = 70
    Top = 330
    Width = 151
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 9
  end
  object Last_Element_TEdit: TEdit
    Left = 250
    Top = 200
    Width = 140
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 7
  end
  object First_Room_Name_TEdit: TEdit
    Left = 70
    Top = 140
    Width = 150
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 4
  end
  object Last_Room_Name_TEdit: TEdit
    Left = 250
    Top = 140
    Width = 140
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 5
  end
  object Layer_TEdit: TEdit
    Left = 70
    Top = 260
    Width = 150
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 13
    TextHint = '"Top Layer" or "Bottom Layer"'
  end
end
