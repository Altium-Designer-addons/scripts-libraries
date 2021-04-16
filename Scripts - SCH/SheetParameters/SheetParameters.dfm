object SchParm: TSchParm
  Left = 44
  Top = 48
  Hint = 
    'Ensure you are using the latest template in all of your schemati' +
    'cs before continuing. This menu will update the system parameter' +
    's in your currently selected project'
  Caption = 'Enter your new schematic parameters'
  ClientHeight = 326
  ClientWidth = 621
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = SchParmActivate
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 24
    Top = 68
    Width = 84
    Height = 12
    Caption = 'Document number'
  end
  object Label2: TLabel
    Left = 24
    Top = 20
    Width = 70
    Height = 12
    Caption = 'Current project'
  end
  object ProjName: TLabel
    Left = 120
    Top = 20
    Width = 24
    Height = 12
    Caption = 'None'
    ParentShowHint = False
    ShowHint = False
  end
  object Label3: TLabel
    Left = 24
    Top = 92
    Width = 37
    Height = 12
    Caption = 'Revision'
  end
  object Label4: TLabel
    Left = 24
    Top = 44
    Width = 18
    Height = 12
    Caption = 'Title'
  end
  object Label5: TLabel
    Left = 24
    Top = 140
    Width = 23
    Height = 12
    Caption = 'Date '
  end
  object Label6: TLabel
    Left = 24
    Top = 188
    Width = 32
    Height = 12
    Caption = 'Author'
  end
  object Label7: TLabel
    Left = 24
    Top = 284
    Width = 61
    Height = 12
    Caption = 'Approved by'
  end
  object Label8: TLabel
    Left = 348
    Top = 188
    Width = 52
    Height = 12
    Caption = 'Total sheets'
  end
  object Label11: TLabel
    Left = 24
    Top = 260
    Width = 55
    Height = 12
    Caption = 'Checked by'
  end
  object Label12: TLabel
    Left = 348
    Top = 116
    Width = 36
    Height = 12
    Caption = 'Address'
  end
  object Label13: TLabel
    Left = 348
    Top = 44
    Width = 71
    Height = 12
    Caption = 'Company name'
  end
  object Label14: TLabel
    Left = 348
    Top = 68
    Width = 57
    Height = 12
    Caption = 'Organization'
  end
  object Label15: TLabel
    Left = 24
    Top = 164
    Width = 21
    Height = 12
    Caption = 'Time'
  end
  object Label16: TLabel
    Left = 24
    Top = 212
    Width = 45
    Height = 12
    Caption = 'Drawn by'
  end
  object Label17: TLabel
    Left = 24
    Top = 236
    Width = 40
    Height = 12
    Caption = 'Engineer'
  end
  object Label18: TLabel
    Left = 24
    Top = 116
    Width = 63
    Height = 12
    Caption = 'Modified date '
  end
  object Label9: TLabel
    Left = 348
    Top = 92
    Width = 64
    Height = 12
    Caption = 'Drawing Type'
  end
  object DocumentNumber: TEdit
    Left = 120
    Top = 66
    Width = 84
    Height = 20
    Hint = 'Obtain this number from Jako. '
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    Text = '*'
  end
  object OkButton: TButton
    Left = 438
    Top = 276
    Width = 66
    Height = 24
    Caption = 'Ok'
    TabOrder = 1
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 534
    Top = 276
    Width = 66
    Height = 24
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
  object PCBrevision: TEdit
    Left = 120
    Top = 90
    Width = 36
    Height = 20
    Hint = 'Obtain this number from Jako. '
    ParentShowHint = False
    ShowHint = False
    TabOrder = 3
    Text = '*'
  end
  object PCBtitle: TEdit
    Left = 120
    Top = 42
    Width = 192
    Height = 20
    ParentShowHint = False
    ShowHint = False
    TabOrder = 4
    Text = '*'
  end
  object DocDate: TEdit
    Left = 120
    Top = 138
    Width = 60
    Height = 20
    Hint = 'Date the CAD design commenced, in Mon YY form'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 5
    Text = '*'
  end
  object Author: TEdit
    Left = 120
    Top = 186
    Width = 60
    Height = 20
    ParentShowHint = False
    ShowHint = False
    TabOrder = 6
    Text = '*'
  end
  object ApprovedBy: TEdit
    Left = 120
    Top = 282
    Width = 60
    Height = 20
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    Text = '*'
  end
  object Totalsheets: TEdit
    Left = 437
    Top = 186
    Width = 25
    Height = 20
    Hint = 'Total number of schematic sheets in the design'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    Text = '*'
  end
  object CheckedBy: TEdit
    Left = 120
    Top = 258
    Width = 60
    Height = 20
    Hint = 'Initials of the person who designed this PCB '
    ParentShowHint = False
    ShowHint = False
    TabOrder = 9
    Text = '*'
  end
  object Address1: TEdit
    Left = 437
    Top = 90
    Width = 162
    Height = 20
    ParentShowHint = False
    ShowHint = False
    TabOrder = 10
    Text = '*'
  end
  object Address2: TEdit
    Left = 437
    Top = 114
    Width = 162
    Height = 20
    ParentShowHint = False
    ShowHint = False
    TabOrder = 11
    Text = '*'
  end
  object Address3: TEdit
    Left = 437
    Top = 138
    Width = 162
    Height = 20
    ParentShowHint = False
    ShowHint = False
    TabOrder = 12
    Text = '*'
  end
  object Address4: TEdit
    Left = 437
    Top = 162
    Width = 162
    Height = 20
    ParentShowHint = False
    ShowHint = False
    TabOrder = 13
    Text = '*'
  end
  object CompanyName: TEdit
    Left = 437
    Top = 42
    Width = 162
    Height = 20
    ParentShowHint = False
    ShowHint = False
    TabOrder = 14
    Text = '*'
  end
  object Organization: TEdit
    Left = 437
    Top = 66
    Width = 162
    Height = 20
    Hint = 'Initials of the person who captured the schematic'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 15
    Text = '*'
  end
  object DocTime: TEdit
    Left = 120
    Top = 162
    Width = 60
    Height = 20
    Hint = 'orm'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 16
    Text = '*'
  end
  object DrawnBy: TEdit
    Left = 120
    Top = 210
    Width = 60
    Height = 20
    ParentShowHint = False
    ShowHint = False
    TabOrder = 17
    Text = '*'
  end
  object Engineer: TEdit
    Left = 120
    Top = 234
    Width = 60
    Height = 20
    ParentShowHint = False
    ShowHint = False
    TabOrder = 18
    Text = '*'
  end
  object ModifiedDate: TEdit
    Left = 120
    Top = 114
    Width = 60
    Height = 20
    ParentShowHint = False
    ShowHint = False
    TabOrder = 19
    Text = '*'
  end
end
