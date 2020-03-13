'----------------------------------------------------
' Draw Rectangle
' Copyright (c) 2008 Mooretronics Pty Ltd
' Melbourne Australia
'----------------------------------------------------
' This script will draw a rectangle.
' User enters the size of the rectangle to draw,
' Metric support added
' Mouse select centre point
' grouped undo
' added multi reference points
' added via at reference option
' fixed incorrect scale rectangle drawn with metric checked.
' added a easy place in the code to set the defaults
' changed Ref location to use the relative origin
' added workaround for PCB Libraries                                        (Ben Benardos 1/10/2008)
' added workaround to fill Layer TComboBox                                  (Ben Benardos 1/10/2008)
' refactored coordinate handling                                            (Ben Benardos 1/10/2008)
' fixed via placement in library                                            (Ben Benardos 3/10/2008)
' fixed garbage values when board is in metric mode                         (Ben Benardos 3/10/2008)
' v92 Get user names, instead of system names for Layers List box           (Darren Moore 5/10/2008)
' added midpoints for rectangles                                            (DS           03/04/2008)
' changed default via size, see "set default via sizes"                     (DS 03/05/08)
' make layer box report user assigned name                                  (Ben Benardos 20/11/2008)
' fix crash in Altium Designer Summer 09                                    (Ben Benardos 23/07/2009)
' v96 add support for Mechanical 17 - Mechanical 32 layers                  (Ben Benardos 06/07/2010)
' v96 change LayerListBox.Style to csDropDownList                           (Ben Benardos 06/07/2010)
' v97 added define board shape checkbox                                     (EA           04/18/2012)
' v97 added selected rectangle checkbox                                     (EA           04/18/2012)
' v97 changed so that the form doesn't pop up twice                         (EA           04/18/2012)
'....................................................
'
' Version 0.97

' still to do

'  have it save the last used values to a ini file
' create bitmap button image

'....................................................

    Dim   Board, PCB_Library
    DIM   xm,ym    'mouse coordinate
    DIm IsMetric
    'Dim ViaDiam
    'Dim ViaHoleSize
    Dim ListBoxIndexToLayerArray

Sub SetupLayerBox(ListBox)
    ListBoxIndexToLayerArray = TList.Create
    ListBox.Items.Clear

    Dim LayerIterator
    LayerIterator = Board.LayerIterator
    While LayerIterator.Next
          ListBoxIndexToLayerArray.Add(LayerIterator.Layer)
          ListBox.Items.Add(LayerIterator.LayerObject.Name)

          If LayerIterator.Layer = Board.CurrentLayer Then
              ListBox.ItemIndex = ListBox.Items.Count - 1
          End If
    WEnd
End Sub

'-----------------------------------------------------------------------
Sub PlaceRectangleFormCreate(Sender)

    Set Board = PCBServer.GetCurrentPCBBoard
    Set PCB_Library = PCBServer.GetCurrentPCBLibrary

    If Board is Nothing Then Exit Sub

    ' get reference location from user with mouse
    If Not Board.ChooseLocation(xm,ym,"Choose rectangle reference location") Then
    End If

    xm = (xm - Board.XOrigin)               'offset to relative origin
    ym = (ym - Board.YOrigin)               '

    IsMetric = False
    PlaceRectangleForm.StartX.Text = CoordToMils(xm)
    PlaceRectangleForm.StartY.Text = CoordToMils(ym)

' **** SET DEFAULT VALUES HERE ***

    PlaceRectangleForm.TrackWidth.Text = 7
    PlaceRectangleForm.SizeX.Text = 500
    PlaceRectangleForm.SizeY.Text = 400

    SetupLayerBox(PlaceRectangleForm.LayerListBox)

    If (Board.DisplayUnit) Then
       PlaceRectangleForm.Metric.Checked = False                ' read 0 = Metric, 1 = Inch
    Else
        PlaceRectangleForm.Metric.Checked  = True
    End If

    SizeX.Color = RGB(236, 233, 216) '&HD8E9EC&
    SizeY.Color = RGB(236, 233, 216)
    StartX.Color = RGB(236, 233, 216)
    StartY.Color = RGB(236, 233, 216)
    TrackWidth.Color = RGB(236, 233, 216)
End Sub
'-----------------------------------------------------------------------

Sub AddPCBObject(Primitive)
    If Board.IsLibrary() And Not (PCB_Library Is Nothing) Then
      ' work around for bug in Altium Designer Summer 08
       PCB_Library.GetState_CurrentComponent.TransferAllPrimitivesBackFromBoard

       If Primitive.ObjectID = eTrackObject Then
          Primitive.X1 = Primitive.X1 - Board.XOrigin
          Primitive.X2 = Primitive.X2 - Board.XOrigin
          Primitive.Y1 = Primitive.Y1 - Board.YOrigin
          Primitive.Y2 = Primitive.Y2 - Board.YOrigin
       End If

       If Primitive.ObjectID = eViaObject Then
          Primitive.X = Primitive.X - Board.XOrigin
          Primitive.Y = Primitive.Y - Board.YOrigin
       End If

       PCB_Library.GetState_CurrentComponent.AddPCBObject(Primitive)
       PCB_Library.GetState_CurrentComponent.TransferAllPrimitivesOntoBoard
    Else
       Board.AddPCBObject(Primitive)
    End If
End Sub

Function GetCoord(Str)
    Dim Units, Coords
    If IsMetric Then
       Units = eMetric
    Else
        Units = eImperial
    End If

    Call StringToCoordUnit(Str, Coords, Units)
    GetCoord = Coords
End Function

Sub XPBitBtn1Click(Sender)

    DIM   Length, Height, Width, RefX, RefY, Layer, Track, Units

    Width    = GetCoord(TrackWidth.Text)
    Length   = GetCoord(SizeX.Text)
    Height   = GetCoord(SizeY.Text)
    Layer    = ListBoxIndexToLayerArray.Items(LayerListBox.ItemIndex)
    RefX     = GetCoord(StartX.Text) + Board.XOrigin
    RefY     = GetCoord(StartY.Text) + Board.YOrigin

    'from ref point set first rectangle segemnt

    CentreX      =   RefX
    CentreY      =   RefY

    If (SetRefPoint1.Checked) Then
       CentreX   =   RefX + Round(Length/2)
       CentreY   =   RefY - Round(Height/2)
    End If

    If (SetRefPoint2.Checked) Then
       CentreX   =   RefX - Round(Length/2)
       CentreY   =   RefY - Round(Height/2)
    End If

    If (SetRefPoint3.Checked) Then
       CentreX   =   RefX + Round(Length/2)
       CentreY   =   RefY + Round(Height/2)
    End If

    If (SetRefPoint4.Checked) Then
       CentreX   =   RefX - Round(Length/2)
       CentreY   =   RefY + Round(Height/2)
    End If

    'ds
    If (SetRefPoint6.Checked) Then
       CentreY   =   RefY + Round(Height/2)
    End If

    If (SetRefPoint7.Checked) Then
       CentreY   =   RefY - Round(Height/2)
    End If

    If (SetRefPoint8.Checked) Then
       CentreX   =   RefX + Round(Length/2)
    End If

    If (SetRefPoint9.Checked) Then
       CentreX   =   RefX - Round(Length/2)
    End If

    PlaceRectangleForm.Hide

    PCBServer.PreProcess

    Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
    Track.Width    = Width
    Track.X1       = CentreX - Round(Length/2)
    Track.Y1       = CentreY + Round(Height/2)
    Track.X2       = CentreX + Round(Length/2)
    Track.Y2       = CentreY + Round(Height/2)
    Track.Layer    = Layer
    AddPCBObject(Track)
    Track.Selected = Selected.Checked Or BoardShape.Checked

    Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
    Track.Width    = Width
    Track.X1       = CentreX + Round(Length/2)
    Track.Y1       = CentreY + Round(Height/2)
    Track.X2       = CentreX + Round(Length/2)
    Track.Y2       = CentreY - Round(Height/2)
    Track.Layer    = Layer
    AddPCBObject(Track)
    Track.Selected = Selected.Checked Or BoardShape.Checked

    Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
    Track.Width    = Width
    Track.X1       = CentreX + Round(Length/2)
    Track.Y1       = CentreY - Round(Height/2)
    Track.X2       = CentreX - Round(Length/2)
    Track.Y2       = CentreY - Round(Height/2)
    Track.Layer    = Layer
    AddPCBObject(Track)
    Track.Selected = Selected.Checked Or BoardShape.Checked

    Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
    Track.Width    = Width
    Track.X1       = CentreX - Round(Length/2)
    Track.Y1       = CentreY - Round(Height/2)
    Track.X2       = CentreX - Round(Length/2)
    Track.Y2       = CentreY + Round(Height/2)
    Track.Layer    = Layer
    AddPCBObject(Track)
    Track.Selected = Selected.Checked Or BoardShape.Checked


    If AddVia.Checked Then

    'Place Via
        Via           = PCBServer.PCBObjectFactory(eViaObject, eNoDimension, eCreate_Default)
        Via.X         = RefX
        Via.Y         = RefY

        ' set default via sizes here
        Via.Size      = MilsToCoord(23)
        Via.HoleSize  = MilsToCoord(11)
        Via.LowLayer  = eTopLayer
        Via.HighLayer = eBottomLayer
        AddPCBObject(Via)
    End If

    If BoardShape.Checked then
        ResetParameters
        Call AddStringParameter("Mode", "BOARDOUTLINE_FROM_SEL_PRIMS")
        RunProcess("PCB:PlaceBoardOutline")
    End If

    Pcbserver.PostProcess
    ' Refresh PCB workspace.
    ResetParameters
    Call AddStringParameter("Action", "Redraw")
    RunProcess("PCB:Zoom")

  Close
End Sub

Function ConvertString(Str, WantMetric)
    If WantMetric Then
       ConvertString = FormatFloat("0.###", CoordToMMs(GetCoord(Str)))
    Else
       ConvertString = FormatFloat("0.###", CoordToMils(GetCoord(Str)))
    End If
End Function

'-----------------------------------------------------------------------
Sub MetricClick(Sender)
    If IsMetric <> Metric.Checked Then
        WantMetric = Metric.Checked
        StartX.Text = ConvertString(StartX.Text, WantMetric)
        StartY.Text = ConvertString(StartY.Text, WantMetric)
        SizeX.Text =  ConvertString(SizeX.Text, WantMetric)
        SizeY.Text =  ConvertString(SizeY.Text, WantMetric)
        TrackWidth.Text = ConvertString(TrackWidth.Text, WantMetric)

        IsMetric = Metric.Checked
    End If
End Sub

'-----------------------------------------------------------------------
Sub Main
    'PlaceRectangleForm.showmodal
End Sub

'-----------------------------------------------------------------------
Sub XPBitBtn2Click(Sender)
    close
End Sub

