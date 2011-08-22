'----------------------------------------------------
' Draw rounded rectangle spiral track
' Copyright (c) 2005 Mooretronics Pty Ltd
' Melbourne Australia
'----------------------------------------------------
' This script will draw a trace around a user defined rectangle.
' User enters the size of the rectangle to draw tracks around,
' the clearance, track width, track to track clearance and number
' of turns to draw.
' Metric support added
' Mouse select start point added
' Improve dialog, DXP style
' get current layer
' much faster calls now
' grouped undo
' removed Str2Int functions so it can handle non integer values
' fixed metric issue, force change to imperial before placing objects
' make some cosmetic changes
' set dialog to open in middle of screen
' add layer names to Layers list again
'....................................................

' Darren Moore
' May 2005

' Version 0.7

'....................................................

    Dim   Board
    DIM   xm,ym    'mouse coordinate

'-----------------------------------------------------------------------
Sub PlacePlanarTxFormCreate(Sender)

    Set Board = PCBServer.GetCurrentPCBBoard
    If Board is Nothing Then Exit Sub

    If Not Board.ChooseLocation(xm,ym,"Choose PlanarTx start location") Then
    End If

    xm = (xm - Board.XOrigin)/10000               'convert to mils, offset to relative origin
    ym = (ym - Board.YOrigin)/10000               '

    PlacePlanarTxForm.StartX.Text = xm
    PlacePlanarTxForm.StartY.Text = ym

    PlacePlanarTxForm.LayerListBox.Text = Layer2String(Board.CurrentLayer)    'get current layer

    If (Board.DisplayUnit) Then
       PlacePlanarTxForm.Metric.Checked = false
    Else
       PlacePlanarTxForm.Metric.Checked = true                   ' read 0 = Metric, 1 = Inch
    End If

    SizeX.Color = RGB(236, 233, 216) '&HD8E9EC&
    SizeY.Color = RGB(236, 233, 216)
    StartX.Color = RGB(236, 233, 216)
    StartY.Color = RGB(236, 233, 216)
    Spacing.Color = RGB(236, 233, 216)
    ClearanceInside.Color = RGB(236, 233, 216)
    SizeRadius.Color = RGB(236, 233, 216)
    TrackWidth.Color = RGB(236, 233, 216)
    Loops.Color = RGB(236, 233, 216)

End Sub
'-----------------------------------------------------------------------

Sub XPBitBtn1Click(Sender)

    DIM   ArcX1, ArcX2, ArcX3, ArcX4, ArcY1, ArcY2, ArcY3, ArcY4, NowX, NowY, RadiusNow
    DIM   Length, Height, Radius, TrackSpacing, Width, Windings, Layer, Track, Arc

' force values to imperial
       Metric.Checked = false

    Width       =  (TrackWidth.Text)
    Radius      =  (SizeRadius.Text)
    Clearance   =  ((ClearanceInside.Text) * 1)
    Length      =  ((SizeX.Text) - (Radius * 2))
    Height      =  ((SizeY.Text) - (Radius * 2))
    TrackSpacing   =  (Spacing.Text)
    Layer       =  String2Layer(LayerListBox.Text)
    Windings    =  ((Loops.Text) * 1)
    NowX     =  (StartX.Text + (Board.XOrigin/10000))
    NowY     =  (StartY.Text + (Board.YOrigin/10000))


    ArcX1 = NowX
    ArcY1 = (NowY - Radius - Clearance)
    ArcX2 = (NowX + Length)
    ArcY2 = ArcY1
    ArcX3 = ArcX2
    ArcY3 = (ArcY2 - Height)
    ArcX4 = ArcX1
    ArcY4 = ArcY3

    PlacePlanarTxForm.Hide

    RadiusNow = (Radius + Clearance)
    ArcY1 = (ArcY1 + TrackSpacing + Width)

    PCBServer.PreProcess

    FOR p0 = 1 TO Windings

    Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
    Track.Width    = MilsToCoord(Width)
    Track.X1       = MilsToCoord(NowX)
    Track.Y1       = MilsToCoord(NowY)
    Track.X2       = MilsToCoord(NowX + Length)
    Track.Y2       = MilsToCoord(NowY)
    Track.Layer    = Layer
    Board.AddPCBObject(Track)

    NowX = (NowX + (Length - Radius))

    Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
    Arc.LineWidth  = MilsToCoord(Width)
    Arc.XCenter    = MilsToCoord(ArcX2)
    Arc.YCenter    = MilsToCoord(ArcY2)
    Arc.Radius     = MilsToCoord(RadiusNow)
    Arc.Layer      = Layer
    Arc.StartAngle = 0
    Arc.EndAngle   = 90
    Board.AddPCBObject(Arc)

    NowX = ArcX2 + RadiusNow
    NowY = ArcY2

    Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
    Track.Width    = MilsToCoord(Width)
    Track.X1       = MilsToCoord(NowX)
    Track.Y1       = MilsToCoord(NowY - Height)
    Track.X2       = MilsToCoord(NowX)
    Track.Y2       = MilsToCoord(NowY)
    Track.Layer    = Layer
    Board.AddPCBObject(Track)

    Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
    Arc.LineWidth  = MilsToCoord(Width)
    Arc.XCenter    = MilsToCoord(ArcX3)
    Arc.YCenter    = MilsToCoord(ArcY3)
    Arc.Radius     = MilsToCoord(RadiusNow)
    Arc.Layer      = Layer
    Arc.StartAngle = 270
    Arc.EndAngle   = 0
    Board.AddPCBObject(Arc)

    NowX = ArcX3
    NowY = ArcY3 - RadiusNow

    Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
    Track.Width    = MilsToCoord(Width)
    Track.X1       = MilsToCoord(NowX - Length)
    Track.Y1       = MilsToCoord(NowY)
    Track.X2       = MilsToCoord(NowX)
    Track.Y2       = MilsToCoord(NowY)
    Track.Layer    = Layer
    Board.AddPCBObject(Track)

    Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
    Arc.LineWidth  = MilsToCoord(Width)
    Arc.XCenter    = MilsToCoord(ArcX4)
    Arc.YCenter    = MilsToCoord(ArcY4)
    Arc.Radius     = MilsToCoord(RadiusNow)
    Arc.Layer      = Layer
    Arc.StartAngle = 180
    Arc.EndAngle   = 270
    Board.AddPCBObject(Arc)

    NowX = ArcX4 - RadiusNow
    NowY = ArcY4

    Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default)
    Track.Width    = MilsToCoord(Width)
    Track.X1       = MilsToCoord(NowX)
    Track.Y1       = MilsToCoord(NowY + Height + TrackSpacing + Width)
    Track.X2       = MilsToCoord(NowX)
    Track.Y2       = MilsToCoord(NowY)
    Track.Layer    = Layer
    Board.AddPCBObject(Track)

    Arc            = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
    Arc.LineWidth  = MilsToCoord(Width)
    Arc.XCenter    = MilsToCoord(ArcX1)
    Arc.YCenter    = MilsToCoord(ArcY1)
    Arc.Radius     = MilsToCoord(RadiusNow)
    Arc.Layer      = Layer
    Arc.StartAngle = 90
    Arc.EndAngle   = 180
    Board.AddPCBObject(Arc)

    NowX = ArcX1
    NowY = ArcY1 + RadiusNow

    RadiusNow = RadiusNow + TrackSpacing + Width

    NEXT

    Pcbserver.PostProcess
    ' Refresh PCB workspace.
    ResetParameters
    Call AddStringParameter("Action", "Redraw")
    RunProcess("PCB:Zoom")

  Close
End Sub

'-----------------------------------------------------------------------
Sub MetricClick(Sender)
    If    Metric.Checked Then
          StartX.Text = (StartX.Text / 39.37)
          StartY.Text = (StartY.Text / 39.37)
          SizeX.Text = (SizeX.Text / 39.37)
          SizeY.Text = (SizeY.Text / 39.37)
          Spacing.Text = (Spacing.Text / 39.37)
          ClearanceInside.Text = (ClearanceInside.Text / 39.37)
          SizeRadius.Text = (SizeRadius.Text / 39.37)
          TrackWidth.Text = (TrackWidth.Text / 39.37)
    Else
          StartX.Text = (StartX.Text * 39.37)
          StartY.Text = (StartY.Text * 39.37)
          SizeX.Text = (SizeX.Text * 39.37)
          SizeY.Text = (SizeY.Text * 39.37)
          Spacing.Text = (Spacing.Text * 39.37)
          ClearanceInside.Text = (ClearanceInside.Text * 39.37)
          SizeRadius.Text = (SizeRadius.Text * 39.37)
          TrackWidth.Text = (TrackWidth.Text * 39.37)
    End If

End Sub

'-----------------------------------------------------------------------
Sub Main
    PlacePlanarTxForm.showmodal
End Sub

'-----------------------------------------------------------------------
Sub XPBitBtn2Click(Sender)
    close
End Sub

