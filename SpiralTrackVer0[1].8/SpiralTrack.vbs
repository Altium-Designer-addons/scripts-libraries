'----------------------------------------------------
' Draw rounded spiral track
' Copyright (c) 2005 Mooretronics Pty Ltd
' Melbourne Australia
'----------------------------------------------------

'This script will draw a trace around a user defined Via,
'based on Via size.

'The via and Gap set the size of the first two Arc's, then
'the Gap and width set the following Arc's

'Each Arc gets a little bigger, so that after 4 Arc's it has
'grown enough to maintain the Gap.

'The resolution could be improved by using smaller Arc's (less
'then 90°, but four seems to work well.

' Improve dialog, DXP style
' Metric support added
' Fixed Via start/finish layer bug also
' Mouse select start point added
' Change the way it draws objects, much faster now,
'  and its a grouped undo now also.
' Gets current layer now also
'....................................................

' Darren Moore
' Jun 2005

' Version 0.8
' Would like to integrate with planar script, and maybe others

'....................................................

    Dim   Board
    DIM   xm,ym    'mouse coordinate

'-----------------------------------------------------------------------
Sub PlaceSpiralTrackFormCreate(Sender)

    Set Board = PCBServer.GetCurrentPCBBoard
    If Board is Nothing Then Exit Sub

    If Not Board.ChooseLocation(xm,ym,"Choose Spiral Track start location") Then
    End If

    xm = xm/10000               'convert to mils
    ym = ym/10000               '

    PlaceSpiralTrackForm.StartX.Text = xm
    PlaceSpiralTrackForm.StartY.Text = ym

   PlaceSpiralTrackForm.LayerListBox.Text = Layer2String(Board.CurrentLayer)    'get current layer

    If (Board.DisplayUnit) Then
       PlaceSpiralTrackForm.Metric.Checked = false
    Else
       PlaceSpiralTrackForm.Metric.Checked = true                   ' read 0 = Metric, 1 = Inch
    End If

    StartX.Color = RGB(236, 233, 216)
    StartY.Color = RGB(236, 233, 216)
    Spacing.Color = RGB(236, 233, 216)
    TrackWidth.Color = RGB(236, 233, 216)
    Loops.Color = RGB(236, 233, 216)
    ViaSize.Color = RGB(236, 233, 216)
    ViaHoleSize.Color = RGB(236, 233, 216)

End Sub
'-----------------------------------------------------------------------

Sub XPBitBtn1Click(Sender)

    DIM   ArcX1, ArcX2, ArcX3, ArcX4, ArcY1, ArcY2, ArcY3, ArcY4
    DIM   Layer, Radius, Spacing, Width, Offset, Temp
    Dim   Via, Track, Arc
    Dim   MouseX, MouseY

    If (Board.DisplayUnit) Then
       Metric.Checked = false
    Else
       Metric.Checked = true                   ' read 0 = Metric, 1 = Inch
    End If

    Width = TrackWidth.Text
    Spacing = (PlaceSpiralTrackForm.Spacing.Text)
    Temp = ((Spacing/1) + (Width/1))
    Offset = Temp/4
    Layer = String2Layer(LayerListBox.Text)
    MouseX = StartX.Text
    MouseY = StartY.Text

    ArcX1 = StartX.Text
    ArcY1 = StartY.Text + Offset
    ArcX2 = StartX.Text
    ArcY2 = StartY.Text
    ArcX3 = StartX.Text - Offset
    ArcY3 = StartY.Text
    ArcX4 = StartX.Text - Offset
    ArcY4 = StartY.Text + Offset

    Radius = (ViaSize.Text/2) + (Spacing/1) + (Width/2)

    PlaceSpiralTrackForm.Hide

    PCBServer.PreProcess

'Place Via
    Via           = PCBServer.PCBObjectFactory(eViaObject, eNoDimension, eCreate_Default)
    Via.X         = MilsToCoord(MouseX)
    Via.Y         = MilsToCoord(MouseY)
    Via.Size      = MilsToCoord(ViaSize.Text)
    Via.HoleSize  = MilsToCoord(ViaHoleSize.Text)
    Via.LowLayer  = eTopLayer
    Via.HighLayer = eBottomLayer
    Board.AddPCBObject(Via)

'Place 1st Arc to met start of sprial
       Arc = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
       Arc.XCenter    = MilsToCoord(StartX.Text/1)
       Arc.YCenter    = MilsToCoord((StartY.Text/1) + Radius/2)
       Arc.Layer      = Layer 'eTopLayer
       Arc.LineWidth  = MilsToCoord(Width/1)
       Arc.Radius     = MilsToCoord(Radius/2)
       Arc.StartAngle = 90
       Arc.EndAngle   = 270
       Board.AddPCBObject(Arc)

' LOOP once for every turn
    FOR p0 = 1 TO Loops.Text

'Place arc quadrant
       Arc = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
       Arc.LineWidth  = MilsToCoord(Width/1)
       Arc.XCenter    = MilsToCoord(ArcX2/1)
       Arc.YCenter    = MilsToCoord(ArcY2/1)
       Arc.Radius     = MilsToCoord(Radius/1)
       Arc.Layer      = Layer 'eTopLayer
       Arc.StartAngle = 0
       Arc.EndAngle   = 90
       Board.AddPCBObject(Arc)

       Radius = (Radius + Offset)

       Arc = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
       Arc.LineWidth  = MilsToCoord(Width/1)
       Arc.XCenter    = MilsToCoord(ArcX3/1)
       Arc.YCenter    = MilsToCoord(ArcY3/1)
       Arc.Radius     = MilsToCoord(Radius/1)
       Arc.Layer      = Layer 'eTopLayer
       Arc.StartAngle = 270
       Arc.EndAngle   = 0
       Board.AddPCBObject(Arc)

       Radius = (Radius + Offset)

       Arc = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
       Arc.LineWidth  = MilsToCoord(Width/1)
       Arc.XCenter    = MilsToCoord(ArcX4/1)
       Arc.YCenter    = MilsToCoord(ArcY4/1)
       Arc.Radius     = MilsToCoord(Radius/1)
       Arc.Layer      = Layer 'eTopLayer
       Arc.StartAngle = 180
       Arc.EndAngle   = 270
       Board.AddPCBObject(Arc)

       Radius = (Radius + Offset)

       Arc = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default)
       Arc.LineWidth  = MilsToCoord(Width/1)
       Arc.XCenter    = MilsToCoord(ArcX1/1)
       Arc.YCenter    = MilsToCoord(ArcY1/1)
       Arc.Radius     = MilsToCoord(Radius/1)
       Arc.Layer      = Layer 'eTopLayer
       Arc.StartAngle = 90
       Arc.EndAngle   = 180
       Board.AddPCBObject(Arc)

       Radius = (Radius + Offset)

    NEXT

   PCBServer.PostProcess
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
          ViaSize.Text = (ViaSize.Text / 39.37)
          ViaHoleSize.Text = (ViaHoleSize.Text / 39.37)
          Spacing.Text = (Spacing.Text / 39.37)
          TrackWidth.Text = (TrackWidth.Text / 39.37)
    Else
          StartX.Text = (StartX.Text * 39.37)
          StartY.Text = (StartY.Text * 39.37)
          ViaSize.Text = (ViaSize.Text * 39.37)
          ViaHoleSize.Text = (ViaHoleSize.Text * 39.37)
          Spacing.Text = (Spacing.Text * 39.37)
          TrackWidth.Text = (TrackWidth.Text * 39.37)
    End If

End Sub

'-----------------------------------------------------------------------
Sub Main
    PlaceSpiralTrackForm.showmodal
End Sub

'-----------------------------------------------------------------------
Sub XPBitBtn2Click(Sender)
    close
End Sub

